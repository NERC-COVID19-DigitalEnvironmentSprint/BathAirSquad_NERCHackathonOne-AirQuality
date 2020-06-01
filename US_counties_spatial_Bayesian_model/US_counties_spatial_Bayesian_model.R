## ---------------------------
##
## Script name: Spatial_Bayes_US_infection_rate
##
## Purpose of script: Run Bayesian spatial model for COVID infection rate on US data
##
## Authors: Piotr Morawiecki, Laura Oporto, Dan Burrows, Tina Zhou
##
## Date Created: 2020-06-01
##
## Copyright (c) Piotr Morawiecki, 2020
## Email: pwm27@bath.ac.uk
##
## ---------------------------
##
## Notes:
##   
##   Code computes COVID-19 infection and fatality rate for US counties and merges them with
##   demographic data (counties population density) and environmental descriptors (CO, NO2, Ozone, PM10, PM2.5, SO2, AQI).
##   Then the data are processed with spatial Bayesian model, which posteriors distributions are computed using INLA package.
##   
## ToDo:
##   
##   - add median income and demographic data
##   - improve model used (currently default)
##   - generate (possibly interactive) maps
##
## ---------------------------


# Load/install the following packages:
# stringr  # required for str_replace
# sp       # required for SpatialPointsDataFrame
# rgeos    # required for gDelaunayTriangulation
# spdep    # required for poly2nb
# INLA     # required for nb2INLA, inla.read.graph, inla

if("stringr" %in% rownames(installed.packages())==FALSE){install.packages("stringr"); require(stringr)}else{require(stringr)}
if("sp" %in% rownames(installed.packages())==FALSE){install.packages("sp"); require(sp)}else{require(sp)}
if("rgeos" %in% rownames(installed.packages())==FALSE){install.packages("rgeos"); require(rgeos)}else{require(rgeos)}
if("spdep" %in% rownames(installed.packages())==FALSE){install.packages("spdep"); require(spdep)}else{require(spdep)}
if("INLA" %in% rownames(installed.packages())==FALSE){install.packages("INLA"); require(INLA)}else{require(INLA)}

# Set the working directory

setwd("C://Users//PiotrMorawiecki//Desktop//Hackaton")

# Read COVID cases data in each county from CSSEGISandData github repository

COVID_cases_US_link = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"
COVID_cases_US <- read.csv(COVID_cases_US_link)

# Read COVID deaths data in each county from CSSEGISandData github repository

COVID_deaths_US_link = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv"
COVID_deaths_US <- read.csv(COVID_deaths_US_link)

# From COVID data sets we remove those counties in which (fittingPeriod) days before today were less than (fittingTreshold) cases
# This reduces amount of highly uncertain data.

fittingPeriod <- 30
fittingTreshold <- 10
COVID_deaths_US <- COVID_deaths_US[COVID_cases_US[ ,ncol(COVID_cases_US)-fittingPeriod]>fittingTreshold, ]
COVID_cases_US <- COVID_cases_US[COVID_cases_US[ ,ncol(COVID_cases_US)-fittingPeriod]>fittingTreshold, ]

# Function findRate for input takes new COVID cases time series. It fits an exponential model to the cumulative sum of x (i.e. total number of cases)
# It returns the exponential factor with uncertainty (st. dev.), interpreted as infection rate.

findRate <- function(x) {
    x <- as.numeric(cumsum(x[12:ncol(COVID_cases_US)]))
    x <- x[(length(x)-fittingPeriod):length(x)]
    fit <- summary(lm(logInfections ~ time, data=data.frame(time = 1:length(x), logInfections = log(as.numeric(x)))))
    return(fit$coefficients[2,1:2])
}

# Compute infection rate for each county 

infectionRate <- apply(COVID_cases_US, 1, function(x) findRate(x) )

# Compute fatality rate (chance of dying after testing positive for coronavirus). We assume that 21 days passes from detection to death
# and therefore we compute fatality rate as (total number of deaths to current day) / (total number of deaths to current day - 21 days)

timeFromDetectionToDeath <- 21
fatalityRate <- apply(COVID_cases_US[, 12:ncol(COVID_cases_US)], 1, sum) / apply(COVID_cases_US[, 12:(ncol(COVID_cases_US)-timeFromDetectionToDeath)], 1, sum)

# Extract the following data: state name, county name, county ID (FIPS number), latitude, longitude,
# base infections (number of infections at 15.05.2020) and recent infections (number of infections at 15.05.2020)

COVID_US <- data.frame(state = COVID_cases_US$Province_State,
                            county = COVID_cases_US$Admin2,
                            id = COVID_cases_US$FIPS,
                            lat = COVID_cases_US$Lat,
                            long = COVID_cases_US$Long_,
                            infectionRate = infectionRate[1,],
                            rateUncertainty = infectionRate[2,],
                            fatalityRate = fatalityRate)


# Read area of each US county, data set can be downloaded from
# https://www2.census.gov/library/publications/2001/compendia/ccdb00/tabB1.pdf?#
county_area_US <- read.csv("LND01.csv")

# Extract the following data: state name, county name, population in 2019 (in millions)
county_area_US <- data.frame(id = county_area_US$STCOU,
                             area = county_area_US$LND010190D)

# Read population in each US county, data set can be downloaded from
# https://www.census.gov/data/datasets/time-series/demo/popest/2010s-counties-total.html#par_textimage_70769902

population_US <- read.csv("co-est2019-alldata.csv")

# Extract the following data: state name, county name, population in 2019 (in millions)

population_US <- data.frame(state = population_US$STNAME,
                            county = str_replace(population_US$CTYNAME, " County", ""),
                            population = population_US$POPESTIMATE2019 / 1e6)

# Download pollution data for US counties from https://aqs.epa.gov/aqsweb/airdata/download_files.html

pollution_US <- read.csv("annual_aqi_by_county_2019.csv")
pollution_US <- data.frame(state = pollution_US$State,
                           county = pollution_US$County,
                           CO = pollution_US$Days.CO,
                           NO2 = pollution_US$Days.NO2,
                           Ozone = pollution_US$Days.Ozone,
                           PM10 = pollution_US$Days.PM10,
                           PM2.5 = pollution_US$Days.PM2.5,
                           SO2 = pollution_US$Days.SO2,
                           AQI = pollution_US$Median.AQI)

# Both datasets are merged by state name and county name

data <- merge(COVID_US, county_area_US)
data <- merge(data, population_US)
data <- merge(data, pollution_US)

# If any data points repeat the duplicates are removed

data <- data[!duplicated(data[,c("lat", "long")]),]

# Compute population density

data$density <- data$population / data$area

# Normalize variables, so that the average value is equal to 1
# Normalization allows to compare the relative effect of different model parameters

normalizeVariables <- TRUE
if (normalizeVariables) {
  data$rateUncertainty <- data$rateUncertainty / mean(data$infectionRate)
  data$infectionRate <- data$infectionRate / mean(data$infectionRate)
  data$fatalityRate <- data$fatalityRate / mean(data$fatalityRate)
  data$density <- data$density / mean(data$density)
  data$CO <- data$CO / mean(data$CO) 
  data$NO2 <- data$NO2 / mean(data$NO2)
  data$Ozone <- data$Ozone / mean(data$Ozone)
  data$PM10 <- data$PM10 / mean(data$PM10)
  data$PM2.5 <- data$PM2.5 / mean(data$PM2.5)
  data$SO2 <- data$SO2 / mean(data$SO2)
  data$AQI <- data$AQI / mean(data$AQI)
}

# Plot dependence between the population density, infection rate and fatality rate

par(mfrow=c(1,2))
plot(data$density, data$infectionRate, log="xy", xlab="Population", ylab="Infection rate")
plot(data$density, data$fatalityRate, log="xy", xlab="Population", ylab="Fatality rate")

# Create spatial point data frame from county geographical coordinates

spdf <- SpatialPointsDataFrame(coords = data.frame(lat=data$lat, long=data$long), data = data,
                               proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

# Create Delaunay Triangulation for counties to find the adjency graph. It is later used to create spatial random effect field

delau <- gDelaunayTriangulation(spdf)
nb <- poly2nb(delau)
nb2INLA("map.adj", nb)
g <- inla.read.graph(filename = "map.adj")

# Create Bayesian model for infection rate
# Logarithm of infection rate is modelled as linear combination of county's population and environmental descriptors
# idareau represents spatial random effects
# idareav represents iid random effects

spdf$idareau <- 1:nrow(spdf@data)
spdf$idareav <- 1:nrow(spdf@data)

formula <- infectionRate ~ density + CO + NO2 + Ozone +
  PM10 + PM2.5 + SO2 + AQI +
  f(idareau, model = "besag", graph = g, scale.model = TRUE) +
  f(idareav, model = "iid")

# Run INtegrated Laplace Approximation (INLA) to find posterior distribution of model parameters

res <- inla(formula,
            family = "gaussian", data = spdf@data,
            control.predictor = list(compute = TRUE)
            )

# Summary fixed allows to check confidence intervals for each of linear fitted parameters.

res$summary.fixed

# Compute Bayesian model for fatality rate (model is the same as for infection rate)

formula <- fatalityRate ~ density + CO + NO2 + Ozone +
  PM10 + PM2.5 + SO2 + AQI +
  f(idareau, model = "besag", graph = g, scale.model = TRUE) +
  f(idareav, model = "iid")

res <- inla(formula,
            family = "gaussian", data = spdf@data,
            control.predictor = list(compute = TRUE)
)

res$summary.fixed
