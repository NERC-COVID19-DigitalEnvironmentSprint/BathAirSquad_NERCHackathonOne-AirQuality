if("stringr" %in% rownames(installed.packages())==FALSE){install.packages("stringr"); require(stringr)}else{require(stringr)}
library(tmap)
library(sf)

setwd("C://Users//PiotrMorawiecki//Desktop//Hackaton")

# Read COVID cases data in each county from CSSEGISandData github repository

COVID_cases_US_link = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"
COVID_cases_US <- read.csv(COVID_cases_US_link)

# Read COVID deaths data in each county from CSSEGISandData github repository

COVID_deaths_US_link = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv"
COVID_deaths_US <- read.csv(COVID_deaths_US_link)

# Function findRate for input takes new COVID cases time series. It fits an exponential model to the cumulative sum of x (i.e. total number of cases)
# It returns the exponential factor with uncertainty (st. dev.), interpreted as infection rate.

# From COVID data sets we remove those counties in which (fittingPeriod) days before today were less than (fittingTreshold) cases
# This reduces amount of highly uncertain data.

findRate <- function(x) {
  fittingPeriod <- 30
  fittingTreshold <- 10
  x <- as.numeric(x)#cumsum(x[12:ncol(COVID_cases_US)]))
  if (x[length(x)-fittingPeriod] < fittingTreshold) {
    return(c(NA,NA))
  }
  x <- x[(length(x)-fittingPeriod):length(x)]
  fit <- summary(lm(logInfections ~ time, data=data.frame(time = 1:length(x), logInfections = log(as.numeric(x)))))
  return(fit$coefficients[2,1:2])
}

# Compute infection rate for each county 

infectionRate <- apply(COVID_cases_US, 1, function(x) findRate(x) )

# Compute fatality rate (chance of dying after testing positive for coronavirus). We assume that 21 days passes from detection to death
# and therefore we compute fatality rate as (total number of deaths to current day) / (total number of deaths to current day - 21 days)

timeFromDetectionToDeath <- 0
deathsTreshold <- 5
fatalityRate <- COVID_deaths_US[, ncol(COVID_deaths_US)] / COVID_cases_US[, ncol(COVID_cases_US)-timeFromDetectionToDeath]
fatalityRate[COVID_deaths_US[, ncol(COVID_deaths_US)] < deathsTreshold] = NA

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

data <- merge(COVID_US, county_area_US, all.x=TRUE)
data <- merge(data, population_US, all.x=TRUE)
data <- merge(data, pollution_US, all.x=TRUE)

# If any data points repeat the duplicates are removed

data <- data[!duplicated(data$id),]
#data <- data[!duplicated(data[,c("lat", "long")]),]

# Compute population density

data$density <- data$population / data$area

write.csv(data, file="US_county_data.csv")

shp <- st_read("acs_2012_2016_county_us_B27001.shp",
               stringsAsFactors = FALSE) %>%
  mutate(STFIPS = stringr::str_sub(GEOID, 1, 2))

colnames(shp)[1] <- "id"
shp$id <- as.numeric(shp$id)

shp <- merge(shp, data, by="id")

map <- tm_shape(shp) + tm_polygons("infectionRate", midpoint=0)
tmap_save(map, "infectionRate_US_map.png", width=1920, height=1080, asp=0)

map <- tm_shape(shp) + tm_polygons("density", style = "log10_pretty")
tmap_save(map, "populationDensity_US_map.png", width=1920, height=1080, asp=0)

plottedVariables <- c("rateUncertainty", "fatalityRate", "CO", "NO2", "Ozone", "PM10", "PM2.5", "SO2", "AQI")
for (variable in plottedVariables) {
  map <- tm_shape(shp) + tm_polygons(variable)
  tmap_save(map, paste(variable, "_US_map.png", sep=""), width=1920, height=1080, asp=0)
}

library(leaflet)

map <- st_transform(shp, 4326)
pal <- colorNumeric("YlOrRd", domain = map$infectionRate)

leaflet(shp) %>%
  addTiles() %>%
  addPolygons(
    color = "white", fillColor = ~ pal(infectionRate),
    fillOpacity = 1
  ) %>%
  addLegend(pal = pal, values = ~infectionRate, opacity = 1)
