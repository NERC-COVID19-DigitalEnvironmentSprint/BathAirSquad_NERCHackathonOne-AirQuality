## ---------------------------
##
## Script name: INLA_data_preprocessing
##
## Purpose of script: Prepare the US county data and daily COVID data for Bayesian Inference
##
## Authors: Piotr Morawiecki, Laura Oporto, Dan Burrows, Yang Zhou
##
## Date Created: 2020-06-05
##
## Copyright (c) Piotr Morawiecki, 2020
## Email: pwm27@bath.ac.uk
##
## ---------------------------
##
## Notes:
##   
##   Code creates input datasets for INLA_Bayesian_framework. The datasets include daily COVID-19 and pollution data
##   for each US county (daily_data) and annual average demographical, economic and pollution data for each US counties.
##   Each county is represented by its name, state's name and unique id number (FIPS).
##   The data include demographic and economic data (population density, average income per capita, financing of healthcare and hospitals)
##   and environmental descriptors (annual average and daily Air QUality Index, and concentration of CO, NO2, NO, NOx, O3, PM10, PM2.5, SO2).
##   COVID data include the daily number of confirmed cases and deaths.
##   
## ---------------------------

# Load/install the following packages:
# stringr   required for str_replace
# readxl    required for read_excel

if("stringr" %in% rownames(installed.packages())==FALSE){install.packages("stringr"); require(stringr)}else{require(stringr)}
if("readxl" %in% rownames(installed.packages())==FALSE){install.packages("readxl"); require(INLA)}else{require(readxl)}

# Set the working directory

setwd("C://Users//PiotrMorawiecki//Desktop//Hackathon") # REMEMBER TO CHANGE TO YOUR OWN DIRECTORY

# Read COVID deaths data in each county from CSSEGISandData GitHub repository

COVID_deaths_US_url = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv"
COVID_deaths_US <- read.csv(COVID_deaths_US_url)

# Read COVID confirmed cases data in each county from CSSEGISandData GitHub repository

COVID_cases_US_url = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"
COVID_cases_US <- read.csv(COVID_cases_US_url)

# The date of first record in downloaded datasets.

first_record <- as.Date("2020-01-22", "%Y-%m-%d")

# The table relating US county and states names to their FIPS number is created.
# This is base data frame to US counties parameters from other datasets will be added.

county_data <- data.frame(state = COVID_deaths_US$Province_State,
                          county = COVID_deaths_US$Admin2,
                          id = COVID_deaths_US$FIPS,
                          population = COVID_deaths_US$Population)


state_policy_data <- read.csv("https://raw.githubusercontent.com/wxwx1993/PM_COVID/master/Data/state_policy0410.csv")
state_policy_data <- state_policy_data[1:51,]

#state_data <- data.frame(state = state_policy_data$State)

dateToDayId <- function (date) {
  dayId <- as.numeric(difftime(as.Date(date, "%m/%d/%Y"), first_record, units="days"))
  dayId[!is.finite(dayId)] <- Inf
  return(dayId)
}

state_policy_dates <- data.frame(state = state_policy_data$State,
                                 stateOfEmergency = dateToDayId(state_policy_data$State.of.emergency),
                                 closedSchools = dateToDayId(state_policy_data$Date.closed.K.12.schools),
                                 closedDayCares = dateToDayId(state_policy_data$Closed.day.cares),
                                 bannedVistitorsToNursingHomes = dateToDayId(state_policy_data$Date.banned.visitors.to.nursing.homes),
                                 stayAtHome = dateToDayId(state_policy_data$Stay.at.home..shelter.in.place),
                                 closedBusinesses = dateToDayId(state_policy_data$Closed.non.essential.businesses),
                                 closedRestaurants = dateToDayId(state_policy_data$Closed.restaurants.except.take.out),
                                 closedGyms = dateToDayId(state_policy_data$Closed.gyms),
                                 closedMovieTheatres = dateToDayId(state_policy_data$Closed.movie.theaters),
                                 frozeEvictions = dateToDayId(state_policy_data$Froze.evictions),
                                 freezingUtilityShutOffs = dateToDayId(state_policy_data$Order.freezing.utility.shut.offs),
                                 frozeMortgagePayments = dateToDayId(state_policy_data$Froze.mortgage.payments),
                                 waivedOneWeekPeriod = dateToDayId(state_policy_data$Waived.one.week.waiting.period.for.unemployment.insurance))

# Territories outside US do not have assigned id (and other parameters in other datasets) and therefore are removed.

county_data <- county_data[!is.na(county_data$id),]

# The daily_data data frame includes information from each day of pandemy. It will include spatial temporal data about number of cases.
# The spatial points are separated by (sampling_period) days and will include the number of new cases and deaths in the last (sampling_period) days
# and number of cases and deaths then.

# In the first dataset (daily_data) both values are 1, which correspond to daily data with daily number of new cases in each record.

daily_data <- data.frame()
time_delay <- 1
sampling_period <- 1

# Rearrange the data from COVID_deaths_US and COVID_cases_US to format of daily_data, where each pair (region, date) are stored in a single row.

i <- ncol(COVID_deaths_US) - time_delay
while (i > 12) {
  additionalData <- data.frame(state = COVID_deaths_US$Province_State,
                               id = COVID_deaths_US$FIPS,
                               day = i - 12,     # day represent the number of days passed since the data started to be collected
                               previous_deaths = COVID_deaths_US[, i],
                               new_deaths = COVID_deaths_US[, i+time_delay] - COVID_deaths_US[, i],
                               previous_cases = COVID_cases_US[, i],
                               new_cases = COVID_cases_US[, i+time_delay-1] - COVID_cases_US[, i-1])
  daily_data <- rbind(daily_data, additionalData)
  i <- i - sampling_period
}

# The dataset is cleaned form unneeded data (data points before first COVID cases occured and data points without assigned county id)

daily_data <- daily_data[!is.na(daily_data$id), ]
daily_data <- daily_data[daily_data$previous_cases>0, ]

# Dataset is sorted by county id

daily_data <- daily_data[order(daily_data$id), ]

# Add binary data corresponding to whether given state policy was active on a given day

daily_data <- merge(daily_data, state_policy_dates)
daily_data[, 8:ncol(daily_data)] <- apply(daily_data[, 8:ncol(daily_data)], 2, FUN = function(x) x <= daily_data$day)

# In the second dataset (weekly_data) both (sampling_period) and (sampling_period)are 7,
# which correspond to weekly data with weekly number of new cases in each record.
# This dataset can be used if the amount of daily data causes the INLA computation to be too long.

weekly_data <- data.frame()
time_delay <- 7
sampling_period <- 7

# Rearrange the data from COVID_deaths_US and COVID_cases_US to format of weekly_data, where each pair (region, date) are stored in a single row.

i <- ncol(COVID_deaths_US) - time_delay
while (i > 12) {
  additionalData <- data.frame(state = COVID_deaths_US$Province_State,
                               id = COVID_deaths_US$FIPS,
                               day = i - 12,     # day represent the number of days passed since the data started to be collected
                               previous_deaths = COVID_deaths_US[, i],
                               new_deaths = COVID_deaths_US[, i+time_delay] - COVID_deaths_US[, i],
                               previous_cases = COVID_cases_US[, i],
                               new_cases = COVID_cases_US[, i+time_delay-1] - COVID_cases_US[, i-1])
  weekly_data <- rbind(weekly_data, additionalData)
  i <- i - sampling_period
}

# The dataset is cleaned form unneeded data (data points before first COVID cases occured and data points without assigned county id)

weekly_data <- weekly_data[!is.na(weekly_data$id), ]
weekly_data <- weekly_data[weekly_data$previous_cases>0, ]

# Dataset is sorted by county id

weekly_data <- weekly_data[order(weekly_data$id), ]

# Add binary data corresponding to whether given state policy was active on a given day

weekly_data <- merge(weekly_data, state_policy_dates)
weekly_data[, 8:ncol(weekly_data)] <- apply(weekly_data[, 8:ncol(weekly_data)], 2, FUN = function(x) x <= weekly_data$day)

# Read the recent pollution data from EPA Air Data. The file is available at https://aqs.epa.gov/aqsweb/airdata/download_files.html

pollution_2020 <- read.csv("raw_data//daily_aqi_by_county_2020.csv")

# The pollution data are merged with county_data to add column with the county ids

colnames(pollution_2020)[1:2] <- c("state", "county") 
pollution_2020 <- merge(pollution_2020, county_data)

# Extract the following data: county id, day (with reference to first COVID record), and average data AQI (Air Quality Index)

pollution_2020 <- data.frame(id = pollution_2020$id,
                             day = as.numeric(difftime(pollution_2020$Date, first_record, units="days")),
                             daily_AQI = pollution_2020$AQI)

# To each entry of daily and weekly data we assign the mean AQI in the last 2 weeks and 2 months before the given day.

# As sometimes no data are available the extended version of mean function is used (NA is returned if no data are avaliable)

mean_extended <- function(x) if (length(x) > 0) return(mean(x)) else return(NA)

# The pollution measurements are extracted for each county separately; as we work on smaller extracted datasets it increases computation speed
# The averages are saved in aqi_2weeks and aqi_2months vectors respectively.

aqi_2weeks <- rep(0, nrow(daily_data))
aqi_2months <- rep(0, nrow(daily_data))
ids <- unique(daily_data$id)

for (i in 1:length(ids)) {
  if (i %% 100 == 0) print(paste(i,"/",length(ids),sep=""))
  id = ids[i]
  
  # Extract rows with given county id from pollution_2020 and daily data data frames 
  
  extracted_rows <- which(daily_data$id==id)
  extracted_pollution <- pollution_2020[pollution_2020$id == id, ]
  extracted_days <- daily_data[extracted_rows, ]
  
  # For each row in extracted daily data compute the 2-week and 2-month AQI mean
  
  aqi_2weeks[extracted_rows] <- apply(extracted_days, 1, FUN = function (x) mean_extended(extracted_pollution[
    as.numeric(x["day"]) - extracted_pollution$day > 0 & as.numeric(x["day"]) - extracted_pollution$day <= 14, "daily_AQI"]))
  aqi_2months[extracted_rows] <- apply(extracted_days, 1, FUN = function (x) mean_extended(extracted_pollution[
    as.numeric(x["day"]) - extracted_pollution$day > 0 & as.numeric(x["day"]) - extracted_pollution$day <= 60, "daily_AQI"]))
}

# Add computed aqi means to the daily_data data frame

daily_data$AQI_2weeks <- aqi_2weeks
daily_data$AQI_2months <- aqi_2months

# The same operation is repeated for weekly_data (see explanation above)

aqi_2weeks <- rep(0, nrow(weekly_data))
aqi_2months <- rep(0, nrow(weekly_data))
for (i in 1:length(ids)) {
  if (i %% 100 == 0) print(paste(i,"/",length(ids),sep=""))
  id = ids[i]
  extracted_rows <- which(weekly_data$id==id)
  extracted_pollution <- pollution_2020[pollution_2020$id == id, ]
  extracted_days <- weekly_data[extracted_rows, ]
  aqi_2weeks[extracted_rows] <- apply(extracted_days, 1, FUN = function (x) mean_extended(extracted_pollution[
    as.numeric(x["day"]) - extracted_pollution$day > 0 & as.numeric(x["day"]) - extracted_pollution$day <= 14, "daily_AQI"]))
  aqi_2months[extracted_rows] <- apply(extracted_days, 1, FUN = function (x) mean_extended(extracted_pollution[
    as.numeric(x["day"]) - extracted_pollution$day > 0 & as.numeric(x["day"]) - extracted_pollution$day <= 14, "daily_AQI"]))
}
weekly_data$AQI_2weeks <- aqi_2weeks
weekly_data$AQI_2months <- aqi_2months

# Read area of each US county, data set can be downloaded from
# https://www2.census.gov/library/publications/2001/compendia/ccdb00/tabB1.pdf?#

county_area_US <- read.csv("raw_data//LND01.csv")

# Extract the following data: state name, county name, population in 2019 (in millions)

county_area_US <- data.frame(id = county_area_US$STCOU,
                             area = county_area_US$LND010190D)

# Read pollution data for US counties. Data set is genereated by pollutant_data_preprocessing.R
# based on data from https://aqs.epa.gov/aqsweb/airdata/download_files.html

pollution_data_2019 <- read.csv("processed_data//pollution_data.csv")

# Read counties economical data. All three files can be downloaded from
# https://www.census.gov/library/publications/2011/compendia/usa-counties-2011.html

# Read the annual income per capita by US counties (from 2007)

income_US <- read_excel("raw_data//PEN01.xls",sheet = "Sheet2")
income_US <- data.frame(id=income_US$STCOU,
                        incomePerCapita=income_US$PEN020207D)

# Read the annual financing of hospitals by US counties (from 2002)

hospitals_US <- read_excel("raw_data//LOG01.xls",sheet = "Sheet9")
hospitals_US <- data.frame(id=hospitals_US$STCOU,
                           hospitalExpenditure=hospitals_US$LOG350202D)

# Read the annual financing of healthcare by US counties (from 2002)

health_US <- read_excel("raw_data//LOG01.xls",sheet = "Sheet10")
health_US <- data.frame(id=health_US$STCOU,
                        healthExpenditure=health_US$LOG360202D)


# All datasets are merged with the county data frame

county_data <- merge(county_data, county_area_US, all.x = TRUE)
county_data <- merge(county_data, pollution_data_2019, all.x = TRUE)
county_data <- merge(county_data, income_US, all.x = TRUE)
county_data <- merge(county_data, hospitals_US, all.x = TRUE)
county_data <- merge(county_data, health_US, all.x = TRUE)

# Compute population density

# county_data$density <- county_data$population / county_data$area


census_county_data <- read.csv("https://raw.githubusercontent.com/wxwx1993/PM_COVID/master/Data/census_county_interpolated.csv")

census_county_data <- census_county_data[rev(order(census_county_data$year)), ]
census_county_data <- census_county_data[!duplicated(census_county_data$fips),]
census_county_data <- census_county_data[,-c(1,3,12)]
colnames(census_county_data)[1] <- "id"

county_data <- merge(county_data, census_county_data, all.x = TRUE)

# Write generated data frames to files. They are used by INLA_Bayesian_framework.R

write.csv(county_data, "processed_data//county_data.csv", row.names = FALSE)
write.csv(daily_data, "processed_data//daily_data.csv", row.names = FALSE)
write.csv(weekly_data, "processed_data//weekly_data.csv", row.names = FALSE)