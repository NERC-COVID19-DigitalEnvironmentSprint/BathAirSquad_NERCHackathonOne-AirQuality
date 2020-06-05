## ---------------------------
##
## Script name: INLA_datasets_preprocessing 
##
## Purpose of script: Prepare the Italy data and daily COVID data for Bayesian Inference
##
## Authors: Piotr Morawiecki, Laura Oporto, Dan Burrows, Tina Zhou
##
## Date Created: 2020-06-03
##
## ---------------------------
##
## Notes:
##   
##   Code creates input datasets for INLA_Bayesian_framework. The datasets include daily COVID-19 data (daily_data) 
##   and pollution, annual average demographical and economic data for each region (region_data) or provinces (city_data)
##   in Italy.
##   Each city is represented by its name, region's name and unique id.
##   The data include demographic and economic data (population density, average income per capita, financing of healthcare)
##   and environmental descriptors (concentration of NO2, PM10, PM2.5).
##   COVID data includes total regional and provincial confirmed cases, regional number of performed and positive swab test,
##   regional number of fatal events ascribed to SARS-CoV-2. 
##   
## ---------------------------

# Load/install the following packages:
library(tidyverse)
library(lubridate)
library(dplyr)
library(readr)

# Set the working directory

setwd("C://Users//lauri//Downloads//Hackathon1//Italy_Bayesian_Framework//raw_data")


# Read COVID data from the Italian Civil Protection Department (ICPD, 2020) GitHub repository
# https://github.com/pcm-dpc/COVID-19	
# COVID data by region

covid_link <- "https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-regioni/dpc-covid19-ita-regioni.csv"
covidbyreg <- read_csv(covid_link) 

#Select relevant columns and rename them 

covidbyregion <- covidbyreg %>% select(-stato ,-casi_testati, -note_it, -note_en) %>% rename(
  date = data,
  region_code= codice_regione,
  region= denominazione_regione,
  latitude =lat,
  longitude= long,
  hosp_with_sympt= ricoverati_con_sintomi,
  intensive_care= terapia_intensiva,
  total_hospitalized = totale_ospedalizzati,
  home_isolation = isolamento_domiciliare,
  total_positive = totale_positivi,
  total_change_positive = variazione_totale_positivi,
  new_positive = nuovi_positivi,
  discharged_healed = dimessi_guariti,
  deceased = deceduti,
  total_cases = totale_casi,
  swabs = tamponi
)


# The date of first record in downloaded datasets.

first_record <- as.Date("2020-02-24", "%Y-%m-%d")


# The daily_data data frame includes information for each day of pandemy from the first record day 
# until the download date (2020-02-24 to 2020-06-04). 

# Create empty dataframe

daily_data <- data.frame()

# Auxiliary variable to calculate the previous deaths and day for each region

prev = covidbyregion %>% group_by(region)%>% mutate(cum_deaths = cumsum(deceased)) %>%
  mutate(day = 1:length(date))

# Add additional data to the daily_data data frame. It includes the id of the region (id), the day 
# when they registered the data (day), where the first_record corresponds to day = 1. The total cases 
# up to day (previous_cases), the new positive cases for the day (new_cases), the deaths up to day 
# (previous_deaths) and the new deaths for day (new_deaths) are included.

additionalData <- data.frame(id = covidbyregion$region_code,
                             day = prev$day,
                             previous_deaths = prev$cum_deaths,
                             new_deaths = covidbyregion$deceased,
                             previous_cases = covidbyregion$total_cases,
                             new_cases = covidbyregion$new_positive
)
daily_data <- rbind(daily_data, additionalData)

# Save data frame in csv file

write.csv(daily_data,'daily_data.csv',row.names=FALSE)

# Table with area, population and population density. Elaborated from: 
# Area per region: http://dati.istat.it/Index.aspx?QueryId=37449#
# Population up to 2019-01-01 (most recent): http://dati.istat.it/Index.aspx?QueryId=18460

info_by_region <- read_csv('info_by_region.csv')

# region name and code as in the COVID data

regid <- data.frame(
  "region" = c("Abruzzo","Basilicata","P.A. Bolzano","Calabria","Campania","Emilia-Romagna","Friuli Venezia Giulia",
               "Lazio","Liguria","Lombardia","Marche","Molise","Piemonte","Puglia","Sardegna","Sicilia","Toscana",
               "P.A. Trento","Umbria","Valle d'Aosta","Veneto"),
  "region_id" = c("13","17","21","18","15","08","06","12","07","03","11","14","01","16","20","19","09","22","10","02","05")
) 
regid <- data.frame(lapply(regid, as.character), stringsAsFactors=FALSE)

# Table created to assign regions to the cities in the pollution data

cityandregion <- read_csv('cityandregion.csv')

# Daily pollutant levels(NO2,PM2.5,PM10) in ug/m3 by province, 2016 - 2019
# https://www.eea.europa.eu/themes/air/air-quality-and-covid19/monitoring-covid-19-impacts-on

# NO2 data 

NO2_1619 <- read_csv('NO2_1619full.csv')
NO2 <- NO2_1619 %>% select(AirQualityLevel, City, datebegin) %>% rename(
                             NO2_val= AirQualityLevel,
                             city= City,
                             date = datebegin
                           )

# Add the region id. Note: there are no observations for Basilicata and Valle d'Aosta regions

NO2 <- left_join(NO2, cityandregion, by = c('city'='city')) # to add region corresponding to each city
NO2 <- left_join(regid,NO2, by = c('region'='region')) # to add region id
NO2$year <- substr(NO2$date,7,10) # create column with the year
NO2_data <- NO2 %>% group_by(region,region_id,year) %>% summarise(meanNO2 =mean(NO2_val)) # annual mean by region
NO2_cdata <- NO2 %>% group_by(region,region_id,city,year) %>% summarise(meanNO2 = mean(NO2_val)) # annual mean by city

# PM2.5 data

PM2.5_1619 <- read_csv('PM2.5_1619full.csv')
PM2.5 <- PM2.5_1619 %>% select(AirQualityLevel, City, datebegin) %>% rename(
  PM2.5_val= AirQualityLevel,
  city= City,
  date = datebegin
)

# Add the region id. Note: there are no observations for Basilicata, Molise, P.A. Bolzano and Valle d'Aosta regions

PM2.5 <- left_join(PM2.5, cityandregion, by = c('city'='city')) # to add region corresponding to each city
PM2.5 <- left_join(regid,PM2.5, by = c('region'='region')) # to add region id
PM2.5$year <- substr(PM2.5$date,7,10) # create column with the year
PM2.5_data <- PM2.5 %>% group_by(region,region_id,year) %>% summarise(meanPM2.5 =mean(PM2.5_val))  # annual mean by region
PM2.5_cdata <- PM2.5 %>% group_by(region,region_id,city,year) %>% summarise(meanPM2.5 = mean(PM2.5_val))  # annual mean by city

# PM10 data

PM10_1619 <- read_csv('PM10_1619full.csv')
PM10 <- PM10_1619 %>% select(AirQualityLevel, City, datebegin) %>% rename(
  PM10_val= AirQualityLevel,
  city= City,
  date = datebegin
)

# Add the region id. Note: there are no observations for Basilicata and Valle d'Aosta

PM10 <- left_join(PM10, cityandregion, by = c('city'='city')) # to add region corresponding to each city
PM10 <- left_join(regid,PM10, by = c('region'='region')) # to add region id
PM10$year <- substr(PM10$date,7,10) # create column with the year
PM10_data <- PM10 %>% group_by(region,region_id,year) %>% summarise(meanPM10 =mean(PM10_val)) # annual mean by region
PM10_cdata <- PM10 %>% group_by(region,region_id,city,year) %>% summarise(meanPM10 = mean(PM10_val)) # annual mean by city

# Table of parameters by region

region_data <- full_join(NO2_data, PM10_data)
region_data <- full_join(region_data, PM2.5_data)

# Table of parameters by city

city_data <- full_join(NO2_cdata, PM10_cdata)
city_data <- full_join(city_data, PM2.5_cdata)

# Add other information by region

region_data <- left_join(region_data, info_by_region,by=c('region' = 'region'))
regions_data <- data.frame(id = region_data$region_id,
                           region = region_data$region,
                           area = region_data$km2,
                           NO2 = region_data$meanNO2,
                           PM2.5 = region_data$meanPM2.5,
                           PM10 = region_data$meanPM10,
                           incomePerCapita = region_data$IncomeAvail18,
                           healthExpenditure = region_data$healthExpenditure16,
                           density = region_data$density
)

# Save in csv file

write.csv(regions_data,'region_data.csv',row.names=FALSE)

# Add other information by region

city_data <- left_join(city_data, info_by_region,by=c('region' = 'region'))
cities_data <- data.frame(id = city_data$region_id,
                           region = city_data$region,
                           city = city_data$city,
                           area = city_data$km2,
                           NO2 = city_data$meanNO2,
                           PM2.5 = city_data$meanPM2.5,
                           PM10 = city_data$meanPM10,
                           incomePerCapita = city_data$IncomeAvail18,
                           healthExpenditure = city_data$healthExpenditure16,
                           density = city_data$density
)

# Save in csv file

write.csv(cities_data,'city_data.csv',row.names=FALSE)
