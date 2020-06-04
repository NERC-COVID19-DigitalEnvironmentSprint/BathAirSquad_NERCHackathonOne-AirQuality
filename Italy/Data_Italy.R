library(tidyverse)
library(lubridate)
library(dplyr)
library(readr)

setwd("C:/Users/lauri/Downloads/Challenge1/Italy/Italy")

# Daily pollutant levels(NO2,PM2.5,PM10) in ug/m3 by province, 2018 - 2020
# https://www.eea.europa.eu/themes/air/air-quality-and-covid19/monitoring-covid-19-impacts-on

NO2_1820 <- read_csv('NO2_1820.csv')
NO2 <- NO2_1820 %>% select(AirPollutant, AirQualityLevel, LatitudeOfMeasurementStation, 
                          LongitudeOfMeasurementStation, City, UA_city_pop, datebegin) %>% rename(
                            pollutant = AirPollutant,
                            NO2_val= AirQualityLevel,
                            latitude= LatitudeOfMeasurementStation,
                            longitude= LongitudeOfMeasurementStation,
                            city= City,
                            city_pop= UA_city_pop,
                            date = datebegin
                          )

PM2.5_1820 <- read_csv('PM2.5_1820.csv')
PM2.5 <- PM2.5_1820 %>% select(AirPollutant, AirQualityLevel, LatitudeOfMeasurementStation, 
                              LongitudeOfMeasurementStation, City, UA_city_pop, datebegin) %>% rename(
                                pollutant = AirPollutant,
                                PM2.5_val= AirQualityLevel,
                                latitude= LatitudeOfMeasurementStation,
                                longitude= LongitudeOfMeasurementStation,
                                city= City,
                                city_pop= UA_city_pop,
                                date = datebegin
                              )

PM10_1820 <- read_csv('PM10_1820.csv')
PM10 <- PM10_1820 %>% select(AirPollutant, AirQualityLevel, LatitudeOfMeasurementStation, 
                            LongitudeOfMeasurementStation, City, UA_city_pop, datebegin) %>% rename(
                              pollutant = AirPollutant,
                              PM10_val= AirQualityLevel,
                              latitude= LatitudeOfMeasurementStation,
                              longitude= LongitudeOfMeasurementStation,
                              city= City,
                              city_pop= UA_city_pop,
                              date = datebegin
                              
                            )

cityandregion <- read_csv('cityandregion.csv')
NO2 <- left_join(NO2, cityandregion, by = c('city'='city'))
PM2.5 <- left_join(PM2.5, cityandregion, by = c('city'='city'))
PM10 <- left_join(PM10, cityandregion, by = c('city'='city'))


# Data on COVID-19 in Italy included total regional and provincial confirmed cases, regional number 
# of performed and positive swab test, regional number of fatal events ascribed to SARS-CoV-2.
# Italian Civil Protection Department (ICPD, 2020)
# https://github.com/pcm-dpc/COVID-19	

#covid by region
covid_link <- "https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-regioni/dpc-covid19-ita-regioni.csv"
covidbyreg <- read_csv(covid_link) 
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

#Table with area, population and population density. Elaborated from: 
#Area per region: http://dati.istat.it/Index.aspx?QueryId=37449#
#Population 01/01/19 (most recent): http://dati.istat.it/Index.aspx?QueryId=18460
info_by_region <- read_csv('info_by_region.csv')

meancovid_region <- aggregate(covidbyregion, list(covidbyregion$region), mean)
meancovid_region <- select(meancovid_region, -region_code, -region) %>% rename(region = Group.1)

covid_It_byregion <- left_join(meancovid_region, info_by_region, by = c('region'='region'))

#regions of Italy .geojson
link= 'https://gist.github.com/datajournalism-it/f1abb68e718b54f6a0fe'
mymap <- st_read("regioni-con-trento-bolzano.geojson")
mymap$Regione <- info_by_region$region
plot(mymap)

