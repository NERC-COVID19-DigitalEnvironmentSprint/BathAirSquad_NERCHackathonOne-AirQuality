## ---------------------------
##
## Script name: Pollutant_data_preprocessing
##
## Purpose of script: Preprocess pollutant data from 2019 to use in INLA_data_preprocessing.R
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
##   The script downloads the annual EPA data about selected number of pollutants (SO2, O3, PM2.5, PM10, NO2, NO, NOx, CO)
##   from all available monitoring stations in US and aggregates them on county level.
##   The output file pollution_data.csv is used by the INLA_data_preprocessing.R script.
##   Script also allows to plot available monitoring data on the USA map and correlation plot for all pollutants.
##   
## ---------------------------

# Load/install the following packages:
# ggplot2     required for ggplot
# dplyr       required for last
# corrplot    required for corrplot

if("ggplot2" %in% rownames(installed.packages())==FALSE){install.packages("ggplot2"); require(ggplot2)}else{require(ggplot2)}
if("dplyr" %in% rownames(installed.packages())==FALSE){install.packages("dplyr"); require(dplyr)}else{require(dplyr)}
if("corrplot" %in% rownames(installed.packages())==FALSE){install.packages("corrplot"); require(corrplot)}else{require(corrplot)}

# Set the working directory

setwd("C://Users//PiotrMorawiecki//Desktop//Hackathon") # REMEMBER TO CHANGE TO YOUR OWN DIRECTORY

# Plots are generated if generatePlots is set to TRUE

generatePlots <- TRUE

# Read annual pollutant data from monitoring stations in US.
# Dataset can be downloaded from https://aqs.epa.gov/aqsweb/airdata/download_files.html

pollutant_data <- read.csv("raw_data//annual_conc_by_monitor_2019.csv")

# Display the number of records for each pollutant (for pollutant selection purposes)

frequency_table <- as.data.frame(table(pollutant_data$Parameter.Name))
frequency_table[order(frequency_table$Freq),]

# The pollutant names, which are extracted from the dataset

pollutantsNames <- c("Sulfur dioxide", "Ozone", "PM2.5 - Local Conditions", "PM10 Total 0-10um STP",
                     "Nitrogen dioxide (NO2)", "Nitric oxide (NO)", "Oxides of nitrogen (NOx)", "Carbon monoxide")

pollutant_data <- pollutant_data[pollutant_data$Parameter.Name %in% pollutantsNames,]

# For each pollutant the unit of measurement is displayed. It is the same for all monitor stations.

unique(pollutant_data[,c("Parameter.Name", "Units.of.Measure")])


# From pollutantData data frame the information about the monitoring stations are extracted (its id, latitude and longitude)
site_data <- data.frame(State.Code = pollutant_data$State.Code,
                        County.Code = pollutant_data$County.Code,
                        Site.Num = pollutant_data$Site.Num,
                        Latitude = pollutant_data$Latitude,
                        Longitude = pollutant_data$Longitude)
site_data <- unique(site_data)

# Take last measurement of each pollutant from each monitor station. The last record corresponds to the latest pollution standard.

pollutant_data <- aggregate(Arithmetic.Mean ~ State.Code + County.Code + Site.Num + Parameter.Name, data=pollutant_data, FUN=last)
pollutant_data <- merge(pollutant_data, site_data)

# This part plots the US map with monitoring data stations measurements for each of the pollutants

if (generatePlots) {
  for (i in pollutantsNames) {
    pollutant_subset <- pollutant_data[pollutant_data$Parameter.Name==i,]
    pollutant_subset <- merge(pollutant_subset, site_data)
    
    ggplot(pollutant_subset) +
      geom_point(aes(Longitude, Latitude, color = Arithmetic.Mean), size = 2) +
      coord_fixed(ratio = 1) +
      scale_color_gradient(low = "blue", high = "orange") +
      borders("state") +
      theme_bw() + ggtitle(i)
    
    ggsave(paste("processed_data//", i, "_monitors_2019.png", sep=""), width = 10, height = 6)
  }
}

# From daily_aqi_by_county_2019.csv (available via EPA website, URL provided above)
# extract the state and county names and ids (required for dataset merging)

pollution_data_2019 <- read.csv("raw_data//daily_aqi_by_county_2019.csv")
pollution_data_2019 <- data.frame(State.Name = pollution_data_2019$State.Name,
                                  county.Name = pollution_data_2019$county.Name,
                                  State.Code = pollution_data_2019$State.Code,
                                  County.Code = pollution_data_2019$County.Code)
pollution_data_2019 <- pollution_data_2019[!duplicated(pollution_data_2019),]

# From annual_aqi_by_county_2019.csv (available via EPA website, URL provided above) extract average Air Quality Index (AQI) for 2019

aqi_data <- data.frame(State.Name = pollution_data_2019$State.Name, county.Name = pollution_data_2019$county.Name)

for (year in c(2019, 2018, 2017, 2016)) {
  aqi_data_new <- read.csv(paste("raw_data//annual_aqi_by_county_", year, ".csv", sep = ""))
  aqi_data_new <- data.frame(State.Name = aqi_data_new$State,
                             county.Name = aqi_data_new$County,
                             aqi_new = aqi_data_new$Median.AQI)
  aqi_data <- merge(aqi_data, aqi_data_new, all = TRUE)
  colnames(aqi_data)[ncol(aqi_data)] <- paste("AQI_", year, sep="")
}

# As sometimes no data are available the extended version of mean function is used (NA is returned if no data are avaliable)

mean_extended <- function(x) if (sum(is.na(x)) == 0) mean(x) else NA

aqi_data$AQI_4years <- (aqi_data$AQI_2019 + aqi_data$AQI_2018 + aqi_data$AQI_2017 + aqi_data$AQI_2016) / 4

aqi_data <- aqi_data[,c("State.Name", "county.Name", "AQI_2019", "AQI_4years")]

# Merge the dataset together

pollution_data_2019 <- merge(pollution_data_2019, aqi_data)

# The pollutant data are aggregated on county level by taking average from all monitoring stations located in given county

pollutant_data <- aggregate(Arithmetic.Mean ~ County.Code + State.Code + Parameter.Name, data=pollutant_data, FUN=mean)

# Add data about each pollutant to the pollution_data_2019 data frame

for (i in pollutantsNames) {
  pollutant_subset <- pollutant_data[pollutant_data$Parameter.Name == i, c("County.Code", "State.Code", "Arithmetic.Mean")]
  pollution_data_2019 <- merge(pollution_data_2019, pollutant_subset, all = TRUE)
  colnames(pollution_data_2019)[ncol(pollution_data_2019)] <- i
}

# Display the total number of counties for which data about given number of pollutants is available

sum(apply(pollution_data_2019, 1, function (x) sum(is.na(x))) == 0)

# The state and counties ids are removed.

pollution_data_2019 <- pollution_data_2019[, -c(1,2)]

# Shorten the names of all columns

colnames(pollution_data_2019) <- c("state", "county", "AQI_2019", "AQI_4years", "SO2", "O3", "PM2.5", "PM10", "NO2", "NO", "NOx", "CO")

# Plot the correlation plot for the available data

if (generatePlots) {
  variables <- pollution_data_2019[,3:11]
  variables <- variables[apply(variables, 1, function(variables) sum(!is.finite(variables))==0),]
  png(height=800, width=800, file="processed_data//Pollutant_correlation_plot.png", type = "cairo")
  corrplot(cor(variables), method = "circle")
  dev.off()
}

# Save the pollution_data_2019 in csv format. This file is used in INLA_data_preprocessing.R script.

write.csv(pollution_data_2019, "processed_data//pollution_data.csv", row.names=FALSE)
