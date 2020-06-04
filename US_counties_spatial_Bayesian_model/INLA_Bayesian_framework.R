## ---------------------------
##
## Script name: INLA_Bayesian_framework
##
## Purpose of script: Run spatial-temporal Bayesian inference on COVID-19 data using environmental and demographic covariates
##
## Authors: Piotr Morawiecki, Laura Oporto, Dan Burrows, Tina Zhou
##
## Date Created: 2020-06-03
##
## Copyright (c) Piotr Morawiecki, 2020
## Email: pwm27@bath.ac.uk
##
## ---------------------------
##
## Notes:
##   
##   The code allows to estimate posteriors distributions of choosen set of model parameters using INLA
##   (INtegrated Laplace Approximaton) package. This packeage computes posterior distribution by approximated
##   numerical integration instead of using MCMC to generate sample from the prosterior distribution,
##   which reduces computation time.
##   There are three basic models to choose from, but it is easy to implement other models as well.
##   In the end of the code the 95% confidence interavals for values of each parameter are plotted for comparison.
##   
## ---------------------------

# Load/install the following packages:
# INLA      required for inla, inla.posterior.sample
# ggplot2   required for ggplot

if("INLA" %in% rownames(installed.packages())==FALSE){
  install.packages("INLA", repos = "https://inla.r-inla-download.org/R/stable", dep = TRUE); require(INLA)}else{require(INLA)}
if("ggplot2" %in% rownames(installed.packages())==FALSE){install.packages("ggplot2"); require(ggplot2)}else{require(ggplot2)}

# Set the working directory

setwd("C://Users//PiotrMorawiecki//Desktop//Hackaton")

# There are two possible COVID-19 descriptors - cases and deaths. To use deaths data set use_deaths to TRUE. To use cases data set deaths to FALSE.
# There are more days with confirmed cases data, but these data are biased as the number of COVID tests may vary.
# Less data with deaths are available, but it is more reliable.

use_deaths <- TRUE

# To use Poisson distrobution set Use_Poisson to TRUE.
# To use negative binomial distrobution set Use_Poisson to FALSE.

use_Poisson <- FALSE

# Vector <covariates> should include the covariates that are used by INLA model. Only rows with full set of data will be used.
# Covariates to choose from are:
# 7  - Air Quality Index (2 week average)
# 8  - Air Quality Index (2 month average)
# 13 - Air Quality Index (average from 2019)
# 14 - Air Quality Index (average 2016-2019)
# 15 - SO2 average concentration
# 16 - O3 average concentration                
# 17 - PM2.5 average concentration
# 18 - PM10 average concentration
# 19 - NO2 average concentration
# 20 - NO average concentration
# 21 - NOx average concentration
# 22 - CO average concentration
# 23 - income per capita
# 24 - hospital expenditure
# 25 - health expenditure
# 26 - population density

# Formula should include the model for the logarithm of infection rate.
# It is modelled as linear combination of county's population and environmental descriptors. f(id) represents spatial random effects,
# f(day) represents temporal random effects and f(rowId) represents the remaining random effects.

# There are six models that were used to obtained data from the code documentation on GitHub:
# Models 1-4 describe the impact of Air Quality Index (averaged over 2 weeks, 2 months, 1 year and 4 years, respectively).
# Model 5 describe the impace of annual Air Quality Index together with different economic factors
# Model 6 describes the impact of each pollutant separately.

model <- 1

if (model==1) {
  covariates <- c(7, 26)
  formula <- new_cases ~ AQI_2weeks + density +
    f(id, model = "iid") +  f(day, model = "ar1") + f(rowId, model = "iid")
  use_weekly <- TRUE
}

if (model==2) {
  covariates <- c(8, 26)
  formula <- new_cases ~ AQI_2months + density +
    f(id, model = "iid") +  f(day, model = "ar1") + f(rowId, model = "iid")
  use_weekly <- TRUE
}

if (model==3) {
  covariates <- c(13, 26)
  formula <- new_cases ~ AQI_2019 + density +
    f(id, model = "iid") +  f(day, model = "ar1") + f(rowId, model = "iid")
  use_weekly <- TRUE
}

if (model==4) {
  covariates <- c(14, 26)
  formula <- new_cases ~ AQI_4years + density +
    f(id, model = "iid") +  f(day, model = "ar1") + f(rowId, model = "iid")
  use_weekly <- TRUE
}

if (model==5) {
  covariates <- c(7, 21, 22, 23, 24)
  formula <- new_cases ~ annual_AQI + incomePerCapita + hospitalExpenditure + healthExpenditure + density +
    f(id, model = "iid") +  f(day, model = "ar1") + f(rowId, model = "iid")
  use_weekly <- FALSE
}

if (model==6) {
  covariates <- c(15, 16, 17, 18, 19, 22, 23, 24, 25, 26)
  formula <- new_cases ~ SO2 + O3 + PM2.5 + PM10 + NO2 + CO + incomePerCapita + hospitalExpenditure + healthExpenditure + density +
    f(id, model = "iid") +  f(day, model = "ar1") + f(rowId, model = "iid")
  use_weekly <- FALSE
}

if (use_Poisson) {
  distribution = "poisson"
} else {
  distribution = "nbinomial"
}

# Read the county_data (demographic/economic data and average pollution data) and daily_data (COVID-19 cases and deaths, and daily air quality data)
# which are generated by US_county_datasets_preprocessing.R script.

county_data <- read.csv("county_data.csv")

if (use_weekly) {
  temporal_data <- read.csv("weekly_data.csv")
} else {
  temporal_data <- read.csv("daily_data.csv")
}

# Merge the data to provide input dataset for INLA

INLA_data <- merge(temporal_data, county_data)

# The appropriate columns are selected for the INLA.

if (use_deaths) {
  # If deaths are chosen only the days before which at least (deathsTreshold) deaths were recorded will be used.
  # As in the original dataset the total number of deaths very occassionally decreases these cases are excluded.
  print("Infection rate is estimated based on number of deaths.")
  deathsTreshold <- 1
  INLA_data <- INLA_data[INLA_data$previous_deaths>=deathsTreshold & INLA_data$new_deaths>=0, ]
  variables <- c(3, 4)
} else {
  # If cases are chosen only the days before which at least (casesTreshold) cases were recorded will be used.
  # As in the original dataset the total number of cases very occassionally decreases these cases are excluded.
  print("Infection rate is estimated based on number of confirmed cases.")
  casesTreshold <- 10
  INLA_data <- INLA_data[INLA_data$previous_cases>=casesTreshold & INLA_data$new_cases>=0, ]
  variables <- c(5, 6)
}

# The INLA_data data frame size is reduced by removing unused columns.

INLA_data <- INLA_data[, c(1, 2, variables, covariates)]
colnames(INLA_data)[c(3, 4)] <- c("previous_cases", "new_cases")

#variables <- INLA_data[,c(7:8,13:26)]
#variables <- variables[apply(variables, 1, function(x) sum(!is.finite(x)))==0,]
#png(height=800, width=800, file="All_variables_correlation_plot.png", type = "cairo")
#corrplot(cor(variables), method = "circle")
#dev.off()

# The data with incomplete set of covariates (for example containing NA values) are removed.

INLA_data <- INLA_data[apply(INLA_data, 1, function(x) sum(!is.finite(x))==0),]

# All covariates are normalised. After normalisation their mean is 0 and standard deviation is 1.

covs <- 5:ncol(INLA_data)
data_statistics <- data.frame(mean = apply(INLA_data[,covs], 2, FUN=mean),
                              sd   = apply(INLA_data[,covs], 2, FUN=sd),
                              min  = apply(INLA_data[,covs], 2, FUN=min),
                              max  = apply(INLA_data[,covs], 2, FUN=max))
INLA_data[,covs] <- apply(INLA_data[,covs], 2, FUN = function(x) (x - mean(x)) / sd(x))

# A unique id is assigned to each row to represent the remaining random effects in INLA model

INLA_data$rowId <- 1:nrow(INLA_data)

# The INLA is performed to obtain posterior distribution of model parameters. The model used is described in GitHub repository.

print(paste("The INLA data set consist of", nrow(INLA_data), "records"))
print("INLA has started.")

res <- inla(formula,
            family = distribution, data = INLA_data, E = previous_cases,
            control.predictor = list(compute = TRUE),
            control.inla = list(int.strategy = "eb"),
            control.compute = list(config = TRUE))

print("INLA has finished.")

# Summary fixed allows to check confidence intervals for each of linear fitted parameters.

res$summary.fixed

# Create directory for output tables and graphs

output_folder <- paste("Model_", model, if (use_deaths) "_deaths" else "_cases", if(use_Poisson) "_poisson" else "_nbinomial", sep="")
if (!dir.exists(output_folder)) dir.create(output_folder)

# Save summary of fitted parameters to csv

write.csv(res$summary.fixed, paste(output_folder, "//fixed_effects_summary.csv", sep=""))

# Here the 95% confidence intervals for each parameter (except for the intercept) are plotted.
# Boxes top and bottom boundary represents 97.5% quantile and 0.025% quantile respectively.
# The horizontal bar inside the rectangle represents the median. Bars with negative median are red,
# while bars with positive median are green.

fixed_parameters <- res$summary.fixed
fixed_parameters <- fixed_parameters[-1,]

x    <- 1:nrow(fixed_parameters)
high <- fixed_parameters$`0.975quant`
mid  <- fixed_parameters$`0.5quant`
low  <- fixed_parameters$`0.025quant`
color  <- rep("red", length(x))
color[mid>0]  <- "green"

png(file=paste(output_folder, "//fixed_effects_plot.png", sep=""), width=600, height=600)

plot(1, type="n", xaxt="n", xlab="Factor", ylab="Effect on infection rate", xlim=c(0.5,length(x)+0.5), ylim=c(min(low), max(high)))
abline(a=0, b=0)
rect(x - 0.2, low, x + 0.2, mid, col=color)
rect(x - 0.2, mid, x + 0.2, high, col=color)
axis(1, at=x, labels=rownames(fixed_parameters))

dev.off()

# Here the effect of each covariate on daily infection rate is plotted including 50% and 90% confidence intervals
# (represented as lighter and darker shaded area). The continous line represents the median and dashed line represents
# mean value of covariate in the training data set. All other covariates are assumed to have mean value.

# The confidence intervals are created by generating n_sims parameter values from their posterior distribution using INLA.

n_sims = 10000
n_values = 100
sim <- inla.posterior.sample(n_sims, res)
sim <- lapply(sim, function(x) as.numeric(tail(x$latent[,1], n=3)))

for (covariate in 2:length(res$names.fixed)-1) {
  
  # Take (n_values) of given covariate from interval between 0 and its maximal value in the input dataset.
  
  x_values <- (0:n_values)/n_values * data_statistics[covariate,"max"]
  
  # Calculate the infection rate for each pairs of posterior model parameters and covariate values
  
  factor_mean <- data_statistics[covariate, "mean"]
  factor_sd <- data_statistics[covariate, "sd"]
  y <- sapply(x_values, FUN=function(x) sapply(sim, FUN=function(s) s[1] + s[covariate+1] * (x - factor_mean) / factor_sd))
  
  # Calculate the appropriate quantiles for the obtained samples.
  
  data_points <- data.frame(x = x_values,
                            med = apply(y, 2, FUN=median),
                            Q10 = apply(y, 2, FUN=function (x) as.numeric(quantile(x, probs = 0.1))),
                            Q25 = apply(y, 2, FUN=function (x) as.numeric(quantile(x, probs = 0.25))),
                            Q75 = apply(y, 2, FUN=function (x) as.numeric(quantile(x, probs = 0.75))),
                            Q90 = apply(y, 2, FUN=function (x) as.numeric(quantile(x, probs = 0.9))))
  
  # Convert the infection rates from log(p) to daily infaction rate p expressed in [%]
  
  if (use_weekly) {
    data_points[,2:6] <- 100 * ((1 + exp(data_points[,2:6]))**(1/7) - 1)
  } else {
    data_points[,2:6] <- 100 * exp(data_points[,2:6])
  }
  
  # Plot the graph of the effect of covariate on infection rate using computed median and quantiles
  
  ggplot(data_points) + aes(x = x, y = med) + geom_line() + geom_ribbon(aes(ymin = Q25, ymax = Q75), alpha = 0.2) +
    geom_ribbon(aes(ymin = Q10, ymax = Q90), alpha = 0.2) + geom_vline(xintercept = data_statistics[covariate,"mean"], linetype="dashed") +
    labs(x=res$names.fixed[covariate+1], y="Daily infection rate [%]")
  ggsave(paste(output_folder, "//", res$names.fixed[covariate+1], "_effect.png", sep=""), height=8, width=8)
}