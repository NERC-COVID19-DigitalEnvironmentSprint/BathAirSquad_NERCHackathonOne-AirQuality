## ---------------------------
##
## Script name: INLA_run_Bayesian_model
##
## Purpose of script: Run Bayesian inference for given model and presents it results graphically.
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
##   Code includes all commands included in Run_Bayesian_Model.ipynb Jupyter Notebook.
##   Notebook includes detailed explaination of the commands used.
##   
## ---------------------------

# Set the working directory

setwd("C://Users//PiotrMorawiecki//Desktop//Hackathon")

# Source functions used below from the INLA_Bayesian_Framework.R file.

source("INLA_Bayesian_Framework.R")

# Using default model

settings <- load_default_model(model=3)

results <- run_Bayesian_model(settings$formula, settings$use_weekly, settings$use_deaths,
                              settings$use_Poisson, settings$output_folder)

# Using own model

formula       <- new_cases ~ AQI_2019 + popdensity + f(id, model = "iid") +  f(day, model = "ar1") + f(rowId, model = "iid")
use_weekly    <- TRUE
use_deaths    <- TRUE
use_Poisson   <- TRUE
output_folder <- "INLA_results//Model_1_deaths_poisson"

results <- run_Bayesian_model(formula, use_weekly, use_deaths, use_Poisson, output_folder)

# Results postprocessing

results$INLA_result$summary.fixed

options(repr.plot.width=6, repr.plot.height=3)
plot_fixed_parameters(results$INLA_result, settings$output_folder)

options(repr.plot.width=6, repr.plot.height=3)
plot_covariates_effect(results$INLA_result, results$data_statistics, settings$use_weekly, settings$output_folder, n_sims=10000)