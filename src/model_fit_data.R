# setwd("~/Dropbox/Research/polling-errors/src/")
rm(list = ls())
library(parallel)
library(rstan)
library(data.table)
stan_model <- 'final_model'
stan_file <- sprintf('./%s.stan', stan_model)

for (elec in c('Senatorial','Gubernatorial','Presidential')) {
  # Load prepared input data
  data_file <- sprintf("../data/stan_data_%s.RData", tolower(elec))
  load(data_file)

  # Fit the model to data
  actual_polls_fit <- stan(file = stan_file, data = actual_polls_data, iter = 10000,
                   chains = 8, cores = 8, seed = 1,
                   control = list(adapt_delta = 0.99) )

  # Fit the model to SRS
  srs_polls_fit <- stan(file = stan_file, data = srs_polls_data, iter = 10000,
                  chains = 8, cores = 8, seed = 1,
                  control = list(adapt_delta = 0.99) )
  
  # Store the fit
  result_name <- sprintf("../data/stan_%s_result_%s.RData", tolower(stan_model), tolower(elec))
  save(actual_polls_fit, srs_polls_fit, file = result_name)
}
