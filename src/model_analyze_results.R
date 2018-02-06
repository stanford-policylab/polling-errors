# setwd("~/Dropbox/Research/polling-errors/src/")
rm(list = ls())
library(data.table)
library(ggplot2)
library(grDevices)
library(gridExtra)
library(reshape2)
library(grid)
library(rstan)
library(scales)
library(boot)
theme_set(theme_bw())
stan_model <- 'final_model'

# Convert stan draw results to quantities of interest like election-level bias and variance
gen_estimates_from_one_draw_f <- function(alpha, beta, tao_sqr, model_input) {
  polls_p <- inv.logit( logit(model_input$v[model_input$r]) + alpha[model_input$r] + model_input$t * beta[model_input$r] )
  polls_sigma_sqr <- polls_p*(1-polls_p)/model_input$n + tao_sqr[model_input$r]
  polls_results <- data.table(r = model_input$r, p = polls_p, v = model_input$v[model_input$r], sigma = sqrt(polls_sigma_sqr))
  
  elections_results <- polls_results[, list(
    b_r = mean(p-v),
    sigma_r = mean(sigma)
  ), by = r]
  elections_q <- inv.logit( logit(model_input$v) + alpha )
  elections_results <- cbind(elections_results[order(r)], b_r_0 = (elections_q-model_input$v))
  
  agg_results <- elections_results[, list(
    mu_b = mean(abs(b_r)),
    mu_b_0 = mean(abs(b_r_0)),
    mu_sigma = mean(sigma_r)
  )]
  
  draw_results <- vector(mode="list", length=2)
  names(draw_results) <- c("elections", "overall")
  draw_results[["elections"]] <- elections_results
  draw_results[["overall"]] <- agg_results
  return(draw_results)
}

# Gather fit results for different elections
election_type_level_results <- data.table()
election_level_results <- data.table()
for (elec in c('Senatorial','Gubernatorial','Presidential')) {
  model_input_data_file <- sprintf("../data/stan_data_%s.RData", tolower(elec))
  load(model_input_data_file)
  
  model_result_file <- sprintf('../data/stan_%s_result_%s.RData', tolower(stan_model), tolower(elec))
  # model_result_file <- sprintf('../data/stan_%s_result_%s_prior_inv_gamma.RData', tolower(stan_model), tolower(elec))
  load(model_result_file)
  
  actual_polls_fitted_params <- rstan::extract(actual_polls_fit)
  srs_polls_fitted_params <- rstan::extract(srs_polls_fit)
  
  election_sim_results <- data.table()
  election_type_sim_results <- data.table()
  # Gather quantities of interest in each draw for real data
  n_sim <- dim(actual_polls_fitted_params$alpha)[1]
  for (sim in seq(1, n_sim)) {
    draw_results <- gen_estimates_from_one_draw_f(
      actual_polls_fitted_params$alpha[sim,],
      actual_polls_fitted_params$beta[sim,],
      actual_polls_fitted_params$tao_sqr[sim,],
      actual_polls_data
    )
    election_sim_results <- rbind(election_sim_results, draw_results[["elections"]])
    election_type_sim_results <- rbind(election_type_sim_results, cbind(src = "Actual", draw_results[["overall"]]))
  }
  # Gather quantities of interest in each draw for SRS
  n_sim <- dim(srs_polls_fitted_params$alpha)[1]
  for (sim in seq(1, n_sim)) {
    draw_results <- gen_estimates_from_one_draw_f(
      srs_polls_fitted_params$alpha[sim,],
      srs_polls_fitted_params$beta[sim,],
      srs_polls_fitted_params$tao_sqr[sim,],
      srs_polls_data
    )
    election_type_sim_results <- rbind(election_type_sim_results, cbind(src = "SRS", draw_results[["overall"]]))
  }
  
  # Generate point estimates and applicable standard deviation of posterior distributions
  elections_point_estimates <- election_sim_results[, list(
    bias = mean(b_r),
    sd = mean(sigma_r)
  ), by = c("r")]
  overall_measures_point_estimates <- election_type_sim_results[, list(
    mu_b = mean(mu_b),
    mu_b_sd = sd(mu_b),
    mu_b_0 = mean(mu_b_0),
    mu_b_0_sd = sd(mu_b_0),
    mu_sigma = mean(mu_sigma),
    mu_sigma_sd = sd(mu_sigma)
  ), by = c("src")]
  
  # Gather results in combined data tables
  election_level_results <- rbind(
    election_level_results,
    data.table(
      election_identifier = actual_polls_data$election_identifiers,
      bias = elections_point_estimates[order(r)]$bias,
      sd = elections_point_estimates[order(r)]$sd
    )
  )
  election_type_level_results <- rbind(
    election_type_level_results,
    cbind(election_type = elec, overall_measures_point_estimates)
  )
  print(sprintf("Results for %s elections are gathered", elec))
}

# Write model results to the log file
sink('../output/logged_results.tsv', append = TRUE)
cat("\n\n**Model Results**\n")
cat("Average election-level absolute bias [percentage-points]:\n")
results <- election_type_level_results[src == "Actual"]
results <- results[, mu_b := round(100*mu_b,1)]
results <- results[, mu_b_sd := round(100*mu_b_sd,3)]
print(results[, c("election_type", "mu_b", "mu_b_sd"), with= FALSE])
cat("Average election-level absolute bias [percentage-points] on election day:\n")
results <- election_type_level_results[src == "Actual"]
results <- results[, mu_b_0 := round(100*mu_b_0,1)]
results <- results[, mu_b_0_sd := round(100*mu_b_0_sd,3)]
print(results[, c("election_type", "mu_b_0", "mu_b_0_sd"), with= FALSE])
cat("Average election-level standard deviation:\n")
results <- election_type_level_results[src == "Actual"]
results <- results[, mu_sigma := round(100*mu_sigma,1)]
results <- results[, mu_sigma_sd := round(100*mu_sigma_sd,3)]
print(results[, c("election_type", "mu_sigma", "mu_sigma_sd"), with= FALSE])
cat("Average election-level absolute bias [percentage-points] for SRS:\n")
print(
  election_type_level_results[src == "SRS", list(
    avg_abs_election_level_bias = mean(round(100*mu_b,2)),
    avg_abs_elecion_level_elecitonday_bias= mean(round(100*mu_b_0,2)),
    avg_election_level_sd = mean(round(100*mu_sigma,1))
  )]
)
sink()

# Extract useful information from the election identifier
election_level_results$election_type <- gsub("^.*_", "", election_level_results$election_identifier)
election_level_results$state_year_comb <- gsub("_[[:upper:]].*$", "", election_level_results$election_identifier)
election_level_results$year <- as.numeric(gsub("^.*_", "", election_level_results$state_year_comb))
election_level_results$election_type <- factor(
  election_level_results$election_type,
  levels=c('Senatorial','Gubernatorial','Presidential')
)

#################################################
#####  Fig 4: Distribution of model estimates ###
#################################################
#

# Absolute Bias
p <- ggplot(election_level_results, aes(x = abs(bias)))
p <- p + geom_histogram(binwidth = 0.0025, fill = 'grey')
p <- p + scale_y_continuous('Number of Elections\n', limits = c(0, 38), expand = c(0,0))
p <- p + scale_x_continuous('', limits=c(0,0.09), breaks = seq(0,0.09,0.02), labels=percent_format(), expand = c(0,0))
p <- p + facet_grid(. ~ election_type)
p <- p + ggtitle('Estimated election-level absolute bias\n')
p <- p + theme(plot.title=element_text(family="Times", hjust = 0.5))
p_election_bias <- p + theme(
  panel.margin = unit(1, "lines"),
  panel.grid.minor=element_blank()
)

# Standard deviation
p <- ggplot(election_level_results, aes(x = sd))
p <- p + geom_histogram(binwidth = 0.0025, fill = 'grey')
p <- p + scale_y_continuous('Number of Elections\n', limits = c(0,95), expand = c(0,0))
p <- p + scale_x_continuous('', limits=c(0,.09), breaks = seq(0,0.09,0.02), labels=percent_format(), expand = c(0,0))
p <- p + facet_grid(. ~ election_type)
p <- p + ggtitle('Estimated election-level standard deviation\n')
p <- p + theme(plot.title=element_text(family="Times", hjust = 0.5))
p_election_sd <- p + theme(
  panel.margin = unit(1, "lines"),
  panel.grid.minor=element_blank()
)

# Plot absolute bias and standard deviation plots side-by-side
pdf('../output/race_level_bias_and_var.pdf', width=9, height=7)
grid.arrange(p_election_bias, p_election_sd, ncol=1)
dev.off()

#################################################
#####  Fig 5: Poll bias trend over the years ####
#################################################
#

years_avg_abs_bias <- election_level_results[, list(
  avg_abs_bias = mean(abs(bias))
), by = c('year', 'election_type') ]

p <- ggplot(years_avg_abs_bias, aes(x=year, y=avg_abs_bias))
p <- p + scale_size_area() + geom_line(aes(linetype = election_type))
p <- p + scale_linetype_manual(values=c('solid','dashed','dotted'))
p <- p + scale_y_continuous("",limits=c(0,0.045), labels=percent_format(), expand = c(0,0))
p <- p + scale_x_continuous('', breaks = seq(2000, 2012, 4))
p <- p + theme(
  legend.title=element_blank(), 
  legend.position="none", 
  legend.background=element_rect(fill="transparent"),
  plot.title=element_text(family="Times", hjust = 0.5),
  panel.grid.minor=element_blank()
)
p <- p + ggtitle('Average absolute bias\n')
p <- p + annotate(
  "text",
  x=c(1998.8, 2004, 2013.5),
  y=c(0.0375, 0.005, 0.034),
  label= c("Gubernatorial", "Presidential", "Senatorial"),
  size=3
)
ggsave(plot=p, file='../output/stan_finalModel_bias_trend.pdf', width=5.5, height=3.5)

#################################################
# Fig 6: Correlated bias between election types #
#################################################
#

state_year_biases <- dcast.data.table(election_level_results, state_year_comb ~ election_type, value.var = 'bias')
# Remove the outlier
state_year_biases <- state_year_biases[state_year_comb != "UT_2004"]

# Presidential vs. Senatorial plot
p <- ggplot(state_year_biases, aes(x=Senatorial, y=Presidential))
p <- p + geom_point(size = 1, alpha = 0.7) + geom_abline(intercept = 0, slope = 1, lty=2)
p <- p + scale_y_continuous("Presidential", limits=c(-0.09,0.09), breaks = seq(-0.08, 0.08, 0.04), labels=percent_format())
p <- p + scale_x_continuous('\nSenatorial', limits=c(-0.09,0.09), breaks = seq(-0.08, 0.08, 0.04), labels=percent_format())
p1 <- p

# Gubernatorial vs. Senatorial plot
p <- ggplot(state_year_biases, aes(x=Senatorial, y=Gubernatorial))
p <- p + geom_point(size = 1, alpha = 0.7) + geom_abline(intercept = 0, slope = 1, lty=2)
p <- p + scale_y_continuous("Gubernatorial",limits=c(-0.09,0.09), breaks = seq(-0.08, 0.08, 0.04), labels=percent_format())
p <- p + scale_x_continuous('\nSenatorial',limits=c(-0.09,0.09), breaks = seq(-0.08, 0.08, 0.04), labels=percent_format())
p2 <- p

# Gubernatorial vs. Presidential plot
p <- ggplot(state_year_biases, aes(x=Presidential, y=Gubernatorial))
p <- p + geom_point(size = 1, alpha = 0.7) + geom_abline(intercept = 0, slope = 1, lty=2)
p <- p + scale_y_continuous("Gubernatorial", limits=c(-0.09,0.09), breaks = seq(-0.08, 0.08, 0.04), labels=percent_format())
p <- p + scale_x_continuous('\nPresidential', limits=c(-0.09,0.09), breaks = seq(-0.08, 0.08, 0.04), labels=percent_format())
p3 <- p

# Plot side-by-side
pdf('../output/stan_finalModel_bias_correlation.pdf', width=9, height=3)
grid.arrange(p2, p1, p3, ncol=3)
dev.off()

# Write the correlations to the log file
sink('../output/logged_results.tsv', append = TRUE)
cor_table_1 <- state_year_biases[!is.na(Presidential) & !is.na(Senatorial)]
cat(sprintf(
  "Correlation between senatorial and presidential:\t%0.2f\n",
  round(cor(cor_table_1$Senatorial, cor_table_1$Presidential), 2)
))
cor_table_2 <- state_year_biases[!is.na(Senatorial) & !is.na(Gubernatorial)]
cat(sprintf(
  "Correlation between gubernatorial and senatorial:\t%0.2f\n",
  round(cor(cor_table_2$Gubernatorial, cor_table_2$Senatorial), 2)
))
cor_table_3 <- state_year_biases[!is.na(Presidential) & !is.na(Gubernatorial)]
cat(sprintf(
  "Correlation between gubernatorial and presidential:\t%0.2f\n",
  round(cor(cor_table_3$Gubernatorial, cor_table_3$Presidential), 2)
))
sink()
