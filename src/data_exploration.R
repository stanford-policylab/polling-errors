# setwd("~/Dropbox/Research/polling-errors/src/")
rm(list = ls())
library(data.table)
library(ggplot2)
library(scales)
library(grDevices)
library(gridExtra)
library(grid)
library(reshape2)
theme_set(theme_bw())
set.seed(235)

# Generate output directory for figures
dir.create(file.path('../', "output"), showWarnings = FALSE)

# Pre-process polls and remove corrupted entries
preprocess_polls_f <- function(input_polls_data) {
  input_polls_data <- input_polls_data[, y_i:= republican/(democratic+republican)]
  input_polls_data <- input_polls_data[, v_r := finalTwoPartyVSRepublican/100]
  input_polls_data <- input_polls_data[, twoparty_voteshare_error := y_i - v_r]
  input_polls_data <- input_polls_data[, days_to_election := as.integer(as.Date(electionDate)-as.Date(endDate)) ]
  input_polls_data <- input_polls_data[, n_i := round(numberOfRespondents * (democratic + republican) / 100)]
  input_polls_data <- input_polls_data[, state_year_concat := paste(as.character(state), as.character(year), sep = "_")]
  
  # Remove corrupted data entries
  input_polls_data <- input_polls_data[!is.na(n_i) & !is.na(twoparty_voteshare_error) & !is.na(state_year_concat) & !is.na(days_to_election)]
  input_polls_data <- input_polls_data[days_to_election >= 1]
  # Don't consider polls too far from the election, national polls, or house election polls
  input_polls_data <- input_polls_data[(days_to_election <= 100) & (state != "USA") & (election != "House")]
  
  input_polls_data$election_type = ""
  input_polls_data[election == "Sen"]$election_type <- "Senatorial"
  input_polls_data[election == "Pres"]$election_type <- "Presidential"
  input_polls_data[election == "Gov"]$election_type <- "Gubernatorial"
  input_polls_data$election_type <- factor(
    input_polls_data$election_type,
    levels=c('Senatorial','Gubernatorial','Presidential')
  )
  input_polls_data$election <- NULL
  
  return(input_polls_data)
}

# Load data
polls_main <- preprocess_polls_f( data.table( read.delim("../data/polls_main_dataset.tsv") ) )
polls_extra <- preprocess_polls_f( data.table( read.delim("../data/polls_auxiliary_dataset.tsv") ) )

# For main dataset, only consider polls conducted in last 3 weeks
polls_main <- polls_main[days_to_election <= 21]

# Write stats of the dataset to the log file
sink('../output/logged_results.tsv')
cat("**Data Description**\n")
cat(sprintf(
  "Number of polls in primary dataset:\t%d\n",
  nrow(polls_main)
  ))
cat(sprintf(
  "Number of polls from 538 in primary dataset:\t%d\n",
  nrow(polls_main[year < 2014])
  ))
cat(sprintf(
  "Number of elections in primary dataset:\t%d\n",
  nrow(polls_main[, list(.N), by = c("election_type", "state_year_concat")])
  ))
cat("Breakdown of poll numbers based on election type in primary dataset:\n")
print(polls_main[, list(n = .N), by = election_type])
cat("Breakdown of election numbers based on election type in primary dataset:\n")
print(polls_main[, list(n = length(unique(state_year_concat))), by = election_type])
cat(sprintf(
  "Number of polls in auxiliary dataset:\t%d\n",
  nrow(polls_extra)
  ))
cat(sprintf(
  "Number of elections in auxiliary dataset:\t%d\n",
  nrow(polls_extra[, list(.N), by = c("election_type", "state_year_concat")])
  ))
cat("\n\n")
sink()


#################################################
##############  Fig 1: Overall error ############
#################################################
#

# Function to simulate a SRS poll
sim_srs_result_f <- function(v_r, n_i) {
  srs_result <- rbinom(length(n_i), size = n_i, prob = v_r) / n_i
  return(srs_result)
}

# Generate many SRS polls based on real polls and calculate their error
each_election_type_srs_polls_count <- 1000000
selected_polls_indexes <- c( 
  sample(which(polls_main$election_type == "Senatorial"), each_election_type_srs_polls_count, replace = T),
  sample(which(polls_main$election_type == "Presidential"), each_election_type_srs_polls_count, replace = T),
  sample(which(polls_main$election_type == "Gubernatorial"), each_election_type_srs_polls_count, replace = T)
  )
selected_polls_srs_results <- sim_srs_result_f(
  polls_main$v_r[selected_polls_indexes],
  polls_main$n_i[selected_polls_indexes]
  )
srs_polls_data <- data.table(
  election_type = polls_main$election_type[selected_polls_indexes],
  twoparty_voteshare_error = selected_polls_srs_results - polls_main$v_r[selected_polls_indexes]
  )

# Gather real polls and SRS polls in a single data table to plot the results
plot_dt <- rbind(
  cbind( src = "Actual" , polls_main[, c("election_type", "twoparty_voteshare_error"), with = FALSE] ),
  cbind( src = "SRS" , srs_polls_data ) 
  )

# Plot the results
histogram_bw <- 0.01

# Density plot (y-axis is density)
p <- ggplot()
p <- p + geom_histogram(data = plot_dt[src == "Actual"], aes(x=twoparty_voteshare_error, y=..density..), binwidth = histogram_bw, fill='grey')
p <- p + geom_density(data = plot_dt[src == "SRS"], aes(x=twoparty_voteshare_error), linetype='dashed')
p <- p + scale_y_continuous('', breaks = NULL, expand = c(0,0))
p <- p + scale_x_continuous('', breaks=seq(-.1,0.1,0.05), limits=c(-.1, .1), labels=percent_format())
p <- p + ggtitle('Difference between poll results and election outcomes\n')
p <- p + facet_grid(. ~ election_type)
p <- p + theme(
  panel.margin = unit(1.5, "lines"),
  plot.title=element_text(family="Times", hjust = 0.5)
)
ggsave(plot=p, file='../output/errors_elections_density_plot.pdf', width=9, height=3.5)

# Histogram plot (y-axis is counts)
p <- ggplot()
p <- p + geom_histogram(data = plot_dt[src == "Actual"], aes(x=twoparty_voteshare_error), binwidth = histogram_bw, fill='grey')
p <- p + geom_density(data = plot_dt[src == "SRS"], aes(x=twoparty_voteshare_error), linetype='dashed')
p <- p + scale_y_continuous('Number of polls\n', limits=c(0,360), expand = c(0,0))
p <- p + scale_x_continuous('', breaks=seq(-.1,0.1,0.05), limits=c(-.1, .1), labels=percent_format())
p <- p + ggtitle('Difference between poll results and election outcomes\n')
p <- p + facet_grid(. ~ election_type)
p <- p + theme(
  panel.margin = unit(1.5, "lines"),
  plot.title=element_text(family="Times", hjust = 0.5)
  )
# Scale the density plot of SRS polls to make them match corresponding counts in the histogram plot
scale_density_to_match_hist <- function(panel_data, num_of_election_polls, histogram_bw){
  # For a density plot the AUC is 1, to generate the corresponding count plot we match the AUC to the corresponding histogram plot
  scaling_factor = num_of_election_polls * histogram_bw
  return(panel_data$ymax * scaling_factor)
}
q <- ggplot_build(p)
q$data[[2]][q$data[[2]]$PANEL == 1,'ymax'] <- scale_density_to_match_hist(
  q$data[[2]][q$data[[2]]$PANEL == 1,],
  nrow(plot_dt[(src == "Actual") & (election_type == "Senatorial")]),
  histogram_bw
)
q$data[[2]][q$data[[2]]$PANEL == 2,'ymax'] <- scale_density_to_match_hist(
  q$data[[2]][q$data[[2]]$PANEL == 2,],
  nrow(plot_dt[(src == "Actual") & (election_type == "Gubernatorial")]),
  histogram_bw
)
q$data[[2]][q$data[[2]]$PANEL == 3,'ymax'] <- scale_density_to_match_hist(
  q$data[[2]][q$data[[2]]$PANEL == 3,],
  nrow(plot_dt[(src == "Actual") & (election_type == "Presidential")]),
  histogram_bw
)
q <- ggplot_gtable(q)
ggsave(plot=q, file='../output/errors_elections.pdf', width=9, height=3.5)

# Compute porition of polls that have the final result inside their confidence interval
polls_main[, se := sqrt(y_i*(1-y_i)/n_i)]
polls_main[, lower_ci_bound := y_i - 2*se]
polls_main[, upper_ci_bound := y_i + 2*se]
polls_main[, in_ci := ifelse( (v_r <= upper_ci_bound) & (v_r >= lower_ci_bound) , 1 , 0)]

# Write stats about RMSE and confidence intervals to the log file
sink('../output/logged_results.tsv', append = TRUE)
cat("**Estimating Total Survey Error**\n")
cat("RMSE of polls in different election types:\n")
rmse_results <- plot_dt[, list( rmse = sqrt(mean(twoparty_voteshare_error^2)) )
               , by = c('election_type','src')]
results <- rmse_results[src == "Actual", c("election_type", "rmse"), with = FALSE]
results <- results[, rmse := round(100*rmse,1)]
print(results)
cat("RMSE of SRS polls in different election types:\n")
results <- rmse_results[src == "SRS", c("election_type", "rmse"), with = FALSE]
results <- results[, rmse := round(100*rmse,1)]
print(results)
cat("Percentage of polls that have the final result of election in their 95% confidence interval:\n")
results <- polls_main[, list(in_ci_portion = mean(in_ci)), by = election_type]
results <- results[, in_ci_portion := round(100*in_ci_portion, 0)]
print(results)
cat("\n\n")
sink()


#################################################
##############  Fig 2: RMSE over time ###########
#################################################
#

# RMSE for polls in a rolling 7-days window
time_window_len <- 7
time_windows_rmse_dt <- data.table()
for (d in seq(0,90)) {
  # Limit to a 7-day window centered at d
  polls_extra_in_window <- polls_extra[(days_to_election > d - time_window_len/2) & (days_to_election < d + time_window_len/2)]
  # Compute RMSE
  rmse_in_window <- polls_extra_in_window[, list(
    count = .N,
    rmse = sqrt(mean(twoparty_voteshare_error^2)) 
    ), by = c('election_type') ]
  # Add to the result data table
  time_windows_rmse_dt <- rbind(
    time_windows_rmse_dt,
    cbind(
      days_to_election = rep(d,3),
      rmse_in_window[, c("election_type", "rmse"), with = FALSE] 
      )
    )
}

# Plot
p <- ggplot(time_windows_rmse_dt, aes(x = days_to_election, y = rmse)) 
p <- p + geom_line()
p <- p + scale_x_reverse('\nDays to Election', limits = c(90,0), breaks = seq(0,90,10) )
p <- p + facet_grid(election_type ~ .)
p <- p + scale_y_continuous("",limits=c(0,0.094), breaks = c(0,0.02,0.04,0.06,0.08), labels=percent_format(), expand = c(0,0))
p <- p + ggtitle('Root mean square poll error over time\n')
p <- p + theme(plot.title=element_text(family="Times", hjust= 0.5), panel.grid.minor=element_blank())
p <- p + geom_vline(aes(xintercept = 21), linetype = "dashed")
ggsave(plot=p, file='../output/rmse_trend_before_election_rev.pdf', width=5, height=5.5)


#################################################
#### Fig 3: Average error for each election #####
#################################################
#

# Generate one result for each poll under (1) SRS and (2) SRS with twice the variance
polls_main$poll_srs_result <- sim_srs_result_f(polls_main$v_r, polls_main$n_i)
polls_main$poll_srs_result_with_2x_variance <- sim_srs_result_f(polls_main$v_r, round(polls_main$n_i/2))

# Structure results of SRS and real polls in one data table
plot_dt <- melt(
  polls_main,
  id.vars=c("election_type", "state_year_concat", "v_r"),
  measure.vars = c("y_i", "poll_srs_result", "poll_srs_result_with_2x_variance"),
  variable.name = "src",
  value.name = "y_i"
  )
plot_dt <- plot_dt[, list(
  y_bar_r = mean(y_i),
  v_r = head(v_r, 1)
  ), by = c('src', 'election_type', 'state_year_concat')]
plot_dt[src == "y_i"]$src <- "Actual"
plot_dt[src == "poll_srs_result"]$src <- "SRS"
plot_dt[src == "poll_srs_result_with_2x_variance"]$src <- "SRS with twice the variance"

# Plot the results
p <- ggplot()
p <- p + geom_point(data = plot_dt, aes(x= v_r, y= y_bar_r-v_r, size=count), alpha = 0.5, size = 1)
p <- p + scale_y_continuous('', limits=c(-.1,.1), labels=percent_format())
p <- p + scale_x_continuous('\nElection outcome', limits=c(0.25,0.75), breaks = seq(0.3,0.7,0.1), labels=percent_format())
p <- p + geom_abline(slope=0, intercept=0, linetype = 'dashed')
p <- p + facet_grid(election_type ~ src)
p <- p + ggtitle('Difference between polling averages and election outcomes\n')
p <- p + theme(
  legend.title=element_blank(),
  legend.position="bottom",
  legend.direction="horizontal",
  plot.title=element_text(family="Times", hjust = 0.5),
  panel.margin = unit(1.5, "lines"),
  panel.grid.minor=element_blank()
  )
ggsave(plot=p, file='../output/avg_poll_result_error_vs_election_outcome.pdf', width=8.5, height=5.5)
