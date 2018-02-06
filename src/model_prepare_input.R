# setwd("~/Dropbox/Research/polling-errors/src/")
rm(list = ls())
library(data.table)
set.seed(2)

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

# Generate one result for each poll under SRS
sim_srs_result_f <- function(v_r, n_i) {
  srs_result <- rbinom(length(n_i), size = n_i, prob = v_r) / n_i
  return(srs_result)
}
polls_main$poll_srs_result <- sim_srs_result_f(polls_main$v_r, polls_main$n_i)

# Write data for each election type into a separate RData file
for (elec in c('Senatorial','Gubernatorial','Presidential')) {
  election_polls <- polls_main[election_type == elec]
  
  # Generate correponding vector of r[i]'s for input to Stan model
  election_polls[, election_identifier := paste(as.character(state), as.character(year), as.character(election_type), sep = "_")]
  election_agg_info <- election_polls[, list(
    num_of_polls_in_election = .N,
    v_r = head(v_r, 1)                
    ), by = election_identifier]
  election_polls[, election_index := match(election_identifier, election_agg_info$election_identifier)]

  # aggregate and return all the data necessary to fit the model in stan
  actual_polls_data <- list(
    N_poll = nrow(election_polls),
    N_election = nrow(election_agg_info),
    v = election_agg_info$v_r,
    y = election_polls$y_i,
    r = election_polls$election_index,
    t = election_polls$days_to_election/30,
    n = election_polls$n_i,
    election_identifiers = election_agg_info$election_identifier
  )
  
  # Generate SRS counterpart
  srs_polls_data  <- list(
    N_poll = nrow(election_polls),
    N_election = nrow(election_agg_info),
    v = election_agg_info$v_r,
    y = election_polls$poll_srs_result,
    r = election_polls$election_index,
    t = election_polls$days_to_election/30,
    n = election_polls$n_i,
    election_identifiers = election_agg_info$election_identifier
  )
  
  file_name <- sprintf("../data/stan_data_%s.RData", tolower(elec))
  save(actual_polls_data, srs_polls_data, file = file_name)
}
