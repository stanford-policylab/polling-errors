# polling-errors

This repo contains data and code for the paper "Disentangling Bias and Variance in Election Polls" by Houshmand Shirani-Mehr, David Rothschild, Sharad Goel, and Andrew Gelman.

The original paper can be accesses here: https://5harad.com/papers/polling-errors.pdf

To reproduce the results, the R scripts should be run from the 'src' directory.

## Data Description
---------------------------------------
1. polls_main_dataset.tsv
  - The primary dataset used for analysis in the paper, containing ~ 4,200 polls completed during the final three weeks of 608 state-level presidential, senatorial, and gubernatorial elections between 1998 and 2014. The dataset contains:
    - 4,154 state-level polls for elections in 1998–2013 that were collected and made available by FiveThirtyEight, all of which were completed during the final three weeks of the campaigns.
    - 67 state-level 2014 polls posted on Pollster.com, where for consistency with the FiveThirtyEight data, we consider only those completed in the last three weeks of the campaigns.

2. polls_auxiliary_dataset.tsv
  - The secondary dataset used for analyzing RMSE of polls over time in the paper. This dataset contains  7,040 polls completed during the last 100 days of 314 state-level presidential, senatorial, and gubernatorial elections between 2004 and 2012. All polls for this secondary dataset were obtained from Pollster.com and RealClearPolitics.com.

## Recommended order to run the scripts
---------------------------------------
1. data_exploration.R
  - Prelimiary analysis of the data
  - Generates figures 1, 2, and 3 in the paper

2. model_prepare_input.R
  - Prepares the input data for the Stan model
  - Writes the prepared data as RData files into `data/`

3. model_fit_data.R
  - Fits the Bayesian model to data using Stan
  - Writes the fit object into `data/`

4. model_analyze_results.R
  - Analysis for the results of the Bayesian model
  - Generates figures 4, 5, and 6 in the paper