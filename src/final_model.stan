data {
  int<lower=1> N_poll; // number of polls
  int<lower=1> N_election; // number of elections
  vector<lower=0,upper=1>[N_election] v; // election results
  vector<lower=0,upper=1>[N_poll] y; // poll results
  int<lower=1,upper=N_election> r[N_poll]; // index specifying the election for each poll
  vector<lower=0,upper=1.5>[N_poll] t; // (number of days before the election each poll was conducted)/30
  vector<lower=1>[N_poll] n; // number of respondents in each poll
}

transformed data {
  vector<lower=-2,upper=2>[N_election] logit_v;
  for (i in 1:N_election)
    logit_v[i] = logit(v[i]);
}

parameters {
  real bias_a;  // mean of alpha
  real bias_b;  // mean of beta
  real<lower=0> sigma_a; // standard deviation of alpha
  real<lower=0> sigma_b; // standard deviation of beta
  real<lower=0> sigma_tao; // standard deviation of excess variance
  vector[N_election] alpha; // bias component independent of time
  vector[N_election] beta; // time-dependent component of bias
  vector<lower=0>[N_election] tao_sqr; // poll excess variance
  }
model {
  vector[N_poll] p;
  vector[N_poll] logit_p;
  sigma_a ~ normal(0, 0.2);
  sigma_b ~ normal(0, 0.2);
  sigma_tao ~ normal(0, 0.05);
  bias_a ~ normal(0, 0.2);
  bias_b ~ normal(0, 0.2);

  alpha ~ normal(bias_a, sigma_a);
  beta ~ normal(bias_b, sigma_b);
  tao_sqr ~ normal(0, sigma_tao);
  
  logit_p = logit_v[r] + t .* beta[r] + alpha[r];

  for (i in 1:N_poll)
    p[i] = inv_logit( logit_p[i] );

  for (i in 1:N_poll)
    y[i] ~ normal(p[i], sqrt( p[i]*(1-p[i])/n[i] + tao_sqr[r[i]] ));
    
}