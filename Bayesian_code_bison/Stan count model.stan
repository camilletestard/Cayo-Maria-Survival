data {
  int<lower=0> N; // Number of dyads
  int<lower=0> event_count[N]; // Counts of behaviour for each dyad
  vector<lower=0>[N] obs; // Observation effort per dyad
  real log_p_mu;
  real<lower=0> log_p_sigma;
}

parameters {
  vector[N] log_p; // Logit edge weights for each dyad
}

model {
  // Main model
  event_count ~ poisson(exp(log_p).*obs);

  // Priors
  log_p ~ normal(log_p_mu, log_p_sigma);
}

generated quantities {
  int event_pred[N] = poisson_rng(exp(log_p).*obs);
}
