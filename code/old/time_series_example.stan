data {
  int<lower=1> N;            // num observations
  real y[N];                 // observed outputs
}
parameters {
  real mu;                   // mean coeff
  real phi;                  // autoregression coeff
  real theta;                // moving avg coeff
  real<lower=0> sigma;       // noise scale
}
model {
  vector[N] nu;              // prediction for time t
  vector[N] err;             // error for time t
  nu[1] = mu + phi * mu;     // assume err[0] == 0
  err[1] = y[1] - nu[1];
  for (t in 2:N) {
    nu[t] = mu + phi * y[t-1] + theta * err[t-1];
    err[t] = y[t] - nu[t];
  }
  mu ~ normal(0, 1);        // priors
  phi ~ normal(0, 1);
  theta ~ normal(0, 1);
  sigma ~ cauchy(0, 1);
  err ~ normal(0, sigma);    // likelihood
}