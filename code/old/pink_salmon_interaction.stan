data {
  int<lower=0> N;
  vector[N] y;
  int location[N];
  }

parameters {
  real<lower=0> x0;
  real<lower=-0.999,upper=0.999> phi;
  vector[N-1] pro_dev;
  real<lower=0> sigma_process;
  real<lower=0> sigma_obs;
  }
  
transformed parameters {
  vector[N] pred;
  pred[1] = x0;
  for(i in 2:N) {
    pred[i] = phi*pred[i-1][location[i]] + pro_dev[i-1];
    }
    }

model {
      x0 ~ lognormal( log(2) , 1 );
      phi ~ normal(0,1);
      sigma_process ~ cauchy(0,1);
      sigma_obs ~ cauchy(0,1);
      pro_dev ~ normal(0, sigma_process);
      y ~ lognormal(pred, sigma_obs);
      }
      
