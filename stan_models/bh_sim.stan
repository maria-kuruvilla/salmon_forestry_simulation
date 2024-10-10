data {
  int<lower=0> N;
  array[N] real R_S;
  array[N] real S;
  array[N] real forest_loss;
  real pRk_mean;
  real pRk_sig;
}
transformed data {
  real logRk_pr;
  real logRk_pr_sig;
  logRk_pr_sig = sqrt(log(1+((pRk_sig)*(pRk_sig))/((pRk_mean)*(pRk_mean))));
  logRk_pr = log(pRk_mean) - 0.5*logRk_pr_sig^2;
}

parameters {
  real alpha;
  real<lower=0> Rk;
  real b;
  real<lower=0> sigma;
}

transformed parameters {
  vector[N] e_t; //stock residual productivity at time t
  vector[N] mu1; //initial expectation at each time
  
  mu1[1] = alpha - log(1+(exp(alpha)/Rk)*S[1]);
  e_t[1] = R_S[1] - mu1[1];
  
  for(t in 2:N){
    mu1[t] = alpha - log(1+(exp(alpha)/Rk)*S[t-1]); //adjust expectation based on previous deviate - rho is raised to the power of the number of time steps (in years) between observations
    e_t[t] = R_S[t] - mu1[t];
  }
  
}

model {
  b ~ normal(0,1);
  sigma ~ normal(0,1);
  alpha ~ normal(4, 5); // uniform prior on interval 0,1
  Rk ~ lognormal(logRk_pr, logRk_pr_sig);
  R_S ~ normal(mu1,sigma);
}


