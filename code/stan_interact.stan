data {
  real dt;
  int N;
  vector[2] y[N];
  cov_matrix[2] V;
  matrix[2,2] A;
}
transformed data {
  matrix[2, 2] V_sqrt;
  V_sqrt = cholesky_decompose(V);
}
parameters {
  vector[2] x_tilde[N];
  cov_matrix[2] Q;
}
transformed parameters {
  matrix[2, 2] Q_sqrt;
  matrix[2, 2] QV_sqrt;
  vector[2] x[N];
  Q_sqrt = cholesky_decompose(Q);
  QV_sqrt = cholesky_decompose(Q + V);
  
  x[1] = y[1] + QV_sqrt*x_tilde[1];
  
  for( t in 2:N )
  {
    
    x[t] = A*x[t-1] + Q_sqrt*x_tilde[t];
    
  }
  
}
model {
  
  for( t in 1:N )
  {
    
    x_tilde[t] ~ std_normal();
    
  }
  
  for( t in 1:N )
  {
    
    y[t] ~ multi_normal(x[t], V);
    
  }
    
}