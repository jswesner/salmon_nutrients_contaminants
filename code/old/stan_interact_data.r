nT <- 100;
dt <- 0.9;
A <- matrix(c(1, dt, 0, 1), nrow = 2, ncol = 2, byrow = TRUE);
origX <- matrix(data = 0, nrow = 2, ncol = nT);
X <- matrix(data = 0, nrow = 2, ncol = nT);

x0 <- matrix(c(2000, 100), nrow = 2, ncol = 1);

X[, 1] <- x0;
origX[, 1] <- x0;

for( iT in 2:nT )
{
  
  origX[, iT] <- A %*% origX[, iT - 1];
  
}

Q <- matrix(c(20^2, 0, 0, 1^2), nrow = 2, ncol = 2, byrow = TRUE);
sqrtQ <- t(chol(Q));

for(iT in 1:nT )
{
  
  # X[, iT] <- X[, iT] + trnQ*rnorm(1, 0, sigmaa);
  X[, iT] <- origX[, iT] + sqrtQ %*% matrix(rnorm(2), nrow = 2, ncol = 1);
  
}

Y <- matrix(0, nrow = 2, ncol = nT)
V <- matrix(c(100^2, 3e-5, 3e-5, 4^2), nrow = 2, ncol = 2);
sqrtV <- t(chol(V));

for( iT in 1:nT )
{
  
  Y[, iT] <- X[, iT] + sqrtV %*% matrix(rnorm(2), nrow = 2, ncol = 1);
  
}

centerQ <- Q/5;


fit <- stan("stan_interact.stan", 
            data=list(dt=dt, 
                      N=nT, 
                      y=t(Y), 
                      V=V, 
                      A=A, 
                      Q0=centerQ), 
            chains = 1, iter = 1000)