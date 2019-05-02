# Optimal CRXO trial design under continuous-time correlation
# decay with fixed trial duration
#
# Kelsey Grantham (kelsey.grantham@monash.edu)

library(tidyr)
library(plyr)
library(dplyr)
library(ggplot2)
library(ltsa)
library(numbers)

desmat <- function(Tp, N){
  # Generates design matrix for a balanced CRXO design with
  # N clusters and Tp periods
  Xcrxo <- matrix(data=0, ncol=Tp, nrow=N)
  Xcrxo[1:N/2, seq(1,Tp,2)] <- 1
  Xcrxo[(N/2 + 1):N, seq(2,Tp,2)] <- 1
  return(Xcrxo)
}

vartheta <- function(N, Tp, Vi_inv) {
  # Returns variance of treatment effect estimator for an inverse covariance
  # matrix and the dimensions of a design matrix
  # Note: Assumes balanced CRXO design for faster computation
  
  m <- nrow(Vi_inv)/Tp
  Xmat <- desmat(Tp, N)
  
  Q <- Xmat %x% t(rep(1,m))
  C <- diag(Tp) %x% rep(1,m)
  term1 <- sum(diag(Q %*% Vi_inv %*% t(Q)))
  term2 <- (N/2) * colSums(Vi_inv) %*% C
  term3 <- solve(t(C) %*% Vi_inv %*% C)
  var <- 1/(term1 - (1/N)*term2 %*% term3 %*% t(term2))
  return(var)
}

contdecayVi <- function(r, M, rho0){
  # Constructs the covariance matrix for a single cluster, under the
  # continuous time decay model for a given total number of subjects
  #
  # Inputs:
  # r - 1-rate of decay over entire trial
  # M - total number of subjects in each cluster
  # rho0 - base correlation between subjects measured at same time
  
  totalvar <- 1
  sig2CP <- rho0*totalvar
  sig2E <- totalvar - sig2CP
  Vi <- diag(sig2E,M) +
        sig2CP*(r^(abs(matrix(rep(1:M), nrow=M, ncol=M, byrow=FALSE) -
                       matrix(rep(1:M), nrow=M, ncol=M, byrow=TRUE))/M))
  return(Vi)
}

optimal_T <- function(r, rho0, M, N){
  # Returns optimal number of periods (giving lowest variance)
  # for specified correlation values, number of subjects
  # in a cluster, M, and number of clusters, N (even).
  # Also saves results for all configurations
  
  d <- divisors(M)
  # Take only even T>=2 from divisors of M
  Tps <- d[d>=2 & d %% 2 == 0]
  # Generate covariance matrix for a cluster
  V <- contdecayVi(r=r, rho0=rho0, M=M)
  # All matrices are Toeplitz since we are assuming evenly-spaced times
  Vi_inv <- TrenchInverse(V)
  # Calculate variances
  vars <- mapply(vartheta, N, Tps, MoreArgs=list(Vi_inv=Vi_inv))
  all <- data.frame(Tp=Tps, variance=vars)
  if(r==1){rchar <- 100}else{rchar <- strsplit(as.character(r),"\\.")[[1]][2]}
  rho0char <- strsplit(as.character(rho0),"\\.")[[1]][2]
  save(all, file=paste0("results/all_M_", M, "_N_", N, "_r_", rchar, "_rho_", rho0char, ".Rda"))
  optimal <- all[which.min(all$variance),]
  return(list(opt=optimal, all=all))
}

total_cost <- function(N, Tp, M, c, s, x){
  # Returns total trial cost, given:
  #   - number of clusters, N
  #   - number of periods, Tp
  #   - cluster size, M
  #   - cost per cluster, c
  #   - cost per subject, s
  #   - cost per crossover, x
  
  B <- N*c + (N*M)*s + N*(Tp-1)*x
  return(B)
}

optimal_N_T_fixedM <- function(r, rho0, M, maxN, B, c, s, x){
  # Returns optimal number of clusters and periods
  # (giving lowest variance) for given:
  #   - proportionate decay in correlation, r
  #   - base correlation, rho0
  #   - total number of subjects per cluster, M
  #   - maximum number of clusters, maxN
  #   - total trial budget, B
  #   - cluster cost, c
  #   - subject cost, s
  #   - crossover cost, x
  
  Ns <- seq(2, maxN, 2) # all possible numbers of clusters
  dM <- divisors(M)
  Tps <- dM[dM %% 2 == 0] # all even divisors of M
  # Determine combinations of N and T that stay within budget
  all <- data.frame(N=rep(Ns, each=length(Tps)), Tp=rep(Tps, times=length(Ns)), cost=NA)
  all$cost <- total_cost(all$N, all$Tp, M, c, s, x)
  if(sum(all$cost <= B) == 0){
    stop('No admissible designs within budget')
  }else{
    underbudget <- all[all$cost <= B,]
    V <- contdecayVi(r=r, rho0=rho0, M=M)
    Vi_inv <- TrenchInverse(V)
    underbudget$variance <- mapply(vartheta, underbudget$N, underbudget$Tp,
                                   MoreArgs=list(Vi_inv=Vi_inv))
    underbudget$RE <- min(underbudget$variance)/underbudget$variance
    # Save results to R data file
    if(r==1){rchar <- 100}else{rchar <- strsplit(as.character(r),"\\.")[[1]][2]}
    rho0char <- strsplit(as.character(rho0),"\\.")[[1]][2]
    save(underbudget, file=paste0("results/r", rchar, "_rho", rho0char, "_M",
                                  M, "_maxN", maxN, "_c", c, "_s", s, "_x", x, ".Rda"))
    return(underbudget)
  }
}

pow <- function(vars, effsize, siglevel=0.05){
  z <- qnorm(siglevel/2)
  pow <- pnorm(z + sqrt(1/vars)*effsize)
  return(pow)
}