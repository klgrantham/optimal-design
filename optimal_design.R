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

desmat <- function(Tp, nclust){
  Xcrxo <- matrix(data=0, ncol=Tp, nrow=nclust)
  Xcrxo[1:nclust/2, seq(1,Tp,2)] <- 1
  Xcrxo[(nclust/2 + 1):nclust, seq(2,Tp,2)] <- 1
  return(Xcrxo)
}

vartheta_ind_vec <- function(Vi, Xmat){
  # Calculates the variance of the treatment effect, theta, for a model at the
  # individual level with a particular treatment schedule
  #
  # Inputs:
  # Xmat - a vector of K x T matrices of the treatment schedule (note: all elements either 0 or 1)
  # Vi - a Tm x Tm variance matrix for one cluster
  
  # All matrices are Toeplitz since we are assuming evenly-spaced times
  Vi_inv <- TrenchInverse(Vi)
  # Calculate variance using inverse covariance matrix for each design matrix
  vars <- laply(Xmat, vartheta, Vi_inv)
  return(vars)
}

vartheta <- function(Xmat, Vi_inv) {
  # Returns variance of treatment effect estimator for an inverse covariance
  # matrix and a design matrix
  
  K <- nrow(Xmat)
  Tp <- ncol(Xmat)
  m <- nrow(Vi_inv)/Tp
  
  Q <- Xmat %x% t(rep(1,m))
  B <- colSums(Xmat) %x% rep(1,m)
  C <- diag(Tp) %x% rep(1,m)
  term1 <- sum(diag(Q %*% Vi_inv %*% t(Q)))
  term2 <- t(B) %*% Vi_inv %*% C
  term3 <- solve(t(C) %*% Vi_inv %*% C)
  term4 <- t(C) %*% Vi_inv %*% B
  var <- 1/(term1 - (1/K)*term2 %*% term3 %*% term4)
  return(var)
}

contdecayVi <- function(r, M, rho0){
  # Constructs the covariance matrix for a single cluster, under the
  # continuous time decay model for a given total number of subjects
  #
  # Inputs:
  # r - 1-rate of decay over entire trial
  # M - total number of subjects each cluster
  # rho0 - base correlation between subjects measured at same time
  
  totalvar <- 1
  sig2CP <- rho0*totalvar
  sig2E <- totalvar - sig2CP
  Vi <- diag(sig2E,M) +
        sig2CP*(r^(abs(matrix(rep(1:M), nrow=M, ncol=M, byrow=FALSE) -
                       matrix(rep(1:M), nrow=M, ncol=M, byrow=TRUE))/M))
  return(Vi)
}


## For different rates of decay, how different are the
## gains in precision going from T=2 to T=4?

vars_doubling <- function(rs, M){
  Xmats <- list(desmat(2,2), desmat(4,2), desmat(8,2), desmat(16,2), desmat(32,2))
  ctvarmat <- llply(rs, contdecayVi, M, 0.035)
  ctres <- laply(ctvarmat, vartheta_ind_vec, Xmat=Xmats)
  varvals <- data.frame(decay=1-rs, varT2=ctres[,1], varT4=ctres[,2],
                        varT8=ctres[,3], varT16=ctres[,4], varT32=ctres[,5])
  return(varvals)
}

optimal_T <- function(r, rho0, M){
  # Returns optimal number of periods (giving lowest variance)
  # for specified correlation values and number of subjects
  # in a cluster. Assumes two clusters.
  
  d <- divisors(M)
  # Take only T>=2 from divisors of M
  Tps <- d[which(d>=2)]
  # Generate covariance matrix for a cluster
  V <- contdecayVi(r=r, rho0=rho0, M=M)
  # Generate design matrices for all values of T
  Xmats <- llply(Tps, desmat, 2)
  vars <- vartheta_ind_vec(V, Xmats)
  results <- data.frame(Tp=Tps, variance=vars)
  optimal <- results[which.min(results$variance),]
  return(list(opt=optimal, all=results))
}

optimal_N_T <- function(r, rho0, NTm){
  # Returns optimal number of clusters and periods
  # (giving lowest variance) for specified correlation values,
  # number of subjects in trial.

  d <- divisors(NTm)
  # Take only even N values from divisors of NTm
  Ns <- d[which(d %% 2 == 0 & d != NTm)]
  res <- list()
  for (i in 1:length(Ns)) {
    N <- Ns[i]
    M <- NTm/N
    V <- contdecayVi(r=r, rho0=rho0, M=M)
    # Must have T>=2 and m>=1
    # Possible values of T will be all divisors of each M except 1
    dM <- divisors(M)
    Tps <- dM[dM != 1]
    Xmats <- llply(Tps, desmat, N)
    vars <- vartheta_ind_vec(V, Xmats)
    res[[i]] <- cbind(rep(N, length(Tps)), Tps, vars)
  }
  resblock <- do.call("rbind", res)
  results <- data.frame(N=resblock[,1], T=resblock[,2], variance=resblock[,3])
  optimal <- results[which.min(results$variance),]
  return(list(opt=optimal, all=results))
}

total_cost <- function(N, Tp, NTm, c, s, x){
  B <- N*c + (NTm)*s + N*(Tp-1)*x
  return(B)
}
