# Optimal CRXO trial design under continuous-time correlation
# decay with fixed trial duration
#
# Kelsey Grantham (kelsey.grantham@monash.edu)

library(tidyr)
library(plyr)
library(dplyr)
library(ggplot2)
library(ltsa)

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

# TODO: Probably need to have input be number of subjects in a cluster (N/2 here)
# OR Redefine N to be number of subjects in a cluster?
contdecayVi <- function(r, N, rho0){
  # Constructs the covariance matrix for a single cluster, under the
  # continuous time decay model for a given total number of subjects
  #
  # Inputs:
  # r - 1-rate of decay over entire trial
  # N - total number of subjects each cluster
  # rho0 - base correlation between subjects measured at same time
  
  totalvar <- 1
  sig2CP <- rho0*totalvar
  sig2E <- totalvar - sig2CP
  Vi <- diag(sig2E,N) +
        sig2CP*(r^(abs(matrix(rep(1:N), nrow=N, ncol=N, byrow=FALSE) -
                       matrix(rep(1:N), nrow=N, ncol=N, byrow=TRUE))/N))
  return(Vi)
}

# Fixed total number of subjects
N <- 800
# Set rate of decay (1-r)
r <- 0.8

## Candidate trial configurations
# Trial A: 2 periods, 400 subjects in each
TpA <- 2; mA <- N/TpA
# Trial B: 4 periods, 200 subjects in each
TpB <- 4; mB <- N/TpB
# Trial C: 8 periods, 100 subjects in each
TpC <- 8; mC <- N/TpC
# Trial D: 16 periods, 50 subjects in each
TpD <- 16; mD <- N/TpD
# Trial E: 32 periods, 25 subjects in each
TpE <- 32; mE <- N/TpE

allTp <- c(TpA, TpB, TpC, TpD, TpE)

# Generate design matrices
XmatA <- desmat(Tp=TpA, nclust=2)
XmatB <- desmat(Tp=TpB, nclust=2)
XmatC <- desmat(Tp=TpC, nclust=2)
XmatD <- desmat(Tp=TpD, nclust=2)
XmatE <- desmat(Tp=TpE, nclust=2)
Xmats <- list(XmatA, XmatB, XmatC, XmatD, XmatE)

# Generate covariance matrix for a cluster
V <- contdecayVi(r=r, N=N, rho0=0.04)

# Calculate variance of treatment effect estimators
allvar <- vartheta_ind_vec(V, Xmats)

plot(allTp, allvar, type='l')


# Hypothesis: minimum variance achieved when T=Tm (1 subject in each period)
N <- 800
TpZ <- N
mZ <- N/TpZ
XmatZ <- desmat(Tp=TpZ, nclust=2)
V <- contdecayVi(r=r, N=N, rho0=0.04)
varZ <- vartheta_ind_vec(Vi=V, Xmat=list(XmatZ))
plot(c(allTp, TpZ), c(allvar, varZ), type='l')

allvar[5]; varZ
(varZ - allvar[5])/allvar[5] # For r=0.8, T=32 vs 800, only a 0.29% decrease in variance


## For different rates of decay, how different are the
## gains in precision going from T=2 to T=4?

vars_doubling <- function(rs, N){
  Xmats <- list(desmat(2,2), desmat(4,2), desmat(8,2), desmat(16,2), desmat(32,2))
  ctvarmat <- llply(rs, contdecayVi, N, 0.035)
  ctres <- laply(ctvarmat, vartheta_ind_vec, Xmat=Xmats)
  varvals <- data.frame(decay=1-rs, varT2=ctres[,1], varT4=ctres[,2],
                        varT8=ctres[,3], varT16=ctres[,4], varT32=ctres[,5])
  return(varvals)
}

res <- vars_doubling(rs=seq(0.5,1,0.01), N=800)
gains <- res %>%
          mutate(ratio4to2=varT4/varT2, ratio8to2=varT8/varT2,
          ratio16to2=varT16/varT2, ratio32to2=varT32/varT2) %>%
          select(decay, starts_with('ratio'))
gains_long <- gather(data=gains, key=comparison, value=relative_variance,
                      ratio4to2:ratio32to2, convert=TRUE)

p <- ggplot(data=gains_long, aes(x=decay, y=relative_variance, group=comparison, color=comparison)) +
  geom_line(size=2.0) +
  geom_hline(yintercept=1.0, size=1.0, color="black", linetype="longdash") +
  expand_limits(y=c(0,1)) +
  xlab("Decay over trial") +
  ylab("Variance ratio") +
  theme_bw() +
  theme(axis.title=element_text(size=20), axis.text=element_text(size=20))
