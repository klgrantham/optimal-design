# Check results under Hooper/Girling model
# CRXO designs with varying numbers of periods
#
# Kelsey Grantham (kelsey.grantham@monash.edu)

source('optimal_design.R')

HooperVar <- function(r, Xmat, m, rho0) {
  
  totalvar <- 1
  sig2CP <- rho0*totalvar
  sig2E <- totalvar - sig2CP
  sig2 <- sig2E/m
  
  T <- ncol(Xmat)
  K <- nrow(Xmat)
  Xvec <- as.vector(t(Xmat))
  
  Vi <- diag(sig2 +(1-r)*sig2CP, T) + matrix(data=sig2CP*r, nrow=T, ncol=T)
  vartheta <- 1/(t(Xvec)%*%(diag(1,K)%x%solve(Vi))%*%Xvec -colSums(Xmat)%*%solve(Vi)%*%(matrix(colSums(Xmat),nrow=T, ncol=1))/K )
  return(vartheta)
}

HooperVi <- function(r, Xmat, m, rho0) {
  
  totalvar <- 1
  sig2CP <- rho0*totalvar
  sig2E <- totalvar - sig2CP
  sig2 <- sig2E/m
  
  T <- ncol(Xmat)
  K <- nrow(Xmat)
  Xvec <- as.vector(t(Xmat))
  
  Vi <- diag(sig2 +(1-r)*sig2CP, T) + matrix(data=sig2CP*r, nrow=T, ncol=T)
  return(Vi)
}

XmatT2 <- desmat(Tp=2, nclust=2) # m=4
XmatT4 <- desmat(Tp=4, nclust=2) # m=2
XmatT8 <- desmat(Tp=8, nclust=2) # m=1

varT2 <- HooperVar(r=0.75, Xmat=XmatT2, m=4, rho0=0.036)
varT4 <- HooperVar(r=0.75, Xmat=XmatT4, m=2, rho0=0.036)
varT8 <- HooperVar(r=0.75, Xmat=XmatT8, m=1, rho0=0.036)

ViT2 <- HooperVi(r=0.75, Xmat=XmatT2, m=4, rho0=0.036)
ViT4 <- HooperVi(r=0.75, Xmat=XmatT4, m=2, rho0=0.036)
ViT8 <- HooperVi(r=0.75, Xmat=XmatT8, m=1, rho0=0.036)

HooperVar_parts <- function(r, Xmat, m, rho0){
  totalvar <- 1
  sig2CP <- rho0*totalvar
  sig2E <- totalvar - sig2CP
  sig2 <- sig2E/m
  
  T <- ncol(Xmat)
  K <- nrow(Xmat)
  Xvec <- as.vector(t(Xmat))
  
  Vi <- diag(sig2 +(1-r)*sig2CP, T) + matrix(data=sig2CP*r, nrow=T, ncol=T)
  vartheta1 <- t(Xvec)%*%(diag(1,K)%x%solve(Vi))%*%Xvec
  vartheta2 <- colSums(Xmat)%*%solve(Vi)%*%(matrix(colSums(Xmat),nrow=T, ncol=1))/K
  vartheta <- 1/(vartheta1 - vartheta2)
  return(c(vartheta1, vartheta2, vartheta))
}

HooperVar_parts(r=0.75, Xmat=XmatT2, m=4, rho0=0.036)
HooperVar_parts(r=0.75, Xmat=XmatT4, m=2, rho0=0.036)
HooperVar_parts(r=0.75, Xmat=XmatT8, m=1, rho0=0.036)

# Compare to continuous-time decay
Vi <- contdecayVi(r=0.75, M=8, rho0=0.036)
vartheta_ind_vec(Vi=Vi, Xmat=list(XmatT2))
vartheta_ind_vec(Vi=Vi, Xmat=list(XmatT4))
vartheta_ind_vec(Vi=Vi, Xmat=list(XmatT8))

HooperVi_ind <- function(r, Xmat, m, rho0) {
  
  totalvar <- 1
  sig2CP <- rho0*totalvar
  sig2E <- totalvar - sig2CP
  sig2 <- sig2E/m
  
  T <- ncol(Xmat)
  K <- nrow(Xmat)
  Xvec <- as.vector(t(Xmat))
  
  Vi <- diag(sig2E, T*m) +
          matrix(r*sig2CP, nrow=T*m, ncol=T*m) +
          diag(1,T) %x% matrix(rep((1-r)*sig2CP,T),nrow=m,ncol=m)
  return(Vi)
}
