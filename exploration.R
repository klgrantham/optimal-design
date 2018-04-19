# Exploration of optimal CRXO trial design under continuous-time
# correlation decay with fixed trial duration
#
# Kelsey Grantham (kelsey.grantham@monash.edu)

source('optimal_design.R')

# Fixed total number of subjects
M <- 800
# Set rate of decay (1-r)
r <- 0.8

## Candidate trial configurations
# Trial A: 2 periods, 400 subjects in each
TpA <- 2; mA <- M/TpA
# Trial B: 4 periods, 200 subjects in each
TpB <- 4; mB <- M/TpB
# Trial C: 8 periods, 100 subjects in each
TpC <- 8; mC <- M/TpC
# Trial D: 16 periods, 50 subjects in each
TpD <- 16; mD <- M/TpD
# Trial E: 32 periods, 25 subjects in each
TpE <- 32; mE <- M/TpE

allTp <- c(TpA, TpB, TpC, TpD, TpE)

# Generate design matrices
XmatA <- desmat(Tp=TpA, nclust=2)
XmatB <- desmat(Tp=TpB, nclust=2)
XmatC <- desmat(Tp=TpC, nclust=2)
XmatD <- desmat(Tp=TpD, nclust=2)
XmatE <- desmat(Tp=TpE, nclust=2)
Xmats <- list(XmatA, XmatB, XmatC, XmatD, XmatE)

# Generate covariance matrix for a cluster
V <- contdecayVi(r=r, M=M, rho0=0.04)

# Calculate variance of treatment effect estimators
allvar <- vartheta_ind_vec(V, Xmats)

plot(allTp, allvar, type='l')


# Hypothesis: minimum variance achieved when T=Tm (1 subject in each period)
M <- 800
TpZ <- M
mZ <- M/TpZ
XmatZ <- desmat(Tp=TpZ, nclust=2)
V <- contdecayVi(r=r, M=M, rho0=0.04)
varZ <- vartheta_ind_vec(Vi=V, Xmat=list(XmatZ))
plot(c(allTp, TpZ), c(allvar, varZ), type='l')

allvar[5]; varZ
(varZ - allvar[5])/allvar[5] # For r=0.8, T=32 vs 800, only a 0.29% decrease in variance


##
res <- vars_doubling(rs=seq(0.5,1,0.01), M=800)
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


##
optres <- optimalTp(800)

# Note: Variances do not monotonically decrease as Tp increases for the following:
weirdresult <- optimalTp(100)
plot(weirdresult$Tp, weirdresult$vartrt, type='l')