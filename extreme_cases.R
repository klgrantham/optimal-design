# Compares extremes of 2 periods with 2 clusters (optimal)
#
# Kelsey Grantham (kelsey.grantham@monash.edu)

source('optimal_design.R')

releff_maxN <- function(rho0, NTm){
  # Returns optimal number of clusters and periods
  # (giving lowest variance) for specified correlation values,
  # number of subjects in trial.
  
  rs <- seq(0, 1, 0.01)
  
  N <- 2
  Tp <- NTm/2
  Vs <- llply(rs, contdecayVi, Tp, rho0)
  Xmat <- list(desmat(Tp, N))
  varsN2 <- laply(Vs, vartheta_ind_vec, Xmat)
  
  N <- NTm/2
  Tp <- 2
  Vs <- llply(rs, contdecayVi, Tp, rho0)
  Xmat <- list(desmat(Tp, N))
  varsT2 <- laply(Vs, vartheta_ind_vec, Xmat)
  
  res <- data.frame(r=rs, varianceN2=varsN2, varianceT2=varsT2, releff=varsN2/varsT2)
  return(res)
}

res_rho035 <- releff_maxN(rho0=0.035, NTm=1000)
res_rho05 <- releff_maxN(rho0=0.05, NTm=1000)
res_rho1 <- releff_maxN(rho0=0.1, NTm=1000)

res035_05_1 <- data.frame(r=res_rho035$r, releff035=res_rho035$releff,
                          releff05=res_rho05$releff, releff05=res_rho1$releff)

res035_05_1_long <- res035_05_1 %>%
  gather(key=rho0, value=relative_efficiency, -r, convert=TRUE)

title <- expression(paste("Relative efficiency of CRXO trial designs, ", var(hat(theta))[optimal]/var(hat(theta))))
subtitle <- "500 clusters, 2 periods, 1 subject in each cluster-period"

p <- ggplot(data=res035_05_1_long, aes(x=1-r, y=relative_efficiency, group=rho0, color=rho0)) +
  geom_line(size=2.0) +
  geom_hline(yintercept=1.0, size=1.0, color="black", linetype="longdash") +
  scale_color_manual(values=colorRampPalette(c("lightgreen", "darkgreen"))(3),
                     name="Base correlation",
                     labels=c("0.035", "0.05", "0.1")) +
  xlab("Decay (1 - r)") +
  ylab("Relative efficiency") +
  labs(title=title, subtitle=subtitle) +
  theme_bw() +
  theme(plot.title=element_text(hjust=0.5, size=20),
        plot.subtitle=element_text(hjust=0.5, size=20),
        axis.title=element_text(size=18), axis.text=element_text(size=18),
        legend.title=element_text(size=16), legend.text=element_text(size=16),
        legend.position="bottom")
ggsave("plots/rel_eff_N500_rho035_05_1.jpg", p, width=9, height=7, units="in", dpi=600)
ggsave("plots/rel_eff_N500_rho035_05_1.pdf", p, width=9, height=7, units="in", dpi=600)
