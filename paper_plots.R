# Creates contour plot of variance for different
# combinations of N and T
#
# Kelsey Grantham (kelsey.grantham@monash.edu)

library(viridis)

source('optimal_design.R')

## Optimal T for fixed M

load("results/all_M_2000_N_40_r_77_rho_036.Rda"); res23N40 <- all

# Plot of variance versus T for 23% decay
title <- expression(paste("Variance of treatment effect estimator, ", Var(hat(theta))))
subtitle <- bquote(paste("40 clusters, 2,000 subjects in each cluster, ", rho==0.036, ", ", r==0.77))

p <- ggplot(data=res23N40, aes(x=Tp, y=variance)) +
  geom_line(size=2.0, color=colorRampPalette(c("lightblue", "darkblue"))(4)[3]) +
  geom_point(size=2.5, color=colorRampPalette(c("lightblue", "darkblue"))(4)[3]) +
  xlab("Number of periods") +
  ylab("Variance") +
  labs(title=title, subtitle=subtitle) +
  theme_bw() +
  theme(plot.title=element_text(hjust=0.5, size=20),
        plot.subtitle=element_text(hjust=0.5, size=20),
        axis.title=element_text(size=18), axis.text=element_text(size=18)) +
  scale_x_log10(breaks=c(2,4,8,10,20,50,100,200,500,1000,2000), minor_breaks=NULL) + # limits=c(0.5,2000)
  scale_y_continuous(limits=c(0,0.0002))
ggsave("plots/var_M2000_N40_r77_rho036.jpg", p, width=9, height=6, units="in", dpi=600)
ggsave("plots/var_M2000_N40_r77_rho036.pdf", p, width=9, height=6, units="in", dpi=600)


# Plot of variance versus T, smaller cluster size
load("results/all_M_200_N_40_r_77_rho_036.Rda"); res23M200N40 <- all

title <- expression(paste("Variance of treatment effect estimator, ", Var(hat(theta))))
subtitle <- bquote(paste("40 clusters, 200 subjects in each cluster, ", rho==0.036, ", ", r==0.77))

p <- ggplot(data=res23M200N40, aes(x=Tp, y=variance)) +
  geom_line(size=2.0, color=colorRampPalette(c("lightblue", "darkblue"))(4)[3]) +
  geom_point(size=2.5, color=colorRampPalette(c("lightblue", "darkblue"))(4)[3]) +
  xlab("Number of periods") +
  ylab("Variance") +
  labs(title=title, subtitle=subtitle) +
  theme_bw() +
  theme(plot.title=element_text(hjust=0.5, size=20),
        plot.subtitle=element_text(hjust=0.5, size=20),
        axis.title=element_text(size=18), axis.text=element_text(size=18)) +
  scale_x_log10(breaks=c(2,4,8,10,20,50,100,200,500,1000,2000), minor_breaks=NULL) + # limits=c(0.5,2000)
  scale_y_continuous(limits=c(0,0.00064))
ggsave("plots/var_M200_N40_r77_rho036.jpg", p, width=9, height=6, units="in", dpi=600)
ggsave("plots/var_M200_N40_r77_rho036.pdf", p, width=9, height=6, units="in", dpi=600)


# Plot of variance versus T for several base correlation values
load("results/all_M_2000_N_40_r_77_rho_01.Rda"); res23rho01N40 <- all
load("results/all_M_2000_N_40_r_77_rho_1.Rda"); res23rho1N40 <- all
load("results/all_M_2000_N_40_r_77_rho_2.Rda"); res23rho2N40 <- all

vars_rho_01_036_1_2 <- data.frame(Tp=res23N40$Tp,
                        rho01=res23rho01N40$variance, rho036=res23N40$variance,
                        rho1=res23rho1N40$variance, rho2=res23rho2N40$variance)
vars_rho_01_036_1_2_long <- vars_rho_01_036_1_2 %>%
  gather(key=rho, value=variance, -Tp, convert=TRUE)

title <- expression(paste("Variance of treatment effect estimator, ", Var(hat(theta))))
subtitle <- bquote(paste("40 clusters, 2,000 subjects in each cluster, ", r==0.77))

p <- ggplot(data=vars_rho_01_036_1_2_long, aes(x=Tp, y=variance, group=rho, color=rho)) +
  geom_line(size=2.0) +
  geom_point(size=2.5) +
  scale_color_manual(values=colorRampPalette(c("lightblue", "darkblue"))(4),
                     name="Base correlation",
                     labels=c("0.01", "0.036", "0.1", "0.2")) +
  xlab("Number of periods") +
  ylab("Variance") +
  labs(title=title, subtitle=subtitle) +
  theme_bw() +
  theme(plot.title=element_text(hjust=0.5, size=20),
        plot.subtitle=element_text(hjust=0.5, size=20),
        axis.title=element_text(size=18), axis.text=element_text(size=18),
        legend.title=element_text(size=16), legend.text=element_text(size=16),
        legend.position="bottom") +
  scale_x_log10(breaks=c(2,4,8,10,20,50,100,200,500,1000,2000), minor_breaks=NULL) +
  scale_y_continuous(limits=c(0,0.0004))
ggsave("plots/var_M2000_N40_r77_rho01_036_1_2.jpg", p, width=9, height=6, units="in", dpi=600)
ggsave("plots/var_M2000_N40_r77_rho01_036_1_2.pdf", p, width=9, height=6, units="in", dpi=600)


# Plot of variance versus T for several decay rates
load("results/all_M_2000_N_40_r_9_rho_036.Rda"); res10N40 <- all
load("results/all_M_2000_N_40_r_5_rho_036.Rda"); res50N40 <- all
load("results/all_M_2000_N_40_r_95_rho_036.Rda"); res5N40 <- all

vars <- data.frame(Tp=res23N40$Tp,
                   decay05=res5N40$variance, decay10=res10N40$variance,
                   decay23=res23N40$variance, decay50=res50N40$variance)
vars_long <- vars %>%
  gather(key=decayrate, value=variance, -Tp, convert=TRUE)

title <- expression(paste("Variance of treatment effect estimator, ", Var(hat(theta))))
subtitle <- bquote(paste("40 clusters, 2,000 subjects in each cluster, ", rho==0.036))

p <- ggplot(data=vars_long, aes(x=Tp, y=variance, group=decayrate, color=decayrate)) +
  geom_line(size=2.0) +
  geom_point(size=2.5) +
  scale_color_manual(values=colorRampPalette(c("lightblue", "darkblue"))(4),
                     name="Correlation decay over trial",
                     labels=c("5%", "10%", "23%", "50%")) +
  xlab("Number of periods") +
  ylab("Variance") +
  labs(title=title, subtitle=subtitle) +
  theme_bw() +
  theme(plot.title=element_text(hjust=0.5, size=20),
        plot.subtitle=element_text(hjust=0.5, size=20),
        axis.title=element_text(size=18), axis.text=element_text(size=18),
        legend.title=element_text(size=16), legend.text=element_text(size=16),
        legend.position="bottom") +
  scale_x_log10(breaks=c(2,4,8,10,20,50,100,200,500,1000,2000), minor_breaks=NULL) +
  scale_y_continuous(limits=c(0,0.00025))
ggsave("plots/var_M2000_N40_r50_77_90_95_rho036.jpg", p, width=9, height=6, units="in", dpi=600)
ggsave("plots/var_M2000_N40_r50_77_90_95_rho036.pdf", p, width=9, height=6, units="in", dpi=600)


# Plot of relative efficiency versus T
res23N40$releff <- min(res23N40$variance)/res23N40$variance

title <- expression(paste("Relative efficiency, ", Var(hat(theta))[optimal]/Var(hat(theta))))
subtitle <- bquote(paste("40 clusters, 2,000 subjects in each cluster, ", rho==0.036, ", ", r==0.77))

p <- ggplot(data=res23N40, aes(x=Tp, y=releff)) +
  geom_line(size=2.0, color=colorRampPalette(c("lightblue", "darkblue"))(4)[3]) +
  geom_point(size=2.5, color=colorRampPalette(c("lightblue", "darkblue"))(4)[3]) +
  geom_hline(yintercept=1.0, size=1.0, color="black", linetype="longdash") +
  xlab("Number of periods") +
  ylab("Relative efficiency") +
  labs(title=title, subtitle=subtitle) +
  theme_bw() +
  theme(plot.title=element_text(hjust=0.5, size=20),
        plot.subtitle=element_text(hjust=0.5, size=20),
        axis.title=element_text(size=18), axis.text=element_text(size=18)) +
  scale_x_log10(breaks=c(2,4,8,10,20,50,100,200,500,1000,2000), minor_breaks=NULL) +
  scale_y_continuous(breaks=c(0,0.2,0.4,0.6,0.8,1.0), limits=c(0,1.0))
ggsave("plots/rel_eff_M2000_N40_r77_rho036.jpg", p, width=9, height=6, units="in", dpi=600)
ggsave("plots/rel_eff_M2000_N40_r77_rho036.pdf", p, width=9, height=6, units="in", dpi=600)


# Plot of relative efficiency versus T for several decay rates
res10N40$releff <- min(res10N40$variance)/res10N40$variance
res50N40$releff <- min(res50N40$variance)/res50N40$variance
res5N40$releff <- min(res5N40$variance)/res5N40$variance

res5_10_23_50 <- data.frame(Tp=res23N40$Tp, 
                            decay05=res5N40$releff, decay10=res10N40$releff,
                            decay23=res23N40$releff, decay50=res50N40$releff)

res5_10_23_50_long <- res5_10_23_50 %>%
  gather(key=decayrate, value=relative_efficiency,
         -Tp, convert=TRUE)

title <- expression(paste("Relative efficiency, ", Var(hat(theta))[optimal]/Var(hat(theta))))
subtitle <- bquote(paste("40 clusters, 2,000 subjects in each cluster, ", rho==0.036))

p <- ggplot(data=res5_10_23_50_long, aes(x=Tp, y=relative_efficiency, group=decayrate, color=decayrate)) +
  geom_line(size=2.0) +
  geom_point(size=2.5) +
  geom_hline(yintercept=1.0, size=1.0, color="black", linetype="longdash") +
  scale_color_manual(values=colorRampPalette(c("lightblue", "darkblue"))(4),
                     name="Correlation decay over trial",
                     labels=c("5%", "10%", "23%", "50%")) +
  xlab("Number of periods") +
  ylab("Relative efficiency") +
  labs(title=title, subtitle=subtitle) +
  theme_bw() +
  theme(plot.title=element_text(hjust=0.5, size=20),
        plot.subtitle=element_text(hjust=0.5, size=20),
        axis.title=element_text(size=18), axis.text=element_text(size=18),
        legend.title=element_text(size=16), legend.text=element_text(size=16),
        legend.position="bottom") +
  scale_x_log10(breaks=c(2,4,8,10,20,50,100,200,500,1000,2000), minor_breaks=NULL) +
  scale_y_continuous(breaks=c(0,0.2,0.4,0.6,0.8,1.0), limits=c(0,1.0))
ggsave("plots/rel_eff_M2000_N40_r50_77_90_95_rho036.jpg", p, width=9, height=6, units="in", dpi=600)
ggsave("plots/rel_eff_M2000_N40_r50_77_90_95_rho036.pdf", p, width=9, height=6, units="in", dpi=600)

# Plot of relative efficiency versus T for several decay rates, smaller cluster size
res23M200N40$releff <- min(res23M200N40$variance)/res23M200N40$variance
res10M200N40$releff <- min(res10M200N40$variance)/res10M200N40$variance
res50M200N40$releff <- min(res50M200N40$variance)/res50M200N40$variance
res5M200N40$releff <- min(res5M200N40$variance)/res5M200N40$variance

res5_10_23_50_M200 <- data.frame(Tp=res23M200N40$Tp, 
                            decay05=res5M200N40$releff, decay10=res10M200N40$releff,
                            decay23=res23M200N40$releff, decay50=res50M200N40$releff)

res5_10_23_50_M200_long <- res5_10_23_50_M200 %>%
  gather(key=decayrate, value=relative_efficiency,
         -Tp, convert=TRUE)

title <- expression(paste("Relative efficiency of CRXO trial designs, ", Var(hat(theta))[optimal]/Var(hat(theta))))
subtitle <- bquote(paste("40 clusters, 200 subjects in each cluster, ", rho==0.036))

p <- ggplot(data=res5_10_23_50_M200_long, aes(x=Tp, y=relative_efficiency, group=decayrate, color=decayrate)) +
  geom_line(size=2.0) +
  geom_point(size=2.5) +
  geom_hline(yintercept=1.0, size=1.0, color="black", linetype="longdash") +
  scale_color_manual(values=colorRampPalette(c("lightblue", "darkblue"))(4),
                     name="Correlation decay over trial",
                     labels=c("5%", "10%", "23%", "50%")) +
  xlab("Number of periods") +
  ylab("Relative efficiency") +
  labs(title=title, subtitle=subtitle) +
  theme_bw() +
  theme(plot.title=element_text(hjust=0.5, size=20),
        plot.subtitle=element_text(hjust=0.5, size=20),
        axis.title=element_text(size=18), axis.text=element_text(size=18),
        legend.title=element_text(size=16), legend.text=element_text(size=16),
        legend.position="bottom") +
  scale_x_log10(breaks=c(2,4,8,10,20,50,100,200,500,1000,2000), minor_breaks=NULL) +
  scale_y_continuous(breaks=c(0,0.2,0.4,0.6,0.8,1.0), limits=c(0,1.0))
ggsave("plots/rel_eff_M200_N40_r50_77_90_95_rho036.jpg", p, width=9, height=7, units="in", dpi=600)
ggsave("plots/rel_eff_M200_N40_r50_77_90_95_rho036.pdf", p, width=9, height=7, units="in", dpi=600)

# Plot of relative efficiency versus T for several decay rates, larger base correlation
load("results/all_M_2000_N_40_r_77_rho_1.Rda"); res23N40 <- all
load("results/all_M_2000_N_40_r_9_rho_1.Rda"); res10N40 <- all
load("results/all_M_2000_N_40_r_5_rho_1.Rda"); res50N40 <- all
load("results/all_M_2000_N_40_r_95_rho_1.Rda"); res5N40 <- all
res23N40$releff <- min(res23N40$variance)/res23N40$variance
res10N40$releff <- min(res10N40$variance)/res10N40$variance
res50N40$releff <- min(res50N40$variance)/res50N40$variance
res5N40$releff <- min(res5N40$variance)/res5N40$variance

res5_10_23_50 <- data.frame(Tp=res23N40$Tp, 
                            decay05=res5N40$releff, decay10=res10N40$releff,
                            decay23=res23N40$releff, decay50=res50N40$releff)

res5_10_23_50_long <- res5_10_23_50 %>%
  gather(key=decayrate, value=relative_efficiency,
         -Tp, convert=TRUE)

title <- expression(paste("Relative efficiency of CRXO trial designs, ", Var(hat(theta))[optimal]/Var(hat(theta))))
subtitle <- bquote(paste("40 clusters, 2,000 subjects in each cluster, ", rho==0.1))

p <- ggplot(data=res5_10_23_50_long, aes(x=Tp, y=relative_efficiency, group=decayrate, color=decayrate)) +
  geom_line(size=2.0) +
  geom_point(size=2.5) +
  geom_hline(yintercept=1.0, size=1.0, color="black", linetype="longdash") +
  scale_color_manual(values=colorRampPalette(c("lightblue", "darkblue"))(4),
                     name="Correlation decay over trial",
                     labels=c("5%", "10%", "23%", "50%")) +
  xlab("Number of periods") +
  ylab("Relative efficiency") +
  labs(title=title, subtitle=subtitle) +
  theme_bw() +
  theme(plot.title=element_text(hjust=0.5, size=20),
        plot.subtitle=element_text(hjust=0.5, size=20),
        axis.title=element_text(size=18), axis.text=element_text(size=18),
        legend.title=element_text(size=16), legend.text=element_text(size=16),
        legend.position="bottom") +
  scale_x_log10(breaks=c(2,4,8,10,20,50,100,200,500,1000,2000), minor_breaks=NULL) +
  scale_y_continuous(breaks=c(0,0.2,0.4,0.6,0.8,1.0), limits=c(0,1.0))
ggsave("plots/rel_eff_M2000_N40_r50_77_90_95_rho1.jpg", p, width=9, height=7, units="in", dpi=600)
ggsave("plots/rel_eff_M2000_N40_r50_77_90_95_rho1.pdf", p, width=9, height=7, units="in", dpi=600)


# Plot relative efficiencies subject to budget constraint
load("results/r77_rho036_M2000_maxN40_c2500_s50_x250.Rda"); rescosts <- underbudget

title <- expression(paste("Relative efficiency of ICU trial designs, ", Var(hat(theta))[optimal]/Var(hat(theta))))
subtitle <- bquote(paste("2,000 subjects in each cluster, ", rho==0.036, " , ", r==0.77))

p <- ggplot(rescosts, aes(x=Tp, y=N, color=RE)) +
  geom_point(shape=16, size=5) +
  scale_color_viridis(direction=-1) +
  xlab("Number of periods (T)") +
  ylab("Number of clusters (N)") +
  labs(title=title, subtitle=subtitle) +
  theme_bw() +
  theme(plot.title=element_text(hjust=0.5, size=20),
        plot.subtitle=element_text(hjust=0.5, size=20),
        axis.title=element_text(size=18), axis.text=element_text(size=14),
        legend.title=element_text(size=16), legend.text=element_text(size=16)) +
  scale_x_log10(breaks=c(2,4,8,10,16,20,40,50,100,200,400,1000,2000), minor_breaks=NULL) + # breaks=c(2,10,100,1000,2000)
  scale_y_continuous(breaks=c(4,8,12,16,20,24))
ggsave("plots/rel_eff_M2000_r77_rho036_B2_5m.jpg", p, width=10, height=7, units="in", dpi=500)
ggsave("plots/rel_eff_M2000_r77_rho036_B2_5m.pdf", p, width=10, height=7, units="in", dpi=600)


# Plot relative efficiencies subject to budget and power constraints
rescosts$power <- pow(vars=rescosts$variance, effsize=0.03, siglevel=0.05)

title <- "Power of ICU trial designs to detect effect size of 0.03"
subtitle <- bquote(paste("Power >=80%, ", r==0.77, " , ", rho==0.036))

reskeep <- rescosts %>% filter(power>=0.80)
p <- ggplot(reskeep, aes(x=Tp, y=N, color=power)) +
  geom_point(shape=16, size=5) +
  scale_color_viridis(direction=-1) +
  xlab("Number of periods (T)") +
  ylab("Number of clusters (N)") +
  labs(title=title, subtitle=subtitle) +
  theme_bw() +
  theme(plot.title=element_text(hjust=0.5, size=20),
        plot.subtitle=element_text(hjust=0.5, size=20),
        axis.title=element_text(size=18), axis.text=element_text(size=18),
        legend.title=element_text(size=16), legend.text=element_text(size=16)) +
  scale_x_log10(breaks=c(2,4,8,10,16,20,40,50,80,100), minor_breaks=NULL)
ggsave("plots/pow_M2000_r77_rho036_B2_5m_ES03.jpg", p, width=9, height=7, units="in", dpi=600)
ggsave("plots/pow_M2000_r77_rho036_B2_5m_ES03.pdf", p, width=9, height=7, units="in", dpi=600)
