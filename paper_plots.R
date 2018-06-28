# Creates contour plot of variance for different
# combinations of N and T
#
# Kelsey Grantham (kelsey.grantham@monash.edu)

source('optimal_design.R')

# Load results from NTm=10,000
load('results/all_NTm_10000_r_75_rho_036.Rda'); res <- all
load('results/all_NTm_10000_r_50_rho_035.Rda'); res50 <- all
load('results/all_NTm_10000_r_90_rho_035.Rda'); res10 <- all

# Relative efficiency for 25% decay over trial
res$releff <- min(res$variance)/res$variance
cats <- cut(res$releff, breaks=c(0.0,0.50,0.80,0.90,0.95,0.99,1.0),
            include.lowest=TRUE)
newcats <- gsub(",", "-", cats, fixed=TRUE)
res$releffcat4 <- gsub("\\(|\\]|\\[", "", newcats)
color_paletteYGDB <-colorRampPalette(c( "yellow", "green", "darkblue"))(6)

title <- expression(paste("Relative efficiency of CRXO trial designs, ", var(hat(theta))[optimal]/var(hat(theta))))
subtitle <- bquote(paste("10,000 subjects in trial, ", r==0.75, " , ", rho==0.036))

p <- ggplot(res, aes(x=T, y=N, color=factor(releffcat4))) +
  geom_point(shape=16, size=5) +
  scale_color_manual(values=color_paletteYGDB,
                     name="Relative efficiency") +
  xlab("T (number of periods)") +
  ylab("N (number of clusters)") +
  labs(title=title, subtitle=subtitle) +
  theme_bw() +
  theme(plot.title=element_text(hjust=0.5, size=20),
        plot.subtitle=element_text(hjust=0.5, size=20),
        axis.title=element_text(size=18), axis.text=element_text(size=18),
        legend.title=element_text(size=16), legend.text=element_text(size=16)) +
  scale_x_log10(breaks=c(2,10,100,1000,5000), minor_breaks=NULL) +
  scale_y_log10(breaks=c(2,10,100,1000,5000), minor_breaks=NULL)
ggsave("plots/rel_eff_NTm10000_r75_rho036.jpg", p, width=9, height=7, units="in", dpi=600)
ggsave("plots/rel_eff_NTm10000_r75_rho036.pdf", p, width=9, height=7, units="in", dpi=600)

# Relative efficiency for 50% decay over trial
res50$releff <- min(res50$variance)/res50$variance
cats <- cut(res50$releff, breaks=c(0,0.5,0.8,0.9,0.95,0.99,1),
            include.lowest=TRUE)
newcats <- gsub(",", "-", cats, fixed=TRUE)
res50$releffcat <- gsub("\\(|\\]|\\[", "", newcats)
color_paletteYGDB <-colorRampPalette(c( "yellow", "green", "darkblue"))(6)

title <- expression(paste("Relative efficiency of CRXO trial designs, ", var(hat(theta))[optimal]/var(hat(theta))))
subtitle <- bquote(paste("10,000 subjects in trial, ", r==0.50, " , ", rho==0.035))

p <- ggplot(res50, aes(x=T, y=N, color=factor(releffcat))) +
  geom_point(shape=16, size=5) +
  scale_color_manual(values=color_paletteYGDB,
                     name="Relative efficiency") +
  xlab("T (number of periods)") +
  ylab("N (number of clusters)") +
  labs(title=title, subtitle=subtitle) +
  theme_bw() +
  theme(plot.title=element_text(hjust=0.5, size=20),
        plot.subtitle=element_text(hjust=0.5, size=20),
        axis.title=element_text(size=18), axis.text=element_text(size=18),
        legend.title=element_text(size=16), legend.text=element_text(size=16)) +
  scale_x_log10(breaks=c(2,10,100,1000,5000), minor_breaks=NULL) +
  scale_y_log10(breaks=c(2,10,100,1000,5000), minor_breaks=NULL)
ggsave("plots/rel_eff_NTm10000_r50_rho035.jpg", p, width=9, height=7, units="in", dpi=600)
ggsave("plots/rel_eff_NTm10000_r50_rho035.pdf", p, width=9, height=7, units="in", dpi=600)

# Relative efficiency for 10% decay over trial
res10$releff <- min(res10$variance)/res10$variance
cats <- cut(res10$releff, breaks=c(0,0.5,0.8,0.9,0.95,0.99,1),
            include.lowest=TRUE)
newcats <- gsub(",", "-", cats, fixed=TRUE)
res10$releffcat <- gsub("\\(|\\]|\\[", "", newcats)
color_paletteYGDB <- colorRampPalette(c( "yellow", "green", "darkblue"))(6)

title <- expression(paste("Relative efficiency of CRXO trial designs, ", var(hat(theta))[optimal]/var(hat(theta))))
subtitle <- bquote(paste("10,000 subjects in trial, ", r==0.90, " , ", rho==0.035))

p <- ggplot(res10, aes(x=T, y=N, color=factor(releffcat))) +
  geom_point(shape=16, size=5) +
  scale_color_manual(values=color_paletteYGDB,
                     name="Relative efficiency") +
  xlab("T (number of periods)") +
  ylab("N (number of clusters)") +
  labs(title=title, subtitle=subtitle) +
  theme_bw() +
  theme(plot.title=element_text(hjust=0.5, size=20),
        plot.subtitle=element_text(hjust=0.5, size=20),
        axis.title=element_text(size=18), axis.text=element_text(size=18),
        legend.title=element_text(size=16), legend.text=element_text(size=16)) +
  scale_x_log10(breaks=c(2,10,100,1000,5000), minor_breaks=NULL) +
  scale_y_log10(breaks=c(2,10,100,1000,5000), minor_breaks=NULL)
ggsave("plots/rel_eff_NTm10000_r90_rho035.jpg", p, width=9, height=7, units="in", dpi=600)
ggsave("plots/rel_eff_NTm10000_r90_rho035.pdf", p, width=9, height=7, units="in", dpi=600)


## Optimal T for fixed M
# Plot of relative efficiency versus T
load("results/all_M_2000_N_2_r_75_rho_036.Rda"); res25N2 <- all
load("results/all_M_2000_N_2_r_9_rho_036.Rda"); res10N2 <- all
load("results/all_M_2000_N_2_r_5_rho_036.Rda"); res50N2 <- all
load("results/all_M_2000_N_2_r_100_rho_036.Rda"); res0N2 <- all
res25N2$releff <- min(res25N2$variance)/res25N2$variance
res10N2$releff <- min(res10N2$variance)/res10N2$variance
res50N2$releff <- min(res50N2$variance)/res50N2$variance
res0N2$releff <- min(res0N2$variance)/res0N2$variance

res0_10_25_50 <- data.frame(Tp=res25N2$Tp, decay25=res25N2$releff,
                            decay0=res0N2$releff, decay10=res10N2$releff,
                            decay50=res50N2$releff)

res0_10_25_50_long <- res0_10_25_50 %>%
  gather(key=decayrate, value=relative_efficiency,
         -Tp, convert=TRUE)

title <- expression(paste("Relative efficiency of CRXO trial designs, ", var(hat(theta))[optimal]/var(hat(theta))))
subtitle <- bquote(paste("Two clusters, 2,000 subjects in each cluster, ", rho==0.036))

p <- ggplot(data=res0_10_25_50_long, aes(x=Tp, y=relative_efficiency, group=decayrate, color=decayrate)) +
  geom_line(size=2.0) +
  geom_point(size=2.5) +
  geom_hline(yintercept=1.0, size=1.0, color="black", linetype="longdash") +
  scale_color_manual(values=colorRampPalette(c("lightblue", "darkblue"))(4),
                     name="Correlation decay over trial",
                     labels=c("0%", "10%", "25%", "50%")) +
  xlab("T (number of periods)") +
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
ggsave("plots/rel_eff_M2000_N2_r50_75_90_100_rho036.jpg", p, width=9, height=7, units="in", dpi=600)
ggsave("plots/rel_eff_M2000_N2_r50_75_90_100_rho036.pdf", p, width=9, height=7, units="in", dpi=600)

# Plot of variance versus T for several decay rates
vars <- data.frame(Tp=res25N2$Tp, decay25=res25N2$variance,
                   decay0=res0N2$variance, decay10=res10N2$variance,
                   decay50=res50N2$variance)
vars_long <- vars %>%
  gather(key=decayrate, value=variance, -Tp, convert=TRUE)

title <- expression(paste("Variance of treatment effect estimators, ", var(hat(theta))))
subtitle <- bquote(paste("Two clusters, 2,000 subjects in each cluster, ", rho==0.036))

p <- ggplot(data=vars_long, aes(x=Tp, y=variance, group=decayrate, color=decayrate)) +
  geom_line(size=2.0) +
  geom_point(size=2.5) +
  scale_color_manual(values=colorRampPalette(c("lightblue", "darkblue"))(4),
                     name="Correlation decay over trial",
                     labels=c("0%", "10%", "25%", "50%")) +
  xlab("T (number of periods)") +
  ylab("Variance") +
  labs(title=title, subtitle=subtitle) +
  theme_bw() +
  theme(plot.title=element_text(hjust=0.5, size=20),
        plot.subtitle=element_text(hjust=0.5, size=20),
        axis.title=element_text(size=18), axis.text=element_text(size=18),
        legend.title=element_text(size=16), legend.text=element_text(size=16),
        legend.position="bottom") +
  scale_x_log10(breaks=c(2,4,8,10,20,50,100,200,500,1000,2000), minor_breaks=NULL) +
  scale_y_continuous(limits=c(0,0.005))
ggsave("plots/var_M2000_N2_r50_75_90_100_rho036.jpg", p, width=9, height=7, units="in", dpi=600)
ggsave("plots/var_M2000_N2_r50_75_90_100_rho036.pdf", p, width=9, height=7, units="in", dpi=600)

# Small cluster size
load("results/all_M_200_N_2_r_75_rho_036.Rda"); res25N2 <- all
load("results/all_M_200_N_2_r_9_rho_036.Rda"); res10N2 <- all
load("results/all_M_200_N_2_r_5_rho_036.Rda"); res50N2 <- all
load("results/all_M_200_N_2_r_100_rho_036.Rda"); res0N2 <- all
res25N2$releff <- min(res25N2$variance)/res25N2$variance
res10N2$releff <- min(res10N2$variance)/res10N2$variance
res50N2$releff <- min(res50N2$variance)/res50N2$variance
res0N2$releff <- min(res0N2$variance)/res0N2$variance

res0_10_25_50 <- data.frame(Tp=res25N2$Tp, decay25=res25N2$releff,
                            decay0=res0N2$releff, decay10=res10N2$releff,
                            decay50=res50N2$releff)

res0_10_25_50_long <- res0_10_25_50 %>%
  gather(key=decayrate, value=relative_efficiency,
         -Tp, convert=TRUE)

title <- expression(paste("Relative efficiency of CRXO trial designs, ", var(hat(theta))[optimal]/var(hat(theta))))
subtitle <- bquote(paste("Two clusters, 200 subjects in each cluster, ", rho==0.036))

p <- ggplot(data=res0_10_25_50_long, aes(x=Tp, y=relative_efficiency, group=decayrate, color=decayrate)) +
  geom_line(size=2.0) +
  geom_point(size=2.5) +
  geom_hline(yintercept=1.0, size=1.0, color="black", linetype="longdash") +
  scale_color_manual(values=colorRampPalette(c("lightblue", "darkblue"))(4),
                     name="Correlation decay over trial",
                     labels=c("0%", "10%", "25%", "50%")) +
  xlab("T (number of periods)") +
  ylab("Relative efficiency") +
  labs(title=title, subtitle=subtitle) +
  theme_bw() +
  theme(plot.title=element_text(hjust=0.5, size=20),
        plot.subtitle=element_text(hjust=0.5, size=20),
        axis.title=element_text(size=18), axis.text=element_text(size=18),
        legend.title=element_text(size=16), legend.text=element_text(size=16),
        legend.position="bottom") +
  scale_x_log10(breaks=c(2,4,8,10,20,50,100,200), minor_breaks=NULL) +
  scale_y_continuous(breaks=c(0,0.2,0.4,0.6,0.8,1.0), limits=c(0,1.0))
ggsave("plots/rel_eff_M200_N2_r50_75_90_100_rho036.jpg", p, width=9, height=7, units="in", dpi=600)
ggsave("plots/rel_eff_M200_N2_r50_75_90_100_rho036.pdf", p, width=9, height=7, units="in", dpi=600)

# Plot of variance versus T for several decay rates
vars <- data.frame(Tp=res25N2$Tp, decay25=res25N2$variance,
                   decay0=res0N2$variance, decay10=res10N2$variance,
                   decay50=res50N2$variance)
vars_long <- vars %>%
  gather(key=decayrate, value=variance, -Tp, convert=TRUE)

title <- expression(paste("Variance of treatment effect estimators, ", var(hat(theta))))
subtitle <- bquote(paste("Two clusters, 200 subjects in each cluster, ", rho==0.036))

p <- ggplot(data=vars_long, aes(x=Tp, y=variance, group=decayrate, color=decayrate)) +
  geom_line(size=2.0) +
  geom_point(size=2.5) +
  scale_color_manual(values=colorRampPalette(c("lightblue", "darkblue"))(4),
                     name="Correlation decay over trial",
                     labels=c("0%", "10%", "25%", "50%")) +
  xlab("T (number of periods)") +
  ylab("Variance") +
  labs(title=title, subtitle=subtitle) +
  theme_bw() +
  theme(plot.title=element_text(hjust=0.5, size=20),
        plot.subtitle=element_text(hjust=0.5, size=20),
        axis.title=element_text(size=18), axis.text=element_text(size=18),
        legend.title=element_text(size=16), legend.text=element_text(size=16),
        legend.position="bottom") +
  scale_x_log10(breaks=c(2,4,8,10,20,50,100,200), minor_breaks=NULL) +
  scale_y_continuous(limits=c(0.008,0.016))
ggsave("plots/var_M200_N2_r50_75_90_100_rho036.jpg", p, width=9, height=7, units="in", dpi=600)
ggsave("plots/var_M200_N2_r50_75_90_100_rho036.pdf", p, width=9, height=7, units="in", dpi=600)