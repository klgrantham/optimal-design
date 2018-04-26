# Creates contour plot of variance for different
# combinations of N and T
#
# Kelsey Grantham (kelsey.grantham@monash.edu)

source('optimal_design.R')

# Load results from NTm=10,000
load('results/all_NTm_10000_r_75_rho_035.Rda'); res <- all
load('results/all_NTm_10000_r_50_rho_035.Rda'); res50 <- all
load('results/all_NTm_10000_r_90_rho_035.Rda'); res10 <- all

# Break variances into quartiles
res$quart <- cut(res$variance, quantile(res$variance), include.lowest=TRUE)

p <- ggplot(res, aes(x=T, y=N, color=factor(quart))) +
  geom_point(shape=16, size=5) +
  scale_color_manual(values=c("#fdcc8a", "#fc8d59", "#e34a33", "#b30000"),
                     name="Quartiles of variance",
                     labels=c("(0.000386,0.0003861]", "(0.0003861,0.0003872]",
                              "(0.0003872,0.0003989]", "(0.0003989,0.001915]")) +
  xlab("T (number of periods)") +
  ylab("N (number of clusters)") +
  theme_bw() +
  theme(axis.title=element_text(size=20), axis.text=element_text(size=20))

p + scale_x_log10() + scale_y_log10() # Ooooh pretty!
# Probably better to use different breaks than quantiles
# Or instead of plotting abolute variance, look at relative variance?

# Break variances manually into 5 equal length bins
res$varcats <- cut(res$variance, breaks=5)
color_paletteYR <-colorRampPalette(c( "yellow", "red"))(5)

p <- ggplot(res, aes(x=T, y=N, color=factor(varcats))) +
  geom_point(shape=16, size=5) +
  scale_color_manual(values=color_paletteYR,
                     name="Levels of variance",
                     labels=levels(res$varcats)) +
  xlab("T (number of periods)") +
  ylab("N (number of clusters)") +
  theme_bw() +
  theme(axis.title=element_text(size=20), axis.text=element_text(size=20)) +
  scale_x_log10() + scale_y_log10()
# Most variances fall into lowest bin
# Makes sense because variances are skewed, but maybe doesn't show enough variety?

# Try continuous range of colors
p <- ggplot(res, aes(x=T, y=N, color=variance)) +
  geom_point(shape=16, size=5) +
  scale_color_continuous(low="lightblue", high="darkblue") +
  xlab("T (number of periods)") +
  ylab("N (number of clusters)") +
  theme_bw() +
  theme(axis.title=element_text(size=20), axis.text=element_text(size=20)) +
  scale_x_log10() + scale_y_log10()

# Quintiles
res$quint <- cut(res$variance, quantile(res$variance, probs=seq(0,1,0.2)),
                   include.lowest=TRUE)
color_paletteLBDB <-colorRampPalette(c( "lightblue", "darkblue"))(5)

p <- ggplot(res, aes(x=T, y=N, color=factor(quint))) +
  geom_point(shape=16, size=5) +
  scale_color_manual(values=color_paletteLBDB,
                     name="Quintiles of variance",
                     labels=c("1st quintile", "2nd quintile", "3rd quintile",
                              "4th quintile", "5th quintile")) +
  xlab("T (number of periods)") +
  ylab("N (number of clusters)") +
  theme_bw() +
  theme(axis.title=element_text(size=20), axis.text=element_text(size=20)) +
  scale_x_log10() + scale_y_log10()

# Take variance of each configuration relative to least efficient configuration
res[which.max(res$variance),] # (N,T,m) = (2,2,2500)
res$relvar <- res$variance/max(res$variance)
res$change <- 1-res$relvar

res$relquint <- cut(res$relvar, quantile(res$relvar, probs=seq(0,1,0.2)),
                    include.lowest=TRUE)
p <- ggplot(res, aes(x=T, y=N, color=factor(relquint))) +
  geom_point(shape=16, size=5) +
  scale_color_manual(values=color_paletteLBDB,
                     name="Quintiles of relative variance",
                     labels=c("1st quintile", "2nd quintile", "3rd quintile",
                              "4th quintile", "5th quintile")) +
  xlab("T (number of periods)") +
  ylab("N (number of clusters)") +
  theme_bw() +
  theme(axis.title=element_text(size=20), axis.text=element_text(size=20)) +
  scale_x_log10() + scale_y_log10()

# Use "relative efficiency" instead?
# Take highest variance and divide by all variances
res$releff <- max(res$variance)/res$variance
p <- ggplot(res, aes(x=T, y=N, color=releff)) +
  geom_point(shape=16, size=5) +
  scale_color_continuous(low="lightblue", high="darkblue") +
  xlab("T (number of periods)") +
  ylab("N (number of clusters)") +
  theme_bw() +
  theme(axis.title=element_text(size=20), axis.text=element_text(size=20)) +
  scale_x_log10() + scale_y_log10()

# Relative efficiency (as defined by Berger and Wong (2009))
# var(optimal)/var(other)
res$releff <- min(res$variance)/res$variance
p <- ggplot(res, aes(x=T, y=N, color=releff)) +
  geom_point(shape=16, size=5) +
  scale_color_continuous(low="lightblue", high="darkblue") +
  xlab("T (number of periods)") +
  ylab("N (number of clusters)") +
  theme_bw() +
  theme(axis.title=element_text(size=20), axis.text=element_text(size=20)) +
  scale_x_log10() + scale_y_log10()

# Relative efficiency with manual, even breaks
res$releffcat <- cut(res$releff, breaks=c(0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1),
                     include.lowest=TRUE)
color_paletteLBDB <-colorRampPalette(c( "lightblue", "darkblue"))(8)

p <- ggplot(res, aes(x=T, y=N, color=factor(releffcat))) +
  geom_point(shape=16, size=5) +
  scale_color_manual(values=color_paletteLBDB,
                     name="Relative efficiency",
                     labels=levels(res$releffcat)) +
  xlab("T (number of periods)") +
  ylab("N (number of clusters)") +
  theme_bw() +
  theme(axis.title=element_text(size=20), axis.text=element_text(size=20)) +
  scale_x_log10() + scale_y_log10()

# Relative efficiency with manual, uneven breaks
res$releffcat2 <- cut(res$releff, breaks=c(0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.95,0.99,1),
                     include.lowest=TRUE)
color_paletteLBDB <-colorRampPalette(c( "lightblue", "darkblue"))(10)

p <- ggplot(res, aes(x=T, y=N, color=factor(releffcat2))) +
  geom_point(shape=16, size=5) +
  scale_color_manual(values=color_paletteLBDB,
                     name="Relative efficiency",
                     labels=levels(res$releffcat2)) +
  xlab("T (number of periods)") +
  ylab("N (number of clusters)") +
  theme_bw() +
  theme(axis.title=element_text(size=20), axis.text=element_text(size=20)) +
  scale_x_log10() + scale_y_log10()

# Relative efficiency with manual, even more uneven breaks
res$releffcat3 <- cut(res$releff, breaks=c(0.2,0.5,0.8,0.9,0.99,1),
                      include.lowest=TRUE)
color_paletteLBDB <-colorRampPalette(c( "yellow", "green", "darkblue"))(5)

p <- ggplot(res, aes(x=T, y=N, color=factor(releffcat3))) +
  geom_point(shape=16, size=5) +
  scale_color_manual(values=color_paletteLBDB,
                     name="Relative efficiency",
                     labels=levels(res$releffcat3)) +
  xlab("T (number of periods)") +
  ylab("N (number of clusters)") +
  theme_bw() +
  theme(axis.title=element_text(size=20), axis.text=element_text(size=20)) +
  scale_x_log10() + scale_y_log10()

# Attempt full contour plot
cats <- cut(res$releff, breaks=c(0.2,0.5,0.8,0.9,0.95,0.99,1),
                      include.lowest=TRUE)
newcats <- gsub(",", "-", cats, fixed=TRUE)
res$releffcat4 <- gsub("\\(|\\]|\\[", "", newcats)
color_paletteYGDB <-colorRampPalette(c( "yellow", "green", "darkblue"))(6)

contourplot <- ggplot(res, aes(x=T, y=N)) +
  geom_raster(aes(fill=res$releffcat4)) + # interpolate=TRUE
  scale_fill_manual(name="Relative efficiency",
                    values=color_paletteYGDB) +
  scale_x_log10() +
  scale_y_log10() +
  xlab("T (number of periods)") +
  ylab("N (number of clusters)") +
  theme_bw() +
  theme(axis.title=element_text(size=20), axis.text=element_text(size=20),
        legend.position="bottom", legend.key.size=unit(0.5,"cm"),
        legend.text=element_text(size=12)) +
  guides(fill=guide_legend(nrow=1, keywidth=2, unit="cm"))
# Large gaps in (T,N) pairs so it's not all filled in

# Next step: Look into filled.contour()

# Relative efficiency for 25% decay over trial
res$releff <- min(res$variance)/res$variance
cats <- cut(res$releff, breaks=c(0.0,0.50,0.80,0.90,0.95,0.99,1.0),
            include.lowest=TRUE)
newcats <- gsub(",", "-", cats, fixed=TRUE)
res$releffcat4 <- gsub("\\(|\\]|\\[", "", newcats)
color_paletteYGDB <-colorRampPalette(c( "yellow", "green", "darkblue"))(6)

title <- expression(paste("Relative efficiency of CRXO trial designs, ", var(hat(theta))[optimal]/var(hat(theta))))
subtitle <- bquote(paste("10,000 subjects in trial, ", r==0.75, " , ", rho[0]==0.035))

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
ggsave("plots/rel_eff_NTm10000_r75_rho035.jpg", p, width=9, height=7, units="in", dpi=600)
ggsave("plots/rel_eff_NTm10000_r75_rho035.pdf", p, width=9, height=7, units="in", dpi=600)

# Relative efficiency for 50% decay over trial
res50$releff <- min(res50$variance)/res50$variance
cats <- cut(res50$releff, breaks=c(0,0.5,0.8,0.9,0.95,0.99,1),
            include.lowest=TRUE)
newcats <- gsub(",", "-", cats, fixed=TRUE)
res50$releffcat <- gsub("\\(|\\]|\\[", "", newcats)
color_paletteYGDB <-colorRampPalette(c( "yellow", "green", "darkblue"))(6)

title <- expression(paste("Relative efficiency of CRXO trial designs, ", var(hat(theta))[optimal]/var(hat(theta))))
subtitle <- bquote(paste("10,000 subjects in trial, ", r==0.50, " , ", rho[0]==0.035))

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
subtitle <- bquote(paste("10,000 subjects in trial, ", r==0.90, " , ", rho[0]==0.035))

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
load("results/all_Tm_5000_r_75_rho_035.Rda"); res25N2 <- all
load("results/all_Tm_5000_r_90_rho_035.Rda"); res10N2 <- all
load("results/all_Tm_5000_r_50_rho_035.Rda"); res50N2 <- all
res25N2$releff <- min(res25N2$variance)/res25N2$variance
res10N2$releff <- min(res10N2$variance)/res10N2$variance
res50N2$releff <- min(res50N2$variance)/res50N2$variance

res10_25_50 <- data.frame(Tp=res25N2$Tp, decay25=res25N2$releff,
                          decay10=res10N2$releff, decay50=res50N2$releff)

res10_25_50_long <- res10_25_50 %>%
  gather(key=decayrate, value=relative_efficiency,
         -Tp, convert=TRUE)

title <- expression(paste("Relative efficiency of CRXO trial designs, ", var(hat(theta))[optimal]/var(hat(theta))))
subtitle <- bquote(paste("Two clusters, 5,000 subjects in each cluster, ", rho[0]==0.035))

p <- ggplot(data=res10_25_50_long, aes(x=Tp, y=relative_efficiency, group=decayrate, color=decayrate)) +
  geom_line(size=2.0) +
  geom_point(size=2.5) +
  geom_hline(yintercept=1.0, size=1.0, color="black", linetype="longdash") +
  scale_color_manual(values=colorRampPalette(c("lightblue", "darkblue"))(3),
                     name="Correlation decay over trial",
                     labels=c("10%", "25%", "50%")) +
  xlab("T (number of periods)") +
  ylab("Relative efficiency") +
  labs(title=title, subtitle=subtitle) +
  theme_bw() +
  theme(plot.title=element_text(hjust=0.5, size=20),
        plot.subtitle=element_text(hjust=0.5, size=20),
        axis.title=element_text(size=18), axis.text=element_text(size=18),
        legend.title=element_text(size=16), legend.text=element_text(size=16),
        legend.position="bottom") +
  scale_x_log10(breaks=c(2,4,10,20,50,100,1000,5000), minor_breaks=NULL) +
  scale_y_continuous(breaks=c(0,0.2,0.4,0.6,0.8,1.0), limits=c(0,1.0))
ggsave("plots/rel_eff_Tm5000_N2_r50_75_90_rho035.jpg", p, width=9, height=7, units="in", dpi=600)
ggsave("plots/rel_eff_Tm5000_N2_r50_75_90_rho035.pdf", p, width=9, height=7, units="in", dpi=600)

# Plot of variance versus T for several decay rates
vars <- data.frame(Tp=res25N2$Tp, decay25=res25N2$variance,
                   decay10=res10N2$variance, decay50=res50N2$variance)
vars_long <- vars %>%
  gather(key=decayrate, value=variance, -Tp, convert=TRUE)

title <- expression(paste("Variance of treatment effect estimators, ", var(hat(theta))))
subtitle <- bquote(paste("Two clusters, 5,000 subjects in each cluster, ", rho[0]==0.035))

p <- ggplot(data=vars_long, aes(x=Tp, y=variance, group=decayrate, color=decayrate)) +
  geom_line(size=2.0) +
  geom_point(size=2.5) +
  scale_color_manual(values=colorRampPalette(c("lightblue", "darkblue"))(3),
                     name="Correlation decay over trial",
                     labels=c("10%", "25%", "50%")) +
  xlab("T (number of periods)") +
  ylab("Variance") +
  labs(title=title, subtitle=subtitle) +
  theme_bw() +
  theme(plot.title=element_text(hjust=0.5, size=20),
        plot.subtitle=element_text(hjust=0.5, size=20),
        axis.title=element_text(size=18), axis.text=element_text(size=18),
        legend.title=element_text(size=16), legend.text=element_text(size=16),
        legend.position="bottom") +
  scale_x_log10(breaks=c(2,4,5,8,10,20,50,100,200,500,1000,2500,5000), minor_breaks=NULL) +
  scale_y_continuous(limits=c(0,0.003))
ggsave("plots/var_Tm5000_N2_r50_75_90_rho035.jpg", p, width=9, height=7, units="in", dpi=600)
ggsave("plots/var_Tm5000_N2_r50_75_90_rho035.pdf", p, width=9, height=7, units="in", dpi=600)
