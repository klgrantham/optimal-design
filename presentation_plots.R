# Creates results plots for optimal design presentations
#
# Kelsey Grantham (kelsey.grantham@monash.edu)

source('optimal_design.R')

## Optimal T for fixed M

load("results/all_M_2000_N_20_r_75_rho_036.Rda"); res25N20 <- all
load("results/all_M_2000_N_20_r_9_rho_036.Rda"); res10N20 <- all
load("results/all_M_2000_N_20_r_5_rho_036.Rda"); res50N20 <- all
load("results/all_M_2000_N_20_r_95_rho_036.Rda"); res5N20 <- all
res25N20$releff <- min(res25N20$variance)/res25N20$variance
res10N20$releff <- min(res10N20$variance)/res10N20$variance
res50N20$releff <- min(res50N20$variance)/res50N20$variance
res5N20$releff <- min(res5N20$variance)/res5N20$variance

# Plot of variance versus T for 25% decay
title <- expression(paste("Variance of treatment effect estimators, ", var(hat(theta))))
subtitle <- bquote(paste("20 clusters, 2,000 subjects in each cluster, ", rho==0.036))
#col <- colorRampPalette(c("gold", "firebrick4"))(4)[3]
col <- c("goldenrod", "seagreen", "steelblue", "darkorchid")

p <- ggplot(data=res25N20, aes(x=Tp, y=variance)) +
  geom_line(size=2.0, color=col[3]) +
  geom_point(size=2.5, color=col[3]) +
  xlab("Number of periods") +
  ylab("Variance") +
#  labs(title=title, subtitle=subtitle) +
  theme_bw() +
  theme(plot.title=element_text(hjust=0.5, size=20),
        plot.subtitle=element_text(hjust=0.5, size=20),
        axis.title=element_text(size=18), axis.text=element_text(size=18)) +
  scale_x_log10(breaks=c(2,4,8,10,20,50,100,200,500,1000,2000), minor_breaks=NULL, limits=c(0.5,2000)) +
  scale_y_continuous(limits=c(0,0.0005))
ggsave("plots/var_M2000_N20_r75_rho036.jpg", p, width=9, height=6, units="in", dpi=600)
ggsave("plots/var_M2000_N20_r75_rho036.pdf", p, width=9, height=6, units="in", dpi=600)

# Plot of variance versus T for several decay rates
vars <- data.frame(Tp=res25N20$Tp,
                   decay05=res5N20$variance, decay10=res10N20$variance,
                   decay25=res25N20$variance, decay50=res50N20$variance)
vars_long <- vars %>%
  gather(key=decayrate, value=variance, -Tp, convert=TRUE)

title <- expression(paste("Variance of treatment effect estimators, ", var(hat(theta))))
subtitle <- bquote(paste("20 clusters, 2,000 subjects in each cluster, ", rho==0.036))

p <- ggplot(data=vars_long, aes(x=Tp, y=variance, group=decayrate, color=decayrate)) +
  geom_line(size=2.0) +
  geom_point(size=2.5) +
  scale_color_manual(values=col,
                     name="Correlation decay over trial",
                     labels=c("5%", "10%", "25%", "50%")) +
  xlab("Number of periods") +
  ylab("Variance") +
#  labs(title=title, subtitle=subtitle) +
  theme_bw() +
  theme(plot.title=element_text(hjust=0.5, size=20),
        plot.subtitle=element_text(hjust=0.5, size=20),
        axis.title=element_text(size=18), axis.text=element_text(size=18),
        legend.position="none") +
  scale_x_log10(breaks=c(2,4,8,10,20,50,100,200,500,1000,2000), minor_breaks=NULL, limits=c(0.5,2000)) +
  scale_y_continuous(limits=c(0,0.0005))
ggsave("plots/var_M2000_N20_r50_75_90_95_rho036.jpg", p, width=9, height=6, units="in", dpi=600)
ggsave("plots/var_M2000_N20_r50_75_90_95_rho036.pdf", p, width=9, height=6, units="in", dpi=600)

# Plot of relative efficiency versus T for several decay rates
res5_10_25_50 <- data.frame(Tp=res25N20$Tp, 
                            decay05=res5N20$releff, decay10=res10N20$releff,
                            decay25=res25N20$releff, decay50=res50N20$releff)

res5_10_25_50_long <- res5_10_25_50 %>%
  gather(key=decayrate, value=relative_efficiency,
         -Tp, convert=TRUE)

title <- expression(paste("Relative efficiency of CRXO trial designs, ", var(hat(theta))[optimal]/var(hat(theta))))
subtitle <- bquote(paste("20 clusters, 2,000 subjects in each cluster, ", rho==0.036))

p <- ggplot(data=res5_10_25_50_long, aes(x=Tp, y=relative_efficiency, group=decayrate, color=decayrate)) +
  geom_line(size=2.0) +
  geom_point(size=2.5) +
  geom_hline(yintercept=1.0, size=1.0, color="black", linetype="longdash") +
  scale_color_manual(values=col,
                     name="Correlation decay over trial",
                     labels=c("5%", "10%", "25%", "50%")) +
  xlab("Number of periods") +
  ylab("Relative efficiency") +
#  labs(title=title, subtitle=subtitle) +
  theme_bw() +
  theme(plot.title=element_text(hjust=0.5, size=20),
        plot.subtitle=element_text(hjust=0.5, size=20),
        axis.title=element_text(size=18), axis.text=element_text(size=18),
        legend.position="none") +
  scale_x_log10(breaks=c(2,4,8,10,20,50,100,200,500,1000,2000), minor_breaks=NULL, limits=c(0.5,2000)) +
  scale_y_continuous(breaks=c(0,0.2,0.4,0.6,0.8,1.0), limits=c(0,1.0))
ggsave("plots/rel_eff_M2000_N20_r50_75_90_95_rho036.jpg", p, width=9, height=6, units="in", dpi=600)
ggsave("plots/rel_eff_M2000_N20_r50_75_90_95_rho036.pdf", p, width=9, height=6, units="in", dpi=600)

# Plot of RE versus N for several values of T

load("results/r75_rho036_M2000_maxN100_c2500_s200_x100.Rda"); res100 <- underbudget
load("results/r75_rho036_M2000_maxN100_c2500_s200_x250.Rda"); res250 <- underbudget
load("results/r75_rho036_M2000_maxN100_c2500_s200_x2500.Rda"); res2500 <- underbudget
load("results/r75_rho036_M2000_maxN100_c2500_s200_x5000.Rda"); res5000 <- underbudget

plotREvsN <- function(res){
  p <- ggplot(data=res, aes(x=N, y=RE, group=as.factor(Tp), color=as.factor(Tp))) +
    geom_line(size=2.0) +
    #  scale_color_manual(values=col,
    #                     name="Correlation decay over trial",
    #                     labels=c("0%", "10%", "25%", "50%")) +
    xlab("Number of clusters") +
    ylab("Relative efficiency") +
    theme_bw() +
    theme(plot.title=element_text(hjust=0.5, size=20),
          plot.subtitle=element_text(hjust=0.5, size=20),
          axis.title=element_text(size=18), axis.text=element_text(size=18)) +
    scale_y_continuous(breaks=c(0,0.2,0.4,0.6,0.8,1.0), limits=c(0,1.0))
  ggsave("plots/REvsN.jpg", p, width=9, height=6, units="in", dpi=600)
  ggsave("plots/REvsN.pdf", p, width=9, height=6, units="in", dpi=600)
}

plotREvsN(res100)

plotREvsT <- function(res, M, x){
  p <- ggplot(data=res, aes(x=Tp, y=RE, group=as.factor(N), color=as.factor(N))) +
    geom_line(size=2.0) +
    geom_point(size=2.5) +
#    scale_color_manual(name="Number of clusters") +
    xlab("Number of periods") +
    ylab("Relative efficiency") +
    theme_bw() +
    theme(plot.title=element_text(hjust=0.5, size=20),
          plot.subtitle=element_text(hjust=0.5, size=20),
          axis.title=element_text(size=18), axis.text=element_text(size=18),
          legend.position="none") +
    scale_x_log10(breaks=c(2,4,10,20,50,100,250,500,1000,2000), minor_breaks=NULL, limits=c(1,2000)) +
    scale_y_continuous(breaks=c(0,0.2,0.4,0.6,0.8,1.0), limits=c(0,1.0))
  ggsave(paste0("plots/REvsT_M", M, "_x", x, ".jpg"), p, width=9, height=6, units="in", dpi=600)
  ggsave(paste0("plots/REvsT_M", M, "_x", x, ".pdf"), p, width=9, height=6, units="in", dpi=600)
  return(p)
}

plotREvsT(res100, 2000, 100)
plotREvsT(res250, 2000, 250)
plotREvsT(res2500, 2000, 2500)
plotREvsT(res5000, 2000, 5000)

# Plot relative efficiencies subject to budget constraint

cats <- cut(res$releff, breaks=c(0.0,0.30,0.60,0.90,0.95,0.99,1.0),
            include.lowest=TRUE)
newcats <- gsub(",", "-", cats, fixed=TRUE)
res$releffcat4 <- gsub("\\(|\\]|\\[", "", newcats)
color_paletteYGDB <-colorRampPalette(c( "yellow", "green", "darkblue"))(6)

title <- expression(paste("Relative efficiency of CRXO trial designs, ", var(hat(theta))[optimal]/var(hat(theta))))
subtitle <- bquote(paste(r==0.75, " , ", rho==0.036))

p <- ggplot(res, aes(x=Tp, y=N, color=factor(releffcat4))) +
  geom_point(shape=16, size=5) +
  #  scale_colour_gradientn(colours=c("yellow", "green", "darkblue"),
  #                                   guide = "colourbar") +
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
  scale_x_log10(breaks=c(2,10,100,1000,2000), minor_breaks=NULL)
ggsave("plots/rel_eff_M2000_r75_rho036_B5m.jpg", p, width=9, height=7, units="in", dpi=600)
ggsave("plots/rel_eff_M2000_r75_rho036_B5m.pdf", p, width=9, height=7, units="in", dpi=600)