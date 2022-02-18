# comparison between mpa and non-mpa sites for the severity and onset of bleaching 
rm(list=ls())

# load libraries

library(tidyverse)
library(MASS)

rm(list=ls())

Bleaching_data_directory="C:/Users/40274182/OneDrive - Queen's University Belfast/PhD/Chapters/Bleaching/MPA/Data_files"
setwd(Bleaching_data_directory)
Bleaching_Data <- read.csv("Bleaching_MPA.csv", header = T, sep = ",")

output_directory <- "C:/Users/40274182/OneDrive - Queen's University Belfast/PhD/Chapters/Bleaching/MPA/Data_files/Output_directory"

# validate dhw - bleaching relationship

Bleaching_Data$average_bleaching <- Bleaching_Data$Count/4
Bleaching_Data$average_bleaching <- round(Bleaching_Data$average_bleaching)

Bleaching_Data$SSTA_DHW <- as.numeric(Bleaching_Data$SSTA_DHW)
Bleaching_Data$SSTFilled <- as.numeric(Bleaching_Data$SSTFilled)

Bleaching_Data$STATUS_YR <- as.numeric(Bleaching_Data$STATUS_YR)

Bleaching_Data$MPA[Bleaching_Data$STATUS_YR>1] <- "MPA"
Bleaching_Data$MPA[is.na(Bleaching_Data$STATUS_YR)] <- "Non MPA"
Bleaching_Data$MPA <- as.factor(Bleaching_Data$MPA)
levels(Bleaching_Data$MPA)

Bleaching_Data$MPA_AGE <- Bleaching_Data$Year-Bleaching_Data$STATUS_YR
Bleaching_Data$MPA_AGE[is.na(Bleaching_Data$MPA_AGE)] <- "Non MPA"

Bleaching_Data <- Bleaching_Data[!is.na(Bleaching_Data$SSTA_DHW),]
Bleaching_Data <- Bleaching_Data[!is.na(Bleaching_Data$SSTFilled),]
Bleaching_Data <- Bleaching_Data[!is.na(Bleaching_Data$average_bleaching),]
Bleaching_Data <- Bleaching_Data[!is.na(Bleaching_Data$MPA),]
Bleaching_Data <- Bleaching_Data[!is.na(Bleaching_Data$MARINE),]
Bleaching_Data <- Bleaching_Data[!is.na(Bleaching_Data$MPA_AGE),]
Bleaching_Data <- Bleaching_Data[!is.na(Bleaching_Data$REP_M_AREA),]
Bleaching_Data <- Bleaching_Data[!is.na(Bleaching_Data$GIS_M_AREA),]
Bleaching_Data <- Bleaching_Data[!is.na(Bleaching_Data$REP_AREA),]
Bleaching_Data <- Bleaching_Data[!is.na(Bleaching_Data$GIS_AREA),]
Bleaching_Data <- Bleaching_Data[!is.na(Bleaching_Data$NO_TK_AREA),]

Bleaching_Data$MPA <- as.factor(Bleaching_Data$MPA)
levels(Bleaching_Data$MPA)
table(Bleaching_Data$MPA)

Bleaching_Data$NO_TAKE <- as.factor(Bleaching_Data$NO_TAKE)
levels(Bleaching_Data$NO_TAKE)
table(Bleaching_Data$NO_TAKE)

Bleaching_Data$MARINE <- as.factor(Bleaching_Data$MARINE)
levels(Bleaching_Data$MARINE)
table(Bleaching_Data$MARINE)

Bleaching_Data$DESIG_TYPE <- as.factor(Bleaching_Data$DESIG_TYPE)
levels(Bleaching_Data$DESIG_TYPE)
table(Bleaching_Data$DESIG_TYPE)
#
hist(Bleaching_Data$SSTA_DHW)

summary(Bleaching_Data$average_bleaching)
names(Bleaching_Data)

Bleaching_Data <-Bleaching_Data %>%
  dplyr::distinct(Date, Site, Region, Depth, Longitude.Degrees, Latitude.Degrees, .keep_all=T)


# onset of bleaching between mpa and non mpa 

BleachMPA=subset(Bleaching_Data, MPA == "MPA" & average_bleaching > 1 & SSTA_DHW < 12, select =c(SSTA_DHW, average_bleaching, MPA))

BleachNON=subset(Bleaching_Data, MPA == "Non MPA" & average_bleaching > 1 & SSTA_DHW < 12, select =c(SSTA_DHW, average_bleaching, MPA))

BleachALL <- rbind(BleachMPA, BleachNON)

BleachMPA$SSTA_DHW <- BleachMPA$SSTA_DHW+1
BleachNON$SSTA_DHW <- BleachNON$SSTA_DHW+1


hist(BleachMPA$SSTA_DHW)
hist(BleachMPA$average_bleaching)

hist(BleachNON$SSTA_DHW)
hist(BleachNON$average_bleaching)

# figure 

setwd(output_directory)

mpa_dist <- fitdistr(BleachMPA$SSTA_DHW, 'gamma')
non_dist <- fitdistr(BleachNON$SSTA_DHW, 'gamma')

#########
## onset of bleaching between connected and non-connected mpas 

Bleaching_Data$MARINE <- as.character(Bleaching_Data$MARINE)

Bleaching_Data$MARINE[Bleaching_Data$MARINE == 1] <- "Marine & terrestrial"
Bleaching_Data$MARINE[Bleaching_Data$MARINE == 2] <- "Marine only"

BleachTES <- subset(Bleaching_Data, MPA == "MPA" & average_bleaching > 1 & SSTA_DHW < 12 & MARINE == "Marine & terrestrial", select =c(SSTA_DHW, average_bleaching, MPA, MARINE))

BleachMAR <- subset(Bleaching_Data, MPA == "MPA" & average_bleaching > 1 & SSTA_DHW < 12 & MARINE == "Marine only", select =c(SSTA_DHW, average_bleaching, MPA, MARINE))

BleachBOT <- rbind(BleachTES, BleachMAR)

hist(BleachTES$SSTA_DHW)
hist(BleachMAR$SSTA_DHW)
hist(BleachTES$average_bleaching)
hist(BleachMAR$average_bleaching)

BleachTES$SSTA_DHW <- BleachTES$SSTA_DHW+1
BleachMAR$SSTA_DHW <- BleachMAR$SSTA_DHW+1

TESdist <- fitdistr(BleachTES$SSTA_DHW, 'gamma')
MARdist <- fitdistr(BleachMAR$SSTA_DHW, 'gamma')

# prob densities
setwd(output_directory)


#par(mfrow=c(1,2))
png(filename = 'gamma_probs.png', height = 2000, width = 2000, res = 350)

hist(BleachMPA$SSTA_DHW, col = rgb(), xlab = expression ("DHW"), ylab = "Probability density", main = "", freq = F, ylim = c(0, 0.2), xlim = c(0,12), cex.lab = 1, cex.axis = 1)
hist(BleachNON$SSTA_DHW, freq = F, col = rgb(), main = "", add = T)
x=seq(0,12,.00001)
y=dgamma(x, mpa_dist$estimate['shape'], mpa_dist$estimate['rate'])
lines(x,y, col=rgb(0,100,255,255,max=255), lwd=2)
y=dgamma(x, non_dist$estimate['shape'], mpa_dist$estimate['rate'])
lines(x,y, col=rgb(255,0,0,255,max=255), lwd=2)
legend("topright", legend=c("MPA", "Non-MPA"), col=c("blue", "red"),pch=NA, lty=1, lwd=3)
dev.off()

hist(BleachTES$SSTA_DHW, col = rgb(), xlab = expression ("DHW"), ylab = "Probability density", main = "", freq = F, ylim = c(0, 0.25), xlim = c(0,12), cex.lab = 1, cex.axis = 1)
hist(BleachMAR$SSTA_DHW, freq = F, col = rgb(), main = "", add = T)
x=seq(0,12,.01)
y=dgamma(x, TESdist$estimate['shape'], TESdist$estimate['rate'])
lines(x,y, col=rgb(0,100,255,255,max=255), lwd=2)
y=dgamma(x, MARdist$estimate['shape'], MARdist$estimate['rate'])
lines(x,y, col=rgb(255,0,0,255,max=255), lwd=2)
legend("topright", legend=c("Marine and terrestrial", "Marine only"), col=c("blue", "red"),pch=NA, lty=1, lwd=3)

dev.off()

# compare bleaching severity

kruskal.test(BleachALL$average_bleaching~BleachALL$MPA) 
# Kruskal-Wallis chi-squared = 2.1741, df = 1, p-value = 0.1404
kruskal.test(BleachBOT$average_bleaching~BleachBOT$MARINE) 
# Kruskal-Wallis chi-squared = 0.080424, df = 1, p-value = 0.7767

# compare gamma models for the onset of bleaching 

summary(BleachMPA$SSTA_DHW)
# mean 3.769
summary(BleachNON$SSTA_DHW)
# mean 3.397
BleachALL$SSTA_DHW <- BleachALL$SSTA_DHW+1
mpaall_dist <- fitdistr(BleachALL$SSTA_DHW, 'gamma')

mpa_dist_pool <- mpa_dist$loglik + non_dist$loglik
mpa_dist_glob <- mpaall_dist$loglik
pchisq(2*(mpa_dist_pool-mpa_dist_glob), df=2, lower.tail = T)
# 0.998637

BleachBOT$SSTA_DHW <- BleachBOT$SSTA_DHW+1
mpates_dist <- fitdistr(BleachBOT$SSTA_DHW, 'gamma')

tes_dist_pool <- TESdist$loglik + MARdist$loglik
tes_dist_glob <- mpates_dist$loglik
pchisq(2*(tes_dist_pool-tes_dist_glob), df=2)



# compare mpa vs non-mpa for different dhw categories

cuberoot = function(x){
  if(x < 0)
  { - (-x)^(1/3)}
  else
  {x^(1/3)}
}

hist(Bleaching_Data$SSTA_DHW)

Bleaching_Data$DHW_cat <- NA
Bleaching_Data$DHW_cat[Bleaching_Data$SSTA_DHW ==0] <- "none"
Bleaching_Data$DHW_cat[Bleaching_Data$SSTA_DHW >0 & Bleaching_Data$SSTA_DHW<=1.5] <- "low"
Bleaching_Data$DHW_cat[Bleaching_Data$SSTA_DHW >1.5 & Bleaching_Data$SSTA_DHW <4] <- "med"
Bleaching_Data$DHW_cat[Bleaching_Data$SSTA_DHW >=4] <- "high"

table(Bleaching_Data$DHW_cat)

hist(cuberoot(Bleaching_Data$average_bleaching))
summary(cuberoot(Bleaching_Data$average_bleaching))

vplot_theme <- theme(axis.title.x = element_text(size = 20, color = "black"), axis.title.y = element_text(size = 20, color = "black")) + theme(axis.text.y = element_text(size=16, color="black"), axis.text.x = element_text(size=16, color="black")) + theme(legend.title = element_text(size=24)) + theme(legend.text = element_text(size =18))

setwd(output_directory)

png(filename = "violin_plots.png", height = 3000, width = 4000, res = 350)
ggplot(Bleaching_Data, aes(MPA, sqrt(average_bleaching), fill = DHW_cat)) +
  geom_violin() +
  scale_fill_viridis_d() +
  geom_jitter() +
  ylab("Average bleaching % (log)") +
  xlab("Protected vs non-protected") +
  theme_classic() +
  vplot_theme 

dev.off()

options(scipen=999)
kruskal.test(average_bleaching~MPA, data=subset(Bleaching_Data, DHW_cat == "none"))
kruskal.test(average_bleaching~MPA, data=subset(Bleaching_Data, DHW_cat == "low"))
kruskal.test(average_bleaching~MPA, data=subset(Bleaching_Data, DHW_cat == "med"))
kruskal.test(average_bleaching~MPA, data=subset(Bleaching_Data, DHW_cat == "high"))

# density plots 

Bleaching_Data$DHW_cat <- factor(Bleaching_Data$DHW_cat, levels = c("high", "med", "low", "none"))

wrap_p <- ggplot(data=Bleaching_Data, aes(x=log1p(average_bleaching), group = MPA, fill = MPA)) +
  geom_density(adjust=1.5, alpha = 0.3) +
  theme_classic() +
  facet_wrap(~DHW_cat) +
  theme(strip.text.x = element_text(
    size = 14),
    panel.spacing = unit(0.1, "lines"),
    axis.ticks.x=element_blank()) +
  vplot_theme + 
  labs(title = "DHW categories", x = "average bleaching (log)") 
wrap_p

# stacked 



stack_p1 <- ggplot(data=subset(Bleaching_Data, MPA == "MPA"), aes(x=log1p(average_bleaching), group=DHW_cat, fill=DHW_cat)) +
  geom_density(adjust=2, position="fill", alpha = 0.9) +
  scale_fill_viridis_d(breaks=c("high","med","low","none" )) +
  theme_classic() +
  vplot_theme +
  theme(legend.position = "none") + 
  labs(title = "MPA sites", x = "average bleaching (log)") 
#guides(fill=guide_legend(title="DHW category"))
stack_p1

stack_p2 <- ggplot(data=subset(Bleaching_Data, MPA == "Non MPA"), aes(x=log1p(average_bleaching), group=DHW_cat, fill=DHW_cat)) +
  geom_density(adjust=2, position="fill", alpha = 0.9) +
  scale_fill_viridis_d(breaks=c("high","med","low","none" )) +
  theme_classic() +
  vplot_theme +
  #theme(legend.position = "none") + 
  labs(title = "non MPA sites", x = "average bleaching (log)") +
  guides(fill=guide_legend(title="DHW category"))
stack_p2

library(patchwork)

dens_plots <- wrap_p / (stack_p1 | stack_p2)

png(filename = "Density_plots.png", height = 5000, width = 5000, res = 350)
dens_plots
dev.off()

# fit nb distirbution from mass on average bleaching prevlenece 

# high dhw 

mpa_high_dhw <- subset(Bleaching_Data, MPA == "MPA" & DHW_cat == "high", select = average_bleaching)
non_high_dhw <- subset(Bleaching_Data, MPA == "Non MPA" & DHW_cat == "high", select = average_bleaching)

mpa_high_dist <- fitdistr(mpa_high_dhw$average_bleaching, "negative binomial")
non_high_dist <- fitdistr(non_high_dhw$average_bleaching, "negative binomial")

high_pooled <- fitdistr(Bleaching_Data$average_bleaching[Bleaching_Data$DHW_cat == "high"], "negative binomial")
# 3415.075
high_dist_sum <- mpa_high_dist$loglik + non_high_dist$loglik

pchisq(2*(high_dist_sum-high_pooled$loglik), df=2, lower.tail = T)
# 1
summary(mpa_high_dhw$average_bleaching)
# mean 6.336
summary(non_high_dhw$average_bleaching)
# mean 3.052

# medium

mpa_med_dhw <- subset(Bleaching_Data, MPA == "MPA" & DHW_cat == "med", select = average_bleaching)
non_med_dhw <- subset(Bleaching_Data, MPA == "Non MPA" & DHW_cat == "med", select = average_bleaching)

mpa_med_dist <- fitdistr(mpa_med_dhw$average_bleaching, "negative binomial")
non_med_dist <- fitdistr(non_med_dhw$average_bleaching, "negative binomial")

med_pooled <- fitdistr(Bleaching_Data$average_bleaching[Bleaching_Data$DHW_cat == "med"], "negative binomial")
med_dist_sum <- mpa_med_dist$loglik + non_med_dist$loglik

pchisq(2*(med_dist_sum-med_pooled$loglik), df=2, lower.tail = TRUE)
# 0.9998833
summary(mpa_med_dhw$average_bleaching)
# mean 2.369
summary(non_med_dhw$average_bleaching)
# mean 2.347

# low

mpa_low_dhw <- subset(Bleaching_Data, MPA == "MPA" & DHW_cat == "low", select = average_bleaching)
non_low_dhw <- subset(Bleaching_Data, MPA == "Non MPA" & DHW_cat == "low", select = average_bleaching)

mpa_low_dist <- fitdistr(mpa_low_dhw$average_bleaching, "negative binomial")
non_low_dist <- fitdistr(non_low_dhw$average_bleaching, "negative binomial")

low_pooled <- fitdistr(Bleaching_Data$average_bleaching[Bleaching_Data$DHW_cat == "low"], "negative binomial")
low_dist_sum <- mpa_low_dist$loglik + non_low_dist$loglik

pchisq(2*(low_dist_sum-low_pooled$loglik), df=2, lower.tail = TRUE)
# 0.9854257
summary(mpa_low_dhw$average_bleaching)
# mean 2.573
summary(non_low_dhw$average_bleaching)
# mean 1.859

# none 

mpa_none_dhw <- subset(Bleaching_Data, MPA == "MPA" & DHW_cat == "none", select = average_bleaching)
non_none_dhw <- subset(Bleaching_Data, MPA == "Non MPA" & DHW_cat == "none", select = average_bleaching)

mpa_none_dist <- fitdistr(mpa_none_dhw$average_bleaching, "negative binomial")
non_none_dist <- fitdistr(non_none_dhw$average_bleaching, "negative binomial")

none_pooled <- fitdistr(Bleaching_Data$average_bleaching[Bleaching_Data$DHW_cat == "none"], "negative binomial")
none_dist_sum <- mpa_none_dist$loglik + non_none_dist$loglik

pchisq(2*(none_dist_sum-none_pooled$loglik), df=2, lower.tail = TRUE)
# 1
summary(mpa_none_dhw$average_bleaching)
# mean 2.069
summary(non_none_dhw$average_bleaching)
# mean 1.122