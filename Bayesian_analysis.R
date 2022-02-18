# colineratiy matrix 

library(tidyverse)
library(corrmorant)
library(MASS)
library(brms)
library(rstanarm)
library(bayesplot)
library(tidybayes)
library(modelr)
library(lme4)
library(emmeans)
library(broom)

Bleaching_data_directory="C:/Users/40274182/OneDrive - Queen's University Belfast/PhD/Chapters/Bleaching/MPA/Data_files"
setwd(Bleaching_data_directory)
Bleaching_Data <- read.csv("Bleaching_MPA.csv", header = T, sep = ",")

output_directory <- "C:/Users/40274182/OneDrive - Queen's University Belfast/PhD/Chapters/Bleaching/MPA/Data_files/Output_directory"

# validate dhw - bleaching relationship
summary(Bleaching_Data$Count)
hist(Bleaching_Data$Count)

Bleaching_Data$average_bleaching <- Bleaching_Data$Count/4
Bleaching_Data$average_bleaching <- round(Bleaching_Data$average_bleaching)

Bleaching_Data$SSTA_DHW <- as.numeric(Bleaching_Data$SSTA_DHW)
Bleaching_Data$SSTFilled <- as.numeric(Bleaching_Data$SSTFilled)
Bleaching_Data$SSTFilled <- Bleaching_Data$SSTFilled-273

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
Bleaching_Data <- Bleaching_Data[!is.na(Bleaching_Data$Depth),]
Bleaching_Data <- Bleaching_Data[!is.na(Bleaching_Data$kd490_value),]


Bleaching_Data <-Bleaching_Data %>%
  dplyr::distinct(Date, Site, Region, Depth, Longitude.Degrees, Latitude.Degrees, .keep_all=T)

df2 <- dplyr::select(Bleaching_Data, c(SSTA_DHW, SSTFilled, MPA_AGE, REP_M_AREA, GIS_M_AREA, REP_AREA, GIS_AREA, NO_TK_AREA, Depth, kd490_value))


df2[] <- lapply(df2, gsub, pattern='Not MPA', replacement= NA)
df2[] <- lapply(df2, gsub, pattern='Non MPA', replacement= NA)
df2 <- na.omit(df2)
df2 <- as.data.frame(sapply(df2, as.numeric))

myStd <- function(x) { (x-mean(x))/sd(x)}

df2 <- as.data.frame(sapply(df2, myStd))


setwd(output_directory)

tiff(file='figure_1.tif',height=3800,width=5000,res=300)
corfig <- ggcorrm(df2, 
                  mapping = aes(col = .corr, fill = .corr),
                  bg_dia = "grey20", 
                  rescale = "by_sd", 
                  corr_method = "pearson") +
  #lotri(geom_smooth(method = "glm", size = .3)) +
  #lotri(geom_point(alpha = 0.5)) +
  lotri_heatcircle(alpha = 1, col = 1) +
  utri_corrtext(nrow = 2, squeeze = 0.4) +
  dia_names(y_pos = 0.15, size = 3, color = "white") +
  dia_histogram(lower = 0.3, color = "grey80", fill = "grey60", size = .3) +
  scale_color_corr(aesthetics = c("fill", "color")) +
  theme(text = element_text(size = 14)) 
corfig
dev.off()

png(file='correlation_matrix.png', height=3800,width=5000,res=350)
corfig
dev.off()

# stan model

##################################################

rm(df2)

df2 <- dplyr::select(Bleaching_Data, c(SSTA_DHW, REP_M_AREA,  REP_AREA, MPA_AGE, GIS_AREA, GIS_M_AREA, average_bleaching, Ecoregion,  Depth, kd490_value))
df2[] <- lapply(df2, gsub, pattern='Not MPA', replacement= NA)
df2[] <- lapply(df2, gsub, pattern='Non MPA', replacement= NA)
df2 <- na.omit(df2)


df2$SSTA_DHW <- as.numeric(df2$SSTA_DHW)
df2$REP_M_AREA <- as.numeric(df2$REP_M_AREA)
df2$REP_AREA <- as.numeric(df2$REP_AREA)
df2$GIS_AREA <- as.numeric(df2$GIS_AREA)
df2$GIS_M_AREA <- as.numeric(df2$GIS_M_AREA)
df2$MPA_AGE <- as.numeric(df2$MPA_AGE)
df2$average_bleaching <- as.numeric(df2$average_bleaching)
df2$Depth <- as.numeric(df2$Depth)
df2$kd490_value <- as.numeric(df2$kd490_value)
df2$Ecoregion <- as.factor(df2$Ecoregion)

df3 <- dplyr::select(df2, c(SSTA_DHW, REP_M_AREA, REP_AREA, GIS_AREA, GIS_M_AREA, MPA_AGE, Depth, kd490_value))

df3 <- as.data.frame(sapply(df3, myStd))


df3 <- cbind(df3, df2$average_bleaching)
df3 <- cbind(df3, df2$Ecoregion)

colnames(df3)[9] <- "average_bleaching"
colnames(df3)[10] <- "ecoregion"

df3$average_bleaching <- as.numeric(df3$average_bleaching)
df3$ecoregion <- as.factor(df3$ecoregion)

hist(df3$average_bleaching)



options(mc.cores = parallel::detectCores())


mod1 <- stan_glmer.nb(average_bleaching~SSTA_DHW*(kd490_value+REP_M_AREA+MPA_AGE+Depth) + (1 | ecoregion), 
                      data = df3, 
                      iter = 5000,
                      chains = 4,
                      cores = 4)


summary(mod1, pars = "beta", probs = c(0.05, 0.95), digits = 4)
prior_summary(mod1)
y<-df3$average_bleaching
yrep_nb <- posterior_predict(mod1, draws = 500)
ppc_dens_overlay(y, yrep_nb[1:50,]) + xlim(0,50)
ppc_hist(y, yrep_nb[1:5,])

prop_zero <- function(x) mean(x == 0)
prop_zero(y) # check proportion of zeros in y

png(filename = 'posterior_predictive_checks.png', height = 1500, width = 1500, res = 350)
ppc_stat(y, yrep_nb, stat = "prop_zero")
dev.off()



theme_set(theme_tidybayes())
setwd(output_directory)

png(file='trace_plots.png',height=2500,width=3000,res=350)
plot(mod1, "trace", pars = c( "beta"),
     facet_args = list(nrow = 5))
dev.off()


plot(mod1, pars = "beta",  prob = 0.8, prob_outer = 0.95)

png(file='mod1_coeffs.png',height=2500,width=3000,res=350)


coefs <- plot(mod1, pars = c("MPA_AGE", "Depth", "kd490_value", "SSTA_DHW:Depth", "SSTA_DHW:kd490_value", "REP_M_AREA", "SSTA_DHW:REP_M_AREA", "SSTA_DHW:MPA_AGE", "SSTA_DHW"),
              prob = 0.8, prob_outer = 0.95) +
  xlab("Model coefficient") +
  scale_y_discrete(breaks=c("MPA_AGE", "Depth", "kd490_value", "SSTA_DHW:Depth", "SSTA_DHW:kd490_value", "REP_M_AREA", "SSTA_DHW:REP_M_AREA", "SSTA_DHW:MPA_AGE", "SSTA_DHW"), labels=c("MPA Age", "Depth", "Turbidity", "DHW*Depth", "DHW*Turbidity", "MPA Size", "DHW*MPA Size", "DHW*MPA Age", "DHW")) +
  theme_classic() +
  theme(axis.text.y = element_text(size = 16)) +
  theme(axis.text.x = element_text(size = 16) ) +
  theme(axis.title.x = element_text(size = 20))

coefs

dev.off()


# ecoregion values 

sims <- as.matrix(mod1)
dim(sims)
para_name <- colnames(sims)
para_name

# Obtain school-level varying intercept a_j
# draws for overall mean
mu_a_sims <- as.matrix(mod1, 
                       pars = "(Intercept)")
# draws for 73 schools' school-level error
u_sims <- as.matrix(mod1, 
                    regex_pars = "b\\[\\(Intercept\\) ecoregion\\:")
# draws for 73 schools' varying intercepts               
a_sims <- as.numeric(mu_a_sims) + u_sims          

# Obtain sigma_y and sigma_alpha^2
# draws for sigma_y
s_y_sims <- as.matrix(mod1, 
                      pars = "Sigma")
# draws for sigma_alpha^2
s__alpha_sims <- as.matrix(mod1, 
                           pars = "Sigma[ecoregion:(Intercept),(Intercept)]")

# Compute mean, SD, median, and 95% credible interval of varying intercepts

# Posterior mean and SD of each alpha
a_mean <- apply(X = a_sims,     # posterior mean
                MARGIN = 2,
                FUN = mean)
a_sd <- apply(X = a_sims,       # posterior SD
              MARGIN = 2,
              FUN = sd)

# Posterior median and 95% credible interval
a_quant <- apply(X = a_sims, 
                 MARGIN = 2, 
                 FUN = quantile, 
                 probs = c(0.025, 0.50, 0.975))
a_quant <- data.frame(t(a_quant))
names(a_quant) <- c("Q2.5", "Q50", "Q97.5")

# Combine summary statistics of posterior simulation draws
rm(a_df)
a_df <- data.frame(a_mean, a_sd, a_quant)
round(head(a_df), 2)

# Sort dataframe containing an estimated alpha's mean and sd for every school
a_df <- a_df[order(a_df$a_mean), ]
a_df$a_rank <- c(1 : dim(a_df)[1])  # a vector of school rank 


a_df <- tibble::rownames_to_column(a_df, "Ecoregion")
a_df$Ecoregion<-gsub("[b[(Intercept) ecoregion:]","",as.character(a_df$Ecoregion))
a_df$Ecoregion<-gsub("]","",as.character(a_df$Ecoregion))


# Plot school-level alphas's posterior mean and 95% credible interval

png(file='Ecoregion intercept variance.png',height=2500,width=3000,res=350)
ggplot(data = a_df, 
       aes(x = reorder(Ecoregion, a_rank), Ecoregion, 
           y = a_mean)) +
  geom_pointrange(aes(ymin = Q2.5, 
                      ymax = Q97.5),
                  position = position_jitter(width = 0.1, 
                                             height = 0)) + 
  geom_hline(yintercept = mean(a_df$a_mean), 
             size = 0.5, 
             col = "red") + 
  coord_flip() + 
  xlab("Ecoregion") +
  ylab("Intercept variance") +
  theme_classic() +
  theme(axis.text.y = element_text(size = 12)) +
  theme(axis.text.x = element_text(size = 12)) +
  theme(axis.title.x = element_text(size = 14)) +
  theme(axis.title.y = element_text(size = 14))
dev.off()

write.csv(a_df, file = "Ecoregioin_outputs.csv")
