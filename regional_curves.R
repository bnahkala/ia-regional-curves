# AUTHOR: BRADY NAHKALA
# DATE: FEB 20 2020
# PROJECT: UMSRS 2020 POSTER PRESENTATION
# IN COLLABORATION WITH PETER MOORE (isu nrem)
# AND NATE HOOGEVEEN (ia dnr)

# =========================
# great plotting tools
library(ggplot2) 
# for functions, and the pipeline operator (%>%)
library(dplyr)
library(MASS)
library(scales)

# DATA ====================
# csv last updated 20 FEB 2020

# assume data is prepared correctly, in the format provided
# and in the folder of the R script/project
data.df <- read.csv("data_Rexport.csv", header = TRUE, sep = ",")
data.df <- data.df[ , 1:9]
data.iowa.df <- data.df %>%
  filter(Regional.Association == "IA")

# POWER LAW FUNCTION ====

# this user-defined function fits a linear model to the log-transformed 
# data of both drainage area and the dependent variable
# the slope and intercept of the linear model are then converted 
# and stored as variables a and b respectively, with R^2 taking 
# the third output spot in 'coeffs'
power.law.regression <- function(data.input, varcol) {
  data.input.filter <- data.input %>%
    filter(data.input$Regional.Association == "IA")
  model <- lm(log(data.input.filter[ , varcol]) ~ log(data.input.filter$Drainage.Area.sq.mi))
  a <- exp(coef(model)[1])
  b <- coef(model)[2]
  rsq <- summary(model)$r.squared
  coeffs <- c(a, b, rsq)
  return(coeffs)
}

# the previous function, but with no data filtering
power.law.all <- function(data.input, varcol) {
  # data.input.filter <- data.input %>%
  #   filter(data.input$Regional.Association == "IA")
  model <- lm(log(data.input[ , varcol]) ~ log(data.input$Drainage.Area.sq.mi))
  a <- exp(coef(model)[1])
  b <- coef(model)[2]
  rsq <- summary(model)$r.squared
  coeffs <- c(a, b, rsq)
  return(coeffs)
}

# MODELS ===============
# Iowa regression models
coeffs.DAvXS <- power.law.regression(data.iowa.df, 6)
coeffs.DAvW <- power.law.regression(data.iowa.df, 7)
coeffs.DAvD <- power.law.regression(data.iowa.df, 8)
coeffs.DAvS <- power.law.regression(data.iowa.df, 9)

# Regresssion models for all imported data as a comparison
# to show improvement of Iowa curves
coeffs.DAvXS.all <- power.law.all(data.df, 6)
coeffs.DAvW.all <- power.law.all(data.df, 7)
coeffs.DAvD.all <- power.law.all(data.df, 8)
coeffs.DAvS.all <- power.law.all(data.df, 9)

# the next 3 loops populate new columns of data that calculate the 
# predicted variable value for each sample in the dataset
# this is a really inefficient way of plotting regression lines, but 
# ggplot was misbehaving
for (x in 1:length(data.df$Regional.Association)) {
  if (data.df$Regional.Association[x] == "IA") {
    data.df$XS.model[x] <- as.numeric(coeffs.DAvXS[1]*data.df$Drainage.Area.sq.mi[x]^coeffs.DAvXS[2])
  } else if (data.df$Regional.Association[x] == "Eastern MN") {
    data.df$XS.model[x] <- as.numeric(5.3096*data.df$Drainage.Area.sq.mi[x]^0.7054)
  } else {
    data.df$XS.model[x] <- as.numeric(4.7456*data.df$Drainage.Area.sq.mi[x]^0.6102)
  }
}

for (x in 1:length(data.df$Regional.Association)) {
  if (data.df$Regional.Association[x] == "IA") {
    data.df$W.model[x] <- as.numeric(coeffs.DAvW[1]*data.df$Drainage.Area.sq.mi[x]^coeffs.DAvW[2])
  } else if (data.df$Regional.Association[x] == "Eastern MN") {
    data.df$W.model[x] <- as.numeric(6.6316*data.df$Drainage.Area.sq.mi[x]^0.4534)
  } else {
    data.df$W.model[x] <- as.numeric(5.7926*data.df$Drainage.Area.sq.mi[x]^0.3867)
  }
}

for (x in 1:length(data.df$Regional.Association)) {
  if (data.df$Regional.Association[x] == "IA") {
    data.df$D.model[x] <- as.numeric(coeffs.DAvD[1]*data.df$Drainage.Area.sq.mi[x]^coeffs.DAvD[2])
  } else if (data.df$Regional.Association[x] == "Eastern MN") {
    data.df$D.model[x] <- as.numeric(0.8032*data.df$Drainage.Area.sq.mi[x]^0.2514)
  } else {
    data.df$D.model[x] <- as.numeric(0.819*data.df$Drainage.Area.sq.mi[x]^0.2235)
  }
}

for (x in 1:length(data.df$Regional.Association)) {
  if (data.df$Regional.Association[x] == "IA") {
    data.df$S.model[x] <- as.numeric(coeffs.DAvS[1]*data.df$Drainage.Area.sq.mi[x]^coeffs.DAvS[2])
  } else {
    data.df$S.model[x] <- 0
  }
}


# XS AREA CURVE ===========

plot.XS <- ggplot(data = data.df, 
                  aes(x=Drainage.Area.sq.mi, y=XS.Area.sq.ft,
                      color=Regional.Association))+
  geom_point(size=2)+
  geom_line(aes(x=Drainage.Area.sq.mi, y=XS.model), size=1.5) +
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) + # these scale_x_log10 functions can be difficult, hard to format correctly
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  xlab("Drainage Area (sq. miles)") +
  ylab("Cross Sectional Area (sq. feet)")+
  labs(color="Region")+
  theme(text = element_text(size = 24), legend.position = c(0.875, 0.125)) 
  
  
# windows(height = 600, width = 800)
# plot.XS

# BNKFULL WIDTH CURVE =====
plot.BNKW <- ggplot(data = data.df, 
                  aes(x=Drainage.Area.sq.mi, y=Bankfull.Width.ft,
                      color=Regional.Association))+
  geom_point(size=2)+
  geom_line(aes(x=Drainage.Area.sq.mi, y=W.model), size=1.5) +
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) + 
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  xlab("Drainage Area (sq. miles)") +
  ylab("Bankfull Width (ft)")+
  labs(color="Region")+
  theme(text = element_text(size = 24), legend.position = c(0.875, 0.125))

# windows()
# plot.BNKW


# BNKFULL DEPTH CURVE =====
plot.BNKD <- ggplot(data = data.df, 
                  aes(x=Drainage.Area.sq.mi, y=Bankfull.Depth.ft,
                      color=Regional.Association))+
  geom_point(size=2)+
  geom_line(aes(x=Drainage.Area.sq.mi, y=D.model), size=1.5) +
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) + 
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  xlab("Drainage Area (sq. miles)") +
  ylab("Bankfull Depth (ft)")+
  labs(color="Region")+
  theme(text = element_text(size = 24), legend.position = c(0.875, 0.125))

# windows()
# plot.BNKD

# SLOPE CURVE =============
plot.S <- ggplot(data = data.df[data.df$Regional.Association == "IA",], 
                  aes(x=Drainage.Area.sq.mi, y=Longitudinal.Slope.pct,
                      color=Regional.Association))+
  geom_point(size=2)+
  geom_line(aes(x=Drainage.Area.sq.mi, y=S.model), size=1.5) +
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) + 
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  xlab("Drainage Area (sq. miles)") +
  ylab("Longitudinal Slope")+
  labs(color="Region")+
  theme(text = element_text(size = 24), legend.position = c(0.125, 0.125))+
  scale_color_manual(values="#00BA38") # color corrects to match the Iowa green color in other graphs, otherwise defaults to red

# windows()
# plot.S


# SAVE IMAGES ===========
png(filename = "XS.png", width = 800, height=600)
plot.XS
dev.off()

png(filename = "BNKW.png", width = 800, height=600)
plot.BNKW
dev.off()

png(filename = "BNKD.png", width = 800, height=600)
plot.BNKD
dev.off()

png(filename = "S.png", width = 800, height=600)
plot.S
dev.off()

