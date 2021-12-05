# Meta-Analysis of the Creation Time of 3D Floor Plan Models
# Written by: Marissa Hamilton
# Institute: University of Calgary
# Course: ENGO 642 - Optical Imaging Metrology
# Purpose: Final Project

# All material used in code is based off of the following:
# Harrer, M., Cuijpers, P., Furukawa, T.A., & Ebert, D.D. (2021). Doing Meta-Analysis with R: A Hands-On Guide. Boca Raton, FL and London: Chapmann & Hall/CRC Press. ISBN 978-0-367-61007-4.
# Shaphiro, S., & Wilk, M. (1965). An analysis of variance test for normality. Biometrika, 52(3), 591-611. 


# Must load libraries first
library(dplyr)
library(ggpubr)
library(tidyverse)
library(meta)
library(metafor)
library(metacor)
library(dmetar)
library(moments)

# Reading file into working file
ds <- read.csv("Meta2.csv")

# Creating a initial box plot to examine the data
boxplot(ds$time,
        main = "Boxplot of the Mean Time ")

# Testing for normality
## Creating groupable data
paper <- c(1,2,3)
## Creating a data frame of the desired data
mydata <- data.frame(paper,ds$time)
## Setting the Seed
set.seed(1234)
## Sampling the grouped data
dplyr::sample_n(mydata,3)

## visualizaing the intial input of data in a density plot
ggdensity(mydata$ds.time,
          main = "Density Plot",
          xlab = "time")
## Visualizing by another plot type
ggqqplot(mydata$ds.time,
         main = "Density Plot")


## Shapiro-Wilk Normality test on initial data input
shapiro.test(mydata$ds.time)
skewness(mydata$ds.time)
kurtosis(mydata$ds.time)



# Getting variables for each study 
TE <- ds$time

# Standard error calculation
seTE<- sd(TE)/sqrt(ds$nstudy)


# Meta-Analysis
## SMD -> Standardized Mean Deviation
## REML -> Restricted Maximum Likelihood 
## Fixed -> due to the amount of studies in the meta analysis
m.gen <- metagen(TE = TE,
                 seTE = seTE,
                 studlab = au,
                 data = ds,
                 id = ds$study,
                 sm = "SMD",
                 fixed = TRUE,
                 random = FALSE,
                 hakn = TRUE,
                 method.tau = "REML",
                 #method.tau.ci = gs("method.tau.ci"),
                 title = "Meta-Analysis of Time"
                 
)
# Viewing the results of the Meta-Analysis
summary(m.gen)

# Finding outliers of the Meta-Analysis
m.gen.out <- dmetar::find.outliers(m.gen)
## Getting a summary of the results
m.gen.out

# Determine which study would have the highest influence on the results of the
m.gen.inf <- InfluenceAnalysis(m.gen, random = TRUE)
## Getting the summary of the results
m.gen.inf

# Plots of the Influence Analysis
##The Baujat Plot of the Influence Analysis
plot(m.gen.inf, "baujat", title = "Baujat Plot")
## The Influence Plot
plot(m.gen.inf, "influence")
## The 
plot(m.gen.inf, "es")
## The
plot(m.gen.inf, "i2")

# Visual of the meta analysis
forest.meta(m.gen, 
            sortvar = TE,
            predict = TRUE, 
            print.tau2 = FALSE,
            leftlabs = c("Author", "Mean Time", "SE"))


# Publication Bias
## Define fill colors for contour
col.contour = c("gray75", "gray85", "gray95")
## Generate funnel plot (we do not include study labels here)
funnel.meta(m.gen,xlim = c(-150,350), contour = c(.90, .95, .99),
            col.contour = col.contour)
## Add a legend
legend(x = 275, y = 1, 
       legend = c("p < 0.1", "p < 0.05", "p < 0.01"),
       fill = col.contour,
       cex = .75)
## Add a title
title("Contour-Enhanced Funnel Plot (3D Floor Plan Creation)")

# Meta Regression
## Creating a variable for the mean number of Point cloud data
npc <- ds$npc
## Running the meta regression
m.gen.reg <- metareg(m.gen, ~npc)
m.gen.reg
## Visual representation of the meta regression
bubble(m.gen.reg, studlab = TRUE)
title("Bubble Plot of Meta Regression")

# Funnel plot of the efficiency 
fun.meta <- funnel.meta(m.gen, studlab = TRUE)
title("Funnel Plot (Efficiency of 3D Floor Plan Creation)")

# Remove all environmental variables
rm(list = ls(all.names = TRUE))

