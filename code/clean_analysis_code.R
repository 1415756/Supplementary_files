library(betareg) # allows beta regression to be performed
library(rstatix) # for coding statistical tests and assumptions
library(ggplot2) # for figure creation
library(ggsignif) # ability to add indicators to test result significance 
library(emmeans) # to use joint_tests() fucntiont o compare means
library(ggeffects)# package to display model outputs from logit link model

###### Determining variables that effect Green Status recovery score  
#  code is adapted from supplementary data from:
# Grace et al. (2021) Testing a global standard for species recovery and assessing conservation impact.
Data2<-read.csv("traits_to_test_clean.csv", header=T)
#review data before analysing
head(Data2) #shows first row
str(Data2)
summary(Data2)
#  testing  a model based on 3 hypotheses within thesis: 
# body mass is logged to follow assumptions of normality 
model_log_mass <-  betareg(gs_as_percentage ~ parental_dependence_midpoint+	log(mass)+ hand_wing_index,
                           data = Data2)
# checking model fit 
par(mfrow = c(3, 2))
plot(model_log_mass, which = 1:6)
par(mfrow = c(1, 1))
# following advice of fit from Cribari-Neto and Zeileis (2010)

summary(model_log_mass)# model output summary

# calculating effects of model 
effects_pd <- ggpredict(model_log_mass_sqt_pd, terms = "parental_dependence_midpoint")
effect_mass <- ggpredict(model_log_mass, terms = "mass")

###### 
# ANOVA of spatial units 
boxplot_data <- read.csv("County_land_use.csv")

# making a new column to represent land remaining (not percentage of land modified)
boxplot_data$flipped <- 100-(boxplot_data$land_use_chnage_since_1750)
# setting values below 0 to 0 (ie values suggesting over 100 of land has been modified due to estimation method)
boxplot_data[c(36,44),5] <- 0

# checking normality and homogeneity of variances
s <- lm(flipped~Region, data = boxplot_data) 
plot(s,1) # homogeneity of variances
plot(s,2) #data is  normally distributed
# running ANOVA after removing Yorkshire and North East spatial unit, as too small sample size to keep
anova_flip <- aov(flipped~Region, boxplot_data[-c(49:51),])
summary(anova_flip)# summary of model to check is significance
TukeyHSD(anova_flip)# running post-hoc test to determine which relationships are significant








