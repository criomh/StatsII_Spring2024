#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c("dplyr", "ggplot2", "stargazer"),  pkgTest)

library(stargazer)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#####################
# Problem 1
#####################

# load data
load(url("https://github.com/ASDS-TCD/StatsII_Spring2024/blob/main/datasets/climateSupport.RData?raw=true"))

### Inspect dataset
head(climateSupport) # Check overall values of dataset
str(climateSupport) # Check shape, and names and classes of variables
summary(climateSupport) # Check summary stats ie frequencies of values
lapply(climateSupport, class) # Confirm explanatory variables are ordered factors
lapply(climateSupport, unique) # Check levels for each variable
levels(climateSupport$countries) # Check level ordering
levels(climateSupport$sanctions) # Check level ordering
names(climateSupport) # Check just variable names
anyNA.data.frame(climateSupport) # FALSE confirms no NA values to account for



### Fit an additive model with response ("choice") and predictors ("countries", "sanctions")
CSmodel <- glm(choice ~ countries + sanctions,
               data = climateSupport,
               family = binomial(link = "logit"))
# Inspect new logistical model details
CSmodel_summary <- summary(CSmodel) # Review summary statistics
CSmodel_table <- stargazer(CSmodel,
          type = "latex",
          title = "Logistic Regression Model",
          align = TRUE)

### Conduct global null hypothesis test

# First, using Chi-Squared test
global_null_chisq <- anova(CSmodel, test = "Chisq")
print(global_null_chisq)
anova1 <- stargazer(global_null_chisq,
                           type = "latex",
                           title = "Analysis of Deviance: CSmodel and Chi-Squared Test",
                           align = TRUE)

# Second, comparing with the nested null model
CSmodel_null <- glm(choice ~ 1,
               data = climateSupport,
               family = binomial(link = "logit"))
global_null_LRT <- anova(CSmodel_null, CSmodel, test = "LRT")
print(global_null_LRT)
anova2 <- stargazer(global_null_LRT,
                    type = "latex",
                    title = "Analysis of Deviance: CSmodel and LRT Test",
                    align = TRUE)



### Computing log-odds
log_odds_change <- as.vector(CSmodel$coefficients[5] - CSmodel$coefficients[4])
odds_ratio_change <- exp(log_odds_change)
print(odds_ratio_change)



### Estimate probability for 80 countries and no sanctions
log_odds_2b <- as.vector(CSmodel$coefficients[1] + CSmodel$coefficients[3])
probability_support_2b <- 1 / (1 + exp(-log_odds_2b))
print(probability_support_2b)

### Conduct hypothesis test on interaction model

# Create interaction model
CSmodel_interaction <- glm(choice ~ countries + sanctions + countries*sanctions,
                           data = climateSupport,
                           family = binomial(link = "logit"))
# Compare with CSmodel as a nested function
global_null_interaction <- anova(CSmodel, CSmodel_interaction, test = "LRT")
anova3 <- stargazer(global_null_interaction,
                    type = "latex",
                    title = "Analysis of Deviance: CSmodel and Interaction with LRT Test",
                    align = TRUE)