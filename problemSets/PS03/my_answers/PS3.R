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

lapply(c("nnet", "MASS", "ggplot2", "stargazer"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


#####################
# Problem 1
#####################

# load data
gdp_data <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsII_Spring2024/main/datasets/gdpChange.csv",
                     stringsAsFactors = F)

# Inspect data
str(gdp_data) # Call structure of the data
summary(gdp_data) # Call summary statistics for each column

head(gdp_data, 5) # Call first 5 observations
tail(gdp_data, 5) # Call final 5 observations
colnames(gdp_data) # Call column names
lapply(gdp_data, typeof) # Call datatypes for each column
View(gdp_data) # View whole dataset

print(unique(gdp_data$CTYNAME)) # Call names of countries in dataset
length(unique(gdp_data$CTYNAME)) # Call total number of countries

# Check response variable. Note is numeric.
print(gdp_data$GDPWdiff)
# Transform variable into factor with levels "negative", "no change", and "positive"
gdp_data$GDPWdiff <- as.factor(ifelse(gdp_data$GDPWdiff < 0, "negative",
                                      ifelse(gdp_data$GDPWdiff == 0, "no change",
                                             "positive")))
# Confirm now a factor
is.factor(gdp_data$GDPWdiff)
# Check levels are updated
levels(gdp_data$GDPWdiff)
# Relevel so "no change" is reference category
gdp_data$GDPWdiff <- relevel(gdp_data$GDPWdiff, ref = "no change")
# Confirm releveling is successful
levels(gdp_data$GDPWdiff)

# Check explanatory variables. Note each are 'sparse' with over 50% 'empty'
print(gdp_data$REG)
cat("Note that the data is", round((1 - sum(gdp_data$REG)/length(gdp_data$REG))*100, 2),
    "% 'empty' or zeros, meaning it is considered sparse")

print(gdp_data$OIL)
cat("Note that the data is", round((1 - sum(gdp_data$OIL)/length(gdp_data$OIL))*100, 2),
    "% 'empty' or zeros, meaning it is considered sparse")

# Visualise dataset

contingency_table <- ftable(xtabs(~ GDPWdiff + REG + OIL, data = gdp_data))

ggplot(gdp_data, aes(x = REG, y = OIL, fill = factor(GDPWdiff))) +
  geom_bar(stat = "identity") +
  labs(x = "REG", y = "OIL", fill = "GDPWdiff") +
  ggtitle("Contingency Table Visualization") +
  theme_minimal() +
  theme(legend.position = "top") +
  scale_fill_manual(values = c("no change" = "blue", "positive" = "red", "negative" = "green"))

ggplot(gdp_data, aes(REG, OIL)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.3) +
  #scale_x_discrete(labels=function(x){sub("\\s", "\n", x)}) +
  theme(axis.text.x = element_text(angle = 45)) +
  facet_grid(gender ~ year)

# ---

# Fit unordered multinomial logistic regression model
model1 <- multinom(GDPWdiff ~ REG + OIL, data = gdp_data)
# Print regression table
summary(model1)
# Print odds ratios
model1_ORs <- exp(coef(model1))


# Fit ordered multinomial logistic regression model
model2 <- polr(GDPWdiff ~ REG + OIL, data = gdp_data, Hess = TRUE)
# Print regression table
summary(model2)
# Print odds ratios
model2_ORs <- exp(coef(model2))

#####################
# Problem 2
#####################

# load data
mexico_elections <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsII_Spring2024/main/datasets/MexicoMuniData.csv")

# Inspect data
str(mexico_elections) # Call the structure of the data
summary(mexico_elections) # Call summary statistics for each column

head(mexico_elections, 5) # Call first 5 observations
tail(mexico_elections, 5) # Call final 5 observations
colnames(mexico_elections) # Call column names
lapply(mexico_elections, typeof) # Call datatypes for each column - note all numeric
summary(mexico_elections) # Call summary statistics for each column
View(mexico_elections) # Call the data in a dedicated window view

# Note binary variables must be coerced into logicals for analysis
is.logical(mexico_elections$competitive.district)
is.logical(mexico_elections$PAN.governor.06)

# Coerce within the dataframe's environment
mexico_elections <- within(mexico_elections, {
  competitive.district <- as.logical(competitive.district)
  mexico_elections$PAN.governor.06 <- as.logical(mexico_elections$PAN.governor.06)
})

# Note binary variables now return TRUE as logicals
is.logical(mexico_elections$competitive.district)
is.logical(mexico_elections$PAN.governor.06)

# Check assumptions of the response variable for Poisson - 
# are the mean and variance approximately equal? No...
with(mexico_elections,
     list(mean(PAN.visits.06), var(PAN.visits.06)))

# Visualise with a histogram - shows very strong right-skew...
ggplot(data = mexico_elections, aes(PAN.visits.06)) +
  geom_histogram()
  
  
  
  
hist(mexico_elections$PAN.visits.06)

plot(mexico_elections, aes(competitive.district, PAN.visits.06, color = PAN.governor.06)) +
  geom_jitter(alpha = 0.5)

plot(mexico_elections$competitive.district, mexico_elections$PAN.visits.06) +
  geom_jitter(alpha = 0.5)

# But we'll do it anyway...

# TASK A:

# Run Poisson regression...
# ... with only competitive.district as predictor
mod.ps1 <- glm(PAN.visits.06 ~ competitive.district,
               data = mexico_elections,
               family = poisson)
summary(mod.ps1)

# ... with competitive.district, marginality.06 and PAN.governor.06 as predictors
mod.ps2 <- glm(PAN.visits.06 ~ competitive.district + marginality.06 + PAN.governor.06,
               data = mexico_elections,
               family = poisson)
summary(mod.ps2)

# Export for Latex
pos_tab1 <- stargazer(mod.ps1, type = "latex")
pos_tab2 <- stargazer(mod.ps2, type = "latex")

# Exponentiating log-count coefficients
exp(mod.ps1$coefficients)
exp(mod.ps2$coefficients)

# Create hypothetical district
pred <- data.frame(MunicipCode = 0,
                   pan.vote.09 = 0,
                   marginality.06 = 0,
                   PAN.governor.06 = 1,
                   competitive.district = 1)

pred_hyp <- predict(mod.ps2, newdata = pred, type = "response")
pred_hyp <- cbind(pred_hyp, "Exp" = exp(pred_hyp))

# plot predictions vs count
ggplot(data = NULL, aes(x = mod.ps2$fitted.values, y = long_data$art)) +
  geom_jitter(alpha = 0.5) +
  geom_abline(color = "blue") #+
#geom_smooth(method = "loess", color = "red")

# calculate pseudo R squared
1 - (mod.ps$deviance/mod.ps$null.deviance)

# calculate RMSE
sqrt(mean((mod.ps$model$art - mod.ps$fitted.values)^2))