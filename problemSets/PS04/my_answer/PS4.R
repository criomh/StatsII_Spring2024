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

lapply(c("eha", "survival", "ggplot2", "ggfortify", "stargazer", "xtable"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#####################
# Problem 1
#####################

### Data preparation

# load data
data <- child

# Inspect data
str(data) # Call structure of the data
summary(data) # Call summary statistics for each column

head(data, 5) # Call first 5 observations
tail(data, 5) # Call final 5 observations
colnames(data) # Call column names
lapply(data, typeof) # Call datatypes for each column
# View(data) # View whole dataset
print(unique(data$socBranch)) # Call names of the father's working branch

# Create survival object
child_surv <- with(data, Surv(enter, exit, event))
class(child_surv) # confirms survival object class

# Note the KM survival curve for mother's age cannot be visualised
# as it is a continuous variable. I transform the variable into bins of
# approx. 10 years for the purposes of visualisation in a separate object.

# Create second dataset for plotting Mother's Age
data1 <- data
# Find max/mean age
max(data1$m.age) # is 50.846
min(data1$m.age) # is 15.828
# Define the boundaries for the age group
age_breaks <- c(15, 18, 25, 35, 45, 51)
# Create labels for the age groups
age_labels <- c("15-17", "18-24", "25-34", "35-44", "45-51")
# Create the new variable m.age_bins by categorizing age into the specified groups
data1$m.age_bins <- cut(data$m.age, breaks = age_breaks, labels = age_labels, right = FALSE)
# Inspect new variable and check level order is correct
print(data1$m.age_bins)
table(data1$m.age_bins) # Check contingency table
levels(data1$m.age_bins) # Check order is correct

# Create second survival object
child_surv1 <- with(data1, Surv(enter, exit, event))
class(child_surv) # confirms survival object class

### Visualise data

# Overall Survival For reference
pdf("overall.pdf", height=8.5, width=11)
km <- survfit(child_surv ~ 1, data = child)
summary(km, times = seq(0, 15, 1))
autoplot(km, #main = "Kaplan-Meier Plot: Mother's Age",
         xlab = "Years",
         ylab = "Estimated Survival Probability",
         ylim = c(0.7, 1))
dev.off()

# Survival by Mother's Age
pdf("age.pdf", height=8.5, width=11)
km_age <- survfit(child_surv1 ~ m.age_bins, data = data1)
summary(km_age, times = seq(0, 15, 1))
autoplot(km_age,
         #main = "Kaplan-Meier Plot: Mother's Age",
         xlab = "Years",
         ylab = "Estimated Survival Probability")
dev.off()

# Survival By Infants Sex
pdf("sex.pdf", height=8.5, width=11)
km_sex <- survfit(child_surv ~ sex, data = child)
summary(km_sex)
autoplot(km_sex, #main = "Kaplan-Meier Plot: Child's Sex",
         xlab = "Years",
         ylab = "Estimated Survival Probability",
         ylim = c(0.7, 1))
dev.off()

# Create contingency table for Mother's Age bins
table <- as.data.frame(table(data1$m.age_bins))
xtable(table, type = "latex")

# Create CPH models
cox1 <- coxph(child_surv ~ m.age + sex, data = data)
summary(cox1)
stargazer(cox1, type = "latex")

cox2 <- coxph(child_surv ~ m.age * sex, data = data)
summary(cox2)
stargazer(cox2, type = "latex")

# Conduct Chi-Square Tests and Compare
test1 <- drop1(cox1, test = "Chisq") # Note statistically significant
test2 <- drop1(cox2, test = "Chisq") # Note not statistically significant