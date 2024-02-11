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

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#####################
# Problem 1
#####################

# Set seed for reproducibility, then generate data
set.seed(123)
data <- rcauchy(1000, location = 0, scale = 1)

# Create empirical distribution for reference
ECDF <- ecdf(data)
empiricalCDF <- ECDF(data)
# Test statistic for reference
D <- max(abs(empiricalCDF - pnorm(data)))

# Create Kolmogorov-Smirnov test function
kolsmir <- function(data, p_value = 0.05){
  # Sort input data
  data_sorted <- sort(data)
  
  # Calculate empirical CDF values
  ecdf_values <- (1: length(data_sorted)) / length(data_sorted)
  
  # Calculate theoretical CDF values
  tcdf_values <- pcauchy(data_sorted, location = 0, scale = 1)
  
  # Calculate maximum absolute difference between empirical and theoretical
  kolsmir_statistic <- max(abs(ecdf_values - tcdf_values))
  
  # Calculate p-value
  kolsmir_p_value <- (sqrt(2 * pi)/kolsmir_statistic) * sum(exp( -((2 * nrow(data_sorted)) - 1)^2 * pi^2 / (8 * kolsmir_statistic^2)))
  
  # Confirm
  kolsmir_result <- kolsmir_p_value < p_value
  
  return(list("KS_Test_Statistic" = kolsmir_statistic, "KS_P-Value" = kolsmir_p_value, "KS_Test_Result" = kolsmir_result))
}

# Implement Kolmogorov-Smirnov test function with generated data
data_result <- kolsmir(data = data)
print(data_result)

# Compare Reference and Custom Function Outputs
KS_compare <- data.frame("Custom Function" = data_result$KS_Test_Statistic, "Reference" = D, "Difference" = abs(data_result$KS_Test_Statistic - D))
# Show how the Test Statistics compare between my custom function and the 
print(KS_compare)


#####################
# Problem 2
#####################

# Set seed
set.seed(123)
# Generate dataframe with variable x
data <- data.frame(x = runif(200, 1, 10))
# Add y variable
data$y <- 0 + 2.75*data$x + rnorm(200, 0, 1.5)

# Use optim() function to estimate OLS regression
# Set starting values for the parameters as c(1, 1) to be optimised over
bfgs_model <- optim(par = c(1, 1),
                    # Set BFGS algorithm for the Newton-Raphson optimisation method
                    method = "BFGS",
                    # Lambda function calculates the residual sum of squares
                    # (ie differences between predicted and observed values)
                    fn = function(coef) {
                      sum((data$y - (coef[1] + coef[2]*data$x)) ^2) })

# Generate OLS regression using lm() for comparison
lm_compare <- lm(y ~ x, data = data)

# Extract coefficients for comparison
lm_coefficients <- lm_compare$coefficients
bfgs_coefficients <- bfgs_model$par

# Compare outputs using all.equal() function in base R, ignoring differences in attributes such as column names.
model_check <- all.equal(lm_coefficients, bfgs_coefficients, check.attributes = FALSE)
print(model_check) # Value is TRUE, confirming my specified function and parameters are correct