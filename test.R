# Testing out with new dataset

# Administrative
rm(list = ls())
set.seed(0)
df <- read.csv("datasets/company_bankruptcy.csv")
library(gamlr)

# Dimensions
dim(df)

# Find number of missing values
sum(is.na(df))

# Y variable
summary(df$Bankrupt.)
Y <- df$Bankrupt.

# Possible unnecessary variable based on visual inspection of the data set
summary(df$Net.Income.Flag)

# Take out unnecessary variable (unique length of 1) and Y variable
df <- subset(df, select = -c(Net.Income.Flag, Bankrupt.))

# Check if variables were taken out
dim(df)

# Naref function from gamlr library
df <- naref(df)

# Create X matrix
X <- model.matrix(~ ., data=df)[,-1]

# Run CV
cv_bankrupt <- cv.gamlr(X, Y, nfold = 10, family="binomial", verb=TRUE)

# Combine the two plots next to each other
#par(mfrow=c(1,2))
plot(cv_bankrupt$gamlr) # lasso regularization path

plot(cv_bankrupt) # plot of cross-validation error against lambda

# the value of the optimal penalty from our CV model
cv_bankrupt[["lambda.min"]]

# the value of the penalty (lambda) from the least complex model
cv_bankrupt[["gamlr"]][["lambda"]][["seg1"]]

# Number of non-zero coefficients in the optimal CV model minus the intercept
cat("number of nonzero coefficients for CV-optimal lambda:", length(which(coef(cv_bankrupt, select="min")!=0))-1, "\n")

# Number of total coefficients of our optimal CV model minus the intercept
cat("number of total coefficients for CV-optimal lambda:", length(coef(cv_bankrupt, select="min"))-1, "\n")

cv_coefs <- drop(coef(cv_bankrupt, select="min"))

# Drop intercept
cv_coefs = cv_coefs[-1]

## The following coefficients are non-zero
non_zero_coef <- cv_coefs[which(cv_coefs!=0)]
sort(non_zero_coef, decreasing = TRUE)

# Computing the predicted probabilities
pred_CV <- drop(predict(cv_bankrupt, select='min', X, type="response"))

# Plotting the histogram of predicted probabilities
hist(pred_CV, xlab = "Predicted Probabilities")
