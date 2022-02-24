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

# Possible unnecessary variable based on visual inspection of the data set
summary(df$Net.Income.Flag)

# Take out unnecessary variable (unique length of 1) and Y variable
df <- subset(df, select = -c(Net.Income.Flag))

# Check if variables were taken out
dim(df)

# Naref function from gamlr library
df <- naref(df)

# Split dataset

split = sort(sample(nrow(df), nrow(df)*.5))
training_set <- df[split,]
test_set <- df[-split,]
dim(training_set)
dim(test_set)

# Y variable
summary(training_set$Bankrupt.)
Y <- training_set$Bankrupt.

training_set <- subset(training_set, select = -c(Bankrupt.))
test_set <- subset(test_set, select = -c(Bankrupt.))


# Create X matrix
X <- model.matrix(~ ., data=training_set)[,-1]



dim(X)
length(Y)
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

# Boxplot
boxplot(pred_CV ~ Y, xlab="bankruptcy", ylab="prob of bankruptcy", col=c("pink","dodgerblue"))

# Setting seed and loading in the ROC.R
set.seed(0)
source("roc.R")

cutoff_points = c(0.05,0.1,0.2,0.4,0.8)
cutoff_colors = c("blue","red","green","yellow","purple")

# IS curve
par(mai=c(.9,.9,.2,.1)) # format margins
roc(pred_CV, Y, bty="n", main="IS ROC") # from roc.R

# For loop to make my code more concise
for (i in cutoff_points){
  points(x=1-mean((pred_CV<=i)[Y==0]), y=mean((pred_CV>i)[Y==1]), cex=1.5, pch=20, col=cutoff_colors[match(i,cutoff_points)])
}

legend("bottomright",fill=cutoff_colors, legend=c(cutoff_points),bty="n",title="cutoffs")


# refit the model using only 1/2 of data
set.seed(0)
test <- sample.int(5000,2500)
logit_lasso_half <- gamlr(training_set[-test,], Y[-test], family="binomial")
pred_oos <- predict(logit_lasso_half, training_set[test,], type="response")
Y_oos <- Y[test]

roc(pred_oos, Y_oos, bty="n", main="OOS ROC")

for (i in cutoff_points){
  points(x=1-mean((pred_CV<=i)[Y==0]), y=mean((pred_CV>i)[Y==1]), cex=1.5, pch=20, col=cutoff_colors[match(i,cutoff_points)])
}
# p = 0.2
#points(x=1-mean((pred_oos<.2)[Y_oos==0]), y=mean((pred_oos>.2)[Y_oos==1]), cex=1.5, pch=20, col='red') 
# p = 0.5
#points(x=1-mean((pred_oos<.5)[Y_oos==0]), y=mean((pred_oos>.5)[Y_oos==1]), cex=1.5, pch=20, col='blue') 
legend("bottomright",fill=cutoff_colors, legend=cutoff_points,bty="n",title="cutoffs")


#### R Squared Calculations


## IS R^2 using the lasso model
pred_lasso <- predict(cv_bankrupt, newdata=training_set)

## IS R^2 formula
IS_rsquared <- 1 - sum((training_set$Bankrupt.-pred_lasso)^2)/sum((training_set$Bankrupt.-mean(training_set$Bankrupt.))^2)
IS_rsquared


## OOS R^2 with test set
pred_lasso_test <- predict(lasso_model, newdata=test_set)

## OOS R^2 formula
OOS_rsquared <- 1 - sum((test_set$Bankrupt.-pred_lasso_test)^2)/sum((test_set$Bankrupt.-mean(training_set$Bankrupt.))^2)
OOS_rsquared

## Compare IS results with OLS
IS_rsquared


## Compare OOS results with OLS
OOS_rsquared


################################################################################################################################################

# Now we will pick our regressors to see if we can improve this model

df2 <- subset(df, select = -c(47:94))

# Create X matrix
X <- model.matrix(~ ., data=df2)[,-1]

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

# Boxplot
boxplot(pred_CV ~ Y, xlab="bankruptcy", ylab="prob of bankruptcy", col=c("pink","dodgerblue"))

# Setting seed and loading in the ROC.R
set.seed(0)
source("roc.R")

cutoff_points = c(0.05,0.1,0.2,0.4,0.8)
cutoff_colors = c("blue","red","green","yellow","purple")

# IS curve
par(mai=c(.9,.9,.2,.1)) # format margins
roc(pred_CV, Y, bty="n", main="IS ROC") # from roc.R

# For loop to make my code more concise
for (i in cutoff_points){
  points(x=1-mean((pred_CV<=i)[Y==0]), y=mean((pred_CV>i)[Y==1]), cex=1.5, pch=20, col=cutoff_colors[match(i,cutoff_points)])
}

legend("bottomright",fill=cutoff_colors, legend=c(cutoff_points),bty="n",title="cutoffs")


# refit the model using only 1/2 of data
set.seed(0)
test <- sample.int(5000,2500)
logit_lasso_half <- gamlr(X[-test,], Y[-test], family="binomial")
pred_oos <- predict(logit_lasso_half, X[test,], type="response")
Y_oos <- Y[test]

roc(pred_oos, Y_oos, bty="n", main="OOS ROC")

for (i in cutoff_points){
  points(x=1-mean((pred_CV<=i)[Y==0]), y=mean((pred_CV>i)[Y==1]), cex=1.5, pch=20, col=cutoff_colors[match(i,cutoff_points)])
}
# p = 0.2
#points(x=1-mean((pred_oos<.2)[Y_oos==0]), y=mean((pred_oos>.2)[Y_oos==1]), cex=1.5, pch=20, col='red') 
# p = 0.5
#points(x=1-mean((pred_oos<.5)[Y_oos==0]), y=mean((pred_oos>.5)[Y_oos==1]), cex=1.5, pch=20, col='blue') 
legend("bottomright",fill=cutoff_colors, legend=cutoff_points,bty="n",title="cutoffs")


