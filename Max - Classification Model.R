## ECON 124 Final Project
## Anthony, Max, Tamar
###################################################################################################
### Part 1: Setting up the data
###################################################################################################
#install.packages("ROCR")
#install.packages("caret")
#install.packages("ranger")

# Administrative cleanup
# Clearing the environment, setting the seed and loading packages we will use in our project.
rm(list = ls())
if(!is.null(dev.list())) dev.off()
set.seed(124)
library(caret)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ranger)

# Read in the data
df <- read.csv("cps_00010.csv")
#df$unemp <- ifelse(df$EMPSTAT>=20 & df$EMPSTAT<=22,1,0)
df$college <- ifelse(df$EDUC>=111,1,0)

## MINORITIES
df <- df[df$RACE!=100,]

# Remove single observations (will cause problems in training set and test set split)
df <- df[df$CLASSWKR!=29,]
df <- df[df$WKSTAT!=14,]
df <- df[df$WHYABSNT!=10,]
#df$unemp <- factor(df$unemp)
#unemployed <- as.data.frame(df[df$unemp==1])
#unemployed <- df[df$unemp==1,]

#college <- as.data.frame(df[df$college==1])
college <- df[df$college==1,]

df <- df[df$AGE>21,]

#income grouping
quantile(df$FAMINC, probs = c(0.25,0.5,0.75))
df$lowerclass <- ifelse(df$FAMINC <= 730,1,0) #classifies someone as being lower class if their family income is less than or equal to $39,999
df$lowermiddle <- ifelse(df$FAMINC >= 740 & df$FAMINC < 830,1,0) #classifies someone as being lower middle class if their family income is greater than or equal
#to 40,000 and less than $60,000
df$uppermiddle <- ifelse(df$FAMINC >= 830 & df$FAMINC < 842,1,0) #classifies someone as being upper middle class if their family income is greater than or equal 
#to $60,000 and less than $100,000
df$upperclass <- ifelse(df$FAMINC >= 842 & df$FAMINC != 999,1,0) #classifies someone as being upper class if their family income is greater than or equal to
#$100,000

# BPL, FBPL, MBPL, OCC
# Take out unnecessary variables
df <- subset(df, select = -c(SERIAL, HWTFINL, CPSID, PERNUM, WTFINL, CPSIDP, EDUC, EARNWEEK, HOURWAGE, PAIDHOUR, UNION, FAMINC, BPL, MBPL, FBPL, OCC, IND))

# Check if factor
is.factor(df$SEX)
factor_columns <- colnames(df[colnames(df)!="AGE" & colnames(df)!="YRIMMIG" & colnames(df)!="college" &colnames(df)!="UHRSWORKT" & colnames(df)!="AHRSWORKT"])
# Make the appropriate variables into factors
for(i in factor_columns) {
  df[,i] <- factor(df[,i])}

# Check if factor
is.factor(df$SEX)

summary(df$RACE)
#df <- df[!(as.numeric(df$IND) %in% which(table(df$IND)<40)),]
df <- df[!(as.numeric(df$RACE) %in% which(table(df$RACE)<20)),]


college <- df[df$college==1,]
## Drop race variables with very low numbers (predict function will not work if training and test sets have different number of levels)
#df<-df[!(df$RACE=="807" | df$RACE=="808" | df$RACE=="814" | df$RACE=="815" | df$RACE=="816" | df$RACE=="817" | df$RACE=="818" | df$RACE=="820"),]
# Splitting the data into december and january
# dec21 will be the dataset we will be using. Then we will check on the jan 22 data to see if our predictions for binary classifications are accurate. (IS vs OOS)
dec21 <- df[df$YEAR==2021,]
jan22 <- df[df$YEAR==2022,]

# See if number of observations are fairly equal


# Set.seed function to replicate the random selections from our project's R code
set.seed(124) 

# Take out year and month
dec21 <- subset(dec21, select = -c(YEAR, MONTH))
jan22 <- subset(jan22, select = -c(YEAR, MONTH))

# Find NA values
sum(is.na(dec21))
mean(is.na(dec21))
#dec21 <- dec21[, sapply(dec21, nlevels) > 1]

# Split our data using 70/30 for the training set and test set
split = sort(sample(nrow(dec21), nrow(dec21)*.7))
train <- dec21[split,]
test <- dec21[-split,]

###################################################################################################
### Part 2: Initial Linear Logit Model
###################################################################################################
# Logistic regression
# STATEFIP + METRO + AGE + SEX + RACE + MARST + CITIZEN + EMPSTAT + CLASSWKR + WKSTAT + DIFFANY
logmodel <- glm(college ~ ., data=train, family = "binomial")
summary(logmodel)
logLik(logmodel)
coefs <- sort(coefficients(logmodel), decreasing = TRUE)
coefs

##################
## Evaluate our logit model IS (in sample)
#pred_logit1 <- predict(logmodel, newdata = train, type="response")
#pred_logit2 <- predict(logmodel, newdata = test, type="response")

Y <- train$college
Y_test <- test$college
X <- train[colnames(train)!="college"]
X_test <- test[colnames(test)!="college"]


# Use model developed on the training dataset
# to make predictions on the test dataset.
prd<- predict(logmodel, train, type = "response")
#hist(prd, xlab = "Predicted Probabilities IS logit")

logit_dev <- function(y, pred) {
  return( -2*sum( y*log(pred) + (1-y)*log(1-pred) ) )
}
logit_dev(Y,prd)
#####
## In-sample Linear Logit Model
####

# Setting seed and loading in the ROC.R
set.seed(124)
source("roc.R")

cutoff_points = c(0.02,0.1,0.33,0.4,0.5,0.6,0.8,0.9)
cutoff_colors = c("blue","red","green","yellow","purple", "darkgreen", "brown", "pink")

# IS curve
par(mai=c(.9,.9,.2,.1)) # format margins
roc(prd, Y, bty="n", main="IS ROC Linear Logit") # from roc.R

# For loop to make my code more concise
for (i in cutoff_points){
  points(x=1-mean((prd<=i)[Y==0]), y=mean((prd>i)[Y==1]), cex=1.5, pch=20, col=cutoff_colors[match(i,cutoff_points)])
}

legend("bottomright",fill=cutoff_colors, legend=c(cutoff_points),bty="n",title="cutoffs")



#####
## Out-of-sample Linear Logit Model
####

prd2<- predict(logmodel, test, type = "response")

# Setting seed and loading in the ROC.R
set.seed(124)
source("roc.R")

cutoff_points = c(0.02,0.1,0.33,0.4,0.5,0.6,0.8,0.9)
cutoff_colors = c("blue","red","green","yellow","purple", "darkgreen", "brown", "pink")

# OOS Logit curve
par(mai=c(.9,.9,.2,.1)) # format margins
roc(prd2, Y_test, bty="n", main="OOS ROC Linear Logit") # from roc.R

# For loop to make my code more concise
for (i in cutoff_points){
  points(x=1-mean((prd2<=i)[Y_test==0]), y=mean((prd2>i)[Y_test==1]), cex=1.5, pch=20, col=cutoff_colors[match(i,cutoff_points)])
}

legend("bottomright",fill=cutoff_colors, legend=c(cutoff_points),bty="n",title="cutoffs")

###################################################################################################
### Part 3: Non-Linear Logit Model
###################################################################################################
# Non-Linear Logistic regression
# STATEFIP + METRO + AGE + SEX + RACE + MARST + CITIZEN + EMPSTAT + CLASSWKR + WKSTAT + DIFFANY
logmodel_nonlinear <- glm(college ~ STATEFIP + (SEX + RACE + EMPSTAT)^2 +., data=train, family = "binomial")
summary(logmodel_nonlinear)
coefs <- sort(coefficients(logmodel_nonlinear), decreasing = TRUE)
coefs
##################
## Evaluate our logit model IS (in sample)
#pred_logit1 <- predict(logmodel, newdata = train, type="response")
#pred_logit2 <- predict(logmodel, newdata = test, type="response")

Y <- train$college
Y_test <- test$college
X <- train[colnames(train)!="college"]
X_test <- test[colnames(test)!="college"]


# Use model developed on the training dataset
# to make predictions on the test dataset.
prd_logit_nonlinear<- predict(logmodel_nonlinear, train, type = "response")
#hist(prd, xlab = "Predicted Probabilities IS logit")

#####
## In-sample Non-Linear Logit Model
####

# Setting seed and loading in the ROC.R
set.seed(124)
source("roc.R")

cutoff_points = c(0.02,0.1,0.33,0.4,0.5,0.6,0.8,0.9)
cutoff_colors = c("blue","red","green","yellow","purple", "darkgreen", "brown", "pink")

# IS curve
par(mai=c(.9,.9,.2,.1)) # format margins
roc(prd_logit_nonlinear, Y, bty="n", main="IS ROC Logit Non-Linear") # from roc.R

# For loop to make my code more concise
for (i in cutoff_points){
  points(x=1-mean((prd_logit_nonlinear<=i)[Y==0]), y=mean((prd_logit_nonlinear>i)[Y==1]), cex=1.5, pch=20, col=cutoff_colors[match(i,cutoff_points)])
}

legend("bottomright",fill=cutoff_colors, legend=c(cutoff_points),bty="n",title="cutoffs")



#####
## Out-of-sample Non-Linear Logit Model
####

prd_logit_nonlinear_oos<- predict(logmodel_nonlinear, newdata=test, type = "response")

# Setting seed and loading in the ROC.R
set.seed(124)
source("roc.R")

cutoff_points = c(0.02,0.1,0.33,0.4,0.5,0.6,0.8,0.9)
cutoff_colors = c("blue","red","green","yellow","purple", "darkgreen", "brown", "pink")

# OOS Logit curve
par(mai=c(.9,.9,.2,.1)) # format margins
roc(prd_logit_nonlinear_oos, Y_test, bty="n", main="OOS ROC Logit Non-Linear") # from roc.R

# For loop to make my code more concise
for (i in cutoff_points){
  points(x=1-mean((prd_logit_nonlinear_oos<=i)[Y_test==0]), y=mean((prd_logit_nonlinear_oos>i)[Y_test==1]), cex=1.5, pch=20, col=cutoff_colors[match(i,cutoff_points)])
}

legend("bottomright",fill=cutoff_colors, legend=c(cutoff_points),bty="n",title="cutoffs")

logit_dev(Y_test,prd2)
###################################################################################################
### Part 4: k-fold CV model
###################################################################################################


library(gamlr)
X <- naref(X)
X_test <- naref(X_test)

# Making the X matrix
X <- sparse.model.matrix(~ .^2, data=X)[,-1]
X_test <- sparse.model.matrix(~ .^2, data=X_test)[,-1]
dim(X)
# Estimating a logit model with lasso regularization and penalty obtained from 10-fold cross-validation
cross_validation <- cv.gamlr(X, Y, nfold = 10, family="binomial", verb=TRUE)


plot(cross_validation$gamlr) # lasso regularization path
plot(cross_validation) # plot of cross-validation error against lambda

# Number of non-zero coefficients in the optimal CV model minus the intercept
cat("number of nonzero coefficients for CV-optimal lambda:", length(which(coef(cross_validation, select="min")!=0))-1, "\n")

# Number of total coefficients of our optimal CV model
cat("number of total coefficients for CV-optimal lambda:", length(coef(cross_validation, select="min"))-1, "\n")

cv_coefs <- drop(coef(cross_validation, select="min"))

# Drop intercept
cv_coefs = cv_coefs[-1]

## The following coefficients are non-zero
non_zero_coef <- cv_coefs[which(cv_coefs!=0)]
sort(non_zero_coef, decreasing = TRUE)

# Computing the predicted probabilities
pred_CV <- drop(predict(cross_validation, select='min', X, type="response"))

# Flag predicted probabilities as 1/0
prd_cv<-as.factor(ifelse(pred_CV>0.5,1,0))

# use caret and compute a confusion matrix
library(caret)
confusionMatrix(data = prd_cv, 
                reference = train$college)
# Plotting the histogram of predicted probabilities
#hist(pred_CV, xlab = "Predicted Probabilities IS CV-Gamlr")

#p <- 0.2
#cat("proportion of positive classifications that are correct:", mean(Y[pred_CV>p] ), "\n")

#cat("proportion of negative classifications that are correct:", mean( (1-Y)[pred_CV<=p] ), "\n")


# Setting seed and loading in the ROC.R
set.seed(124)
source("roc.R")

cutoff_points = c(0.02,0.1,0.33,0.4,0.5,0.6,0.8,0.9)
cutoff_colors = c("blue","red","green","yellow","purple", "darkgreen", "brown", "pink")

# IS curve
par(mai=c(.9,.9,.2,.1)) # format margins
roc(pred_CV, Y, bty="n", main="IS ROC CV") # from roc.R

# For loop to make my code more concise
for (i in cutoff_points){
  points(x=1-mean((pred_CV<=i)[Y==0]), y=mean((pred_CV>i)[Y==1]), cex=1.5, pch=20, col=cutoff_colors[match(i,cutoff_points)])
}

legend("bottomright",fill=cutoff_colors, legend=c(cutoff_points),bty="n",title="cutoffs")

# refit the model using test data
set.seed(124)
pred_oos <- drop(predict(cross_validation, select='min', X_test, type="response"))
Y_oos <- Y_test

roc(pred_oos, Y_oos, bty="n", main="OOS ROC CV")

for (i in cutoff_points){
  points(x=1-mean((pred_oos<=i)[Y_oos==0]), y=mean((pred_oos>i)[Y_oos==1]), cex=1.5, pch=20, col=cutoff_colors[match(i,cutoff_points)])
}
# p = 0.2
#points(x=1-mean((pred_oos<.2)[Y_oos==0]), y=mean((pred_oos>.2)[Y_oos==1]), cex=1.5, pch=20, col='red') 
# p = 0.5
#points(x=1-mean((pred_oos<.5)[Y_oos==0]), y=mean((pred_oos>.5)[Y_oos==1]), cex=1.5, pch=20, col='blue') 
legend("bottomright",fill=cutoff_colors, legend=cutoff_points,bty="n",title="cutoffs")


# Computing the predicted probabilities
pred_CV2 <- drop(predict(cross_validation, select='min', X_test, type="response"))

logit_dev(Y, pred_CV2)
logit_dev(Y, pred_CV)
logit_dev(Y_test, pred_oos)
# Flag predicted probabilities as 1/0
prd_cv2<-as.factor(ifelse(pred_CV2>0.5,1,0))

# use caret and compute a confusion matrix
library(caret)
confusionMatrix(data = prd_cv2, 
                reference = test$college)

#########
## Evaluate using january data
#########


# Make sure there is the same number of X's so we will randomly drop some observations from jan22 data frame


# refit the model using test data
set.seed(124)
Y_jan <- jan22$college
X_jan <- jan22[colnames(test)!="college"]
X_jan <- sparse.model.matrix(~ .^2, data=X_jan)[,-1]
sample_split <- sample.int(nrow(jan22),nrow(train))
pred_oos <- drop(predict(cross_validation, select='min', X_jan[sample_split,], type="response"))
Y_oos <- Y_jan

roc(pred_oos, Y_oos, bty="n", main="OOS ROC January 2022 Data")

for (i in cutoff_points){
  points(x=1-mean((pred_oos<=i)[Y_oos==0]), y=mean((pred_oos>i)[Y_oos==1]), cex=1.5, pch=20, col=cutoff_colors[match(i,cutoff_points)])
}
# p = 0.2
#points(x=1-mean((pred_oos<.2)[Y_oos==0]), y=mean((pred_oos>.2)[Y_oos==1]), cex=1.5, pch=20, col='red') 
# p = 0.5
#points(x=1-mean((pred_oos<.5)[Y_oos==0]), y=mean((pred_oos>.5)[Y_oos==1]), cex=1.5, pch=20, col='blue') 
legend("bottomright",fill=cutoff_colors, legend=cutoff_points,bty="n",title="cutoffs")


# Computing the predicted probabilities
pred_CV3 <- drop(predict(cross_validation, select='min', X_jan, type="response"))

# Flag predicted probabilities as 1/0
prd_cv3<-as.factor(ifelse(pred_CV3>0.5,1,0))

# use caret and compute a confusion matrix
library(caret)
confusionMatrix(data = prd_cv3, 
                reference = jan22$college)

###################################################################################################
### Part 4: Random Forests model
###################################################################################################
#install.packages("randomForest")
library(pROC)
library(randomForest)
#library("ranger")
set.seed(124)
Y_randomforest <- factor(train$college)
X_randomforest <- train[colnames(train)!="college"]
#train <- subset(train, select = -c(college))
classifier_RF = randomForest(x = X_randomforest,
                             y = Y_randomforest,
                             ntree = 100)

classifier_RF

# Predicting the Test set results
y_pred = predict(classifier_RF, newdata = test)

# Confusion Matrix
confusion_mtx = table(test[, 5], y_pred)
confusion_mtx

# Plotting model
plot(classifier_RF)

# Importance plot
importance(classifier_RF)

# Variable importance plot
varImpPlot(classifier_RF)


predictionsman <- predict(classifier_RF, train, type = "prob")
prd_randomforest <- predictionsman[,2]
logit_dev(Y,prd_randomforest)
p <- as.numeric(prd_randomforest)
rf_prediction <- predict(classifier_RF, test, type = "prob")
rf_pred <- rf_prediction[,2]
ROC_rf <- roc(test$college, rf_prediction[,2])
ROC_lr <- roc(test$college, prd2)
ROC_rf_auc <- auc(ROC_rf)
plot(ROC_rf, col = "green", main = "ROC For Random Forest (GREEN)")
lines(ROC_lr, col = "red")

random_forest <- ranger(college ~ ., data = train, write.forest = TRUE, num.tree = 400, min.node.size = 1, importance = "impurity", probability = TRUE, classification = TRUE)

probabilities_rf = predict(random_forest, data = train, type = "response")$predictions
logit_dev(Y, probabilities_rf)
logit_dev(Y, prd)
hist(probabilities_rf)

pred_rf_oos <- predict(random_forest, data = test, type = "response")$predictions
logit_dev(Y_test, pred_rf_oos)
barplot(sort(importance(random_forest), decreasing = TRUE), las = 2)
plot(random_forest)
rf <- ranger(college ~)
#####
## In-sample Non-Linear Logit Model
####

# Setting seed and loading in the ROC.R
set.seed(124)
source("roc.R")

cutoff_points = c(0.02,0.1,0.33,0.4,0.5,0.6,0.8,0.9)
cutoff_colors = c("blue","red","green","yellow","purple", "darkgreen", "brown", "pink")

# IS curve
par(mai=c(.9,.9,.2,.1)) # format margins
roc(rf_pred, Y_test, bty="n", main="IS ROC Random Forest") # from roc.R

# For loop to make my code more concise
for (i in cutoff_points){
  points(x=1-mean((rf_pred<=i)[Y_test==0]), y=mean((rf_pred>i)[Y_test==1]), cex=1.5, pch=20, col=cutoff_colors[match(i,cutoff_points)])
}

legend("bottomright",fill=cutoff_colors, legend=c(cutoff_points),bty="n",title="cutoffs")


########
## No spec model
#######
no_specifications_random_forest <- ranger(college ~ ., data = train, write.forest = TRUE, importance = "impurity", probability = TRUE)
no_specifications_probabilities_rf = predict(no_specifications_random_forest, data = train)$predictions


