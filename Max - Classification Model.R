## ECON 124 Final Project
## Anthony, Max, Tamar
###################################################################################################
### Part 1: Setting up our initial logit model
###################################################################################################
#install.packages("ROCR")
#install.packages("caret")

# Importing the data 
rm(list = ls())
set.seed(124)
library(caret)

#source("Tamar - Descriptive Statistics.R")

df <- read.csv("cps_00010.csv")
df$unemp <- ifelse(df$EMPSTAT>=20 & df$EMPSTAT<=22,1,0)
df$college <- ifelse(df$EDUC>=111,1,0)
df <- df[df$RACE!=100 & df$RACE!=200,]
#df$unemp <- factor(df$unemp)
#unemployed <- as.data.frame(df[df$unemp==1])
#unemployed <- df[df$unemp==1,]

#college <- as.data.frame(df[df$college==1])
college <- df[df$college==1,]

df <- df[df$AGE>21,]
# Take out unnecessary variables
df <- subset(df, select = -c(SERIAL, HWTFINL, CPSID, PERNUM, WTFINL, CPSIDP, BPL, EDUC, FBPL, MBPL, OCC, EARNWEEK, HOURWAGE, PAIDHOUR, UNION))

# Check if factor
is.factor(df$SEX)
factor_columns <- colnames(df[colnames(df)!="AGE" & colnames(df)!="YRIMMIG" & colnames(df)!="UHRSWORKT" & colnames(df)!="AHRSWORKT"])
# Make the appropriate variables into factors
for(i in factor_columns) {
  df[,i] <- factor(df[,i])}

# Check if factor
is.factor(df$SEX)

summary(df$RACE)
df <- df[!(as.numeric(df$IND) %in% which(table(df$IND)<200)),]
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

# Split our data using 80/20 for the training set and test set
split = sort(sample(nrow(dec21), nrow(dec21)*.8))
train <- dec21[split,]
test <- dec21[-split,]
logmodel <- glm(college ~ STATEFIP + METRO + AGE + SEX + RACE + MARST + CITIZEN + EMPSTAT + CLASSWKR + WKSTAT + DIFFANY, data=train, family = "binomial"(link="logit"))
summary(logmodel)
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
prd<- predict(logmodel, newdata = train, type = "response")

# Flag predicted probabilities as 1/0
prd_bin<-as.factor(ifelse(prd>0.5,1,0))

# use caret and compute a confusion matrix
library(caret)
confusionMatrix(data = prd_bin, 
                reference = train$college)
#confusionMatrix(train$college, predict(logmodel, test))

# Setting seed and loading in the ROC.R
set.seed(0)
source("roc.R")

cutoff_points = c(0.02,0.1,0.33,0.4,0.5,0.6,0.8,0.9)
cutoff_colors = c("blue","red","green","yellow","purple", "darkgreen", "brown", "pink")

# IS curve
par(mai=c(.9,.9,.2,.1)) # format margins
roc(prd, Y, bty="n", main="IS ROC Logit") # from roc.R

# For loop to make my code more concise
for (i in cutoff_points){
  points(x=1-mean((prd<=i)[Y==0]), y=mean((prd>i)[Y==1]), cex=1.5, pch=20, col=cutoff_colors[match(i,cutoff_points)])
}

legend("bottomright",fill=cutoff_colors, legend=c(cutoff_points),bty="n",title="cutoffs")

### OOS Logit predictions

prd<- predict(logmodel, newdata = test, type = "response")

# Flag predicted probabilities as 1/0
prd_bin<-as.factor(ifelse(prd>0.5,1,0))

# use caret and compute a confusion matrix
library(caret)
confusionMatrix(data = prd_bin, 
                reference = test$college)
#confusionMatrix(train$college, predict(logmodel, test))

# Setting seed and loading in the ROC.R
set.seed(0)
source("roc.R")

cutoff_points = c(0.02,0.1,0.33,0.4,0.5,0.6,0.8,0.9)
cutoff_colors = c("blue","red","green","yellow","purple", "darkgreen", "brown", "pink")

# IS curve
par(mai=c(.9,.9,.2,.1)) # format margins
roc(prd, Y_test, bty="n", main="OOS ROC Logit") # from roc.R

# For loop to make my code more concise
for (i in cutoff_points){
  points(x=1-mean((prd<=i)[Y_test==0]), y=mean((prd>i)[Y_test==1]), cex=1.5, pch=20, col=cutoff_colors[match(i,cutoff_points)])
}

legend("bottomright",fill=cutoff_colors, legend=c(cutoff_points),bty="n",title="cutoffs")


###################################################################################################
### Part 2: k-fold CV model
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
hist(pred_CV, xlab = "Predicted Probabilities")

#p <- 0.2
#cat("proportion of positive classifications that are correct:", mean(Y[pred_CV>p] ), "\n")

#cat("proportion of negative classifications that are correct:", mean( (1-Y)[pred_CV<=p] ), "\n")


# Setting seed and loading in the ROC.R
set.seed(0)
source("roc.R")

cutoff_points = c(0.02,0.1,0.33,0.4,0.5,0.6,0.8,0.9)
cutoff_colors = c("blue","red","green","yellow","purple", "darkgreen", "brown", "pink")

# IS curve
par(mai=c(.9,.9,.2,.1)) # format margins
roc(pred_CV, Y, bty="n", main="IS ROC") # from roc.R

# For loop to make my code more concise
for (i in cutoff_points){
  points(x=1-mean((pred_CV<=i)[Y==0]), y=mean((pred_CV>i)[Y==1]), cex=1.5, pch=20, col=cutoff_colors[match(i,cutoff_points)])
}

legend("bottomright",fill=cutoff_colors, legend=c(cutoff_points),bty="n",title="cutoffs")

# refit the model using test data
set.seed(0)
pred_oos <- drop(predict(cross_validation, select='min', X_test, type="response"))
Y_oos <- Y_test

roc(pred_oos, Y_oos, bty="n", main="OOS ROC")

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

# Flag predicted probabilities as 1/0
prd_cv2<-as.factor(ifelse(pred_CV2>0.5,1,0))

# use caret and compute a confusion matrix
library(caret)
confusionMatrix(data = prd_cv2, 
                reference = test$college)



