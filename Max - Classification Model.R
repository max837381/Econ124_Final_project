## ECON 124 Final Project
## Anthony, Max, Tamar

# Tamar - descriptive statistics and tables
#do it for education, race, and gender
#regression and then scatter plot
rm(list = ls())
library(dplyr)
library(tidyr)
library(ggplot2)

df <- read.csv("cps_00010.csv") #loads the dataset
df <- df[df$AGE >= 22,] #restricts dataset to ages where one typically gets a bachelor's degree
df$college <- ifelse(df$EDUC>=111,1,0) #creates a dummy variable =1 if person has a bachelor's degree (or higher)

#Race Descriptive Statistics
#groupings for race
df$asian_pi <- ifelse(df$RACE >= 650 & df$RACE <= 652,1,0)
df$mixed_ancestry <- ifelse(df$RACE >= 801 & df$RACE<= 999,1,0)

#group variable for race
df$race_group <- 0
df$race_group[df$RACE==100] <- 1 #=1 if person is white
df$race_group[df$RACE==200] <- 2 #=2 if person is Black
df$race_group[df$RACE==300] <- 3 #=3 if person is Native American
df$race_group[df$asian_pi==1] <- 4 #=4 if person is Asian or Pacific Islander
df$race_group[df$mixed_ancestry==1] <- 5 #=5 if person has mixed ancestry of 2 or more races

df$total_white_college <- ifelse(df$race_group==1 & df$college==1,1,0) #=1 if person is white and completed college

tabulate(as.numeric(df$total_white_college)) #calculates the total number of white people in college

df <- df[df$race_group != 1,] #drops white people from the dataset

race_totals <- df %>%
  group_by(race_group) %>%
  summarise(totals=sum(tabulate(race_group))) #data with the total number of each race group
race_college_totals <- df %>%
  group_by(race_group, college) %>%
  summarise(totals=sum(tabulate(race_group)))

ggplot(race_college_totals, aes(y=totals, x=race_group, fill=college)) +
  geom_bar(position = "dodge", stat = "identity") +
  ggtitle("Total Number of Individuals Who \n Completed College by Race") +
  xlab("Race Group") +
  ylab("Total Number of Individuals") #plots the total number of people who completed college by race

## MAX test
race_college_totals2 <- filter(race_college_totals, college == 1)
race_college_barplot_df <- cbind(race_totals, race_college_totals2)
race_college_barplot_df$college_totals <- race_college_barplot_df[,5]
race_college_barplot_df <- subset(race_college_barplot_df, select = -c(3,4,5))
race_college_barplot_df$race_group[race_college_barplot_df$race_group == 2] <- "Black"
race_college_barplot_df$race_group[race_college_barplot_df$race_group == 3] <- "Native-American"
race_college_barplot_df$race_group[race_college_barplot_df$race_group == 4] <- "Asian/Pacific Islander"
race_college_barplot_df$race_group[race_college_barplot_df$race_group == 5] <- "Mixed"

ggplot(data= race_college_barplot_df, aes(x=race_group)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_bar(aes(y=totals), stat="identity", position ="identity", alpha=1, fill='lightblue') +
  geom_bar(aes(y=college_totals), stat="identity", position="identity", alpha=1, fill='palegreen3') +
  ggtitle("Total Number of Individuals Who Completed College by Race") +
  xlab("Race Group") +
  ylab("Total Number of Individuals") #plots the total number of people who completed college by race

#Gender Descriptive Statistics
gender_totals <- df %>%
  group_by(SEX) %>%
  summarise(totals=sum(tabulate(SEX))) #data with the total number of each gender
gender_college_totals <- df %>%
  group_by(SEX, college) %>%
  summarise(totals=sum(tabulate(SEX))) #data with the total number that completed 
#college of each gender
ggplot(gender_college_totals, aes(y=totals, x=SEX, fill=college)) +
  geom_bar(position = "dodge", stat = "identity") +
  ggtitle("Total Number of Individuals Who \n Completed College by Gender") +
  xlab("Gender") +
  ylab("Total Number of Individuals") #plots the total number of people who completed college by income group

#Family Income Descriptive Statistics
#income grouping
quantile(df$FAMINC, probs = c(0.25,0.5,0.75)) #shows the 25th, 50th, and 75th percentiles for family income
df$lowerclass <- ifelse(df$FAMINC <= 720,1,0) #classifies someone as being lower class if their family income is less than or equal to $34,999
df$lowermiddle <- ifelse(df$FAMINC >= 730 & df$FAMINC < 830,1,0) #classifies someone as being lower middle class if their family income is greater than or equal
#to 35,000 and less than $60,000
df$uppermiddle <- ifelse(df$FAMINC >= 830 & df$FAMINC < 842,1,0) #classifies someone as being upper middle class if their family income is greater than or equal 
#to $60,000 and less than $100,000
df$upperclass <- ifelse(df$FAMINC >= 842 & df$FAMINC != 999,1,0) #classifies someone as being upper class if their family income is greater than or equal to
#$100,000

#variable for income group
df$income_group <- 0
df$income_group[df$lowerclass==1] <- 1 #=1 if person is in the lower class
df$income_group[df$lowermiddle==1] <- 2 #=2 if person is in the lower middle class
df$income_group[df$uppermiddle==1] <- 3 #=3 if person is in the upper middle class
df$income_group[df$upperclass==1] <- 4 #=4 if person is in the upper class

income_totals <- df %>%
  group_by(income_group) %>%
  summarise(totals=sum(tabulate(income_group))) #data with the total number of each income group
income_college_totals <- df %>%
  group_by(income_group, college) %>%
  summarise(totals=sum(tabulate(income_group)))

ggplot(income_college_totals, aes(y=totals, x=income_group, fill=college)) +
  geom_bar(position = "dodge", stat = "identity") +
  ggtitle("Total Number of Individuals Who \n Completed College by Income Group") +
  xlab("Income Group") +
  ylab("Total Number of Individuals") #plots the total number of people who completed college by income group

###################################################################################################
### Part 1: Setting up the data
###################################################################################################

# These packages will be used. Commented out since you may some of these already!

#install.packages("randomForest")
#install.packages("dplyr")
#install.packages("tidyr")
#install.packages("ggplot2")
#install.packages("gamlr")

# Administrative cleanup
# Clearing the environment, setting the seed and loading packages we will use in our project.
rm(list = ls())
set.seed(124)
library(dplyr)
library(tidyr)
library(ggplot2)
library(randomForest)
library(gamlr)

# Read in the data from CPS
df <- read.csv("cps_00010.csv")

# Removing white people from our sample, only focusing on POCs.
df <- df[df$RACE!=100,]

# Coding our Y variable - whether someone completed college or not (the minimum required is a Bachelor's Degree)
df$college <- ifelse(df$EDUC>=111,1,0)

# Remove single observations (will cause problems in training set and test set split)
df <- df[df$CLASSWKR!=29,]
df <- df[df$WKSTAT!=14,]
df <- df[df$WHYABSNT!=10 & df$WHYABSNT!=7,]

# Limiting our sample to people of age to have generally completed a Bachelor's degree by removing anyone under 22 years old
df <- df[df$AGE>21,]

#income grouping from Tamar's file
quantile(df$FAMINC, probs = c(0.25,0.5,0.75))
df$lowerclass <- ifelse(df$FAMINC <= 730,1,0) #classifies someone as being lower class if their family income is less than or equal to $39,999
df$lowermiddle <- ifelse(df$FAMINC >= 740 & df$FAMINC < 830,1,0) #classifies someone as being lower middle class if their family income is greater than or equal
#to 40,000 and less than $60,000
df$uppermiddle <- ifelse(df$FAMINC >= 830 & df$FAMINC < 842,1,0) #classifies someone as being upper middle class if their family income is greater than or equal 
#to $60,000 and less than $100,000
df$upperclass <- ifelse(df$FAMINC >= 842 & df$FAMINC != 999,1,0) #classifies someone as being upper class if their family income is greater than or equal to
#$100,000


# Take out unnecessary variables such as linking variables from CPS
df <- subset(df, select = -c(SERIAL, HWTFINL, CPSID, PERNUM, WTFINL, CPSIDP, EDUC, EARNWEEK, HOURWAGE, PAIDHOUR, UNION, FAMINC, BPL, MBPL, FBPL, OCC, IND))

# Check if factor
is.factor(df$SEX)

# Make the appropriate variables into factors using a for loop
factor_columns <- colnames(df[colnames(df)!="AGE" & colnames(df)!="YRIMMIG" & colnames(df)!="college" &colnames(df)!="UHRSWORKT" & colnames(df)!="AHRSWORKT"])
for(i in factor_columns) {
  df[,i] <- factor(df[,i])}

# Check again if factor
is.factor(df$SEX)

# Make sure that we dont have factors in the RACE variable with small number of observations that may cause issues during our splitting into training and test set
summary(df$RACE)

# Take out any factor with fewer than 10 observations for the RACE variable
df <- df[!(as.numeric(df$RACE) %in% which(table(df$RACE)<10)),]


# dec21 will be the dataset we will be using from the December 2021 CPS data.
dec21 <- df[df$YEAR==2021,]

# Take out year and month since we don't need them
dec21 <- subset(dec21, select = -c(YEAR, MONTH))

# Find NA values since those will cause issues for our models
sum(is.na(dec21))

# Split our data using 70/30 for the training set and test set using the December 2021 data
split = sort(sample(nrow(dec21), nrow(dec21)*.7))
train <- dec21[split,]
test <- dec21[-split,]

# We have completed our initial setup and we are ready to run our first model

###################################################################################################
### Part 2: Initial Logistic regression model
###################################################################################################
# Logistic regression where we are regressing having a Bachelor's degree or higher on the covariates in our dataset
logmodel <- glm(college ~ ., data=train, family = "binomial")
summary(logmodel)

# Checking the coefficients of our logistic regression model
coefs <- sort(coefficients(logmodel), decreasing = TRUE)
coefs


# Prepare our Y and X 
Y <- train$college
X <- train[colnames(train)!="college"]

# for test set too
Y_test <- test$college
X_test <- test[colnames(test)!="college"]


# Use model developed on the training dataset to compute the predicted probabilities
# based on the initial logistic regression model
prd<- predict(logmodel, train, type = "response")

#####
## In-sample Logit Model ROC Curve
####
par(mfrow=c(1,2))
# Setting seed and loading in the ROC.R
set.seed(124)
source("roc.R")

cutoff_points = c(0.1,0.2,0.3,0.4,0.5,0.6,0.8,0.9)
cutoff_colors = c("blue","red","green","yellow","purple", "darkgreen", "brown", "pink")

# IS curve
par(mai=c(.9,.9,.2,.1)) # format margins
roc(prd, Y, bty="n", main="In-sample ROC Initial Logit Model") # from roc.R

# For loop to make my code more concise
for (i in cutoff_points){
  points(x=1-mean((prd<=i)[Y==0]), y=mean((prd>i)[Y==1]), cex=1.5, pch=20, col=cutoff_colors[match(i,cutoff_points)])
}

legend("bottomright",fill=cutoff_colors, legend=c(cutoff_points),bty="n",title="cutoffs")



#####
## Out-of-sample Logit Model ROC Curve
####

# Computing predicted probabilities for out-of-sample data using the test set
prd_oos<- predict(logmodel, test, type = "response")

# Setting seed and loading in the ROC.R
set.seed(124)
source("roc.R")

cutoff_points = c(0.1,0.2,0.3,0.4,0.5,0.6,0.8,0.9)
cutoff_colors = c("blue","red","green","yellow","purple", "darkgreen", "brown", "pink")

# OOS Logit curve
par(mai=c(.9,.9,.2,.1)) # format margins
roc(prd_oos, Y_test, bty="n", main="Out-of-sample ROC Initial Logit Model") # from roc.R

# For loop to make my code more concise
for (i in cutoff_points){
  points(x=1-mean((prd_oos<=i)[Y_test==0]), y=mean((prd_oos>i)[Y_test==1]), cex=1.5, pch=20, col=cutoff_colors[match(i,cutoff_points)])
}

legend("bottomright",fill=cutoff_colors, legend=c(cutoff_points),bty="n",title="cutoffs")

###################################################################################################
### Part 3: Logit Model with interaction terms
###################################################################################################
# Logistic regression with some interaction terms (could not interact everything since the glm command would crash R)
# IMPORTANT: Even with some interaction terms might take 1-2 minutes to run
logmodel_interactions <- glm(college ~ STATEFIP + (SEX + RACE + EMPSTAT + AGE)^2 +., data=train, family = "binomial")
summary(logmodel_interactions)

# Checking the coefficients of our logistic regression model with interactions
coefs <- sort(coefficients(logmodel_interactions), decreasing = TRUE)
coefs

# Use model developed on the training dataset to compute the predicted probabilities
# based on the logistic regression model with interaction terms
prd_logit_interactions <- predict(logmodel_interactions, train, type = "response")


#####
## In-sample Logit Interaction terms Model
####

# Setting seed and loading in the ROC.R
set.seed(124)
source("roc.R")

cutoff_points = c(0.1,0.2,0.3,0.4,0.5,0.6,0.8,0.9)
cutoff_colors = c("blue","red","green","yellow","purple", "darkgreen", "brown", "pink")

# IS curve
par(mai=c(.9,.9,.2,.1)) # format margins
roc(prd_logit_interactions, Y, bty="n", main="In-sample ROC Logit Interactions Model") # from roc.R

# For loop to make my code more concise
for (i in cutoff_points){
  points(x=1-mean((prd_logit_interactions<=i)[Y==0]), y=mean((prd_logit_interactions>i)[Y==1]), cex=1.5, pch=20, col=cutoff_colors[match(i,cutoff_points)])
}

legend("bottomright",fill=cutoff_colors, legend=c(cutoff_points),bty="n",title="cutoffs")



# We can see that the interaction model performed slightly better, having a lower binomial deviance. However we are more interested in 
# out-of-sample performance.

#####
## Out-of-sample Logit Interactions Model
####

prd_logit_interactions_oos<- predict(logmodel_interactions, newdata=test, type = "response")

# Setting seed and loading in the ROC.R
set.seed(124)
source("roc.R")

cutoff_points = c(0.1,0.2,0.3,0.4,0.5,0.6,0.8,0.9)
cutoff_colors = c("blue","red","green","yellow","purple", "darkgreen", "brown", "pink")

# OOS Logit curve
par(mai=c(.9,.9,.2,.1)) # format margins
roc(prd_logit_interactions_oos, Y_test, bty="n", main="Out-of-sample ROC Logit Interactions model") # from roc.R

# For loop to make my code more concise
for (i in cutoff_points){
  points(x=1-mean((prd_logit_interactions_oos<=i)[Y_test==0]), y=mean((prd_logit_interactions_oos>i)[Y_test==1]), cex=1.5, pch=20, col=cutoff_colors[match(i,cutoff_points)])
}

legend("bottomright",fill=cutoff_colors, legend=c(cutoff_points),bty="n",title="cutoffs")


###################################################################################################
### Part 4: k-fold Cross-Validation model
###################################################################################################

# We will use the gamlr library to run our k-fold CV model using a LASSO regression

# use naref() to ensure that R does not set a reference level for any of the categorical variables.
X <- naref(X)
X_test <- naref(X_test)

# Making the X matrix with interactions since cv.gamlr will use regularization to pick the most 'important' regressors
X <- sparse.model.matrix(~ .^2, data=X)[,-1]
X_test <- sparse.model.matrix(~ .^2, data=X_test)[,-1]

# Checking the dimensions of our matrices
dim(X)
dim(X_test)


# Estimating a logit model with lasso regularization and penalty obtained from 10-fold cross-validation
cross_validation <- cv.gamlr(X, Y, nfold = 10, family="binomial", verb=TRUE)

par(mfrow=c(1,1))
# lasso regularization path
plot(cross_validation$gamlr)

par(mfrow=c(1,1))
# plot of cross-validation error against lambda
plot(cross_validation)

# Number of non-zero coefficients in the optimal CV model minus the intercept
cat("number of nonzero coefficients for CV-optimal lambda:", length(which(coef(cross_validation, select="min")!=0))-1, "\n")

# Number of total coefficients of our optimal CV model
cat("number of total coefficients for CV-optimal lambda:", length(coef(cross_validation, select="min"))-1, "\n")

# Percentage of non-zero coefficients
percentage_coef <- (length(which(coef(cross_validation, select="min")!=0))-1)/(length(coef(cross_validation, select="min"))-1)
percentage_coef <- percentage_coef*100

# Optimal CV model has approximately 3% non-zero coefficients compared to the total number of coefficients
percentage_coef

# Creating an object with coefficients from the optimal model
cv_coefs <- drop(coef(cross_validation, select="min"))

# Drop intercept
cv_coefs = cv_coefs[-1]

## The following coefficients are non-zero
non_zero_coef <- cv_coefs[which(cv_coefs!=0)]
sort(non_zero_coef, decreasing = TRUE)

# Computing the predicted probabilities
pred_CV <- drop(predict(cross_validation, select='min', X, type="response"))

#####
## In-sample Cross-Validation Model
####
par(mfrow=c(1,2))
# Setting seed and loading in the ROC.R
set.seed(124)
source("roc.R")

cutoff_points = c(0.1,0.2,0.3,0.4,0.5,0.6,0.8,0.9)
cutoff_colors = c("blue","red","green","yellow","purple", "darkgreen", "brown", "pink")

# IS curve
par(mai=c(.9,.9,.2,.1)) # format margins
roc(pred_CV, Y, bty="n", main="In-sample ROC Cross-Validation Model") # from roc.R

# For loop to make my code more concise
for (i in cutoff_points){
  points(x=1-mean((pred_CV<=i)[Y==0]), y=mean((pred_CV>i)[Y==1]), cex=1.5, pch=20, col=cutoff_colors[match(i,cutoff_points)])
}

legend("bottomright",fill=cutoff_colors, legend=c(cutoff_points),bty="n",title="cutoffs")

#####
## Out-of-sample Cross-Validation Model
####

set.seed(124)
pred_CV_oos <- drop(predict(cross_validation, select='min', X_test, type="response"))


roc(pred_CV_oos, Y_test, bty="n", main="Out-of-sample ROC Cross-Validation Model")

for (i in cutoff_points){
  points(x=1-mean((pred_CV_oos<=i)[Y_test==0]), y=mean((pred_CV_oos>i)[Y_test==1]), cex=1.5, pch=20, col=cutoff_colors[match(i,cutoff_points)])
}

legend("bottomright",fill=cutoff_colors, legend=cutoff_points,bty="n",title="cutoffs")

###################################################################################################
### Part 4: Random Forests model
###################################################################################################
set.seed(124)

# Convert Y variable to factor to use in RF model
Y_randomforest <- factor(train$college)
X_randomforest <- as.matrix(train[colnames(train)!="college"])
Y_randomforest_test <- factor(test$college)
X_randomforest_test <- as.matrix(test[colnames(test)!="college"])


# Random Forest model with 300 trees
Random_Forests_model = randomForest(x = X_randomforest,
                             y = Y_randomforest,
                             ntree = 300)

# Random Forests Model plots. Error rates against number of trees and Variable Importance of each variable in the RF model.
par(mfrow=c(1,2))
plot(Random_Forests_model, main = "Random Forests Model")
varImpPlot(Random_Forests_model, main = "Random Forests Model")


#####
## In-sample Random Forests Model
####

# Predicting the in-sample predicted probabilities
rf_pred_train <- predict(Random_Forests_model, newdata = X_randomforest, type = "prob")
rf_pred_train <- rf_pred_train[,2]

# Random forest model will have some 0 and 1 predicted probablities which will crash our logit_dev function
rf_pred_train[rf_pred_train==0] <- 0.00000000001
rf_pred_train[rf_pred_train==1] <- 0.000000000099


# Setting seed and loading in the ROC.R
set.seed(124)
source("roc.R")

cutoff_points = c(0.1,0.2,0.3,0.4,0.5,0.6,0.8,0.9)
cutoff_colors = c("blue","red","green","yellow","purple", "darkgreen", "brown", "pink")

# IS curve
par(mai=c(.9,.9,.2,.1)) # format margins
roc(rf_pred_train, Y, bty="n", main="In-sample ROC Random Forests") # from roc.R

# For loop to make my code more concise
for (i in cutoff_points){
  points(x=1-mean((rf_pred_train<=i)[Y==0]), y=mean((rf_pred_train>i)[Y==1]), cex=1.5, pch=20, col=cutoff_colors[match(i,cutoff_points)])
}

legend("bottomright",fill=cutoff_colors, legend=c(cutoff_points),bty="n",title="cutoffs")

#####
## Out-of-sample Random Forests model
####

# Predicting the out-of-sample predicted probabilities
rf_pred_test <- predict(Random_Forests_model, newdata = X_randomforest_test, type = "prob")
rf_pred_test <- rf_pred_test[,2]

# Random forest model will have some 0 and 1 predicted probablities which will crash our logit_dev function
rf_pred_test[rf_pred_test==0] <- 0.00000000001
rf_pred_test[rf_pred_test==1] <- 0.000000000099

# Setting seed and loading in the ROC.R
set.seed(124)
source("roc.R")

cutoff_points = c(0.1,0.2,0.3,0.4,0.5,0.6,0.8,0.9)
cutoff_colors = c("blue","red","green","yellow","purple", "darkgreen", "brown", "pink")

# OOS curve
par(mai=c(.9,.9,.2,.1)) # format margins
roc(rf_pred_test, Y_test, bty="n", main="Out-of-sample ROC Random Forests") # from roc.R

# For loop to make my code more concise
for (i in cutoff_points){
  points(x=1-mean((rf_pred_test<=i)[Y_test==0]), y=mean((rf_pred_test>i)[Y_test==1]), cex=1.5, pch=20, col=cutoff_colors[match(i,cutoff_points)])
}

legend("bottomright",fill=cutoff_colors, legend=c(cutoff_points),bty="n",title="cutoffs")

###################################################################################################
### Part 5: Comparing all our models
###################################################################################################


#####
# In-sample comparison ROC Curves
####

# Setting seed and loading in the ROC.R
set.seed(124)
source("rocCOL.R")

cutoff_points = c("Logit","Logit Interactions","Cross-Validation","Random Forests")
cutoff_colors = c("1","2","3","4")

# IS curve
par(mai=c(.9,.9,.2,.1)) # format margins
roc(prd, Y, prd_logit_interactions, Y, pred_CV, Y, rf_pred_train, Y, bty="n", main="In-Sample Model Comparison") # from roc.R

legend("bottomright",fill=cutoff_colors, legend=c(cutoff_points),bty="n",title="Model Reference")


#####
# Out-of-sample comparison ROC curves
####
# Setting seed and loading in the ROC.R
set.seed(124)
source("rocCOL.R")

cutoff_points = c("Logit","Logit Interactions","Cross-Validation","Random Forests")
cutoff_colors = c("1","2","3","4")

# OOs curve
par(mai=c(.9,.9,.2,.1)) # format margins
roc(prd_oos, Y_test, prd_logit_interactions_oos, Y_test, pred_CV_oos, Y_test, rf_pred_test, Y_test, bty="n", main="Out-of-Sample Model Comparison") # from roc.R

legend("bottomright",fill=cutoff_colors, legend=c(cutoff_points),bty="n",title="Model Reference")

#########
# In-sample histograms
#########
# Plotting the histogram of predicted probabilities
par(mfrow=c(2,2))
hist(prd, main ="Logit Model", xlab = "In-sample Predicted Probabilities",col='orangered2')
hist(prd_logit_interactions, main ="Logit Interactions Model", xlab = "In-sample Predicted Probabilities", col='lightblue')
hist(pred_CV, main ="Cross-Validation", xlab = "In-sample Predicted Probabilities", col='palegreen3')
hist(rf_pred_train, main ="Random Forests", xlab = "In-sample Predicted Probabilities", col='thistle2')

#########
# Out-of-sample histograms
#########
# Plotting the histogram of predicted probabilities
par(mfrow=c(2,2))
hist(prd_oos, main ="Logit Model", xlab = "Out-of-sample Predicted Probabilities",col='orangered2')
hist(prd_logit_interactions_oos, main ="Logit Interactions Model", xlab = "Out-of-sample Predicted Probabilities", col='lightblue')
hist(pred_CV_oos, main ="Cross-Validation", xlab = "Out-of-sample Predicted Probabilities", col='palegreen3')
hist(rf_pred_test, main ="Random Forests", xlab = "Out-of-sample Predicted Probabilities", col='thistle2')


######
# In-sample Binomial Deviance
#####

n_train <- nrow(train)
n_test <- nrow(test)
# function to compute binomial deviance
logit_dev <- function(y, pred) {
  return( -2*sum( y*log(pred) + (1-y)*log(1-pred) ) )
}

# Computing binomial deviance for in-sample with all models
bin_dev_logit <- logit_dev(Y,prd)
cat("The in-sample binomial deviance for our logit model is", bin_dev_logit)
cat("The average in-sample binomial deviance for our logit model is", bin_dev_logit/n_train)

bin_dev_logit_interaction <- logit_dev(Y,prd_logit_interactions)
cat("The in-sample binomial deviance for our logit model with interactions is", bin_dev_logit_interaction)
cat("The average in-sample binomial deviance for our logit model with interactions is", bin_dev_logit_interaction/n_train)

bin_dev_cv <- logit_dev(Y, pred_CV)
cat("The in-sample binomial deviance for our Cross-Validation model is", bin_dev_cv)
cat("The average in-sample binomial deviance for our Cross-Validation model is", bin_dev_cv/n_train)

bin_dev_rf <- logit_dev(Y, rf_pred_train)
cat("The in-sample binomial deviance for our Random Forests model is", bin_dev_rf)
cat("The average in-sample binomial deviance for our Random Forests model is", bin_dev_rf/n_train)

#####
# Out-of-sample Binomial Deviance
#####

# Computing binomial deviance for out-of-sample with all models
bin_dev_logit_oos <- logit_dev(Y_test, prd_oos)
cat("The out-of-sample binomial deviance for our logit model is", bin_dev_logit_oos)
cat("The average out-of-sample binomial deviance for our logit model is", bin_dev_logit_oos/n_test)

bin_dev_logit_interaction_oos <- logit_dev(Y_test,prd_logit_interactions_oos)
cat("The out-of-sample binomial deviance for our logit model with interactions is", bin_dev_logit_interaction_oos)
cat("The average out-of-sample binomial deviance for our logit model with interactions is", bin_dev_logit_interaction_oos/n_test)

bin_dev_cv_oos <- logit_dev(Y_test, pred_CV_oos)
cat("The out-of-sample binomial deviance for our Cross-Validation model is", bin_dev_cv_oos)
cat("The average out-of-sample binomial deviance for our Cross-Validation model is", bin_dev_cv_oos/n_test)

bin_dev_rf_oos <- logit_dev(Y_test, rf_pred_test)
cat("The out-of-sample binomial deviance for our Random Forests model is", bin_dev_rf_oos)
cat("The average out-of-sample binomial deviance for our Random Forests model is", bin_dev_rf_oos/n_test)



