rm(list = ls())
set.seed(0)


df <- read.csv("cps_00007.csv")
# remove unnecessary variables
df <- subset(df, select = -c(2,4,5,6,7,8))
summary(jan22$YEAR)

dec21 <- df[df$YEAR==2021,]
jan22 <- df[df$YEAR==2022,]

# dec21 will be the dataset we will be using. Then we will check on the jan 22 data to see if our predictions for binary classifications are accurate. (IS vs OOS)

head(dec21)
dim(dec21)


dec21$unemp <- ifelse(dec21$EMPSTAT>=20 & dec21$EMPSTAT<=22,1,0) #creates a dummy variable =1 if person is unemployed
df <- as.data.frame(dec21)
#install.packages("caret")
library(caret)

###################################################################################################
### Part 1: Training the model
###################################################################################################

# Importing the data from Tamar's R file
rm(list = ls())
#source("Tamar - Descriptive Statistics.R")

df <- read.csv("cps_00007.csv")
#df$unemp <- ifelse(df$EMPSTAT>=20 & df$EMPSTAT<=22,1,0)
df$college <- ifelse(df$EDUC>=111,1,0)

df$unemp <- factor(df$unemp)
unemployed <- as.data.frame(df[df$unemp==1])
unemployed <- df[df$unemp==1,]

college <- as.data.frame(df[df$college==1])
college <- df[df$college==1,]
# Take out unnecessary variables
df <- subset(df, select = -c(SERIAL, HWTFINL, CPSID, PERNUM, WTFINL, CPSIDP, VETSTAT, BPL, COVIDLOOK, COVIDTELEW, COVIDUNAW, COVIDPAID, DIFFANY, EDUC))

is.factor(df$SEX)
factor_columns <- colnames(df[colnames(df)!="AGE"])
# Make the appropriate variables into factors
for(i in factor_columns) {
  df[,i] <- factor(df[,i])}

is.factor(df$SEX)

summary(df$RACE)

## Drop race variables with very low numbers (predict function will not work if training and test sets have different number of levels)
df<-df[!(df$RACE=="807" | df$RACE=="808" | df$RACE=="814" | df$RACE=="815" | df$RACE=="816" | df$RACE=="817" | df$RACE=="818" | df$RACE=="820"),]
# Splitting the data into december and january
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

# Run a logistic regression using training set
logmodel <- glm(college ~ ., data=train, family = "binomial")
summary(logmodel)
##################



## Evaluate our logit model
pred_logit1 <- drop(predict(logmodel, newdata = test, type="response"))

# Plotting the histogram of predicted probabilities
hist(pred_logit1, xlab = "Predicted Probabilities")

Y <- train$college
X <- train[colnames(train)!="college"]

library(gamlr)


# Making the X matrix
X <- sparse.model.matrix(~ ., data=X)[,-1]
dim(X)
# Estimating a logit model with lasso regularization and penalty obtained from 10-fold cross-validation
cross_validation <- cv.gamlr(X, Y, nfold = 10, family="binomial", verb=TRUE)


plot(cross_validation$gamlr) # lasso regularization path
plot(cross_validation) # plot of cross-validation error against lambda

# Number of non-zero coefficients in the optimal CV model minus the intercept
cat("number of nonzero coefficients for CV-optimal lambda:", length(which(coef(cross_validation, select="min")!=0))-1, "\n")

# Number of total coefficients of our optimal CV model
cat("number of total coefficients for CV-optimal lambda:", length(coef(cross_validation, select="min"))-1, "\n")

# Computing the predicted probabilities
pred_CV <- drop(predict(cross_validation, select='min', X, type="response"))

# Plotting the histogram of predicted probabilities
hist(pred_CV, xlab = "Predicted Probabilities")

# Setting seed and loading in the ROC.R
set.seed(0)
source("roc.R")

cutoff_points = c(0.02,0.1,0.33,0.8,0.9)
cutoff_colors = c("blue","red","green","yellow","purple")

# IS curve
par(mai=c(.9,.9,.2,.1)) # format margins
roc(pred_CV, Y, bty="n", main="IS ROC") # from roc.R

# For loop to make my code more concise
for (i in cutoff_points){
  points(x=1-mean((pred_CV<=i)[Y==0]), y=mean((pred_CV>i)[Y==1]), cex=1.5, pch=20, col=cutoff_colors[match(i,cutoff_points)])
}

legend("bottomright",fill=cutoff_colors, legend=c(cutoff_points),bty="n",title="cutoffs")

#df[colnames(df)!="AGE"]

pred_acc <- as.factor(ifelse(pred_CV>0.5,1,0))

# Evaluate and show why we might want to use machine learning models

# Caret library to compute a confusion matrix
library(caret)
confusionMatrix(data = pred_acc,
                reference = test$college)

## This part of the code doesn't work yet
## \/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/
#################

# Make predictions using test set
predictions <- predict(logmodel, newdata=test, type = "response")
pred_acc <- as.factor(ifelse(predictions>0.5,1,0))

# Evaluate and show why we might want to use machine learning models

# Caret library to compute a confusionmatrix
library(caret)
confusionMatrix(data = pred_acc,
                reference = test$unemp)
## OLD ANALYSIS WITH WRONG DATASET
hospitalization <- read.csv("datasets/hospitalization.csv")
library(gamlr)
<<<<<<< HEAD


=======
# Tamar test
# Anthony Testing
>>>>>>> 20ae62e207693173c6177fc80dfe6bdeedbf4aef
# Checking dimensions
dim(hospitalization)

Y <- hospitalization$hospital_death
X <- subset(hospitalization, select = -c(hospital_death))
length(Y)
dim(X)
Not_unique = NULL

for(i in 1:ncol(hospitalization)) {      
  if (length(unique(hospitalization[,i]))>0){
    Column_name <- (colnames(hospitalization[i]))
    Number_unique <- (length(unique(hospitalization[,i])))
    Is_a_Factor <- is.factor(hospitalization[,i])
    Not_unique = rbind(Not_unique, data.frame(Column_name,Number_unique,Is_a_Factor))}
}
Not_unique
X <- subset(hospitalization, select = c(age, bmi, elective_surgery, ethnicity, gender, height, icu_admit_source, weight, heart_rate_apache, ventilated_apache, aids, cirrhosis, diabetes_mellitus, hepatic_failure, immunosuppression, leukemia, lymphoma, solid_tumor_with_metastasis))
dim(X)

Not_unique2 = NULL
dim(X)
for(i in 1:ncol(X)) {      
  if (length(unique(X[,i]))<75){
    Column_name2 <- (colnames(X[i]))
    Number_unique2 <- (length(unique(X[,i])))
    Is_a_Factor2 <- is.factor(X[,i])
    Not_unique2 = rbind(Not_unique2, data.frame(Column_name2,Number_unique2,Is_a_Factor2))}
}
Not_unique2


column_excluded <- c(Not_unique2$Column_name2)

for(i in column_excluded){
  print(i)
  j <- (match(i,colnames(X)))
  X[,j] <- as.factor(X[,j])
}

Not_unique3 = NULL

for(i in 1:ncol(X)) {      
  if (length(unique(X[,i]))<75){
    Column_name3 <- (colnames(X[i]))
    Number_unique3 <- (length(unique(X[,i])))
    Is_a_Factor3 <- is.factor(X[,i])
    Not_unique3 = rbind(Not_unique3, data.frame(Column_name3,Number_unique3,Is_a_Factor3))}
}
Not_unique3
dim(X)

# Removing the previous dataframes that we will no longer us
rm(Not_unique)
rm(Not_unique2)
rm(Not_unique3)
hospitalization[is.na(hospitalization)] <- 0
X <- naref(X)
dim(X)
# Remove Y variable from data frame that will become the X matrix
X <- subset(X, select = -c(hospital_death))
dim(X)
# Making the X matrix
#X <- model.matrix(~ ., data=X)[,-1]
dim(X)
# Check dimensions of our X matrix
dim(X)


# Estimating a logit model with lasso regularization and penalty obtained from 10-fold cross-validation
cross_validation_NY <- cv.gamlr(X, Y, nfold = 10, family="binomial", verb=TRUE)

