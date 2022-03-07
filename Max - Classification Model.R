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








################################################################################################################################################################################################################################

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

