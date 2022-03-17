# Tamar - descriptive statistics and tables
#do it for education, race, and gender
#regression and then scatter plot
library(dplyr)
library(tidyr)
df <- read.csv("cps_00010.csv") #loads the dataset
df <- df[df$AGE >= 22,] #restricts dataset to ages where one typically gets a bachelor's degree
df$college <- ifelse(df$EDUC>=111,1,0) #creates a dummy variable =1 if person has a bachelor's degree (or higher)

#gender descriptive statistics
gender_totals <- df %>%
  group_by(SEX) %>%
  summarise(totals=sum(tabulate(SEX)))
gender_college_totals <- df %>%
  group_by(SEX, college) %>%
  summarise(totals=sum(tabulate(SEX)))

#race descriptive statistics
#groupings for race
df$asian_pi <- ifelse(df$RACE >= 650 & df$RACE <= 652,1,0)
df$mixed_ancestry <- ifelse(df$RACE >= 801 & df$RACE<= 999,1,0)

df$race_group <- 0
df$race_group[df$RACE==100] <- 1 #=1 if person is white
df$race_group[df$RACE==200] <- 2 #=2 if person is Black
df$race_group[df$RACE==300] <- 3 #=3 if person is Native American
df$race_group[df$asian_pi==1] <- 4 #=4 if person is Asian or Pacific Islander
df$race_group[df$mixed_ancestry==1] <- 5 #=5 if person has mixed ancestry of 2 or more races

race_totals <- df %>%
  group_by(race_group) %>%
  summarise(totals=sum(tabulate(race_group)))
race_college_totals <- df %>%
  group_by(race_group, college) %>%
  summarise(totals=sum(tabulate(race_group)))

#income grouping
quantile(df$FAMINC, probs = c(0.25,0.5,0.75))
df$lowerclass <- ifelse(df$FAMINC <= 730,1,0) #classifies someone as being lower class if their family income is less than or equal to $39,999
df$lowermiddle <- ifelse(df$FAMINC >= 740 & df$FAMINC < 830,1,0) #classifies someone as being lower middle class if their family income is greater than or equal
#to 40,000 and less than $60,000
df$uppermiddle <- ifelse(df$FAMINC >= 830 & df$FAMINC < 842,1,0) #classifies someone as being upper middle class if their family income is greater than or equal 
#to $60,000 and less than $100,000
df$upperclass <- ifelse(df$FAMINC >= 842 & df$FAMINC != 999,1,0) #classifies someone as being upper class if their family income is greater than or equal to
#$100,000

df$income_group <- 0
df$income_group[df$lowerclass==1] <- 1 #=1 if person is in the lower class
df$income_group[df$lowermiddle==1] <- 2 #=2 if person is in the lower middle class
df$income_group[df$uppermiddle==1] <- 3 #=3 if person is in the upper middle class
df$income_group[df$upperclass==1] <- 4 #=4 if person is in the upper class

income_totals <- df %>%
  group_by(income_group) %>%
  summarise(totals=sum(tabulate(income_group)))
income_college_totals <- df %>%
  group_by(income_group, college) %>%
  summarise(totals=sum(tabulate(income_group)))

hist(as.numeric(df$incomegroup), main = "Histogram of Income Group",
     xlab="Income Group")

