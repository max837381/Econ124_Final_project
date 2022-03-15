# Tamar - descriptive statistics and tables
#do it for education, race, and gender
#regression and then scatter plot
library(dplyr)
library(tidyr)
df <- read.csv("cps_00007.csv") #loads the dataset
df <- na.omit(df) #drops the rows with missing data
df <- df[df$AGE >= 18 & df$AGE <= 65,] #restricts dataset to working age individuals
df <- df[!(df$EMPSTAT==0 | df$EMPSTAT>=30),] #excludes those not in the labor force
df$unemp <- ifelse(df$EMPSTAT>=20 & df$EMPSTAT<=22,1,0) #creates a dummy variable =1 if person is unemployed

#education descriptive statistics
df <- df[!(df$EDUC==999),]

#grouping for education levels
df$lessthanhs <- ifelse(df$EDUC <= 072,1,0) #dummy variable =1 if person did not get a high school diploma
#or if person was in school for less than high school, =0 otherwise
df$hs <- ifelse(df$EDUC==073,1,0) #dummy variable =1 if person's highest educational attainment was a high school dimploma
df$somecollege <- ifelse(df$EDUC==081,1,0)
df$associates <- ifelse(df$EDUC==091 | df$EDUC==092,1,0)
df$bachelors <- ifelse(df$EDUC==111,1,0)
df$masters_professional <- ifelse(df$EDUC==123 | df$EDUC==124,1,0)
df$doctorate <- ifelse(df$EDUC==124,1,0)

df$highest_education <- numeric(0)
df$highest_education[df$lessthanhs==1] <- 1
df$highest_education[df$hs==1] <- 2
df$highest_education[df$somecollege==1] <- 3
df$highest_education[df$associates==1] <- 4
df$highest_education[df$bachelors==1] <- 5
df$highest_education[df$masters_professional==1] <- 6
df$highest_education[df$doctorate==1] <- 7

highest_education_totals <- df %>%
  group_by(highest_education) %>%
  summarise(totals=sum(tabulate(highest_education)))
highest_education_unemp_totals <- df %>%
  group_by(highest_education, unemp) %>%
  summarise(totals=sum(tabulate(highest_education)))

hist(as.numeric(df$highest_education), main = "Histogram of Highest Education Levels",
     xlab="Highest Education Level Group")
legend(x="topright",
       legend = c("1-Less Than High School", "2-High School Diploma",
                  "3-Some College","4-Associates Degree",
                  "5-Bachelors Degree", "6-Masters or Professional Degree",
                  "7-Doctorate")) #maybe dont include this legend and just explain histogram in the paper

#gender descriptive statistics
gender_totals <- df %>%
  group_by(SEX) %>%
  summarise(totals=sum(tabulate(SEX)))
geneder_unemp_totals <- df %>%
  group_by(SEX, unemp) %>%
  summarise(totals=sum(tabulate(SEX)))

#race descriptive statistics
#groupings for race
df$asian_pi <- ifelse(df$RACE >= 650 & df$RACE <= 652,1,0)
df$mixed_ancestry <- ifelse(df$RACE >= 801 & df$RACE<= 999,1,0)

#missing Latinx race group - not captured in race variable (actually can be any race i think)
df$race_group <- numeric(0)
df$race_group[df$RACE==100] <- 1 #=1 if person is white
df$race_group[df$RACE==200] <- 2 #=2 if person is Black
df$race_group[df$RACE==300] <- 3 #=3 if person is Native American
df$race_group[df$asian_pi==1] <- 4 #=4 if person is Asian or Pacific Islander
df$race_group[df$mixed_ancestry==1] <- 5 #=5 if person has mixed ancestry of 2 or more races



race_totals <- df %>%
  group_by(race_group) %>%
  summarise(totals=sum(tabulate(race_group)))
race_unemp_totals <- df %>%
  group_by(race_group, unemp) %>%
  summarise(totals=sum(tabulate(race_group)))