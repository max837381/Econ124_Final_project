# Econ124_Final_project


Anthony, Tamar and Max's final project for ECON 124: Machine Learning for Economics at UCSC.


We are using CPS data to perform binomial deviance calculations using logistic regression models in addition to k-fold CV gamlr and Random Forests

Our project explores the question: How can machine learning help us predict college completion by using the Current Population Survey (CPS)? By running this experiment, we wanted to see whether variables such as age, sex, race, marital status, and more could be valid predictors of whether you earned a bachelor’s degree (or higher). We find this interesting because most studies regarding college completion are prospective of high school students, whereas we are trying to find out if you have already completed college based on certain characteristics. This study can answer questions such as, “if you are married, are you more likely to have completed college?” and “if you are ‘x’ race, are you more likely to have a bachelor’s degree?”. 

Data
	Our data comes from the CPS, which is a monthly survey that records data from approximately 60,000 households in the United States. The United States Census Bureau conducts this survey for the Bureau of Labor Statistics, and this survey is available to the public for anyone to analyze. We decided to subset the data to the months of December 2021 and January 2022. Because our main outcome variable is college completion, we restricted our sample to include only individuals aged 22 or older as that is typically the youngest age one receives after finishing their undergraduate degree. This restriction shrinks our dataset from 204,681 observations to 152,051 observations. We then excluded white people from our dataset since our analysis focuses on racial minorities, which decreased our number of observations to 29,467. Our dataset initially had 43 variables, including but not limited to age, sex, educational attainment, family income, state, marital status, and citizenship status. Our key dependent variable is a dummy variable on college completion, where a “1” indicates that the individual earned a bachelor’s degree and a “0” indicates otherwise. Individuals whose highest level of education is a masters, professional, or doctorate degree were included in having a bachelor’s degree, since one needs a bachelor’s degree to earn a graduate school degree. 
One important variable for college completion is race. There have been several studies and reports on racial inequality within higher education, and one of these inequalities could be college completion rates. In 2013, an article from Georgetown Public Policy Institute reported that Black and Latinx individuals who scored the same SAT or ACT scores as their white counterparts were less likely to earn their bachelor’s degree than their white counterparts (Carnevale and Strohl, 2013). This trend may still hold true, as our initial data showed that white people are overrepresented in earning bachelor’s degrees. 45,649 white people had a bachelor’s degree or higher in December of 2021. The second highest was for Asian Americans/Pacific Islanders, with a total of 5,307 having completed their undergraduate degree. Due to white people being overrepresented in higher education, we excluded them from our sample and thus our dataset has 29,467 observations. 25.8% of the Black people in our data completed college; 18.55% of the Native American people in our data completed college; 55.37% of the Asian Americans/Pacific Islanders in our data completed college; and 33.49% of people with mixed ancestry completed college. Figure 1 below shows the total number of people who completed college by race, where the green represents those who did complete college and the blue represents those who did not complete college.


Below ar esome of our results in the form of ROC curves and LASSO, RF plots

<img src="https://github.com/bigfinanceguy/Econ124_Final_project/blob/master/2%20Interaction%20Logit%20model.png" width="720">
<img src="https://github.com/bigfinanceguy/Econ124_Final_project/blob/master/3%20regularization%20path" width="720">
<img src="https://github.com/bigfinanceguy/Econ124_Final_project/blob/master/4%20CV%20error%20plot%20lambda.png" width="720">
<img src="https://github.com/bigfinanceguy/Econ124_Final_project/blob/master/5%20CV%20ROC.png" width="720">
<img src="https://github.com/bigfinanceguy/Econ124_Final_project/blob/master/6%20RF.png" width="720">
<img src="https://github.com/bigfinanceguy/Econ124_Final_project/blob/master/7%20RF%20ROC.png" width="720">
<img src="https://github.com/bigfinanceguy/Econ124_Final_project/blob/master/8%20MODEL%20ROC.png" width="720">
