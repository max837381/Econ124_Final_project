# Econ124_Final_project


**Anthony, Tamar and Max's final project for ECON 124: Machine Learning for Economics at UCSC.**


We are using CPS data to perform binomial deviance calculations using logistic regression models in addition to k-fold CV gamlr and Random Forests

Our project explores the question: How can machine learning help us predict college completion by using the Current Population Survey (CPS)? By running this experiment, we wanted to see whether variables such as age, sex, race, marital status, and more could be valid predictors of whether you earned a bachelor’s degree (or higher). 

We find this interesting because most studies regarding college completion are prospective of high school students, whereas we are trying to find out if you have already completed college based on certain characteristics. This study can answer questions such as, “if you are married, are you more likely to have completed college?” and “if you are ‘x’ race, are you more likely to have a bachelor’s degree?”. 

**Data**


Our data comes from the CPS, which is a monthly survey that records data from approximately 60,000 households in the United States. The United States Census Bureau conducts this survey for the Bureau of Labor Statistics, and this survey is available to the public for anyone to analyze. We decided to subset the data to the months of December 2021 and January 2022.
	


Below are some of our results in the form of ROC curves and LASSO, RF plots

<img src="https://github.com/bigfinanceguy/Econ124_Final_project/blob/master/2%20Interaction%20Logit%20model.png" width="720">
<img src="https://github.com/bigfinanceguy/Econ124_Final_project/blob/master/3%20regularization%20path" width="720">
<img src="https://github.com/bigfinanceguy/Econ124_Final_project/blob/master/4%20CV%20error%20plot%20lambda.png" width="720">
<img src="https://github.com/bigfinanceguy/Econ124_Final_project/blob/master/5%20CV%20ROC.png" width="720">
<img src="https://github.com/bigfinanceguy/Econ124_Final_project/blob/master/6%20RF.png" width="720">
<img src="https://github.com/bigfinanceguy/Econ124_Final_project/blob/master/7%20RF%20ROC.png" width="720">
<img src="https://github.com/bigfinanceguy/Econ124_Final_project/blob/master/8%20MODEL%20ROC.png" width="720">
