# New York State High School Graduation Data<br/>STAT  471 Final Project<br/>Kennedy Manley<br/>December 19, 2021

High school graduates are proven to have better quality of life due to higher income, better living conditions,
access to health care, and overall more opportunities. It is important to encourage the population to graduate from high school because it also benefits the economy. Hence, for my final project, I decided to look into the graduation rates from the New York high school system for the 2019-2020 school year. Although, this is just a singular state, the New York population is one of the largest in the United States. Additionally, this data only looks at one school year which could have some particular factor that caused that specific year to give unique results. However, analyzing New York High School Graduation Rates could increase knowledge about trends and find certain groups/subsets of people that need to be supported more throughout high school to encourage high school graduation.

The data for this research project was acquired from the New York State Education Department website. The New York State Education Department (NYSED) is committed to publicly reporting data so that people can be better informed in their work to improve student achievement. The NYSED separates data by school year and provided their graduation rate database for download. The graduation rate database is described as "This database contains annual graduation, and dropout data for the state as well as by county, Need to Resource Capacity group, district, public school and charter school. Annual graduation data is included for the current four-year cohort (June and August graduates), five-year June and August, and six-year June and August cohorts." The primary response variable of interest was the graduation rate, which had to be calculated in R since the formatting of the downloaded data set did not allow for much manipulation of the given variable. The numerical explanatory variables were also manipulated to be in decimal form to allow for calculation and manipulation. There are both categorical and quantitative variables, but this research focuses primarily on the quantitative factors.

After downloading the data, but before exploring and running analysis, the data was split into a training dataset and test dataset. The test dataset was exclusively used for evaluating performance of the difference models. The training dataset was used to run many different tests and models to see how the different variables contributed to the graduation rates. First, normality assumptions for linear regression and correlations between variables were checked. Then, models were built and run to determine the best way to classify this data. The different models included ordinary least squares, ridge regression, LASSO regression, elastic net regression, random forest, and boosting. The efficiency of the model was evaluated based on the computed root mean squared error (RMSE).


Interestingly, we found that the boosted and elastic net regression both pointed to similar types of variables as the strongest predictors of deaths per cases. Specifically, our optimal boosted model revealed that variables related to residential segregation and unemployment emerged as the most significant predictors, revealing that structural economic and health access inequalities were more predictive of COVID-19 deaths per cases than other variables. We hope that this analysis can inform policies aimed at improving health outcome determinants, both in the context of COVID-19 and more generally going forward. 
