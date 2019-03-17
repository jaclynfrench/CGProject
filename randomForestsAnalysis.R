# This is an analysis of the publicly available College Scorecard Data
# This analysis is inspired by the Kaggle 'Welcome to Data Science in R' tutorial by Rachael Tatman


#Load in packages
library(tidyverse)
library(rpart)
library(randomForest)

#read in dataset
#this has been precleaned in Tableau Prep to filter for primarily Bachelor's Degree-granting institutions and to eliminate unnecessary columns

college_data <- CollegeScorecard

head(college_data)


#make sure all fields are numeric and drop NAs
summary(college_data)

college_data$GRAD_DEBT_MDN10YR_SUPP <- as.numeric(college_data$GRAD_DEBT_MDN10YR_SUPP)

college_data <-drop_na(college_data)

summary(college_data)





#split into testing and training sets
library(modelr)
collegeSplit <- resample_partition(college_data, c(test = .2, train = .8))

lapply(collegeSplit, dim)



#model with randomForests
fitRF <- randomForest(MD_EARN_WNE_P10 ~ CONTROL+ SINGLEGENDER + RELIGIOUS + SAT_AVG_ALL + PCTPELL + GRAD_DEBT_MDN10YR_SUPP, data = collegeSplit$train)

mae(model=fitRF, data=collegeSplit$test)


fitRF

importance(fitRF)


#Try a more traditional tree to compare results

fitDT <-rpart(MD_EARN_WNE_P10 ~ CONTROL+ SINGLEGENDER + RELIGIOUS + SAT_AVG_ALL + PCTPELL + GRAD_DEBT_MDN10YR_SUPP, data = collegeSplit$train)

plot(fitDT, uniform=TRUE)
text(fitDT, cex=.6)



#the mae for this decision tree is only slightly higher than randomForests
mae(model=fitDT, data=collegeSplit$test)



#trying to see what happens without SAT as a factor
fitNOSAT <- rpart(MD_EARN_WNE_P10 ~ CONTROL+ SINGLEGENDER + RELIGIOUS + PCTPELL + GRAD_DEBT_MDN10YR_SUPP, data = collegeSplit$train)

plot(fitNOSAT, uniform=TRUE)
text(fitNOSAT, cex=.6)



#unsurprisingly, the mae is higher without the SAT scores
mae(model=fitNOSAT, data=collegeSplit$test)




