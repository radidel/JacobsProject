'''# 1. Some important functions in caret:
Caret stands for classification and regression
training. use for  data preparation, model building, and model evaluation
a) createDataPartition: 
Description:
A series of test/training partitions are created 
using createDataPartition while createResample
creates one or more bootstrap samples.
createFolds splits the data into k groups while
createTimeSlices creates cross-validation split 
for series data. groupKFold splits the data based 
on a grouping factor.#https://www.rdocumentation.org/packages/caret/versions/6.0-93/topics/createDataPartition.

'''
library(readxl)
library(caret)
library(dplyr)
insurancedata = read_excel("C://Users//didel//Downloads//JUB University//Data Analytics//insurance.xlsx")

trainIndex <- createDataPartition(insurancedata$age, p = .8, 
                                  list = FALSE, 
                                  times = 1)
head(trainIndex)


str(insurancedata)



"""
b) train
 train() is the workhorse of caret. It takes the following information then trains (tunes) the requested model:
form, a formula, such as y ~ .
This specifies the response and which predictors (or transformations of) should be used.
data, the data used for training
trControl which specifies the resampling scheme, that is, how cross-validation should be performed to find the best values of the tuning parameters
preProcess which allows for specification of data pre-processing such as centering and scaling
method, a statistical learning method from a long list of availible models
tuneGrid which specifies the tuning parameters to train over
"""

'''
c) trainControl
trainControl() will specify the resampling scheme
#https://www.rdocumentation.org/packages/caret/versions/6.0-92/topics/trainControl
After splitting the data, we can begin training a number of models. We begin with a simple additive logistic regression.
'''

insurancedata = read_excel("C://Users//didel//Downloads//JUB University//Data Analytics//insurance.xlsx")
sample <- sample(c(TRUE, FALSE), nrow(insurancedata), replace=TRUE, prob=c(0.8,0.2))
train  <- insurancedata[sample, ]
test   <- insurancedata[!sample, ]

apply(train,2, class)
train <- data.frame(train)
control <- trainControl(method="repeatedcv", number=10, repeats=5)


my_glm <- train(train[,1:3], train[,4],
                method = "lm",
                trControl = control)


my_gleapbackward <- train(train[,1:3], train[,4],
                method = "leapBackward",
                trControl = control)

my_gleapforward <- train(train[,2:5], train[,1],
                          method = "leapForward",
                          trControl = control)
my_glmStepAIC <- train(train[,2:5], train[,1],
                         method = "lmStepAIC",
                         preProc = c("center", "scale"),
                         trControl = control)




#"question::3"

insurancedata

data <- data.frame(x = insurancedata$charges)
data$`180018R` <- data$x
data$x <- NULL
data$Response <- as.factor(insurancedata$charges)
table(data$Response)
data2 <- data
response_col <- which(colnames(data2) == "Response")
colnames(data2)[-response_col] <- paste0( "V", colnames(data2)[-response_col])

gbmFit1 <- train(Response ~., data = data2, 
                 method = "leapBackward", 
                 trControl = control,
                 verbose = FALSE)



step.model <- train(train[,2:5], train[,1],
                    method = "leapBackward", 
                    tuneGrid = data.frame(nvmax = 1:5),
                    trControl = control
)



#Question 5  Dimension Reduction

#a) PCR
library(pls)

my_gpcr <- train(train[,2:5], train[,1],
                       method = "pcr",
                       preProc = c("center", "scale"),
                       trControl = control)


mpr = my_gpcr$results %>%
  rename(CV_MSE = RMSE) %>%
  mutate(min_CV_MSE = as.numeric(ncomp == my_gpcr$bestTune$ncomp)) %>% 
  as.data.frame()

ggplot(mpr,aes(x = ncomp, y = CV_MSE)) + 
  geom_line(col = "grey55") + 
  geom_point(size = 2, aes(col = factor(min_CV_MSE))) + 
  scale_x_continuous(breaks = seq(1, ncol(train)-1), minor_breaks = NULL) +
  scale_color_manual(values = c("blue", "red")) + 
  theme(legend.position = "none") + 
  labs(title = "insurance train Dataset - Principal Components Regression", 
       subtitle = "Selecting number of principal components with cross-validation",
       x = "Principal Components", 
       y = "CV MSE")

min(mpr$CV_MSE)

#b) PLS Model


my_gpls <- train(train[,2:5], train[,1],
                 method = "pls",
                 preProc = c("center", "scale"),
                 trControl = control)

mpr = my_gpls$results %>%
  rename(CV_MSE = RMSE) %>%
  mutate(min_CV_MSE = as.numeric(ncomp == my_gpls$bestTune$ncomp)) %>% 
  as.data.frame()

ggplot(mpr,aes(x = ncomp, y = CV_MSE)) + 
  geom_line(col = "grey55") + 
  geom_point(size = 2, aes(col = factor(min_CV_MSE))) + 
  scale_x_continuous(breaks = seq(1, ncol(train)-1), minor_breaks = NULL) +
  scale_color_manual(values = c("blue", "red")) + 
  theme(legend.position = "none") + 
  labs(title = "insurance train Dataset - Principal Components Regression", 
       subtitle = "Selecting number of principal components with cross-validation",
       x = "Principal Components", 
       y = "CV MSE")

min(mpr$CV_MSE)











