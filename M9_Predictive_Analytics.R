#

## Setting Working directory
setwd("/Path/To/STAT4030/Datasets/")

options(scipen = 99) # Prevents scientific notation
install.packages("glmnet")

# Manually repeating model buidling and testing
wine_data_train <- read.csv("wine_quality_train.csv")
wine_data_test <- read.csv("wine_quality_test.csv")

reg_model <- lm(quality ~ citric.acid + pH + alcohol + fixed.acidity, 
                data=wine_data_train)

predicted <- predict(reg_model, newdata = wine_data_test)

wine_data_test$predicted <- predicted # Adding a new column in test dataset
                                      # Newly added column should have all 
                                      # predicted values

mean((wine_data_test$quality - predicted)^2) # PMSE Calculation

# Our first PMSE: 57.25465 (citric.acid + residual.sugar + alcohol)
# Our second PMSE: 55.09944 (citric.acid + pH + alcohol + fixed.acidity)


## Building a prediction model using cross-validation (creating blocks of data)
## Lasso regression - wine dataset
install.packages("glmnet")
library(glmnet)

wine_data_full <- read.csv("wine_quality_full.csv")
wine_data_full

x <- model.matrix(quality ~ ., data = wine_data_full) # Data Preparation
x = x[,-1] # The module First column is meaningless 

glmnet1<-cv.glmnet(x=x,y=wine_data_full$quality, type.measure='mse', 
                   nfolds=5, alpha=1)

lasso_coefficients <- coef(glmnet1, s='lambda.min', exact=TRUE)
lasso_coefficients


# Lasso regression - WHO dataset
who_data <- read.csv("WHO_Life_Expectancy_Data_2000.csv")

unwanted_variables <- c("Country")
who_data <- who_data[,!(names(who_data) %in% unwanted_variables)]
who_data <- who_data[complete.cases(who_data), ] # drop rows with missing values

x <- model.matrix(Life.expectancy ~ ., data = who_data)
x = x[,-1]

summary(lm(Life.expectancy ~ ., data = who_data))

glmnet1<-cv.glmnet(x=x, y=who_data$Life.expectancy, type.measure='mse', 
                   nfolds=5, alpha=1)

lasso_coefficients <- coef(glmnet1, s='lambda.min', exact=TRUE)
lasso_coefficients

