

install.packages("statar")
install.packages("caret")
library(statar)
library(caret)

## Setting Working directory
setwd("/Path/To/STAT4030/Datasets/")

## Read in Grad School Admit Data ##
gs <- read.csv("binary.csv")
View(gs)

## Integrity Check ##
## Basic R functions
summary(gs)
str(gs)
## statar module functions
sum_up(gs)


## Since "rank" is technically a categorical variable (rank "1" means high prestige 
## undergrad school,rank "4" means lowest prestige) let's make sure we have a 
## reasonable amount of 0's and 1' for admit within each level of rank 
table(gs$rank)
tab(gs, rank)

table(gs$admit, gs$rank)
tab(gs, admit, rank)


## Okay, it looks like we won't have a problem with imbalanced class sizes ##
## Let's go ahead and fit our logistic regression model ##

gs$rank <- relevel(factor(gs$rank), ref="4") #set rank 4 as baseline rank
## creating a new column 

## logistic regression
lmod <- glm(admit ~ gre + gpa + rank, data = gs, family = "binomial")

## Despite Logistic Regression not having quite as restrictive assumptions
## as linear, we still have to make sure we don't have too much multicollinearity ##
## variance inflation factor
car::vif(lmod)

## Okay, great! Let's check out the summary of our model ##
summary(lmod)

## odds instead of logged odds
exp(lmod$coefficients)

# As compared to a rank 4 school, Those who come from a rank 1 school have 
# an increased odds of getting into graduate school by 372%, while all other 
# variables remain constant.


stroke <- read.csv("Framingham_Stroke_Data.csv")
View(stroke)

# For independent variables - choose Male, Age, TOTCHOLESTEROL
lmod <- glm(STROKE ~ Male + TOTCHOLESTEROL + AGE, data = stroke, family = "binomial")
summary(lmod)

exp(lmod$coefficients)

# Interpret Age:
# As age increases by one year, the odds of getting stroke increases by 7.11%, 
# while all other variables remains constant.


# Evaluation
library(caret)
your_cutoff_point = 0.5
predicted <- predict(lmod, newdata = gs, type = "response")
predicted<-ifelse(predicted > your_cutoff_point,1,0)

gs$predicted_prob <- predicted
gs$predicted_result <-ifelse(predicted> 0.5,1,0)

# Using different cut-off point will offer you different performance
confusionMatrix(table(gs$admit, predicted))

