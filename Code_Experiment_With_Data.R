# Step 1 - Load the data
setwd("C:/Umaima/Data Science/Hackathon - Analytics Vidhya/Experiments with data")

train <- read.csv("train_gbW7HTd.csv", na.strings = "")
test <- read.csv("test_2AFBew7.csv", na.strings = "")

# Step 2 - Univariate analysis
str(train)

train_Cont <- subset(train, select = c(ID, Age, Hours.Per.Week))
train_Cat <- subset(train, select = -c(ID, Age, Hours.Per.Week))

# Of COntinuous variable
summary(train_Cont)

install.packages("pastecs")

library(pastecs)

# 'scipen' is a penalty for scientific display i.e nos will not be displayed in e10 format
options(scipen = 100)
options(digits = 2)

stat.desc(train_Cont)

# Of Categorical Variable

#no of unique values in each variable
apply(train_Cat, 2, function(x){length(unique(x))})

table(train_Cat$Race)

as.matrix(prop.table(table(train_Cat$Race)))

# Insights - whites are 85%, black 9.5%...total 94.5%

head(sort(table(train_Cat$Native.Country), decreasing = TRUE), 20)

head(round(sort(prop.table(table(train_Cat$Native.Country)), decreasing = TRUE), 6), 20)

# Insights - US has 91% of data & Mexico has 2%

table(train_Cat$Sex)

# Continuous variable
summary(train_Cont$Age)

# Step 3 - Multivariate analysis

# Between both Categorical variables
library(gmodels)

CrossTable(train_Cat$Sex, train_Cat$Income.Group)

# Insights - 1) 89.1% females have income <=50k and 10.9% females have >50k
# 2) Amongst people having income >50k, 15% are females and 85% are males

library(ggplot2)

ggplot(train, aes(Sex, fill= Income.Group)) + geom_bar() + 
  labs(title="Stacked Bar Graph",x="Sex", y="Count") + theme_bw()
# Insight - More Females earn <=50k

# Between Both continuous variable
ggplot(train, aes(Age, Hours.Per.Week)) + geom_point() + 
  labs(title="Scatter Plot",x="Age", y="Hours Per Week") + theme_classic()

# Insight - No relation between age and hours/week

# Between Categorical & COntinuous
ggplot(train, aes(Sex, Hours.Per.Week)) + geom_boxplot() + labs(title="Boxplot")

# Insight - Males have higher working hours. However median working hours for both is the same.

# Step 4 - Missing Values
table(is.null(train))

colSums(is.na(train))

colSums(is.na(test))

# Insight - Missing values found in 'Workclass', 'Occupation' and 'Native.COuntry'

# Missing value imputation
install.packages("mlr")

library(mlr)

imputed_data <- impute(train, classes = list(factor = imputeMode()))

train <- imputed_data$data

colSums(is.na(train))

imputed_test_data <- impute(test, classes = list(factor = imputeMode()))

test <- imputed_test_data$data

colSums(is.na(test))

# Step 5 - Outlier treatment

library(ggplot2)

# For Continuous variables
ggplot(train, aes(train$ID, train$Age)) + geom_jitter()
ggplot(train, aes(train$ID, train$Hours.Per.Week)) + geom_jitter()

# Insight - NO outliers

# Variable Transformation
sapply(train, class)

# For variable Workclass
as.matrix(prop.table(table(train$Workclass)))

# Combining workclasses with < 5% values
library(car)

train$Workclass <- recode(train$Workclass, "c('Federal-gov','Never-worked', 'State-gov', 'Self-emp-inc', 'Without-pay')='Others'")

test$Workclass <- recode(test$Workclass, "c('Federal-gov','Never-worked', 'State-gov', 'Self-emp-inc', 'Without-pay')='Others'")

as.matrix(prop.table(table(train$Workclass)))

# For variable Education
as.matrix(prop.table(table(train$Education)))

train$Education <- recode(train$Education, "c('10th','11th','12th','1st-4th','5th-6th','7th-8th','9th','Assoc-acdm','Assoc-voc','Doctorate','Preschool','Prof-school')='Others'")

test$Education <- recode(test$Education, "c('10th','11th','12th','1st-4th','5th-6th','7th-8th','9th','Assoc-acdm','Assoc-voc','Doctorate','Preschool','Prof-school')='Others'")

as.matrix(prop.table(table(train$Education)))

# For variable Marital.Status
as.matrix(prop.table(table(train$Marital.Status)))

train$Marital.Status <- recode(train$Marital.Status, "c('Married-AF-spouse','Married-spouse-absent','Separated','Widowed')='Others'")

test$Marital.Status <- recode(test$Marital.Status, "c('Married-AF-spouse','Married-spouse-absent','Separated','Widowed')='Others'")

as.matrix(prop.table(table(train$Marital.Status)))

as.matrix(prop.table(table(test$Marital.Status)))

# For variable Occupation
as.matrix(prop.table(table(train$Occupation)))

train$Occupation <- recode(train$Occupation, "c('Armed-Forces','Farming-fishing','Handlers-cleaners','Priv-house-serv','Protective-serv','Tech-support','Transport-moving')='Others'")

test$Occupation <- recode(test$Occupation, "c('Armed-Forces','Farming-fishing','Handlers-cleaners','Priv-house-serv','Protective-serv','Tech-support','Transport-moving')='Others'")

as.matrix(prop.table(table(train$Occupation)))

# For variable Relationship
as.matrix(prop.table(table(train$Relationship)))

train$Relationship <- recode(train$Relationship, "c('Other-relative','Wife')='Others'")

test$Relationship <- recode(test$Relationship, "c('Other-relative','Wife')='Others'")

as.matrix(prop.table(table(train$Relationship)))

# For variable Race
as.matrix(prop.table(table(train$Race)))

train$Race <- recode(train$Race, "c('Amer-Indian-Eskimo','Other')='Others'")

test$Race <- recode(test$Race, "c('Amer-Indian-Eskimo','Other')='Others'")

as.matrix(prop.table(table(train$Race)))

#For variable Native Country
#as.matrix(prop.table(table(train$Native.Country)))

#train$Native.Country <- recode(train$Native.Country, "c('Cambodia','Canada','China','Columbia','Cuba','Dominican-Republic','Ecuador','El-Salvador','England','France','Germany','Greece')='Others'")

as.matrix(prop.table(table(train$Hours.Per.Week)))

install.packages("classInt")
library(classInt)
x <- classIntervals(train$Hours.Per.Week, 20, style = 'fixed', fixedBreaks = c(1,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100) )
x
as.matrix(x)

# Step 6 - Predictions
# Using decision trees

#Data Preprocessing
table(train$Income.Group)

#Convert >50k to 1 and <= 50k to 0

train$Income.Group <- ifelse(train$Income.Group == "<=50K", 0, 1)

table(train$Income.Group)

train <- subset(train, select = -c(ID))

# Building the model
library(rpart)

set.seed(111)

train.tree <- rpart(Income.Group ~., data = train, method='class', control = rpart.control(minsplit = 20, minbucket = 100, maxdepth = 10, xval = 5))

summary(train.tree)

install.packages("rpart.plot")
library(rpart.plot)

rpart.plot(train.tree)

# Insights - 1) Marital Status is the most imp variable
# 2) There are 4 leaf nodes
# 3) If Marital.Status is not Divorced, Never-married, Others then Income is <=50k

# Make predictions

prediction.train <- predict(train.tree, newdata = train, type="class")

prediction.test <- predict(train.tree, newdata = test, type="class")

# Analyse the results

library(caret)

confusionMatrix(prediction.train, train$Income.Group)

# Accuracy is 81.3%

solutiton_frame <- data.frame(ID = test$ID, Income.Group=prediction.test)

write.csv(solutiton_frame, file="final_solution.csv")
