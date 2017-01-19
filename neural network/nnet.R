library(plyr)
library(foreign)

# Load the data sets
train <- read.csv("Data/train.csv", stringsAsFactors = FALSE)  # 891 obs
test <- read.csv("Data/test.csv", stringsAsFactors = FALSE)    # 418 obs

###
### Data structures
###

# Create a survived variable in the test data set
# Set "0" (did not survive) as the default value
test$survived <- 0

# Convert catagorical variables to factors
train$survived <- factor(train$survived)
train$sex <- factor(train$sex)
train$pclass <- factor(train$pclass)
test$survived <- factor(test$survived)
test$sex <- factor(test$sex)
test$pclass <- factor(test$pclass)
test$embarked <- factor(test$embarked)

###
### Fixing missing values
###
# 177 missing ages in TRAIN
# 86 missing ages in TEST
# 1 missing fare in TEST
# 2 missing embarked in TRAIN

# Combine the data sets for age/fare modeling
full <- join(test, train, type = "full")

# Multiple Imputation
#library(mi)
#inf <- mi.info(train)
#imp <- mi(train, info = inf, check.coef.convergence = FALSE, n.imp = 2, n.iter = 6, seed = 111)
#plot(imp)

# Create LM models for predicting missing values in AGE and FARE
age.mod <- lm(age ~ pclass + sex +
                sibsp + parch + fare, data = full)
fare.mod<- lm(fare ~ pclass + sex +
                sibsp + parch + age, data = full)

# Replace missing values in AGE and FARE with prediction
train$age[is.na(train$age)] <- predict(age.mod, train)[is.na(train$age)]
test$age[is.na(test$age)] <- predict(age.mod, test)[is.na(test$age)]
test$fare[is.na(test$fare)] <- predict(fare.mod, test)[is.na(test$fare)]

# Random Forest to find missing values
#full.age <- full[!is.na(full$age), ]  # Remove NA's
#full.age$fare[is.na(full.age$fare)] <- predict(fare.mod, full.age)[is.na(full.age$fare)]

#age.rf <- randomForest(age ~ pclass + sex + sibsp + parch + fare, data = full.age, ntree = 15000)
#train$age[is.na(train$age)] <- predict(age.rf, train)[is.na(train$age)]
#test$age[is.na(test$age)] <- predict(age.rf, test)[is.na(test$age)]

# Replace missing values in embarked with most popular
train$embarked[train$embarked == ""] <- "S"
train$embarked <- factor(train$embarked)

###
### Create "sex.name" variable"
###
library(stringr)
train$sex.name <- 0
test$sex.name <- 0
train$sex.name[!is.na(str_extract(train$name, "Mr"))] <- "Mr"
train$sex.name[!is.na(str_extract(train$name, "Mrs"))] <- "Mrs"
train$sex.name[!is.na(str_extract(train$name, "Mme"))] <- "Mrs"
train$sex.name[!is.na(str_extract(train$name, "Miss"))] <- "Miss"
train$sex.name[!is.na(str_extract(train$name, "Ms"))] <- "Miss"
train$sex.name[!is.na(str_extract(train$name, "Mlle"))] <- "Miss"
train$sex.name[!is.na(str_extract(train$name, "Capt"))] <- "Mr"
train$sex.name[!is.na(str_extract(train$name, "Major"))] <- "Mr"
train$sex.name[!is.na(str_extract(train$name, "Col"))] <- "Mr"
train$sex.name[!is.na(str_extract(train$name, "Master"))] <- "Mast"
train$sex.name[!is.na(str_extract(train$name, "Rev"))] <- "Mr"
train$sex.name[!is.na(str_extract(train$name, "Dr"))] <- "Mr"
train$sex.name[!is.na(str_extract(train$name, "Don"))] <- "Mr"
train$sex.name[!is.na(str_extract(train$name, "Countess"))] <- "Mrs"
train$sex.name[!is.na(str_extract(train$name, "Jonkheer"))] <- "Mr"

test$sex.name[!is.na(str_extract(test$name, "Mr"))] <- "Mr"
test$sex.name[!is.na(str_extract(test$name, "Mrs"))] <- "Mrs"
test$sex.name[!is.na(str_extract(test$name, "Mme"))] <- "Mrs"
test$sex.name[!is.na(str_extract(test$name, "Miss"))] <- "Miss"
test$sex.name[!is.na(str_extract(test$name, "Ms"))] <- "Miss"
test$sex.name[!is.na(str_extract(test$name, "Mlle"))] <- "Miss"
test$sex.name[!is.na(str_extract(test$name, "Capt"))] <- "Mr"
test$sex.name[!is.na(str_extract(test$name, "Major"))] <- "Mr"
test$sex.name[!is.na(str_extract(test$name, "Col"))] <- "Mr"
test$sex.name[!is.na(str_extract(test$name, "Master"))] <- "Mast"
test$sex.name[!is.na(str_extract(test$name, "Rev"))] <- "Mr"
test$sex.name[!is.na(str_extract(test$name, "Dr"))] <- "Mr"
test$sex.name[!is.na(str_extract(test$name, "Don"))] <- "Mr"
test$sex.name[!is.na(str_extract(test$name, "Countess"))] <- "Mrs"
test$sex.name[!is.na(str_extract(test$name, "Jonkheer"))] <- "Mr"

test$name[test$sex.name == 0]
train$name[train$sex.name == 0]

train$sex.name <- factor(train$sex.name)
test$sex.name <- factor(test$sex.name)

###
### Create "fare-distance" variable
###

# fare-distance = fare - mean(fare of pclass)
# Are those who pay less than the average for a ticket less likely to survive?

# Find the mean fare for each pclass
class1 <- subset(full, pclass == 1)
class2 <- subset(full, pclass == 2)
class3 <- subset(full, pclass == 3)
fare1 <- mean(class1$fare, na.rm = TRUE)
fare2 <- mean(class2$fare, na.rm = TRUE)
fare3 <- mean(class3$fare, na.rm = TRUE)

# Create fare_avg column
train$fare_avg[train$pclass == 1] <- fare1
train$fare_avg[train$pclass == 2] <- fare2
train$fare_avg[train$pclass == 3] <- fare3
test$fare_avg[test$pclass == 1] <- fare1
test$fare_avg[test$pclass == 2] <- fare2
test$fare_avg[test$pclass == 3] <- fare3

# Create fare-distance metric for Train
train <- transform(train, fare.distance = fare - fare_avg)
train <- train[, !names(train) %in% c("fare_avg")]

# Create fare-distance metric for Test
test <- transform(test, fare.distance = fare - fare_avg)
test <- test[, !names(test) %in% c("fare_avg")]

###
### Add family column
###
train$family <- NA
test$family <- NA
train$family[which(train$sibsp != 0 | train$parch != 0)] <- 1
train$family[which(train$sibsp == 0 & train$parch == 0)] <- 0
test$family[which(test$sibsp != 0 | test$parch != 0)] <- 1
test$family[which(test$sibsp == 0 & test$parch == 0)] <- 0
test$family <- factor(test$family)
train$family <- factor(train$family)
test$familia <- test$sibsp + test$parch
train$familia <- train$sibsp + train$parch

### 
###  Scale the non factors
###
train$age_scale <- (train$age-min(train$age))/(max(train$age-min(train$age)))
train$fare_scale <- (train$fare-min(train$fare))/(max(train$fare-min(train$fare)))

test$age_scale <- (test$age-min(test$age))/(max(test$age-min(test$age)))
test$fare_scale <- (test$fare-min(test$fare))/(max(test$fare-min(test$fare)))
