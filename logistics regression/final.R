library(ggplot2)
library("reshape2")


home_dir = '/home/lyan/Documents/big_data/final/'
setwd(home_dir)

train_data <- read.csv('train.csv')
test_csv <- read.csv('test.csv')

train_data$Age[is.na(train_data$Age)] <- mean(train_data$Age, na.rm=T)

train <- train_data[1:800,]
test <- train_data[801:889,]

summary(train)
head(train)

#feature research
ggplot(train_data, aes(x=Fare)) +
geom_histogram(data=train_data[train_data$Survived==0,], alpha=0.2, fill="red") +
geom_histogram(data=train_data[train_data$Survived==1,], alpha=0.2, fill="green")

ggplot(train_data, aes(x=Age))+
geom_histogram(data=train_data[train_data$Survived==0,], alpha=0.2, fill="red")+
geom_histogram(data=train_data[train_data$Survived==1,], alpha=0.2, fill="green")

age <- (train$Age)
surv <- (train$Survived)

surv_train <- data.frame(
  cbind(
    train$Age, as.factor(train$Sex), train$Fare, train$Survived
  )
)

surv_test <- data.frame(
  cbind(
    test$Age, as.factor(test$Sex),test$Fare
  )
)

logist <- glm(formula = surv_train$X4 ~.,family=binomial(link='logit'),data=surv_train)

fitted.results <- predict(logist, surv_test)
fitted.results <- ifelse(fitted.results > 0.5, 1,0)

error <- mean(fitted.results != test$Survived)

print(1-error)

