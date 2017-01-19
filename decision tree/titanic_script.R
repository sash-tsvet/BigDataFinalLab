library("rpart")
library("rpart.plot")
library(ggplot2)
library("reshape2")

setwd("D:\\development\\SBT\\bigdata\\titanic")
mydata = read.csv("titanic_data.csv")

train <- mydata[1:668,]
test <- mydata[669:889,]

summary(mydata)
head(mydata)

#feature research
#Fare
ggplot(mydata, aes(x=Fare)) +
  geom_histogram(data=mydata[mydata$Survived==0,], alpha=0.2, fill="red") +
  geom_histogram(data=mydata[mydata$Survived==1,], alpha=0.2, fill="green")
#Age
ggplot(mydata, aes(x=Age)) +
  geom_histogram(data=mydata[mydata$Survived==0,], alpha=0.2, fill="red") +
  geom_histogram(data=mydata[mydata$Survived==1,], alpha=0.2, fill="green")
#Pclass
ggplot(mydata, aes(x=Pclass)) +
  geom_histogram(data=mydata[mydata$Survived==0,], alpha=0.2, fill="red") +
  geom_histogram(data=mydata[mydata$Survived==1,], alpha=0.2, fill="green")
#SibSp
ggplot(mydata, aes(x=SibSp)) +
  geom_histogram(data=mydata[mydata$Survived==0,], alpha=0.2, fill="red") +
  geom_histogram(data=mydata[mydata$Survived==1,], alpha=0.2, fill="green")
#Parch
ggplot(mydata, aes(x=Parch)) +
  geom_histogram(data=mydata[mydata$Survived==0,], alpha=0.2, fill="red") +
  geom_histogram(data=mydata[mydata$Survived==1,], alpha=0.2, fill="green")

fit <- rpart(Survived ~ Age + Sex + Pclass + Fare + SibSp + Parch, method="class", data=train, 
             control=rpart.control(minsplit=1))

summary(fit)
rpart.plot(fit, type=1)

fitted.results <- predict(fit, test)
fitted.results <- ifelse(fitted.results > 0.5, 1,0)

error <- mean(fitted.results[,2] != test$Survived)
print(1-error)
