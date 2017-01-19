library(nnet)

load("D:/R_projects/Final_Lab/train_clean.RData");
load("D:/R_projects/Final_Lab/test_clean.RData");

## Make vectors because neuralnet is weird
sex <- train$sex
pclass <- train$pclass
fare <- train$fare
age <- train$age
survived <- train$survived

vectors <- cbind(sex, pclass, fare, age, family, survived)
vectors <- as.data.frame(vectors)
vectors$sex <- factor(vectors$sex)
vectors$pclass <- factor(vectors$pclass)
vectors$family <- factor(vectors$family)
vectors$survived <- factor(vectors$survived)

sex.test <- test$sex
pclass.test <- test$pclass
fare.test <- test$fare
age.test <- test$age
#survived.test <- test$survived

vectors.test <- cbind(sex.test, pclass.test, fare.test, age.test)
vectors.test <- as.data.frame(vectors.test)
colnames(vectors.test) <- c("sex", "pclass", "fare", "age")

vectors.test$sex.test <- factor(vectors.test$sex)
vectors.test$pclass.test <- factor(vectors.test$pclass)

net3 <- nnet(survived ~ sex + pclass + fare + age + family, data = train, size = 7,
             linout = FALSE, maxit = 10000)
model <- "nnet(survived ~ sex + pclass + fare + age, data = train, size = 2, linout = FALSE, maxit = 10000)"

fitted.results <- predict(net3, test, type = "class")
fitted.results <- ifelse(fitted.results > 0.5, 1,0)

error <- mean(fitted.results != test$survived)

print(1-error)

library(devtools)
source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')

#plot each model
plot.nnet(net3)
