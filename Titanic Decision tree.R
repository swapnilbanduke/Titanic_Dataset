titanic <- read.csv("titanic.csv", header=TRUE, stringsAsFactors=TRUE)
plot(titanic$age, titanic$fare, xlab = "Age", ylab = "Fare", main = "Scatter Plot of Age vs. Fare", col='red',pch=16)

plot(titanic[titanic$sex == "male", "age"], titanic[titanic$sex == "male", "fare"],
     xlab = "Age", ylab = "Fare", main = "Scatter Plot of Age vs. Fare (Male and Female)",
     col = "blue")


points(titanic[titanic$sex == "female", "age"], titanic[titanic$sex == "female", "fare"],
       col = "red")


legend("topright", legend = c("Male", "Female"), col = c("blue", "red"), pch = 1)
correlation_coefficient_male <- cor(titanic[titanic$sex == "male", "age"], titanic[titanic$sex == "male", "fare"])

correlation_coefficient_male

correlation_coefficient_female <- cor(titanic[titanic$sex == "female", "age"], titanic[titanic$sex == "female", "fare"])

correlation_coefficient_female

set.seed(345)

train = sample(1:nrow(titanic), nrow(titanic)*(2/3))

titanic.train=titanic[train,]
titanic.test = titanic[-train,] 
install.packages("rpart")
library(rpart)
fit = rpart(survived ~ ., 
            data=titanic.train, 
            method="class", 
            control=rpart.control(xval=0, minsplit=100),
            parms=list(split="gini"))

install.packages("rpart.plot")
library(rpart.plot)
rpart.plot(fit, type = 1, extra = 1)

titanic.pred <- predict(fit, titanic.train, type="class")

titanic.actual <- titanic.train$survived

confusion.matrix <- table(titanic.pred, titanic.actual) 
confusion.matrix

install.packages("caret")

library(caret)


confusion_matrix <- confusionMatrix(titanic.pred, titanic.actual)
accuracy <- confusion_matrix$overall["Accuracy"]

accuracy


To improve the accuracy by using K-fold tech:

fit.good = rpart(survived ~ ., 
              data=titanic.train, 
               
              control=rpart.control(xval=20, minsplit=5,cp=0.01))

rpart.plot(fit.good, type=1,extra=1)
            
titanic.pred <- predict(fit.good, titanic.train, type="class")
confusion.matrix <- table(titanic.pred, titanic.actual) 
confusion.matrix

accuracy <- confusion_matrix$overall["Accuracy"]

accuracy




