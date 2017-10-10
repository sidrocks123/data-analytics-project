library("corrplot")
library("caret")
library("kernlab")
library("e1071")
data2 <- read.csv("/Users/sid/Desktop/Data Analytics Project/2.csv")
data2<- data2[-c(25,42,141,147,160,166,237,251,277,294,296,299,317,323,413,619), ]
M <- cor(data2[ ,-c(1,7,11)])
corrplot(M)
data2$classes <- as.factor(data2$classes)
summary(data2)
prop.table(table(data2$classes))
set.seed(312)
index <- createDataPartition(data2$classes, p=0.8, list = FALSE)
traind <- data2[index, -1]
testd <- data2[-index, -1]
fit <- trainControl(method="cv",
                           number = 6,
                           classProbs = TRUE,
                           summaryFunction = twoClassSummary)
SVMModel <- train(classes~.,
                   traind,
                   method="svmRadial",
                   metric="ROC",
                   preProcess='scale',
                   trace=FALSE,
                   trControl=fit)
prediction <- predict(SVMModel, testd)
confusion <- confusionMatrix(prediction, testd$classes, positive = "M")
confusion