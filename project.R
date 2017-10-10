# Import libraries
library("corrplot")
library("caret")
library("kernlab")
library("e1071")

# Import data 
data2 <- read.csv("/Users/sid/Desktop/Data Analytics Project/2.csv")

# Remove missing values
data2<- data2[-c(25,42,141,147,160,166,237,251,277,294,296,299,317,323,413,619), ]

# Draw correlation plot (optional) to check strength of correlation between variables
M <- cor(data2[ ,-c(1,7,11)])
corrplot(M)

# Since we have to classify the classes column, make it a factor
data2$classes <- as.factor(data2$classes)

# Summarize the table (optional)
summary(data2)
prop.table(table(data2$classes))

# Set a seed for randomization
set.seed(312)

# Partition into training and testing data
index <- createDataPartition(data2$classes, p=0.8, list = FALSE)
traind <- data2[index, -1]
testd <- data2[-index, -1]

# Fitness function control
fit <- trainControl(method="cv",
                           number = 6,
                           classProbs = TRUE,
                           summaryFunction = twoClassSummary)
# Train SVM model
SVMModel <- train(classes~.,
                   traind,
                   method="svmRadial",
                   metric="ROC",
                   preProcess='scale',
                   trace=FALSE,
                   trControl=fit)
# Find and print confusion matrix
prediction <- predict(SVMModel, testd)
confusion <- confusionMatrix(prediction, testd$classes, positive = "M")
confusion
