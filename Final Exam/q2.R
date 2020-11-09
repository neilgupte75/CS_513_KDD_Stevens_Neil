# CS 513 HW
# Name: Neil Gupte
# CWID:10445674
# purpose: final exam q2

rm(list=ls())
library(randomForest)

df <- read.csv('Admission_cat.csv',na.strings = "?")

View(df)
summary(df)
size <- floor(0.70 * nrow(df))

#Set the seed 
set.seed(123)
random <- sample(seq_len(nrow(df)), size = size)

#70% data in train dataset
train <- df[random, ]

#30% data in test dataset
test <- df[-random, ]

fit <- randomForest(ADMIT~., data=train[,c(-1)],importance=TRUE, ntree=100)
importance(fit)
varImpPlot(fit)
pred <- predict(fit, test)
print(pred)
#Confusion Matrix
conf_matrix <- table(pred,class=test$ADMIT)
print(conf_matrix)

#Accuracy
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(conf_matrix)
