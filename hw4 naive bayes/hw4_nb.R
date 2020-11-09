

#  Course          :CS 513 Data Mining
#  First Name      : Neil 
#  Last Name       : Gupte
#  Id              : 10445674
#  purpose         : hw 4 Naive Bayes


rm(list=ls())

getwd()


dataset<-read.csv('breast-cancer-wisconsin.data.csv',na.string="?")
dataset1<-na.omit(dataset)

dataset1$F6<-as.integer(dataset1$F6)

#converting into factor type
dataset1$Class<- factor(dataset1$Class , levels = c("2","4") , labels = c("Benign","Malignant"))
summary(dataset1)

library(e1071)
library(class)

set.seed(222)

#sample size 70/30 split
sample_size = round(nrow(dataset1)*.70)
index <- sample(seq_len(nrow(dataset1)), size = sample_size)

training <- dataset1[index, ]
test<-dataset1[-index,]

?naiveBayes
model<- naiveBayes(Class ~ ., data = training)


prediction<- predict(model, test)



confusionmatrix <- table(prediction_model=prediction, test_class=test$Class)
print(confusionmatrix)

accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(confusionmatrix)















