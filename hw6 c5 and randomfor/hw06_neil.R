# CS 513 
# Name: Neil Gupte
# CWID:10445674
# purpose: hw 6 C5.0

rm(list=ls())
getwd()
dataset<-read.csv('breast-cancer-wisconsin.data.csv',na.string="?")

table(dataset$Class)

dataset$Class <- factor(dataset$Class, levels = c(2,4),labels = c("Benign", "Malignant"))

idx<-sort(sample(nrow(dataset),as.integer(.70*nrow(dataset))))
training<-dataset[idx,]
test<-dataset[-idx,]

library(C50)
model<-C5.0(Class~.,training[,-1])
summary(model)

plot(model)

prediction<-predict(model,test[,-1],type="class") 
#Forming the confusin matrix
conf_matrix<-table(test[,11],prediction)
conf_matrix
str(prediction)
#Showing the error rate 
wrong<-sum(test[,11]!=prediction)
error_rate<-wrong/length(test[,11])
error_rate

accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(conf_matrix)
