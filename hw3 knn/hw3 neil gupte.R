

#  Course          :CS 513 Data Mining
#  First Name      : Neil 
#  Last Name       : Gupte
#  Id              : 10445674
#  purpose         : hw 3 knn 

rm(list=ls())

getwd()

dataset<-read.csv('breast-cancer-wisconsin.data.csv',na.string="?")
dataset1<-na.omit(dataset)

dataset1$Class<- factor(dataset1$Class , levels = c("2","4") , labels = c("Benign","Malignant"))
summary(dataset1)
?set.seed


set.seed(222)
#sample size 70/30 split
sample_size = round(nrow(dataset1)*.70)
index <- sample(seq_len(nrow(dataset1)), size = sample_size)

norm<-function(x) { (x -min(x))/(max(x)-min(x))   }
minmaxnorm <- as.data.frame(lapply(dataset1[,c(2,3,4,5,6,7,8,9,10)], norm))

dataset1Class = dataset1['Class']

summary(minmaxnorm)
train <- minmaxnorm[index, ]

trainclass<-dataset1Class[index,]
test <- minmaxnorm[-index, ]

testclass<-dataset1Class[-index,]
library(class)

model1 <- knn(train,test,cl=trainclass,k=3)

confusionmatrix1 <- table(model1, testclass)
print(confusionmatrix1)

accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(confusionmatrix1)


model2 <- knn(train,test,cl=trainclass,k=5)

confusionmatrix2 <- table(model2, testclass)
print(confusionmatrix2)

accuracy(confusionmatrix2)


model3 <- knn(train,test,cl=trainclass,k=10)

confusionmatrix3 <- table(model3, testclass)
print(confusionmatrix3)

accuracy(confusionmatrix3)

