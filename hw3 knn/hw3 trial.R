
rm(list=ls())

getwd()

dataset<-read.csv('breast-cancer-wisconsin.data.csv',na.string="?")
dataset1<-na.omit(dataset)

library(kknn)

dataset1$Class<- factor(dataset1$Class , levels = c("2","4") , labels = c("Benign","Malignant"))
summary(dataset1)


sample_size = round(nrow(dataset1)*.70) # setting what is 70% 
index <- sample(seq_len(nrow(dataset1)), size = sample_size)


norm<-function(x) { (x -min(x))/(max(x)-min(x))   }
minmaxnorm <- as.data.frame(lapply(dataset1[,c(2,3,4,5,6,7,8,9,10)], norm))

dataset1Class = dataset1['Class']

summary(minmaxnorm)

train <- minmaxnorm[index, ]

trainclass<-dataset1Class[index,]
test <- minmaxnorm[-index, ]

testclass<-dataset1Class[-index,]



model1<-kknn(formula= trainclass~.,train,test, k=3, kernel='rectangular')
prediction <- predict(model1)


confusionmatrix1 <- table(predict,testclass~.)
