#  Course          :CS 513 Data Mining
#  First Name      : Neil 
#  Last Name       : Gupte
#  Id              : 10445674
#  purpose         : Final Project


rm(list=ls())
getwd()
?read.csv
#loading dataset
library('ggplot2')
dataset<-read.csv('attrition_data.csv',na.string="?")
summary(dataset)
sum(is.na(dataset))
colSums(is.na(dataset))
dataset[is.na(dataset)]<-0
View(dataset)
pairs(dataset[2:27],main = "Employee retention status",
      pch = 20,bg =c("red","blue")[factor(dataset$Status)])

boxplot(dataset[2:7])
bp1<-ggplot(dataset,aes(x=STATUS,y=ANNUAL_RATE,color=STATUS))+geom_boxplot()
bp2<-ggplot(dataset,aes(x=STATUS,y=AGE,color=STATUS))+geom_boxplot()
tab1<-ftable(dataset$STATUS,dataset$PERFORMANCE_RATING)
bp3<-ggplot(dataset.agg,aes(x=STATUS,y=AGE,color=STATUS))+geom_bar(stat="identity")
bp4<-ggplot(dataset,aes(x=STATUS,y=HRLY_RATE,color=STATUS))+geom_bar(stat="identity")+geom_text()
bp5<-ggplot(dataset,aes(x=ETHNICITY,y=STATUS,color=STATUS))+geom_bar(stat="identity")
bp6<-ggplot(dataset,aes(x=SEX,y=STATUS,color=STATUS))+geom_bar(stat="identity")
barplot(tab1)
bp7<-ggplot(dataset,aes(x=MARITAL_STATUS,y=STATUS,color=STATUS))+geom_bar(stat="identity")

scat1<-ggplot(dataset,aes(x=ETHNICITY,y=ANNUAL_RATE,color=STATUS))+geom_point()
scat2<-ggplot(dataset,aes(x=SEX,y=HRLY_RATE,color=STATUS))+geom_point()
scat3<-ggplot(dataset,aes(x=MARITAL_STATUS,y=AGE,color=STATUS))+geom_point()
scat4<-ggplot(dataset,aes(x=EDUCATION_LEVEL,y=STATUS,color=STATUS))+geom_point()

ftable(dataset$STATUS,dataset$JOB_SATISFACTION)


ftable(dataset$STATUS,dataset$ETHNICITY)

ftable(dataset$STATUS,dataset$SEX)
ftable(dataset$STATUS,dataset$NUMBER_OF_TEAM_CHANGED)

bar1<-ggplot(dataset,aes(STATUS,color=STATUS))+geom_bar()
bar2<-ggplot(dataset,aes(ETHNICITY,color=STATUS))+geom_bar()
bar3<-ggplot(dataset,aes(NUMBER_OF_TEAM_CHANGED,color=STATUS))+geom_bar()
bar4<-ggplot(dataset,aes(SEX,color=STATUS))+geom_bar()
bar5<-ggplot(dataset,aes(MARITAL_STATUS,color=STATUS))+geom_bar()


hist1<-ggplot(dataset,aes(x=AGE,color=STATUS))+geom_histogram()
hist2<-ggplot(dataset,aes(x=JOB_SATISFACTION,color=STATUS))+geom_histogram()
hist3<-ggplot(dataset,aes(x=PERFORMANCE_RATING,color=STATUS))+geom_histogram()
dev.off()
dataset$STATUS<- factor(dataset$STATUS , levels = c("A","T") , labels = c("Active","Terminated"))
norm<-function(x) { (x -min(x))/(max(x)-min(x))   }

#sample_size = round(nrow(dataset)*.70)
#index <- sample(seq_len(nrow(dataset)), size = sample_size)

set.seed(100)
idx<-sort(sample(nrow(dataset),as.integer(.70*nrow(dataset))))
library(kknn)
library(gridExtra)
library(grid)
train <- dataset[idx, ]
test <- dataset[-idx, ]

predict_k1 <- kknn(formula= STATUS~., train[,c(-1,-4,-11,-12,-14,-22)] , test[,c(-1,-4,-11,-12,-14,-22)], k=20,kernel ="rectangular")
fit <- fitted(predict_k1)
summary(predict_k1)
confusionmatrix<-table(test$STATUS,fit)
#ggplot(confusionmatrix, aes(x=fit, y = Freq, fill=Var1)) + geom_bar(stat="identity")
grid.table(confusionmatrix)
confusionmatrix
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(confusionmatrix)


predict_k1 <- kknn(formula= STATUS~., train[,c(-1,-4,-11,-12,-14,-22)] , test[,c(-1,-4,-11,-12,-14,-22)], k=50,kernel ="triangular")
fit <- fitted(predict_k1)
confusionmatrix<-table(test$STATUS,fit)
#ggplot(confusionmatrix, aes(x=fit, y = Freq, fill=Var1)) + geom_bar(stat="identity")
grid.table(confusionmatrix)
confusionmatrix





accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(confusionmatrix)



##########################


library(e1071)
library(class)



model2<- naiveBayes(STATUS ~ ., data = train[c(-1,-4,-11,-12,-14,-22)])
prediction2<- predict(model2, test[c(-1,-4,-11,-12,-14,-22)])
confusionmatrix2 <- table(prediction_model=prediction2, test_class=test$STATUS)
print(confusionmatrix2)
grid.table(confusionmatrix2)

summary(model2)


accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(confusionmatrix2)

############################

library(class)
library(rpart)
library(rpart.plot) 
library(rattle) 
train3<-train[c(-1,-4,-11,-12,-14,-22)]
test3<-test[c(-1,-4,-11,-12,-14,-22)]
cartmodel <- rpart(STATUS ~ ., data = train3, method = "class")
rpart.plot(cartmodel)

fancyRpartPlot(cartmodel)

prediction3<- predict(cartmodel, test3,type="class")

confusionmatrix3 <- table(prediction_model=prediction3, test_class=test$STATUS)
print(confusionmatrix3)
grid.table(confusionmatrix3)
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(confusionmatrix3)

#########################################


# library(C50)
# 
# 
# model4<-C5.0(STATUS~.,train3)
# summary(model4)
# 
# prediction4<- predict(model4, test[,-1])
# confusionmatrix4 <- table(prediction_model=prediction4, test_class=test$STATUS)
# print(confusionmatrix4)
# 
# 
# accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
# accuracy(confusionmatrix4)

####################################
set.seed(100)
library(randomForest)
train1<-train
test1<-test
train1$JOBCODE<-NULL
train1$JOB_GROUP<-NULL
train1$REFERRAL_SOURCE<-NULL
test1$JOBCODE<-NULL
test1$JOB_GROUP<-NULL
test1$REFERRAL_SOURCE<-NULL

fit <- randomForest( STATUS~., data=train1[,c(-1,-4,-11,-12,-14,-22)], importance=TRUE, ntree=1000)
imp<-importance(fit)
grid.table(imp)
varImpPlot(fit)

Prediction <- predict(fit, test1[,c(-1,-4,-11,-12,-14,-22)])

confusionmatrix5 <- table(actual=test1$STATUS,Prediction)
print(confusionmatrix5)
grid.table(confusionmatrix5)

accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(confusionmatrix5)

###############################################

set.seed(100)
library(neuralnet)
dataSet<-data.frame(lapply(na.omit(dataset),as.numeric))
idx<-sort(sample(nrow(dataSet),as.integer(.70*nrow(dataSet))))

train2 <- dataSet[idx, ]
test2 <- dataSet[-idx, ]
?neuralnet()
model6<- neuralnet(STATUS~.,train2[c(-1,-4,-11,-12,-14,-22)], hidden=7, threshold=0.05)

plot(model6)
summary(model6)

ann <-compute(model6,test2[,c(-1,-4,-11,-12,-14,-22)])
ann$net.result 

ann_cat<-ifelse(ann$net.result <1.5,1,2)
length(ann_cat)
length(test2$STATUS)
confusionmatrix6<-table(ann_cat,test2$STATUS)
grid.table(confusionmatrix6)


wrong<- (test2$STATUS!=ann_cat)
errorRate<-sum(wrong)/length(wrong)
errorRate
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(confusionmatrix6)

######################################################






