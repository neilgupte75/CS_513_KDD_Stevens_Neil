#  Course          :CS 513 Data Mining
#  First Name      : Neil 
#  Last Name       : Gupte
#  Id              : 10445674
#  purpose         : q7 Midterm




remove(list=ls())
library(kknn)

getwd()
?read.csv
#loading dataset
dataset<-read.csv('COVID19_v3.CSV')
df<- data.frame(dataset)
View(df)
summary(df)
df<-na.omit(df)

sample_size = round(nrow(df)*.70)
index <- sample(seq_len(nrow(df)), size = sample_size)
train <- df[index, ]
test <- df[-index, ]

predict_k1 <- kknn(formula= Infected~., train[,-1] , test[,-1], k=5,kernel ="rectangular"  )

fit <- fitted(predict_k1)
confusionmatrix<-table(test$Infected,fit)
confusionmatrix

wrong<- ( test$Infected!=fit)
rate<-sum(wrong)/length(wrong)
rate

accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(confusionmatrix)