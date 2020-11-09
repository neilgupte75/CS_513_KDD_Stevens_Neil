#  Course          :CS 513 Data Mining
#  First Name      : Neil 
#  Last Name       : Gupte
#  Id              : 10445674
#  purpose         : q5 Midterm




rm(list=ls())
#import libraries
library(e1071)
library(class)

getwd()
?read.csv
#loading dataset
dataset<-read.csv('COVID19_v3.CSV')
df<- data.frame(dataset)
View(df)
summary(df)
#remove missing values
df<-na.omit(df)
?cut

#discretize data
df$MonthAtHospital<- cut(df$MonthAtHospital,breaks=c(-1,6,100),labels =c("less than 6 months","6 or more months"),right = FALSE) 

df$Age<- cut(df$Age, breaks = c(-1,35,50,120),labels = c("less than 35","35 to 50","51 or over"),right=FALSE)

# sample data 70/30 split used
sample_size = round(nrow(df)*.70)
index <- sample(seq_len(nrow(df)), size = sample_size)

#training and testing 
train <- df[index, ]
test <- df[-index, ]


#naive bayes classifier added
nb<- naiveBayes(Infected ~ ., data = train)
#Predicting class for test set
predict <- predict(nb, test)

#Confusion Matrix
conf_matrix <- table(nb=predict,class=test$Infected)
print(conf_matrix)

#Accuracy
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(conf_matrix)

wrong<-sum(test$Infected!=predict)

#error rate 
error_rate<-wrong/length(test$Infected)
error_rate