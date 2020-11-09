#  Course          :CS 513 Data Mining
#  First Name      : Neil 
#  Last Name       : Gupte
#  Id              : 10445674
#  purpose         : q3 final exam


rm(list=ls())
library(C50)

df <- read.csv('Admission_cat.csv',na.strings = "?")

View(df)
summary(df)
df$ADMIT <- factor(df$ADMIT, levels = c(0,1))

size <- floor(0.70 * nrow(df))

#Set the seed 
set.seed(123)
random <- sample(seq_len(nrow(df)), size = size)

#70% data in train dataset
train <- df[random, ]

#30% data in test dataset
test <- df[-random, ]

#c5.0 model
model<-C5.0(ADMIT~.,train[,-1])
summary(model)
plot(model)
prediction<-predict(model,test[,-1],type="class")
#confusion matrix
conf_matrix<-table(test[,2],prediction)
conf_matrix
# accuracy of model
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(conf_matrix)

