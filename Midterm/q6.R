#  Course          :CS 513 Data Mining
#  First Name      : Neil 
#  Last Name       : Gupte
#  Id              : 10445674
#  purpose         : q6 Midterm





#########################################################
##  Step 0: Clear the environment
##           
##
#########################################################
rm(list=ls())
library(rpart)
library(rpart.plot)  			# Enhanced tree plots
library(rattle)           # Fancy tree plot
library(RColorBrewer)     # colors needed for rattle
dataset<-read.csv('COVID19_v3.CSV')
df<- data.frame(dataset)
View(df)
summary(df)
df<-na.omit(df)

#discretize data
df$MonthAtHospital<- cut(df$MonthAtHospital,breaks=c(-1,6,100),labels =c("less than 6 months","6 or more months"),right = FALSE) 

df$Age<- cut(df$Age, breaks = c(-1,35,50,120),labels = c("less than 35","35 to 50","51 or over"),right=FALSE)

set.seed(222)
sample_size = round(nrow(df)*.70)
index <- sample(seq_len(nrow(df)), size = sample_size)


train <- df[index, ]
test <- df[-index, ]
?rpart
  

#for cart classifier
cartmodel <- rpart(Infected ~ ., data = train[,-1], method = "class")
rpart.plot(cartmodel)


fancyRpartPlot(cartmodel)
CART_predict2<-predict(cartmodel,test, type="class")
df<-as.data.frame(cbind(test,CART_predict2))

#confusion matrix 
conf_matrix<-table(Actual=test[,"Infected"],CART=CART_predict2)

CART_wrong<-sum(test[,"Infected"]!=CART_predict2)
#error rate
error_rate=CART_wrong/length(test$Infected)
error_rate

#Accuracy
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(conf_matrix)


