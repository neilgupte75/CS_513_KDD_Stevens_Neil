# CS 513 HW-2 EDA
# Name: Neil Gupte
# CWID:10445674
# purpose: hw 2 eda 

rm(list=ls())
getwd()
?read.csv
#loading dataset
dataset<-read.csv('breast-cancer-wisconsin.data.csv',na.string="?")
#Summarizing each column
summary(dataset)

typeof(dataset)
df<- data.frame(dataset)
View(df)
summary(df)
#identify the missing values
sum(is.na(df))

colSums(is.na(df))
#Replacing the missing values with the "mean" of the column.

for(i in 1:ncol(df)){
  df[is.na(df[,i]), i] <- mean(df[,i], na.rm = TRUE)
}


#Displaying the frequency table of "Class" vs. F6
ftable(df$Class,df$F6)

#Displaying the scatter plot of F1 to F6, one pair at a time
pairs(df[2:7],main = "Breast Cancer W",
      pch = 21,bg =c("red","blue")[factor(df$Infected)])

#Show histogram box plot for columns F7 to F9
boxplot(df[8:10])

#Delete all the objects from your R- environment.
rm(list=ls())

#Reload the "breast-cancer-wisconsin.data.csv" from canvas into R.
dataset1<-read.csv('breast-cancer-wisconsin.data.csv',na.string="?")
nrow(dataset1)

#Remove any row with a missing value in any of the columns.
cleandataset<-na.omit(dataset1)
nrow(cleandataset)

