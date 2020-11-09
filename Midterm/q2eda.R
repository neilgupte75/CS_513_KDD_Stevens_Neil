#  Course          :CS 513 Data Mining
#  First Name      : Neil 
#  Last Name       : Gupte
#  Id              : 10445674
#  purpose         : q2 Midterm

#clears memory cache
rm(list=ls())

getwd()
?read.csv
#loading dataset
dataset<-read.csv('COVID19_v3.CSV')
#min.max,median,summary of each column
summary(dataset)
typeof(dataset)
#creates a dataframe
df<- data.frame(dataset)
View(df)
summary(df)
#sum of all missing values
sum(is.na(df))
colSums(is.na(df))
#frequency table
ftable(df$MaritalStatus,df$Infected)

#scatterplot
pairs(df[c(2,4,6)], main = "Covid Infected ",
      pch = 22, bg = c("yellow","red")[factor(df$Infected)])

#boxplot 
boxplot(df[c(2,4,6)])

#change missing values to mean 
df$Cases[is.na(df$Cases)] <- round(mean(df$Cases, na.rm = TRUE))