#  Course          :CS 513 Data Mining
#  First Name      : Neil 
#  Last Name       : Gupte
#  Id              : 10445674
#  purpose         : hw 8 kmeans



rm(list=ls())
cancer<-read.csv("wisc_bc_ContinuousVar.csv",na.strings = '?')
View(cancer)
summary(cancer)
table(cancer$diagnosis)
#To factor the data set
cancer<-na.omit(cancer)
cancer<-cancer[-1]
kmeans_2<- kmeans(cancer[,-1],2,nstart = 10)
kmeans_2$cluster
table(kmeans_2$cluster,cancer[,1])
