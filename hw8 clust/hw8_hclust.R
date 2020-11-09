#  Course          :CS 513 Data Mining
#  First Name      : Neil 
#  Last Name       : Gupte
#  Id              : 10445674
#  purpose         : hw 8 hclust


rm(list=ls())
cancer<-read.csv("wisc_bc_ContinuousVar.csv",na.strings = '?')
View(cancer)
summary(cancer)
table(cancer$diagnosis)
#To factor the data set
cancer<-na.omit(cancer)
cancer<-cancer[-1]
cancer_dist<-dist(cancer[,-1])
hclust_results<-hclust(cancer_dist)
plot(hclust_results)
hclust_2<-cutree(hclust_results,2)
table(hclust_2,cancer[,1])