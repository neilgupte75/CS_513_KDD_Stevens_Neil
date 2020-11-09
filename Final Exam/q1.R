#  Course          :CS 513 Data Mining
#  First Name      : Neil 
#  Last Name       : Gupte
#  Id              : 10445674
#  purpose         : q1 final exam


rm(list=ls())


df <- read.csv('Admission.csv',na.strings = "?")
head(df, n=5)
#Summarizing each column (e.g. min, max, mean )
summary(df)

#Check the number of rows before
nrow(df)

#Remove the rows with missing values 
df <- na.omit(df)

#Check the number of rows after
nrow(df)

?kmeans
 
View(df)

# kmeans 

kmeans_1<- kmeans(df[,c(-1,-2,-5)],2,nstart = 10)
kmeans_1$cluster
conf_matrix = table(kmeans_1$cluster,df[,2])
print(conf_matrix)


#hclust 
hclust_dist<-dist(df[,c(-1,-2,-5)])
hclust_results<-hclust(hclust_dist)
plot(hclust_results)
hclust_2<-cutree(hclust_results,2)
conf_matrix2=table(hclust_2,df[,2])
print(conf_matrix2)
