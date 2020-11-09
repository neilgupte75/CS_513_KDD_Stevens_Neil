data(iris)
View(iris)
length(iris)
nrow(iris)

is.data.frame(iris)

?read.csv()
?factor

avector<-c(1,2,3,4)
typeof(avector)
second<-avector[2]
second
names(avector)<-c("first","second","third","fourth")
second<-avector["second"]


avector
avector[2]<-22
avector
my.lst<-list(stud.id=34445,stud.name=c("neil","asdads"),stud.marks=c(2,1,2))
my.lst[2]
typeof(my.lst[2])



my.lst[[2]]
typeof(my.lst[[2]])
is.vector(my.lst[[2]])





element2<-my.lst[[2]]
ln<-my.lst[[2]][2]
rm(list=ls())



data()
data(iris)
View(iris)
iris[5,4]
iris[,-3]#everything except 3 column

iris[-3,] #everything except 3 row

iris[c(13,5,10),c(5,2,4)]

subset4<-iris[c(T,F,F,F),]


?sample
sample(160,60)

idx<-sample(nrow(iris),as.integer(.65*nrow(iris)))
training<-iris[idx,]
test<-iris[-idx,]

idx<-seq(1,nrow(iris),5)
training<-iris[-idx,]
test<-iris[idx,]


mnnorm<-function(x,minx,maxx)
{
  z<-((x-minx)/(maxx-minx))
  return(z)
}
vec<-1:20
mnnorm(vec,1,20)





