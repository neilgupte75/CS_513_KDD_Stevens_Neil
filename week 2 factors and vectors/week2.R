?is.vector
x1<-c(10,5,6,6,6,7)
print(x1)
mode(x1)
hello<-"hello world"
mode(hello)
x2<-c(1,2,3,4)

x3<-x1+x2
print(x3)
x2<-c(1,2,3)
?seq()
x4<-seq(from=10.2,to=26.5,by=1)
print(x4)
typeof(x4)

x5<-seq(from=1,to=6,by=1)
print(x5)

x6<-1:6
print(x6)
typeof(x6)

x7<-as.integer(x5)
typeof(x7)



mixed_vector<-c(1,2,8.5,'5')
mode(mixed_vector)
typeof(mixed_vector)


ls()
?rm()

rm(list=ls())

myfirstname<-"Neil"
print(myfirstname)
mylastname<-"Gupte"
print(mylastname)


myfirstlast<-c(myfirstname,mylastname)
print(myfirstlast)



rm(myfirstname)

myfirstname<-"Atharva"
avector<-c(1,2,3,4)
avector

typeof(avector)

names(avector)<-c("one","two","three","four")
avector

typeof(avector)

elementnames<-names(avector)




is.vector(avector)


?factor()

cat<-c("good","bad","good","bad","good","bad")

typeof(cat)

cat2<-factor(cat)
typeof(cat2)

cat3<-factor(cat,levels=(c("good","bad")))
cat3

catnum<-as.numeric(cat3)
catnum



days<-c("mon","tues","wed","thurs","fri")
days_fac<-factor(days)

