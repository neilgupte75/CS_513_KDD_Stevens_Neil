my.lst<-list(stud.id=34445,stud.name=c("neil","asdads"),stud.marks=c(2,1,2))
my.lst
is.list(my.lst)
mode(my.lst)
typeof(my.lst)
length(my.lst)

my.lst2<-list(seq=1:10,my.lst)
my.lst2

length(my.lst2)

my.dataset<-data.frame(site=c('A','B','C','D','E'),ph=c(1,2,3,4,5))
length(my.dataset)
is.list(my.dataset)
is.matrix(my.dataset)
is.data.frame(my.dataset)
typeof(my.dataset)
View(my.dataset)



