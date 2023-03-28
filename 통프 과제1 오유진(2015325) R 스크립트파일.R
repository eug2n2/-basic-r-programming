# 과제1 오유진(2015325)
# 1번
25+99 #1(1) 
456-123 #1(2)
2*(3+4) #1(3)
(3+5*6)/7 #1(4)
(7-4)*3 #1(5)
210+35 #1(6)
1256%%7 #1(7)
184%%5 #1(8)
1976/24 #1(9)
16*25 + 186*5- 67*22 #1(10)
# 2번
x = c(6,8,10)
y = 2*x^2 + 5*x + 10
y
# 3번
d <-c(101:200) #3(1)
d   #3(2)
d[10] #3(3)
tail(d,n=10) #3(4)
d[d%%2==0] #3(5)
d.20<-head(d,n=20) #3(6)
d.20 #3(6)
d.20[-5] #3(7)
d.20[-c(5,7,9)] #3(8)
#4번
(d1<-1:50) #4(1)
(d2<-51:100) #4(1)
d1+d2 #4(2)
d2-d1 #4(2)
d1*d2 #4(2)
d2/d1 #4(2)
sum(d1) #4(3)
sum(d2) #4(3)
sum(d1,d2) #4(4)
max(d2) #4(5)
min(d2) #4(5)
mean(d2) #4(6)
mean(d1) #4(6)
abs(mean(d2)-mean(d1)) #4(6)
sort(d1,decreasing=T) #4(7)
d3 <- c(tail(d1,n=10),tail(d2,n=10)) #4(8)
# 5번
v1<-51:90
v1[v1<60] #5(1)
length(v1[v1<70]) #5(2)
sum(v1[v1>65]) #5(3)
v1[v1>60&v1<73] #5(4)
v1[v1<65|v1>80] #5(5)
v1[v1%%7==3] #5(6)
# 6번
score <- matrix(c(10,40,60,20,21,60,70,30),nrow=4,ncol=2) #6(1)
colnames(score)<-c("m","f") #6(1)
score #6(1)
colnames(score)<-c("male","female") #6(2)
score[2,] #6(3)
score[,"female"] #6(4)
score[3,2] #6(5)
# 7번
a<-rnorm(5,mean=0,sd=1) #7(1)
b<-rnorm(4,mean=1,sd=1) #7(2)
c<-rnorm(4,mean=1,sd=0.5) #7(3)
lst<-list(lev1=a,lev2=b,lev3=c) #7(4)
lst1<-list(lev2=b,lev3=c) #7(5)
median(unlist(lst1)) #7(5)
#8번
id <-c(10,20,30,40,50) #8(1)
name <- c("John","Tom","Paul","Jane","Grace") #8(1)
score <- c(95,46,98,74,85) #8(1)
df <- data.frame(id,name,score) #8(1)
df #8(1)
df$score #8(2)
df[,3] #8(2)
df[,"score"] #8(2) 이 세가지 코드의 결과가 모두 일치하는것을 알 수있다.
df[c("id","score")] #8(3)
df[c(2,3),] #8(4)
df[2,3] #8(5)
# 9번
st <- as.data.frame(state.x77) #9(1)
colnames(st) #9(2)
rownames(st) #9(3)
dim(st) #9(4)
summary(st) #9(5)
st["Florida",] #9(6)
st[,"Income"] #9(7)
st["Texas","Area"] #9(8)
st["Ohio",c("Population","Income")] #9(9)
subset(st, Population>=5000) #9(10)
nrow(subset(st,Income>=4500)) #9(11)
abs(mean(st[st$Illiteracy<2.0,"Income"])-mean(st[st$Illiteracy>=2.0,"Income"])) #9(12)
# 10번
#10(1)
ko <- matrix(c(93,76,87,92,98,75,82),nrow = 7,ncol =1 )
rownames(ko) <- c("kim", "lee", "park", "oh", "yang", "min", "jung")
colnames(ko) <- "korean"
ko
en <- matrix(c(90,94,88,75,79,87,88,90),nrow = 8,ncol =1 )
rownames(en) <- c("kim", "lee", "park", "oh", "yang", "min", "jung", "choi")
colnames(en) <- "English"
en
#10(2)
y <- merge(ko, en, by = 'row.names', all = T)
y
y$korean[is.na(y$korean)]= 0
y
ym<-matrix(c(cumsum(y$korean),cumsum(y$English)),nrow=8, ncol=2)
rownames(ym) <- c("kim", "lee", "park", "oh", "yang", "min", "jung", "choi")
colnames(ym) <- c("Korean","English")
ym