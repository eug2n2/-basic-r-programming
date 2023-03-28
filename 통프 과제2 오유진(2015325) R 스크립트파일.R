# 통프 과제2 오유진 (2015325) 
# 1번
#1(1)
Beer <- c(3,4,1,1,3,4,3,3,1,3,2,1,2,1,2,3,1,1,1,1,4,3,1,2)
Beer[Beer==1] = "domestic can" 
Beer[Beer==2] = "domestic bottle" 
Beer[Beer==3] = "microbrew"
Beer[Beer==4] = "import"
#1(2)
barplot(table(Beer))
pie(table(Beer))
# 2번
# 2(1)
score<-c(90, 85, 73, 80, 85, 65, 78, 50, 68, 96)
names(score)<-c("KOR", "ENG", "MATH", "HIST", "SOC", "MUSIC", "BIO", "EARTH", "PHY", "ART")
score # 2(2)
mean(score) #2(3)
median(score) #2(4)
sd(Score) #2(5)
#2(6)
index <- which.max(score)
names(score[index])
boxplot(score) #2(7) 이상점은 없다
hist(score,main="Hong's Score" ,col="purple") #2(8)
#3번
boxplot(airquality$Month,airquality$Temp,xlab="month")

