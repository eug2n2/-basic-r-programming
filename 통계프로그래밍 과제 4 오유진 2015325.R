#1
setwd("D:")
Employee <- read.csv("Employee.csv",header=T,stringsAsFactors = T)
#1.1
#1.1(1)
t.test(Employee$salbegin, mu=16000, alternative = "two.sided")
#P값이 유의수준 0.05보다 작으므로 귀무가설 '초봉은 16000이다'는 기각할 수 있다. 따라서 초봉은 16000이라고 할 수 없다. 
#1.1(2)
Lsal<-log(Employee$salbegin)
#(분석 전)가설검정
with(Employee,tapply(Lsal,Employee$gender,shapiro.test))#정규성 검정; 남성,여성 모두 p값이 0.05보다 작으므로 귀무가설을 기각할 수있다. 따라서 정규성을 따른다고 볼 수 없다.
var.test(Lsal~Employee$gender, data=Employee)#등분산성 검정; p값이 0.05보다 작으므로 귀무가설을 기각할 수있다. 따라서 등분산이라고 보기 어렵다.
by(Lsal,Employee$gender,mean) #독립표본 평균비교; 남성의 평균 초봉이 여성의 평균 초봉보다 약간 더 높다. 
#1.1(3)
Lsa<-log(Employee$salary)
#분석 전 가설검정
t.test(Lsal,Lsa,) #짝표본 평균비교
#1.2
boxplot(Employee$salbegin,Employee$salary)$stats #이상점확인
Employee$salbegin <- ifelse(Employee$salbegin < 9000 | Employee$salbegin > 25000, NA, Employee$salbegin)
Employee$salary <- ifelse(Employee$salary < 15750 | Employee$salary > 56550, NA, Employee$salary)
#1.2(1)
t.test(Employee$salbegin, mu=16000, alternative = "two.sided",na.rm=T)
#1.2(2)
Lsal<-log(Employee$salbegin)
#(분석 전)가설검정
with(Employee,tapply(Lsal,Employee$gender,shapiro.test))#정규성 검정; 모든 성별의 p값이 0.05보다 작으므로 귀무가설을 기각할 수있다. 따라서 정규성을 따른다고 볼 수 없다.
var.test(Lsal~Employee$gender, data=Employee)#등분산성 검정; p값이 0.05보다 작으므로 귀무가설을 기각할 수있다. 따라서 등분산이라고 보기 어렵다.
by(Lsal,Employee$gender,mean,na.rm = T) #독립표본 평균비교; 남성의 평균 초봉이 이상점 제거 전보다 조금 낮아져, 두 평균값이 비슷하다 . 
#1.2(3)
Lsa<-log(Employee$salary)
t.test(Lsal,Lsa) 

#2
setwd("D:")
imports <- read.csv("imports-85.data",header=F,stringsAsFactors = T)
boxplot(imports) #이상점 유무확인 ; 이상점있음
import <- subset(imports, V8!="fwd")
import$V26<-as.numeric(import$V26)
LV26<-log(import$V26)
#가설 검정
with(import,tapply(LV26,import$V8,shapiro.test))#정규성 검정; 4wd, rwd 모두 p값이 0.05보다 작으므로 귀무가설을 기각할 수있다. 따라서 정규성을 따른다고 볼 수 없다.
var.test(LV26~import$V8, data=import)#등분산성 검정; p값이 0.05보다 작으므로 귀무가설을 기각할 수있다. 따라서 등분산이라고 보기 어렵다.
#2(1)
by(LV26,import$V8,mean) # 4wd와 rwd의 각각의 평균은 별 차이가 안난다고 볼 수있다.
#2(2)
t.test(LV26~import$V8,data=import)

#3
setwd("D:")
Employee <- read.csv("Employee.csv",header=T,stringsAsFactors = T)
#3(1)
#분석 전 가설 검정 
with(Employee,tapply(salary,Employee$jobcat,shapiro.test))#정규성 검정; 모든 직종의 p값이 0.05보다 작으므로 귀무가설을 기각할 수있다. 따라서 정규성을 따른다고 볼 수 없다.
bartlett.test(Employee$salary~Employee$jobcat, data=Employee) #등분산성 검정; p값이 0.05보다 작으므로 귀무가설을 기각할 수있다. 따라서 등분산이라고 보기 어렵다.
by(Employee$salary,Employee$jobcat,mean) #평균비교;경영자의 연봉이 나머지 두 직급보다 월등히 높고 관리직과 사무직의 연봉의 평균은 비슷하다. 
pairwise.t.test(Employee$salary,Employee$jobcat)#사후검정; 경영자와 관리자, 경영자와 사무직의 p값이 0.05보다 작으므로 경영자와 나머지 두 직급과 연봉 차이가 있다고 볼 수 있다.
#3(2)
#분석전 가설 검정정
with(Employee,tapply(salbegin,Employee$jobcat,shapiro.test))#정규성 검정; 모든 직종의 p값이 0.05보다 작으므로 귀무가설을 기각할 수있다. 따라서 정규성을 따른다고 볼 수 없다.
bartlett.test(Employee$salbegin~Employee$jobcat, data=Employee) #등분산성 검정; p값이 0.05보다 작으므로 귀무가설을 기각할 수있다. 따라서 등분산이라고 보기 어렵다.
by(Employee$salbegin,Employee$jobcat,mean) #평균비교;경영자의 초봉이 나머지 두 직급보다 월등히 높고 관리직과 사무직의 초봉의 평균은 비슷하다. 
pairwise.t.test(Employee$salbegin,Employee$jobcat)#사후검정; 경영자와 관리자, 경영자와 사무직의 p값이 0.05보다 작으므로 경영자와 나머지 두 직급과 초봉 차이가 있다고 볼 수 있다.
#3(3)
emm <- subset(Employee, gender == "남성") 
by(emm$salbegin,emm$jobcat, mean) #남성의 직무별  초봉
emf <- subset(Employee, gender == "여성")
by(emf$salbegin,emf$jobcat,mean) #여성의 직무별 초봉(여성 중 관리직은 없기때문에 결측값이 나오는 거임)
# 성별간 직무별 초봉을 비교했을 때 비교대상이없는 관리직을 제외하고는 경영자, 사무직은 모두 남성의 초봉이 여성의 초봉보다 높다

#4
#4(1)
#가정검정 
with(iris,tapply(iris$Sepal.Length,iris$Species,shapiro.test))#정규성 검정; 남성,여성 모두 p값이 0.05보다 크므로 귀무가설을 기각할 수 없다. 따라서 정규성을 따른다고 볼 수 있다.
bartlett.test(iris$Sepal.Length~iris$Species, data=iris)#등분산성 검정; p값이 0.05보다 작으므로 귀무가설을 기각할 수있다. 따라서 등분산이라고 보기 어렵다.
with(iris,tapply(iris$Petal.Length,iris$Species,shapiro.test))#정규성 검정; 남성,여성 모두 p값이 유의수준 0.05보다 크므로 귀무가설을 기각할 수 없다. 따라서 정규성을 따른다고 볼 수 있다.
bartlett.test(iris$Petal.Length~iris$Species, data=iris) #등분산성 검정; p값이 0.05보다 작으므로 귀무가설을 기각할 수있다. 따라서 등분산이라고 보기 어렵다.
by(iris$Sepal.Length,iris$Species, mean) #vriginica , versicolor, setosa 순으로 Sepal.length가 크다.
by(iris$Petal.Length,iris$Species, mean) #setosa의 petal.length가 다른 두 값에 비해 월등히 작고 virgiinca의 petal.length가 versicolor보다는 좀 크다.
#4(2)
#가정검정
lm_out=lm(iris$Sepal.Length~iris$Petal.Length, data=iris)
plot(lm_out)
#가설검정결과:which=1; 분산 불일치임을 알 수가 있다
# which=2;거의 모든점이 직선위에 점이있기때문에 정규분포를 따른다고 할 수 있다.
# which=3;15,107,132번째 점이 이상점으로 의심된다
# which=4;15,107,132번째 점이 영향을 크게 미치고있다는 것을 알 수 있다. 
plot(iris$Sepal.Length,iris$Petal.Length)
cor(iris$Sepal.Length,iris$Petal.Length) #큰 상관계수로, 굉장히 강한 양의 선형관계를 보임을 알 수 있다.
#4(3)
predict(lm_out, newdata=data.frame(Petal.Length=5))
#4(4)
ggplot(iris,aes(Petal.Length,Sepal.Length))+ geom_smooth(formula = y~x, method='lm',color='black')
#4(5)
#가정검정
lm_ss=lm(iris$Sepal.Length~iris$Species, data=iris)
plot(lm_ss)
#가정검정 분석결과: which1; 등분산임을 알 수 있다.
#which2; 거의 직선에 일치하다고 볼수있으므로 정규분포를 따른다고 할 수 있다
#which3; 107,118,132 번째 값이 이상점일 가능성이 있다
#which4; 107,118,132번째 값이 영향력이 높다는 것을 알 수 있다. 
lm_ps=lm(iris$Petal.Length~iris$Species, data=iris)
plot(lm_ps)
#가정검정 분석결과: which1; 등분산임을 알 수 있다.
#which2; 양 끝 이상점들을 제외하고는 거의 직선에 일치하다고 볼 수 있으므로 정규분포를 따른다고 할 수 있다
#which3; 99,118,119 번째 값이 이상점일 가능성이 있다
#which4; 99,118,119번째 값이 영향력이 높다는 것을 알 수 있다. 
iriss <- subset(iris, Species == "setosa")
irisv <- subset(iris, Species == "versicolor")
iriv <- subset(iris, Species == "virginica")
plot(iriss$Sepal.Length,iriss$Petal.Length, xlab="Sepal.Length", ylab="Petal.Length")
cor(iriss$Sepal.Length,iriss$Petal.Length) #종이 setosa일 때, sepal.length와 petal.length는 0에 가까운 양의값으로 큰 상관관계가 없다고 볼 수 있다
plot(irisv$Sepal.Length,irisv$Petal.Length, xlab="Sepal.Length", ylab="Petal.Length")
cor(irisv$Sepal.Length,irisv$Petal.Length) #종이 versicolor일 때, sepal.length와 petal.length는 1에 가까운 양의 상관계수를 가지므로 강한 양의 상관관계를 가진다고 볼 수 있다
plot(iriv$Sepal.Length,iriv$Petal.Length, xlab="Sepal.Length", ylab="Petal.Length")
cor(iriv$Sepal.Length,iriv$Petal.Length) #종이 virginica일 때, sepal.length와 petal.length는 1에 가까운 양의 상관계수를 가지므로 강한 양의 상관관계를 가진다고 볼 수 있다