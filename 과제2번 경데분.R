#경데분 과제 2
library(dplyr)
library(psych)
library(readr)
bike <- read_csv("bike.csv", col_names = T)
#문제 1
bike <- bike %>% mutate(eng = case_when(engine < 14~"C", engine < 19.5~"B", engine < 24~"A", engine >= 24~"S"))
library(descr)
freq(bike$eng) 

#문제 3
bike_C <- bike %>% filter(eng == "C")
bike_B <- bike %>% filter(eng == "B")
bike_A <- bike %>% filter(eng == "A")
bike_S <- bike %>% filter(eng == "S")
shapiro.test(bike_C$price)
shapiro.test(bike_B$price)
shapiro.test(bike_A$price)
shapiro.test(bike_S$price)
#문제 4
install.packages("car")
library(car)
leveneTest(price~eng, data = bike)
#문제 5
oneway.test(price~eng, data = bike)
#문제6
dunn.test(bike$price, bike$eng, method = "bonferroni")
