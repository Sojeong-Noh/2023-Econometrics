#계량경제학강의 - 한치록
# 2023 01 19

# set working directory
setwd("C:/Users/re970/Desktop/계량경제학강의 & R")

# load the library
library(readxl)
library(tidyverse)
library(ggplot2)
library(MASS)
library('ggrepel')

library(loedata)

install.packages('writexl')     
library(writexl)

#계량경제학용 load the library
install.packages("Ecdat")
install.packages("AER")
install.packages("lmtest")
install.packages("sandwich")


#----------------------------------------------------------------
#PART 3.0 소득과 환경오염 - 환경 쿠즈네츠 곡선 가설
#--------------------------------------------------------------
data(Ekc)

names(Ekc)#dataset 내의 변수명
nrow(Ekc) #dataset의 표본크기

head(Ekc) # 처음 몇 관측치
tail(Ekc) # 마지막 몇 관측치

summary(Ekc) #dataset의 요약정보

plot(co2pc ~ gdppcppp, data=Ekc)


#----------------------------------------------------------------
#PART 3.1 우리나라 군별 공무원수와 재정자립도 
#--------------------------------------------------------------

##Import Data
serv <- read.csv("C:/Users/re970/Desktop/계량경제학강의 & R/serv.csv")
data(Pubserv, package="loedata")
nrow(Pubserv) #dataset의 표본크기

names(Pubserv)
summary(Pubserv) # 공무원의 비율(servpc), 재정자립도(finind)

plot(finind~servpc, data=Pubserv, subset=servpc<28)

#------------------------------------------------------------------------------

#최소제곱법과 푀소절대편차법에 따른 두 직선그리기
install.packages("quantreg")
library(quantreg)

serv1 <- serv[serv$servpc<28, ]
plot(finind~servpc, data= serv1) #그래프 그리기
plot(finind~servpc, data= serv1, pch=19) #문자바꿔서 그래프 그리기

abline(lm(finind~servpc, data=serv1), lty=1) #최소제곱법에 따른 직선(실선)
abline(rq(finind~servpc, data=serv1), lty=2) #최소절대편차법에 따른 직선(점선)


#----------------------------------------------------------------
#PART 3.2 교육수준과 임금
#--------------------------------------------------------------
##Import Data
klips <- read.csv("C:/Users/re970/Desktop/계량경제학강의 & R/klips11subset.csv")
data(Klips, package="loedata")
Klips2 <- subset(Klips, regular==1 & married==1)
nrow(Klips2)

head(Klips2)
names(Klips2)
summary(Klips2)

plot(labinc~educ, data =Klips2, log ='y')
#소득에 로그를 취한 효과, 좌표측에는 원래 소득 값 자체 표기
#y는 labinc(연간 근로소득), x는 educ(교육수준)

plot(log(labinc)~educ, data =Klips2) #log(labinc) 로그 연간 근로소득
Klips2$mloginc <- with(Klips2, ave(log(labinc), educ, FUN=mean))
points(mloginc~educ, data=Klips2, pch=19, cex=2)

#-----------------------------------------
#3.4 짧은 예제들 - 공무원 비율과 재정자립도
#-----------------------------------------
Pubserv1 <-  subset(Pubserv, servpc<28)
nrow(Pubserv1)
names(Pubserv1)

lm(log(finind)~log(servpc), data=Pubserv1)

x <- log(Pubserv1$servpc)
y <- log(Pubserv1$finind)
xd <- x-mean(x)
b1 <- sum(xd*y)/sum(xd^2)
b0 <- mean(y)-b1*mean(x)
c(b0,b1)


#--------------------------------------
# 교육수준과 연간 근로소득
#--------------------------------------
ols <- lm(log(labinc)~educ, data =Klips2) # OLS추정을 한다
ols #결과를 화면에 보인다.

plot(log(labinc)~educ, data=Klips2) #자료의 산포도 그림을 그니다
abline(ols) #OLS 회귀결과를 덧그린다

#--------------------------------------
# 고령인구비율과 흡연율
#--------------------------------------
data(Death, package= "loedata")
reg <- lm(smoke~aged, data=Death, subset=year==2010) #fit a linear regression model to the dataset
reg
##aged:-0.2286, "고령인구비율이 10%point 높은 지역의 흡연인구비율이 약 2.3%포인트 낮다"

plot(smoke~aged, data=Death, subset=year==2010)
abline(reg)







