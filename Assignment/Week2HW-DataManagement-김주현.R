# 0405_2nd week_HW


setwd("D:/Unicorn/Data")
getwd()

install.packages("data.table")
install.packages("ggplot2")
install.packages("gcookbook")

library(data.table)
library(ggplot2)
library(gcookbook)


## 2. cowNA_data에서 결측치가 발생한 곳에 평균 값을 집어넣은 새로운 열을 만드세요

cowNA <- as.data.frame(fread("cowNA_data.csv", encoding = "UTF-8"))
head(cowNA)
str(cowNA)

summary(cowNA$weight)

mean_weight <- mean(cowNA$weight, na.rm = T)

cowNA$weight.fix <- ifelse(is.na(cowNA$weight), mean_weight, cowNA$weight)

summary(cowNA$weight.fix)


  
## 3. co 데이터에서 taste 이라는 새로운 열을 추가하고
##    grade가 3,2,1 순서대로 새로운 taste 열을 만들어
##    이곳에 등급에 따라"normal","good","best" 을 작성해주세요 

co <- as.data.frame(fread("co.csv", encoding = "UTF-8"))
head(co)
str(co)

co$taste <- ifelse(co$grade >= 3, "normal", ifelse(co$grade >= 2, "good", "best"))



## 4. co 데이터에서 결측치 행을 지운 새로운 데이터 co1을 만들고
##    결측치가 없어진 co1의 price와 grade의 관계를 구하세요(ex : 회귀,corr 등)
  
summary(co$price)

co1 <- co[complete.cases(co),] # co 데이터에서 price 변수의 결측치만 제거하여 새로 만든 데이터

head(co1)

### 회귀분석
reg <- lm(formula = price~grade , data = co1)

### 산점도
plot(co1$grade, co1$price)
abline(coef(reg)) # 산점도에 회귀직선을 덧그리기

### 상관계수
corr <- co1[,3:4]
head(corr)

cor(corr)

#### 상관계수를 구하였더니 약 -0.73 이 나왔다. 
#### 이를 통해 두 grade 와 price 변수 사이에는 음의 상관관계가 있음을 알 수 있다.



## 5. co1 데이터에서 패키지 ggplot2를 이용하여
##    가격과 등급의 산점도(크기 0.5의 파란색 point)를 그리세요

sp <- ggplot(co1, aes(x = grade, y = price)) 
sp + geom_point(size = 0.5, colour="blue")


## 5-1(회귀 분석을 수강하신 분만)
##    co1 데이터에서 ggplot2과gcookbook패키지를 이용하여
##    가격과 등급의 산점도와 회귀식 모델선을 추가하여 그리세요(99% 확률)

sp + geom_point() + stat_smooth(method=lm, level = 0.99)



## 7. co1 데이터에서 price를 cut function을 사용하여
##    3개의 범주로 나누고 '저가' ,'중가' ,'고가'로 표시된
##    groups라는 새로운 열을 만드세요 (범주기준 : [6000 ~9000),[9000~12000),[12000~15200) )

breaks <- c(6000, 9000, 12000, 15200)
group_name <- c("저가", "중가", "고가")

co1$groups <- cut(co1$price, breaks = breaks, label = group_name, include.lowest = T)

