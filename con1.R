library(tibble)
library(readr)
library(lubridate)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(plotly)
sessionInfo()
# 데이터 1()
all <- read_csv("data/all_fit.csv")#데이터 추가
alldata <- na.omit(all)# na 삭제

# 데이터 2 (운동처방)
health <- read_csv("data/fit_health.csv")
# 프레임 지우기 
healthdata <- select(health, -(입력구분:상장구분))
운동처방 = count(healthdata, 운동처방)
# 필터
운동 <- all %>%
  group_by(나이구분, 운동처방)%>%
  summarise(개수=n())

# 데이터 3 (항목)
item <- read_csv("data/fit_item.csv")
# 프레임 지우기                  
itemdata <- select(item,-(회차),-(입력구분:상장구분),-(나이구분:측정구분),-(윗몸올리기:미정), -(일리노이:협응력계산결과값),-(의자에앉았다일어서기:보행8자), -(트레드밀_안정시:체공시간2),-(피두부겹합))
# 결측값제거
itemdata <- na.omit(daitem)
#연속형 범주 (bml, 나이대)
attach(itemdata)
itemdata <- transform(itemdata, 
      bml분류 = ifelse(BMI < 18.5, "저체중",
                ifelse(BMI >= 18.5 & BMI <  22.9, "정상",
                ifelse(BMI >= 23.0 & BMI <  24.9, "과체중", "비만"))),
      연령대  = ifelse(측정나이 < 20, "10대",
                ifelse(측정나이  >= 20 & 측정나이 < 30,"20대",
                ifelse(측정나이  >= 30 & 측정나이 < 40,"30대", 
                ifelse(측정나이  >= 40 & 측정나이 < 50,"40대",           
                ifelse(측정나이  >= 50 & 측정나이 < 60,"50대","60대"))))))
# 막대
table(itemdata$bml분류, useNA = "ifany")
qplot(itemdata$bml분류)

table(itemdata$bml분류, useNA = "ifany")
pie(itemdata$bml분류)


#원그래프
연령bml <-itemdata %>%
  group_by(연령대, bml분류)%>%
  summarise(개수=n())%>%
  mutate(lab.ypos = cumsum(개수) - 0.5*개수)
연령bml

mycols <- c("#0073C2FF", "#EFC000FF", "#868686FF", "#CD534CFF")

ggplot(연령bml, aes(x = 2, y = 개수,fill = bml분류)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  geom_text(aes(y = lab.ypos, label =개수), color = "white")+
  scale_fill_manual(values = mycols) +
  theme_void()




# 데이터 4 (기간)

time <- read_csv("data/fit_time.csv")

# 데이터 5 (지역)
zone <- read_csv("data/fit_zone.csv")

