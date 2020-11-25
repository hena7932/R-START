library(tibble)
library(readr)
library(lubridate)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(plotly)
sessionInfo()
# 데이터 1(추천운동)
all <- read_csv("data/국민연령운동.csv") # 데이터 추가
#영어열 한국어로 바꿈
colnames(all) = c("연령", "BMI지수등급", "성별", "상장구분", "운동단계명", "구분별추천운동순위", "추천운동")

연령BMI <- all %>%
  group_by(연령, BMI지수등급)%>%
  summarise(개수=n())

ffic = count(all, BMI지수등급)

연령성별 <- all %>%
  group_by(연령, 성별)%>%
  summarise(개수=n())















# 데이터 2(걷기분석)
walk <- read_csv("data/국민유형걷기.csv") # 데이터 추가
#영어열 한국어로 바꿈
colnames(walk) = c("연령", "성별", "주차수", "일별차수", "측정인원수", "평균걸음수")

연령 <- walk %>%
  group_by(연령, 평균걸음수)
 
