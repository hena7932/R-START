library(tibble)
library(readr)
library(readxl)
library(lubridate)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(plotly)
sessionInfo()
# 데이터 1()
all <- read_csv("data/all_fit.csv")#데이터 추가
datall <- na.omit(all)# na 삭제

# 데이터 2 (운동처방)
health <- read_csv("data/fit_health.csv")
dataitem <- na.omit(daitem)
# 데이터 3 (항목)
item <- read_csv("data/fit_item.csv")
# 프레임 지우기                  
daitem <- select(item,-(윗몸올리기:미정), -(일리노이:협응력계산결과값),-(의자에앉았다일어서기:보행8자), -(트레드밀_안정시:체공시간2),-(피두부겹합))
# 결측값제거
dataitem <- na.omit(daitem)
# 데이터 4 (기간)
time <- read_csv("data/fit_time.csv")
# 데이터 5 (지역)
zone <- read_csv("data/fit_zone.csv")

