library(tibble)
library(readr)
library(readxl)
library(lubridate)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(plotly)
sessionInfo()

readr::guess_encoding("data/pop.csv",  n_max = 100) # 정보 
pop <- read.csv("data/pop.csv", header = T, fileEncoding = "EUC-KR")

palette_2 <- c('여'='#f08080', '남'='#66cdaa')
#전체인구(남,녀)
ggplot(data = pop, aes(x = 지역, y = 인구수, fill = 성별)) +
  geom_bar(stat='identity') +
# 배경을 흰색으로 설정
  theme_bw() +
    # 색상
    scale_fill_manual(values=palette_2) +
    # 그래프 타이틀 설정
    ggtitle("지역별, 인구조사") +
    # x축 제목 설정
    xlab("지역") +
    # y축 제목 설정
    ylab("수") +
    # y축 간격 및 데이터에 대한 세자리 콤마 적용
    scale_y_continuous(breaks=seq(0, 860000, 50000)) +
    # 각 텍스트의 색상, 크기, 각도, 글꼴 설정
    theme(plot.title=element_text(family="NanumGothic", color="#0066ff",
                                  size=15, face="bold", hjust=0.5),
          axis.title.x=element_text(family="NanumGothic", color="#999999",
                                    size=10, face="bold"),
          axis.title.y=element_text(family="NanumGothic", color="#999999",
                                    size=10, face="bold"),
          axis.text.x=element_text(family="NanumGothic", color="#000000",
                                   size=15, angle=0),
          axis.text.y=element_text(family="NanumGothic", color="#000000",
                                   size=5, angle=0))
#########
#남녀 만
ggplot(data = pop, aes(x = 지역, y = 인구수, fill = 성별)) +
  geom_col(position = "dodge") +
# 배경을 흰색으로 설정
theme_bw() +
  # 색상
  scale_fill_manual(values=palette_2) +
  # 그래프 타이틀 설정
  ggtitle("지역별, 인구조사") +
  # x축 제목 설정
  xlab("지역") +
  # y축 제목 설정
  ylab("수") +
  # y축 간격 및 데이터에 대한 세자리 콤마 적용
  scale_y_continuous(breaks=seq(0, 450000,100000)) +
   # 각 텍스트의 색상, 크기, 각도, 글꼴 설정
   theme(plot.title=element_text(family="NanumGothic", color="#0066ff",
                                 size=20, face="bold", hjust=0.5),
         axis.title.x=element_text(family="NanumGothic", color="#999999",
                                   size=10, face="bold"),
         axis.title.y=element_text(family="NanumGothic", color="#999999",
                                   size=10, face="bold"),
         axis.text.x=element_text(family="NanumGothic", color="#000000",
                                  size=12, angle=0),
         axis.text.y=element_text(family="NanumGothic", color="#000000",
                                  size=8, angle=0)) 
  
