library(tibble)
library(readr)
library(lubridate)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(plotly)
library(showtext)
library(corrplot)
sessionInfo()

font_add_google('Nanum Pen Script', 'NanumGothic')#글씨체 수정
showtext_auto() # 글씨체 불러오기
mycols <- c("#0073C2FF", "#EFC000FF", "#868686FF", "#CD534CFF")


# 데이터 1(금액)
monny <- read_csv("data/장애인스포츠강좌금액.csv")#데이터 추가
mon <- select(monny,-(강좌상세내용:용품판매여부),-(장애타입명),-(종료시각:가능요일구분))
mon <- na.omit(mon)# na 삭제

glimpse(mon)


mycols <- c("#0073C2FF", "#EFC000FF", "#868686FF", "#CD534CFF")



# 데이터 2 (비율)
rate <- read_csv("data/장애인스포츠비율.csv")#데이터 추가
rat <- select(rate,-(강좌종목명))
rat_운동 <- select(rat,-(수요운동종목소분류명),-(수요운동종목지수값:강좌종목비율))

# 상관분석
cor.test(rat$운동자자수요비율, rat$경험자시간여유선호비율)
cor.test(rat$경험자시간여유선호비율, rat$운동경험자금전여유선호비율)
cor.test(rat$운동경험자금전여유선호비율, rat$운동경험자종목선호비율)
cor.test(rat$운동자자수요비율, rat$운동경험자종목선호비율)

rat_cor <- cor(rat_운동)
round(rar_cor, 2)
corrplot(rat_cor)


summary(rat_운동)

rat1 <- select(rat_운동,-(수요운동종목대분류명),-(경험자시간여유선호비율:운동경험자금전여유선호비율))
summary(rat1)
boxplot(rat1, col=rainbow(2), ylim=c(-0.5,2), ylab="비율")







glimpse(비율)

평균 <- rat_운동 %>% # 종목별 평균내기(수요, 종목)
  group_by(수요운동종목대분류명)%>%
  summarise(운동자선호수요=mean(운동자자수요비율,2)*100)
  
평균종목 <- rat_운동 %>%
  group_by(수요운동종목대분류명)%>%
  summarise(운동자선호종목=mean(운동경험자종목선호비율,2)*100)

운동종목평균 <- merge(평균, 평균종목, by="수요운동종목대분류명")#데이터합하기

options(repr.plot.width=10, repr.plot.height=10, warn=1)

ggplot(data=평균) +
  # x축은 지정 안함, y축은 값의 종류, fill은 각 종류별 빈도
  geom_col(aes(x="", y=운동자선호수요, fill=수요운동종목대분류명), width = 1, color = "white") +
  # y축을 기준으로 회전시킴 --> 파이그래프 표현
  coord_polar('y', start = 0) + 
  # 배경을 흰색으로 설정
  theme_bw() +
  # Add color scale (hex colors)
  scale_fill_manual(values=c("#55DDE0", "#33658A", "#2F4858", "#F6AE2D", "#F26419", "#999999")) 
  # 그래프 타이틀 설정
  ggtitle("운동종목대분류") +
  # x축 제목 설정 --> 표시안함을 위해 빈 문자열 설정
  xlab("") +
  # y축 제목 설정 --> 표시안함을 위해 빈 문자열 설정
  ylab("") +
  theme(
    # 각 텍스트의 색상, 크기, 각도, 글꼴 설정
    plot.title=element_text(family="NanumGothic", color="#0066ff", size=24, face="bold", hjust=0.5),
    # 범주의 제목 표시 안함
    legend.title = element_blank(),
    # 범주의 각 항목별 텍스트
    legend.text=element_text(family="NanumGothic", face="bold", size=16, color="#330066"),
    # 범주의 배경상자 여백
    legend.box.margin = margin(1, 1, 1, 1),
    # 범주를 구분하는 각 색상 박스에 대한 크기
    legend.key.size = unit(0.5,"cm"))
  
  ggplot(data=평균종목) +
    # x축은 지정 안함, y축은 값의 종류, fill은 각 종류별 빈도
    geom_col(aes(x="", y=운동자선호종목, fill=수요운동종목대분류명), width = 1, color = "white") +
    # y축을 기준으로 회전시킴 --> 파이그래프 표현
    coord_polar('y', start = 0) + 
    # 배경을 흰색으로 설정
    theme_bw() +
    # Add color scale (hex colors)
    scale_fill_manual(values=c("#55DDE0", "#33658A", "#2F4858", "#F6AE2D", "#F26419", "#999999")) 
  # 그래프 타이틀 설정
  ggtitle("운동종목대분류") +
    # x축 제목 설정 --> 표시안함을 위해 빈 문자열 설정
    xlab("") +
    # y축 제목 설정 --> 표시안함을 위해 빈 문자열 설정
    ylab("") +
    theme(
      # 각 텍스트의 색상, 크기, 각도, 글꼴 설정
      plot.title=element_text(family="NanumGothic", color="#0066ff", size=24, face="bold", hjust=0.5),
      # 범주의 제목 표시 안함
      legend.title = element_blank(),
      # 범주의 각 항목별 텍스트
      legend.text=element_text(family="NanumGothic", face="bold", size=16, color="#330066"),
      # 범주의 배경상자 여백
      legend.box.margin = margin(1, 1, 1, 1),
      # 범주를 구분하는 각 색상 박스에 대한 크기
      legend.key.size = unit(0.5,"cm"))
  
  









