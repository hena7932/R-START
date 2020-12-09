library(tibble)
library(readr)
library(lubridate)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(plotly)
library(showtext)
library(corrplot)
library(treemap)
sessionInfo()

font_add_google('Nanum Pen Script', 'NanumGothic')#글씨체 수정
showtext_auto() # 글씨체 불러오기

# 데이터 1(금액)
off <- read_csv("data/시설.csv")#데이터 1  추가

glimpse(off)
offic <- select(off,-(시도코드),-(시군구코드),-(대표자전화번호:시설상세주소),-(주요종목코드)
                ,-(시설좌표경도:시설좌표위도),-(강좌종목명:강좌명),-(시설명))
str(offic)

sogi <- offic %>%  # 경기도만 추출
  filter(시도명 %in% c("서울", "경기"))
  
so <- sogi %>%  # 경기도만 추출
  filter(시도명 %in% c("서울", "경기"))%>%
  filter(주요종목명 %in% c("헬스", "유도", "요가", "수영", "복싱"))%>%
  group_by(시도명,주요종목명) %>%
  summarise(n=n())  
  
gr <- ggplot(data = so) +
  geom_col(aes(x=주요종목명, y=n, fill=factor(시도명)), position='dodge')
gr +
  # 배경을 흰색으로 설정
  theme_bw() +
  # 그래프 타이틀 설정
  ggtitle("서울,경기 종목 ") +
  # x축 제목 설정
  xlab("시도") +
  # y축 제목 설정
  ylab("n") +
  # y축 간격 및 데이터에 대한 세자리 콤마 적용
  scale_y_continuous(breaks=seq(0, 40, 10))  +
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
                                 size=8, angle=0)) +
  # 범주 설정
  theme(legend.title = element_blank(),
        legend.text = element_text(face="bold", size=8, color="#330066"),
        legend.key = element_rect(color="black", fill="white"),
        legend.key.size = unit(0.5,"cm"),
        legend.box.background = element_rect(fill="skyblue"),
        legend.box.margin = margin(5, 5, 5, 5),
        legend.position = c(0.05, 0.93))  


  
  
  

  
  
  
  
  # 데이터 2  추가    
peple <- read_csv("data/장애인수.csv")
str(peple)

treemap(peple, # 데이터 입력           
        index=c("시도명"), # 계층 구조 표현            
        vSize="소계", # 각 모양 크기         
        vColor="소계", # 각 모양  컬러         
        type="value", # 각 모양  색깔 방법         
        bg.labels="yellow") # 전체 배경색

  