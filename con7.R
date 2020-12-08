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
install.packages("treemap")
library(treemap)

str()
treemap(GNI2014, # 데이터 입력           
        index=c("continent","iso3"), # 계층 구조 표현            
        vSize="population", # 각 모양 크기         
        vColor="GNI", # 각 모양  컬러         
        type="value", # 각 모양  색깔 방법         
        bg.labels="yellow") # 전체 배경색




sogi <- offic %>%  # 경기도만 추출
  filter(시도명 %in% c("서울", "경기"))
  
sogi <- offic %>%  # 경기도만 추출
  filter(시도명 %in% c("서울", "경기"))
  group_by(시도명,시군주요종목명) %>%
  summarise(n=n())  
  

  
  
  
  
peple <- read_csv("data/장애인수.csv")#데이터 2  추가  
str(peple)

treemap(peple, # 데이터 입력           
        index=c("시도명"), # 계층 구조 표현            
        vSize="소계", # 각 모양 크기         
        vColor="소계", # 각 모양  컬러         
        type="value", # 각 모양  색깔 방법         
        bg.labels="yellow") # 전체 배경색

  