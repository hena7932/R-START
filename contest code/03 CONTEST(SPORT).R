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
options(scipen=999)# 범례숫자정확하게나오기 
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
summary(sogi)
  
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
  xlab("") +
  # y축 제목 설정
  ylab("") +
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
                                 size=20, angle=0),
        axis.text.y=element_text(family="NanumGothic", color="#000000",
                                 size=15, angle=0)) +
  # 범주 설정
  theme(legend.title = element_blank(),
        legend.text = element_text(face="bold", size=20, color="#330066"),
        legend.key = element_rect(color="black", fill="white"),
        legend.key.size = unit(1,"cm"),
        legend.box.background = element_rect(fill="skyblue"),
        legend.box.margin = margin(5, 5, 5, 5),
        legend.position = c(0.10, 0.90))  


# 데이터 3  추가   

peple <- read_csv("data/장애인수.csv")
str(peple)

treemap(peple, # 데이터 입력           
        index=c("시도명"), # 계층 구조 표현            
        vSize="소계", # 각 모양 크기         
        vColor="소계", # 각 모양  컬러         
        type="value", # 각 모양  색깔 방법         
        bg.labels="yellow") # 전체 배경색

head(peple)
summary(peple)

# 데이터 1(금액)
monny <- read_csv("data/장애인스포츠강좌금액.csv")#데이터 추가
mon <- select(monny,-(강좌상세내용:용품판매여부),-(장애타입명),-(종료시각:가능요일구분))
mon <- na.omit(mon)# na 삭제


# 데이터 4 (비율)
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




















treemap(peple, # 데이터 입력           
        index=c("시도명","시군구"), # 계층 구조 표현            
        vSize="소계", # 각 모양 크기         
        vColor="소계", # 각 모양  컬러         
        type="value", # 각 모양  색깔 방법         
        bg.labels="yellow") # 전체 배경색