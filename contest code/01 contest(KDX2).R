library(tibble)
library(readr)
library(readxl)
library(lubridate)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(plotly)
sessionInfo()

# 데이터 불러오기 
readr::guess_encoding("data/TC.csv",  n_max = 100) # 정보 
peple <- read.csv("data/TC.csv", header = T, fileEncoding = "EUC-KR")
# 데이터 전처리 
#영어열 한국어로 바꿈
colnames(peple) = c("기준년월", "시도명", "시군구명", "행정동명", "업종대분류코드", "업종중분류코드", "업종대분류코드드", "업종중분류코드드", "가공기업구분코드", "가공기업구분", "업력구간코드","업력구간", "총기업수", "종업원수", "종업원평균수", "입사수", "입사평균수", "퇴직수", "퇴직평균수",  "평균임금", "등록일자", "작업자명")
# 프레임 지우기                  
peple2 <- select(peple,-(기준년월), -(가공기업구분코드:업력구간), -(종업원수:작업자명))
#peple3 데이터 내용 합치기(ex = B광업)
peple3 <- paste(peple2$업종대분류코드, peple2$업종대분류코드드, sep = "")
#peple4 프레임에 추가
peple4 <- cbind(peple2, 업종대분류=peple3)
# 프레임 지우기     
peple5 <- select(peple4, -(업종대분류코드), -(업종대분류코드드))  
#######
#데이터 전처리_2
peple10 <- peple2
#데이터 합치기 1
peple11 <- paste(peple10$업종대분류코드, peple10$업종대분류코드드, sep = "") 
#데이터 합치기 2
peple12 <- paste(peple10$업종대분류코드, peple10$업종중분류코드드, sep = "")
# 프레임추가 3
peple13 <- cbind(peple10, 업종대분류=peple11, 업종중분류=peple12)
# 프레임 지우기     
peple14 <- select(peple13,-(업종대분류코드:업종중분류코드),-(업종대분류코드드:업종중분류코드드), -(행정동명))
#데이터전처리  
posh <- peple14 %>% 
  filter(시도명 == "경기")%>%
  filter(시군구명 %in% c("평택시", "안성시", "화성시", "오산시")) %>%
  filter(업종대분류 == "R예술; 스포츠 및 여가관련 서비스업")
##########

glimpse(posh)
#경기만 추출
gugi <- peple5 %>% 
  filter(시도명 == "경기")
#시군구 (경기도)
SD = count(gugi, 시군구명)
SD %>%  
  ggplot(aes(x = reorder(시군구명, -n), y = n)) +
  geom_bar(stat = "identity", position= 'dodge', width=.5, fill= "#026645") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=50,size = 7, face = "bold"),
        axis.text.y = element_text(size =5, face = 'bold'),
        axis.title=element_text(size=10, face='bold'),
        title = element_text(size=10, face='bold'),
        legend.position = "top") +
  labs(title="경기도",
       y="", x= "")
#지역별 하위 5개
#업종 팔레트
oj_palette <- c('B광업'='#FFBB00',
                'D전기; 가스; 증기 및 공기조절 공급업'='#FFE400',
                'A농업; 임업 및 어업'='#FF5E00',
                'R예술; 스포츠 및 여가관련 서비스업'='#0055FF',
                'Q보건업 및 사회복지 서비스업'='#ABF200')

gifour <- peple5 %>% 
  filter(시군구명 %in% c("평택시", "안성시", "화성시", "오산시")) %>%
  filter(업종대분류 %in% c("B광업", "D전기; 가스; 증기 및 공기조절 공급업", "A농업; 임업 및 어업", "R예술; 스포츠 및 여가관련 서비스업", "Q보건업 및 사회복지 서비스업")) %>%
  group_by(시군구명, 업종대분류) %>%
  summarise(n=n())

graph <- ggplot(data = gifour) +
  geom_col(aes(x=시군구명, y=n, fill=factor(업종대분류)), position='dodge')
graph +
  # 배경을 흰색으로 설정
  theme_bw() +
  # 그래프 타이틀 설정
  ggtitle("지역별, 업종대분류조사") +
  # 색상
  scale_fill_manual(values=oj_palette) +
  # x축 제목 설정
  xlab("지역") +
  # y축 제목 설정
  ylab("수") +
  # y축 간격 및 데이터에 대한 세자리 콤마 적용
  scale_y_continuous(breaks=seq(0, 1500, 100))  +
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
        legend.position = c(0.30, 0.85))
  
#업종대분류
offic = count(gugi, 업종대분류)
offic %>%  
  ggplot(aes(x = reorder(업종대분류, -n), y = n)) +
  geom_bar(stat = "identity", position= 'dodge', width=.5, fill= "#00B2D9") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=60, vjust=0.6, size = 6, face = "bold"),
        axis.text.y = element_text(size =5, face = 'bold'),
        axis.title=element_text(size=10, face='bold'),
        title = element_text(size=10, face='bold'),
        legend.position = "top") +
  labs(title="경기도",
       y="", x= "")

#상워10개
top10 <- offic %>%            # 데이터에서
  arrange(desc(n)) %>%   # 올림차순으로 나열
  head(10)                # 상위 10개만 추출 후, 최종적으로 top10 에 저장
top10 %>%
  ggplot(aes(x = reorder(업종대분류, n), y = n)) +
  geom_bar(stat = "identity", position= 'dodge', width=.5, fill= "#EC5A96") +
  theme_minimal() +
  coord_flip() +
  theme(axis.text.x = element_text(vjust=0.4, size = 5, face = "bold"),
        axis.text.y = element_text(size =8, face = 'bold'),
        axis.title=element_text(size=6, face='bold'),
        title = element_text(size=10, face='bold'),
        legend.position = "top") +
  labs(title="업종대분류",
       subtitle="top10",
       y="", x= "업종")

#하위10개(가로)
bottom10 <- offic %>% #하위10개
  arrange(n) %>%
  head(7)
bottom10 %>%
  filter(업종대분류 %in% c("B광업", "D전기; 가스; 증기 및 공기조절 공급업", "A농업; 임업 및 어업", "R예술; 스포츠 및 여가관련 서비스업", "Q보건업 및 사회복지 서비스업")) %>%
  ggplot(aes(x = reorder(업종대분류, n), y = n)) +
  geom_bar(stat = "identity", position= 'dodge', width=.7, fill= "#EC5A96") +
  theme_minimal() +
  coord_flip() +
  theme(axis.text.x = element_text(vjust=0.4, size = 5, face = "bold"),
        axis.text.y = element_text(size =8, face = 'bold'),
        axis.title=element_text(size=6, face='bold'),
        title = element_text(size=10, face='bold'),
        legend.position = "top") +
  labs(title="업종대분류",
       subtitle="top10",
       y="", x= "업종")

#지역별로
#화성
ha <- four %>%
  filter(시군구명 == '화성시') %>%
  select(업종대분류) 

ha = count(ha, 업종대분류)

ha %>%
  ggplot(aes(x = reorder(업종대분류, -n), y = n)) +
  geom_bar(stat = "identity", position= 'dodge', width=.5, fill= "#ECC846") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, vjust=0.6, size = 6, face = "bold"),
        axis.text.y = element_text(size =10, face = 'bold'),
        axis.title=element_text(size=10, face='bold'),
        title = element_text(size=10, face='bold'),
        legend.position = "top") +
  labs(title="화성",
       y="", x= "")
#오산
os <- four %>%
  filter(시군구명 == '오산시') %>%
  select(업종대분류) 

os = count(os, 업종대분류)

os %>%
  ggplot(aes(x = reorder(업종대분류, -n), y = n)) +
  geom_bar(stat = "identity", position= 'dodge', width=.5, fill= "#73B761") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, vjust=0.6, size = 6, face = "bold"),
        axis.text.y = element_text(size =10, face = 'bold'),
        axis.title=element_text(size=10, face='bold'),
        title = element_text(size=10, face='bold'),
        legend.position = "top") +
  labs(title="오산",
       y="", x= "")

#평택
p <- four %>%
  filter(시군구명 == '평택시') %>%
  select(업종대분류) 

p = count(p, 업종대분류)

p %>%
  ggplot(aes(x = reorder(업종대분류, -n), y = n)) +
  geom_bar(stat = "identity", position= 'dodge', width=.5, fill= "#4A588A") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, vjust=0.6, size = 6, face = "bold"),
        axis.text.y = element_text(size =10, face = 'bold'),
        axis.title=element_text(size=10, face='bold'),
        title = element_text(size=10, face='bold'),
        legend.position = "top") +
  labs(title="평택",
       y="", x= "")
#안성
an <- four %>%
  filter(시군구명 == '안성시') %>%
  select(업종대분류) 

an = count(an, 업종대분류)

an %>%
  ggplot(aes(x = reorder(업종대분류, -n), y = n)) +
  geom_bar(stat = "identity", position= 'dodge', width=.5, fill= "#EE9E64") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, vjust=0.6, size = 6, face = "bold"),
        axis.text.y = element_text(size =10, face = 'bold'),
        axis.title=element_text(size=10, face='bold'),
        title = element_text(size=10, face='bold'),
        legend.position = "top") +
  labs(title="안성",
       y="", x= "")

#지역별 하위 10개(ha, os, p, an)
# 화성
hab <- ha %>% 
  arrange(n) %>%
  head(9)
hab %>%
  ggplot(aes(x = reorder(업종대분류, - n), y = n)) +
  geom_bar(stat = "identity", position= 'dodge', width=.5, fill= "#F3C911") +
  ylim(0, 3000) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, vjust = 0.4, size =6, face = "bold"),
        axis.text.y = element_text(size =5, face = 'bold'),
        axis.title=element_text(size=7, face='bold'),
        title = element_text(size=7, face='bold'),
        legend.position = "top") +
  labs(title="화성",
       subtitle="bottom9",
       y="", x= "")

# 오산
osb <- os %>% 
  arrange(n) %>%
  head(9)
osb %>%
  ggplot(aes(x = reorder(업종대분류, - n), y = n)) +
  geom_bar(stat = "identity", position= 'dodge', width=.5, fill= "#33AE81") +
  ylim(0,3000) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, vjust = 0.4, size =6, face = "bold"),
        axis.text.y = element_text(size =5, face = 'bold'),
        axis.title=element_text(size=7, face='bold'),
        title = element_text(size=7, face='bold'),
        legend.position = "top") +
  labs(title="오산",
       subtitle="bottom9",
       y="", x= "")
# 평택
pb <- p %>% 
  arrange(n) %>%
  head(9)
pb %>%
  ggplot(aes(x = reorder(업종대분류, - n), y = n)) +
  geom_bar(stat = "identity", position= 'dodge', width=.5, fill= "#4C5D8A") +
  ylim(0, 3000)+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, vjust = 0.4, size =6, face = "bold"),
        axis.text.y = element_text(size =5, face = 'bold'),
        axis.title=element_text(size=7, face='bold'),
        title = element_text(size=7, face='bold'),
        legend.position = "top") +
  labs(title="평택",
       subtitle="bottom9",
       y="", x= "업종")
# 안성
anb <- an %>%
  arrange(n) %>%
  head(9)
anb %>%
  ggplot(aes(x = reorder(업종대분류, - n), y = n)) +
  geom_bar(stat = "identity", position= 'dodge', width=.6, fill= "#DD915F") +
  ylim(0, 3000) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, vjust = 0.4, size =6, face = "bold"),
        axis.text.y = element_text(size =5, face = 'bold'),
        axis.title=element_text(size=7, face='bold'),
        title = element_text(size=7, face='bold'),
        legend.position = "top") +
  labs(title="안성",
       subtitle="bottom9",
       y="", x= "업종")
# 안성
os %>% 
  arrange(n) %>%
  head(10) 
  ggplot(aes(x = reorder(업종대분류, - n), y = n)) +
  geom_bar(stat = "identity", position= 'dodge', width=.6, fill= "#DD915F") +
  ylim(0, 50000) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, vjust = 0.7, size =6, face = "bold"),
        axis.text.y = element_text(size =5, face = 'bold'),
        axis.title=element_text(size=7, face='bold'),
        title = element_text(size=7, face='bold'),
        legend.position = "top") +
  labs(title="안성",
       subtitle="bottom10",
       y="", x= "업종")
상위5개
  hab <- ha %>% 
    arrange(desc(n)) %>%
    head(5)
  
  hab %>%
    ggplot(aes(x = reorder(업종대분류, n), y = n)) +
    geom_bar(stat = "identity", position= 'dodge', width=.7, fill= "#F3C911") +
    ylim(0, 110000) +
    coord_flip() +# 가로형코드
    theme_minimal() +
    theme(axis.text.x = element_text(size = 6, face = "bold"),
          axis.text.y = element_text(size = 8, face = 'bold'),
          axis.title=element_text(size=7, face='bold'),
          title = element_text(size=7, face='bold'),
          legend.position = "top") +
    labs(title="화성",
         y="", x= "")
  
  # 오산
  osb <- os %>% 
    arrange(desc(n)) %>%
    head(5)
  
  osb %>%
    ggplot(aes(x = reorder(업종대분류,  n), y = n)) +
    geom_bar(stat = "identity", position= 'dodge', width=.7, fill= "#33AE81") +
    ylim(0,12000) +
    coord_flip() +# 가로형코드
    theme_minimal() +
    theme(axis.text.x = element_text(size = 6, face = "bold"),
          axis.text.y = element_text(size = 8, face = 'bold'),
          axis.title=element_text(size=7, face='bold'),
          title = element_text(size=7, face='bold'),
          legend.position = "top") +
    labs(title="오산",
         y="", x= "")
  # 평택
  pb <- p %>% 
    arrange(desc(n)) %>%
    head(5)
  
  pb %>%
    ggplot(aes(x = reorder(업종대분류,  n), y = n)) +
    geom_bar(stat = "identity", position= 'dodge', width=.7, fill= "#4C5D8A") +
    ylim(0,50000) +
    coord_flip() +# 가로형코드
    theme_minimal() +
    theme(axis.text.x = element_text(size = 6, face = "bold"),
          axis.text.y = element_text(size = 8, face = 'bold'),
          axis.title=element_text(size=7, face='bold'),
          title = element_text(size=7, face='bold'),
          legend.position = "top") +
    labs(title="평택",
         y="", x= "업종")
  # 안성
  anb <- an %>%
    arrange(desc(n)) %>%
    head(5)
 
   anb %>%
     ggplot(aes(x = reorder(업종대분류,  n), y = n)) +
     geom_bar(stat = "identity", position= 'dodge', width=.7, fill= "#DD915F") +
     ylim(0,35000) +
     coord_flip() +# 가로형코드
     theme_minimal() +
     theme(axis.text.x = element_text(size = 6, face = "bold"),
           axis.text.y = element_text(size = 8, face = 'bold'),
           axis.title=element_text(size=7, face='bold'),
           title = element_text(size=7, face='bold'),
           legend.position = "top") +
    labs(title="안성",
         y="", x= "업종")
#데이터 2 
#업종대분류
poshh = count(posh, 업종중분류)
poshh %>% 
  ggplot(aes(x = reorder(업종중분류, -n), y = n)) +
     geom_bar(stat = "identity", position= 'dodge', width=.5, fill= "#00B2D9") +
     theme_minimal() +
     theme(axis.text.x = element_text(size = 8, face = "bold"),
           axis.text.y = element_text(size =5, face = 'bold'),
           axis.title=element_text(size=10, face='bold'),
           title = element_text(size=10, face='bold'),
           legend.position = "top") +
     labs(title="경기도",
          y="", x= "")

j_palette <- c('R스포츠 및 오락관련 서비스업'='#F64F5C','R창작; 예술 및 여가관련 서비스업'='#F5C869' )

gem <- posh %>% 
  filter(시군구명 %in% c("평택시", "안성시", "화성시", "오산시")) %>%
  filter(업종중분류 %in% c("R스포츠 및 오락관련 서비스업", "R창작; 예술 및 여가관련 서비스업")) %>%
  group_by(시군구명, 업종중분류) %>%
  summarise(n=n())

graph <- ggplot(data = gem) +
  geom_col(aes(x=시군구명, y=n, fill=factor(업종중분류)), position='dodge')
graph +
  # 배경을 흰색으로 설정
  theme_bw() +
  # 색상
  scale_fill_manual(values=j_palette) +
  # 그래프 타이틀 설정
  ggtitle("지역별, 업종중분류조사") +
  # x축 제목 설정
  xlab("지역") +
  # y축 제목 설정
  ylab("수") +
  # y축 간격 및 데이터에 대한 세자리 콤마 적용
  scale_y_continuous(breaks=seq(0, 850, 50))  +
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
        legend.position = c(0.30, 0.85))





