library(tibble)
library(readr)
library(readxl)
library(lubridate)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(stringr)
sessionInfo()

#지역별로 확인하는 사업체와 종사자

readr::guess_encoding("data/청년기업.csv",  n_max = 100) # 대표자 정보 청년

young <- read.csv("data/청년기업.csv", fileEncoding = "UTF-8")

glimpse(young)

# 데이터 전처리

young_a <- select(young, - (등록일자:작업자명), - (업종중분류코드:업력구간코드))

glimpse(young_a)

# 시각화
col = count(young_a, 업종대분류코드)

# 업종별 (내림차순) 
col = count(young_a, 업종대분류코드)

col %>%
  ggplot(aes(x = reorder(업종대분류코드, -n), y = n)) +
  geom_bar(stat = "identity", position= 'dodge', width=.5, fill= "#B66DFF") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 10, face = "bold"),
        axis.text.y = element_text(size =10, face = 'bold'),
        axis.title=element_text(size=10, face='bold'),
        title = element_text(size=10, face='bold'),
        legend.position = "top") +
  labs(title="업종대분류")

# 가로형
ggplot(data = col, aes(x = reorder(업종대분류코드, -n), y = n)) + geom_col(aes(colour=업종대분류코드)) + coord_flip()

# 연령대 코드
age = count(young_a, 연령대코드)
age %>%
  filter(연령대코드 %in% c("20", "30", "40", "50", "60", "70")) %>% 
  ggplot(aes(x = 연령대코드, y = n)) +
  geom_bar(stat = "identity", position= 'dodge', width=.5, fill= "#FF6666") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 10, face = "bold"),
        axis.text.y = element_text(size =10, face = 'bold'),
        axis.title=element_text(size=10, face='bold'),
        title = element_text(size=10, face='bold'),
        legend.position = "top") +
  labs(title="연령대")

# 시도명 코드
SD = count(young_a, 시도명)
SD %>%
ggplot(aes(x = reorder(시도명, -n), y = n)) +
  geom_bar(stat = "identity", position= 'dodge', width=.5, fill= "#00B2D9") +
  coord_flip() + # 가로형코드
  theme_minimal() +
  theme(axis.text.x = element_text(size = 10, face = "bold"),
        axis.text.y = element_text(size =10, face = 'bold'),
        axis.title=element_text(size=10, face='bold'),
        title = element_text(size=10, face='bold'),
        legend.position = "top") +
  labs(title="시도")


SD %>%
  filter(시도명 %in% c("서울", "경기", "부산", "경남&경북", "전남&전북", "충북&충남", "인천")) %>% 
  ggplot(aes(x = 시도명, y = n)) +
  geom_bar(stat = "identity", position= 'dodge', width=.5, fill= "#FF6666") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 10, face = "bold"),
        axis.text.y = element_text(size =10, face = 'bold'),
        axis.title=element_text(size=10, face='bold'),
        title = element_text(size=10, face='bold'),
        legend.position = "top") +
  labs(title="시도명")

# 시도명 코드(상위)
top10 <- SD %>%            # 데이터에서
  arrange(desc(n)) %>%   # 내림차순으로 나열
  head(10)                # 상위 10개만 추출 후, 최종적으로 top10 에 저장
top10 %>%
  ggplot(aes(x = reorder(시도명, -n), y = n)) +
  geom_bar(stat = "identity", position= 'dodge', width=.5, fill= "#EC5A96") +
  coord_flip() + # 가로형코드
  theme_minimal() +
  theme(axis.text.x = element_text(size = 10, face = "bold"),
        axis.text.y = element_text(size =10, face = 'bold'),
        axis.title=element_text(size=10, face='bold'),
        title = element_text(size=10, face='bold'),
        legend.position = "top") +
  labs(title="시도",
       caption="TC_EN_YGMN_ENTRPRS_INFO",
       subtitle="top10",
       y="시도수", x= "시도")

#하위10개
bottom10 <- SD %>% #하위10개
  arrange(n) %>%
  head(10)
bottom10 %>%
  ggplot(aes(x = reorder(시도명, -n), y = n)) +
  geom_bar(stat = "identity", position= 'dodge', width=.5, fill= "#F64F5C") +
  coord_flip() +# 가로형코드
  ylim(0, 1500000) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 10, face = "bold"),
        axis.text.y = element_text(size =10, face = 'bold'),
        axis.title=element_text(size=10, face='bold'),
        title = element_text(size=10, face='bold'),
        legend.position = "top") +
  labs(title="시도",
       caption="TC_EN_YGMN_ENTRPRS_INFO",
       subtitle="botton 10",
       y="시도수", x= "시도")

#시도명 (4개만)
SD %>%
  filter(시도명 %in% c("서울", "경기", "부산", "인천")) %>% 
  ggplot(aes(x = 시도명, y = n)) +
  geom_bar(stat = "identity", position= 'dodge', width=.5, fill= "#FF6666") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 10, face = "bold"),
        axis.text.y = element_text(size =10, face = 'bold'),
        axis.title=element_text(size=10, face='bold'),
        title = element_text(size=10, face='bold'),
        legend.position = "top") +
  labs(title="시도명",
       caption="TC_EN_YGMN_ENTRPRS_INFO",
       subtitle="서울, 경기, 부산, 인천",
       y="시도수", x= "시도")
