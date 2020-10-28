# 1단계 패키지 불러오기



library(tidyverse)

library(readxl)

library(dplyr)

library(ggplot2)

sessionInfo()


# 2단계데이터 불러오기

## 1.삼성데이터


readr::guess_encoding("data/Samsungcard.csv", n_max = 100) # 삼성카드



samsung_card <- read_xlsx("data/Samsungcard.xlsx") # 삼성카드가 두가지 데이터 있음 

samsung_card2 <- read.csv("data/Samsungcard.csv", fileEncoding = "EUC-KR")



head(samsung_card)

head(samsung_card2)



rm(samsung_card2) # 객체 지우는 함수
ls() # 현재 저장된 객체 확인하는 함수

## 2.신한데이터 불러오기


shinhancard <- read_xlsx("data/Shinhancard.xlsx")
head(shinhancard)


# 위 데이터를 불러오니 불필요한 6:8 변수가 불러온 것을 확인할 수 있다.
# 실제 엑셀 데이터를 열어도 빈값임을 확인할 수 있다.
# 따라서, 6:8 변수는 삭제한다.


shinhancard <- shinhancard %>% 
  select(-c(6:8))

head(shinhancard)

# 지인플러스는 아파트시세(GIN00009A)와 아파트 거래량(GIN00008B)을 담은 코드이다


gin_8a <- read_csv("data/GIN00008A.csv")

gin_9a <- read_csv("data/GIN00009A.csv") 



glimpse(gin_8a)



glimpse(gin_9a)



glimpse(gin_9a)


## 4.json 파일불러오기


library(jsonlite)



GIN_10m <- fromJSON("data/center_GIN00010M.json")
glimpse(GIN_10m)


# PYN_CN의 값이 조금 다른 것을 확인할 수 있다.

# 이 부분은 추후 전처리할 때 정리하는 것으로 확인한다.

## 5.SSC-data

# 이번에는 Mcorporation내 폴더 데이터를 올리도록 한다.

# 이번에 파일을 불러올 때는 readr::read_csv()를 활용하여 불러온다.


readr::guess_encoding("data/Mcorporation/KDX시각화경진대회_SSC_DATA.csv")



ssc_data <- read_csv("data/Mcorporation/KDX시각화경진대회_SSC_DATA.csv", locale = locale("ko", encoding = "EUC-KR"))

glimpse(ssc_data)


## 6.다중 파일 불어오기


list.files(path = "data/Mcorporation/상품 카테고리 데이터_KDX 시각화 경진대회 Only/")

# 엑셀 데이터의 변수 등이 동일한 것을 확인할 수 있다.
# 이제 위 데이터를 한꺼번에 불러와서 하나의 데이터셋으로 합친다.(head를 이용)
# 검색키워드 Multiple Excel Files import in R

files <- list.files(path = "data/Mcorporation/상품 카테고리 데이터_KDX 시각화 경진대회 Only/", pattern = "*.xlsx", full.names = T)

products <- sapply(files, read_excel, simplify=FALSE) %>% 
  bind_rows(.id = "id")

glimpse(products)


install.packages("lubridate") # 형식을 data형식으로 변경

library(lubridate)

samsungcard$소비일자 <- ymd(samsungcard$소비일자) #모든 형식을 data형식으로 변경

shinhancard1$일별 <- as.Date(shinhancard1$일별, format = "%Y%m%d") #chr형식을 data형식으로 변경

library(tidyverse)

shinhancard <- shinhancard %>% # 행을 나눌때 사용→, sep=숫자: 데이터 나눌 위치를 정할때 사용
  
separate(업종, c(" ", "업종"), sep = 5)


