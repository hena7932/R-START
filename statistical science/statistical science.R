#### 1. 척도별 기술 통계량 구하기 ####
setwd("~/Documents/R_edu")

data <- read.csv("R_NCS_2020/2_day/data/descriptive.csv", stringsAsFactors = FALSE)

head(data) # 데이터의 상위 6개
dim(data) # 차원 보기
length(data) # 변수의 길이 (열 11개)
length(data$survey) # survey 칼럼의 관측치 (행 32개)

summary(data)
# 위 소스를 실행하고 나면, 각 갈럼 최대, 최소, 평균, 중간 값을 확인할 수 있다. 
# NA's는 각 데이터의 Missing Values를 의미한다. 

## gender를 확인해보자.
summary(data$gender)
table(data$gender) # 각 값의 빈도수를 알 수 있도록 도와주는 함수임

# 이상치 제거 소스 코드 예제
data <- subset(data, data$gender == 1 | data$gender == 2)
table(data$gender)

# 기술 통계량 분석 보고서 작성의 예시 
# 기술 통계량은 말 그대로, 빈도수와 Percent로 구성이 된다.
# 패키지 활용을 활용한 기술 통계량을 보다 쉽게 구하도록 한다. 
# prettyR 패키지에서 freq() 함수 사용
# install.packages("prettyR")
library(prettyR)

freq(data)

#### 2. 기술 통계량 보고서 작성 요령 ####
#### (1) 거주지역
# 변수의 리코딩 using base R
data$resident2[data$resident == 1] <- "특별시"
data$resident2[data$resident >= 2 & data$resident <= 4 ] <- "광역시"
data$resident2[data$resident == 5] <- "시구군"

prop.table(table(data$resident2))

# 변수의 리코딩 dplyr 버전
library(dplyr)
data$resident3 <- recode(data$resident, 
                         `1` = "특별시", 
                         `2` = '광역시', 
                         `3` = '광역시',
                         `4` = '광역시',
                         `5` = '시구군'
                         )
prop.table(table(data$resident3))

#### (2) 성별 ####
# 무엇이 조금더 직관적인지 판단해보세요. 
data$gender2[data$gender == 1] <- "남자"
data$gender2[data$gender == 2] <- "여자"

round(prop.table(table(data$gender2)), 2)

#### (3) 나이 ####
data$age2[data$age <= 45] <- "중년층"
data$age2[data$age >= 46 & data$age <= 59] <- "장년층"
data$age2[data$age >= 60] <- "노년층"

round(prop.table(table(data$age2)), 2)

#### (4) 학력 ####
data$level2[data$level == 1] <- "고졸"
data$level2[data$level == 2] <- "대졸"
data$level2[data$level == 3] <- "대학원졸"

round(prop.table(table(data$level2)), 2)

#### (5) 합격 ####
data$pass2[data$pass == 1] <- "합격"
data$pass2[data$pass == 2] <- "불합격"

round(prop.table(table(data$pass2)), 2)

head(data)

freq(data)

#### 3. 교차분석 ####
# 교차분석은 범주형 자료를 대상으로 두개 이상의 변수들에 대한 관련성을 알아보기 위한 것이며, 교차 분할표를 작성하고 이를 통해서 변수 상호 간의 관련성 여부를 분석하는 방법임. 

# 교차분석시 일반적인 고려사항
# 교차분석에 사용되는 변수는 값이 10미만인 범주형 변수(명목척도, 서열척도)이어야 하며, 비율척도인 경우 코딩변경을 통해서 범주형 자료로 변환해야 함. 
# 학년의 경우 초등학교는 1, 중학교는 2, 고등학교는 3 대학교는 4 등으로 범주화 하여 변경을 한다. 

#### (1) 패키지 설치 ####
library(gmodels)

#### (2) 교차 분할표 생성) ####
CrossTable(x = data$level2, y = data$pass2) 

(107 * 153) / 266
((59-61.54) ^ 2) / 61.54

#### 4. 카이제곱 검정 ####
# 카이제곱검정(Chi-Square Test)은 범주(Category)별로 관측빈도와 기대빈도의 차이를 통해서 확률 모형이 데이터를 얼마나 잘 설명하는지를 검정하는 통계적 방법임
# 카이제곱검정 중요사항
# 카이제곱검정을 위해서는 교차분석과 동일하게 범주형 변수를 대상으로 함
# 집단별로 비율이 같은지를 검정하여 독립성 여부를 검정함
# 유의확률에 의해서 집단 간의 '차이가 있는가?' 또는 '차이가 없는가?'로 가설 검증함
# CrossTable() 함수에 'chisq = TRUE' 속성을 적용하면 카이제곱 검정 결과를 볼 수 있다. 
# ----- (1) 독립성 검정 ----- 
# 동일 집단의 두 변인을 대상으로 관련성이 있는가? 또는 없는가?를 검정하는 방법임
# 독립성 검정 가설의 예
# 귀무가설: 경제력과 대학진학 합격률과 관련성이 없다(=독립적이다)
# 대립가설: 경제력과 대학진학 합격률과 관련성이 있다(=독립적이지 않다)

# 본 데이터에 적용
# 귀무가설: 부모의 학력 수준과 자녀의 대학진학 여부는 관련성이 없다. 
# 대립가설(=연구가설): 부모의 학력 수준과 자녀의 대학진학 여부는 관련성이 있다. 
CrossTable(x = data$level2, y = data$pass2, chisq = TRUE) 

# 논문이나 보고서에서는 귀무가설을 기각하고, 연구가설을 채택하는 것이 목적임.

# 유의확률 해석 방법
# 유의확률(P-value: 0.408)
# 유의확률(p-value: 0.4085)이 0.05 이상이기 때문에 유의미한 수준(a = 0.05)에서 귀무가설을 기각할 수 없다. 
# 따라서, 귀무가설을 기각할 수가 없고, 부모의 학력 수준과 자녀의 대학진학 여부와는 관련성이 없는 것으로 분석됨. 

# 기대빈도 구하기
(113 * 107) / 266 # 고졸 불합격
(153 * 107) / 266 # 고졸 합격
(113 * 92) / 266 # 대졸 불합격
(153 * 92) / 266 # 대졸 합격
(113 * 67) / 266 # 대학원졸 불합격
(153 * 67) / 266 # 대학원졸 합격

#### 5. 집단 간 차이 분석 ####
# 전체 표본 크기(N): 20,000명
# 표본 평균(X): 170.1 cm
# 표본 표준편차(S): 2cm

N <- 20000 # 표본 크기(N) > 신뢰수준 95%의 신뢰구간 구하기
X <- 170.1 # 표본 평균(X)
S <- 2 # 표본 표준편차(S)

low <- X - 1.96 * S/sqrt(N) # 신뢰구간 하한값
high <- X + 1.96 * S/sqrt(N) # 신뢰구간 하한값

low; high

# 신뢰수준 95%의 모평균 신뢰구간: 170.0723 <= m <= 170.1277
# 표본오차 구하기
(low - X) * 100 # -2.771
(high - X) * 100 # 2.771

# 표본오차는 +- 2.77
#### (1) 단일집단 비율 검정 ####
# 데이터셋
# CS교육 데이터
# 연구환경 예시
# 2018년도 CS 시, 고객의 불만율 20%였음
# 이를 개선하기 위해, 2019년도 CS 150명 고객 대상 설문결과 20명만 불만을 가지고 있음
# 기존 20%보다 불만율이 낮아졌을까? 

# 귀무가설: 기존 2018년도 고객 불만율과 2019년도 CS교육 후, 불만율에 차이가 없다. 
# 연구가설: 기존 2019년도 고객 불만율과 2019년도 CS교육 후, 불만율에 차이가 있다. 
# 패키지 설치
library(prettyR)

getwd()

cs_data <- read.csv("R_NCS_2020/2_day/data/one_sample.csv", stringsAsFactors = FALSE)
head(cs_data)

table(cs_data$survey) # 0: 불만족(14), 1: 만족(136)
freq(cs_data$survey)

# 이항분포 비율 검정
# 명목척도의 비율을 바탕으로 binom.test() 함수를 이용하여 이항분포의 양측 검정을 통해, 검정 통계량을 구한 후 이를 이용하여 가설 검정
# binom.test()
binom.test(c(136, 14), p = 0.8)

# 방향성이 없는 양측 검정
binom.test(c(136, 14), p = 0.8)
# 만족고객 136명을 대상으로 95% 신뢰 수준에서 양측 검정을 실시한 결과 검정 통계량(p-value)값은 0.00006735로 유의수준 0.05보다 작기 때문에 기존 만족률(80%)과 차이가 있다고 볼 수 있음.
# 다만, 여기서 주의해야 할 점은, 우리가 원하는 답! 2018년보다 만족도가 증가했는지, 감소했는지, 방향성은 알지 못한다. 방향성을 알기 원한다면 이 때에는 단측검정을 사용한다.  

# 방향성이 있는 단측 검정
binom.test(c(136, 14), p = 0.8, alternative = "greater", conf.level = 0.95)

# alternative = "greater"의 뚯은, 150명 중에서 136명의 만족 고객이 전체비율의 80%보다 더 큰 비율인가를 검정하기 위한 속성임. 
# p-value 값은 0.0003179로 유의수준 0.05보다 작기 때문에 기존 만족률보다 80% 이상의 효과를 얻을 수 있음. 
# 결과적으로 기존 20%보다 불만율이 낮아졌다고 할 수 있음
# 따라서, 귀무가설이 기각되고, 연구가설이 채택됨 = CS교육에 효과가 있다고 볼 수 있음. 

#### (2) 단일표본 T 검정 ####
# 데이터가 주어지면 T검정을 바로 시행히지는 않는다.
# shapiro.test()를 통해서 정규분포를 따르는지 따르지 않는지에 따라 검정 방법이 달라집니다. 
# 정규분포를 따른다면 t.test() / 정규분포를 따르지 않는다면 wilcox.test()를 시행합니다. 

# 연구환경
# A 동영상 앱의 평균 사용시간이 5.2시간으로 발표가 된 상황에서 B 동영상의 평균 사용시간과 차이가 있는지를 검정하기 위해 B 업체 고객의 설문조사를 실시한 하였다. 150명의 설문조사를 실시한 데이터를 가지고 검정을 실시한다.    

# 연구가설
# 귀무가설, A 동영상 앱과 B 동영상 앱의 평균 사용시간에 차이가 없다.
# 대립가설, A 동영상 앱과 B 동영상 앱의 평균 사용시간에 차이가 있다. 
time_data <- read.csv("R_NCS_2020/2_day/data/one_sample.csv", stringsAsFactors = FALSE)
str(time_data)
summary(time_data$time) # NA 41개임 확인

# NA를 제외한 평균 (1)
mean(time_data$time, na.rm = TRUE)

# NA 제거 후 새로운 데이터 생성 (2)
time_data2 <- time_data[is.na(time_data$time) == FALSE, ]
mean(time_data2$time)

# 정규분포 검정
# shapiro.test의 귀무가설: x의 데이터는 정규분포이다 / 대립가설: x의 데이터는 정규분포가 아니다. 
# 즉, 유의확률(p > 0.05)보다 크면 x의 데이터는 정규분포이라는 뜻임 
shapiro.test(time_data2$time)

# p-value = 0.7242, 유의확률보다 높기 때문에 모수검정인 T검정을 실시한다. 
# 정규분포 시각화
hist(time_data2$time)
qqnorm(time_data2$time)
qqline(time_data2$time, lty = 1, col = "blue")

# t-test 양측 검정 & 단측 검정
t.test(time_data2$time, mu = 5.2) # 차이가 있다 없다
# 유의 수준이 0.05보다 작기 때문에 귀무가설을 기각하고 대립가설을 채택한다. 
# 대립가설: A 앱 평균 시청 시간과 B 앱 평균 시청 시간의 차이가 있다. 

t.test(time_data2$time, alter = "greater", mu = 5.2) # 5.2보다 크다 작다
# 귀무가설: B앱의 평균 사용 시간이 A앱의 평균 사용시간보다 크지 않다.
# 
# 'B앱의 평균 시청시간이 더 길다'라는 방향성을 갖는 연구가설 검정 결과 P-value값이 유의확률 이내임. 
# 따라서, B회사의 앱 사용 시간이 A회사의 평균 5.2시간보다 더 긴 것을 확인할 수 있음. 
# 귀무가설 임계값 구하기
library(stats)
qt(7.083e-05, 108) # qt(p.value, df)
# (절대값 적용), 즉, t통계량 3.946 이상이면 귀무가설 기각할 수 있고, 현재 t통계량은 3.9461로 귀무가설을 기각하고, 대립가설을 채택한다. 

#### (3) 두 집단 비율 검정 ####
# 독립표본 이항분포의 비율 검정은 prop.test() VS. 단일표본 이항분포 binom.test() 비교
# 비율 차이 검정 통계량을 바탕으로 귀무가설의 기각 여부를 결정함. 

# 연구환경
# IT 교육센터에서 PT를 이용한 프레젠테이션 교육방법과 실시간 코딩 교육방법을 적용하여 교육을 실시하였다. 2가지 교육방법 중 더 효과적인 교육방법을 조사하기 위해서 교육생 300명을 대상으로 설문을 실시하였다. 

# 연구가설
# 귀무가설: 두 가지 교육방법에 따라 교육생의 만족율에 차이가 없다. 
# 연구가설: 두 가지 교육방법에 따라 교육생의 만족률에 차이가 있다. 

# 데이터 불러오기
data <- read.csv("R_NCS_2020/2_day/data/two_sample.csv", header = TRUE)
summary(data)

# 데이터 전처리하기
# method 비율
table(data$method)
# 1: 150명, 2: 150명

# survey 비율
table(data$survey)
# 0 (불만족): 55명 / 1 (만족): 245명

# 두 변수에 대한 교차분석
# table(method, survey, useNA="ifany") # useNA="ifany": 결측치까지 출력
table(data$method, data$survey, useNA = "ifany")

# 위 표 해석
# 300명 중 교육 방식에 만족했다고 응답한 사람은 245명임. 
# 245명 중 교육방법 1에 만족한 사람은 110명 VS 교육방법 2에 만족한 사람은 135명

# 두 집단 비율 차이 검정
prop.test(x = c(110, 135), n = c(150, 150))

# 방향성을 갖는 단측가설 검정
prop.test(x = c(110, 135), n = c(150, 150), alter = "greater")
# 첫번째 교육방법이 두번째 교육방법보다 만족율이 클 것이다 가정하고 검정 진행
# p-value 값이 0.9998이기 때문에 귀무가설을 채택함. 
# 즉, 첫번째 교육방법이 두번째 교육방법보다 만족율이 크다로 분석할 수 있음. 

#### (4) 두 집단 평균 검정 (독립표본 T 검정) ####
# 평균 차이 검정을 위해서는 기술 통계량으로 평균을 구한다. 
# 독립표본 평균검정은 두 집단 간 분산의 동질성 검증(정규분포 검정) 여부를 판정한다. 
# 결과에 따라서, t.test를 진행하거나 윌콕스 검정을 수행한다. 

# 연구환경
# IT교육센터에서 PT를 이용한 프레젠테이션 교육방법과 실시간 코딩 교육방법을 적용하여 1개월동안 교육받는 교육생 각 150명을 대상으로 실기시험을 실시하였다. 집단간 실기시험의 평균에 차이가 있는가를 검정한다. 

# 연구가설
# 귀무가설 HO: 교육방법에 따른 실기시험의 평균의 차이가 없다.
# 대립가설 H1: 교육방법에 따른 실기시험의 평균의 차이가 있다.

summary(data)
# 여기에서 결측치 73개를 제외하는 코드를 작성한다. 
# base R 방식
result <- subset(data, !is.na(score), c(method, score))

# dplyr 방식
library(dplyr)

result2 <- data %>% 
  filter(!is.na(score)) %>% 
  select(method, score)

# 피벗 테이블 작성 & 기술 통계량
result %>% 
  group_by(method) %>% 
  summarise(length = n(), 
            avg_score = mean(score))

# 동질성 검정
# 동질성 검정의 귀무가설: 두 집단 간 분포의 모양이 동질적이다. 
# 동질성 검정의 대립가설: 두 집단 간 분포의 모양이 동질적이지 않다. 
# var.test(x, y)에서 x와 y는 숫자형 벡터가 들어가야 한다. 
# 따라서, 데이터셋에서 각 방법에 따라 숫자형 벡터로 변환하는 것이 중요하다. 
method_1 <- subset(result, method == 1)
method_2 <- subset(result, method == 2)

var.test(method_1$score, method_2$score)

# 검정 통계량 p-value 0.3002로 유의수준 0.05보다 크기 때문에 두 집단 간의 분포 형태가 동질하다고 볼 수 있음. 
# 즉, 이 때에는 윌콕스 검정이 아니라 t.test() 검정을 수행한다. 
t.test(method_1$score, method_2$score) 

# 차이가 있느냐 없느냐만 가늠함
# p-value = 0.0411이기 때문에, 두 집단 실기시험의 평균에는 차이가 있다로 결론내릴 수 있으나, 방법1이 좋은지 아니면 2가 좋은지는 판단하지 못함
# 이럴 때에는 방향성을 갖는 단측검정을 실시함

t.test(method_1$score, method_2$score, 
       alter="greater", conf.int = TRUE, conf.level = 0.95)

# 귀무가설: method_2$score가 method_1$score 보다 크다
# 대립가설: method_2$score가 method_1$score 크지 않다
# p-value가 0.9794
# 귀무가설을 기각할 수 없기 때문에, method_2의 실기 시험 점수가 method_1의 점수보다 크다
# 따라서, 교육방법에 따른 두 집단 간 실기시험의 평균에 차이가 있고, method_2가 method_1보다 결과가 더 좋았다라고 결론 내릴 수 있음. 

#### (5) 대응 두 집단 평균 검정 (대응표본 T 검정) ####
# 정책 효과, 마케팅 효과 등, 이런 것들을 검정할 때 매우 유용합니다. 
# 참고로 마케팅 공부할 때, 가장 유용하고 사용했던 검정 방법이기도 합니다.  
# 동일한 표본을 대상으로 측정된 두 변수의 평균 차이를 검정하는 분석방법임.

# 연구환경
# A교육센터에서 교육생 100명을 대상으로 교수법 프로그램 적용 전에 실기시험을 실시한 후 1개월 동안 동일한 교육생에세 교수법 프로그램을 적용한 후 실기시험을 실시한 점수와 평균에 차이가 있는지 검정합니다. 

# 가설검정
# 귀무가설: 교수법 프로그램을 적용하기 전 학생들의 학습력과 교수법 프로그램을 적용한 후 학생들의 학습력에 차이가 없다. 
# 연구가설: 교수법 프로그램을 적용하기 전 학생들의 학습력과 교수법 프로그램을 적용한 후 학생들의 학습력에 차이가 있다. 

# 데이터 불러오기
data <- read.csv("R_NCS_2020/2_day/data/paired_sample.csv", header = TRUE)
summary(data)

# 대응 두 집단 subset 생성하기
# dplyr way

result <- data %>% 
  filter(!(is.na(after))) %>% 
  select(before, after)

# 동질성 검정
var.test(result$before, result$after, paired = TRUE)

# p-value 값이 0.05보다 크기 때문에 귀무가설을 그대로 채택함
# 귀무가설: 두 그룹간 데이터 분포의 모양은 동질적이다. ---> t.test()
# 대립가설: 두 그룹간 데이터 분포의 모양은 동질적이지 않다. ---> wilcox.test()

t.test(result$before, result$after, paired = TRUE)
# p-value값은 유의수준 0.05보다 매우 작기 때문에 귀무가설을 기각하고 대립가설을 채택한다, 즉, 두 집단 간의 평균에 차이가 있는 것으로 나타났다. 

# 방향성을 갖는 단측 검정을 실시한다. 
t.test(result$before, result$after, paired = TRUE, alter = "greater", conf.level = 0.95, conf.int = TRUE)

# 귀무가설: result$after의 평균 점수가 result$before의 평균 점수보다 크다
# 대립가설: result$after의 평균 점수가 result$before의 평균 점수보다 크지 않다.
# 유의확률이 1, 즉 귀무가설을 기각할 수 없다, result$after의 평균 점수가 result$before의 평균 점수보다 크다. 

# End of Document

