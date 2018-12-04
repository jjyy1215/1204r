# 09. 데이터 분석 프로젝트
# 09-1. '한국복지패널데이터' 분석 준비하기

install.packages("foreign") # foreign 패키지 설치 
library(foreign) # SPSS 파일 로드 
library(dplyr) # 전처리 
library(ggplot2) # 시각화 
library(readxl) # 엑셀 파일 불러오기

raw_welfare <- read.spss(file = "Koweps_hpc10_2015_beta3.sav", to.data.frame = T)
welfare <- raw_welfare

# 데이터 검토하기 
head(welfare) 
tail(welfare) 
View(welfare) 
dim(welfare) 
str(welfare) 
summary(welfare)

# 변수명 바꾸기 
welfare <- rename(welfare, sex = h10_g3, # 성별 
                  birth = h10_g4, # 태어난 연도 
                  marriage = h10_g10, # 혼인 상태 
                  religion = h10_g11, # 종교 
                  income = p1002_8aq1, # 월급 
                  code_job = h10_eco9, # 직종 코드 
                  code_region = h10_reg7) # 지역 코드

str(welfare)
View(welfare$sex)


# 09-2. 성별에 따른 월급 차이
- "성별에 따라 월급이 다를까?"

class(welfare$sex)
table(welfare$sex)

# 이상치 결측 처리 
welfare$sex <- ifelse(welfare$sex == 9, NA, welfare$sex)

# 결측치 확인 
table(is.na(welfare$sex))

welfare$sex <- ifelse(welfare$sex == 1, "male", "female")  # 1대신 남자, 2대신 여자

table(welfare$sex)
qplot(welfare$sex)

# 월급 변수 검토 및 전처리
# 1. 변수 검토하기

class(welfare$income)
summary(welfare$income)

qplot(welfare$income)
qplot(welfare$income) + xlim(0, 1000)  # 좀 더 자세히 보자

# 2. 전처리
# 이상치 확인  : 수입은 공개하지 않은 사람도 많다. 
summary(welfare$income)

# 이상치 결측 처리
welfare$income <- ifelse(welfare$income %in% c(0, 9999), NA, welfare$income)

# 결측치 확인
table(is.na(welfare$income))  # NA가 12044개

# 성별에 따른 월급 차이 분석하기
# 1. 성별 월급 평균표 만들기
sex_income <- welfare %>%
  filter(!is.na(income)) %>%
  group_by(sex) %>%
  summarise(mean_income = mean(income))

sex_income
ggplot(data = sex_income, aes(x = sex, y = mean_income)) + geom_col()

# 09-3. 나이와 월급의 관계
# - "몇 살 때 월급을 가장 많이 받을까?"   pdf 17쪽

class(welfare$birth)
## [1] "numeric"
summary(welfare$birth)
## Min. 1st Qu. Median Mean 3rd Qu. Max.
## 1907 1946 1966 1968 1988 2014
qplot(welfare$birth)

# 이상치 확인
summary(welfare$birth)
## Min. 1st Qu. Median Mean 3rd Qu. Max.
## 1907 1946 1966 1968 1988 2014
# 결측치 확인
table(is.na(welfare$birth))
##
## FALSE
## 16664
# 이상치 결측 처리
welfare$birth <- ifelse(welfare$birth == 9999, NA, welfare$birth)
table(is.na(welfare$birth))
##
## FALSE
## 16664

welfare$age <- 2015 - welfare$birth + 1
summary(welfare$age)
## Min. 1st Qu. Median Mean 3rd Qu. Max.
## 2.00 28.00 50.00 48.43 70.00 109.00
qplot(welfare$age)

#나이에 따른 월급 평균표 만들기
age_income <- welfare %>%
  filter(!is.na(income)) %>%
  group_by(age) %>%
  summarise(mean_income = mean(income))
head(age_income)
## # A tibble: 6 x 2
## age mean_income
## <dbl> <dbl>
## 1 20 121.3000
## 2 21 105.5185
## 3 22 130.0923
## 4 23 141.7157
## 5 24 134.0877
## 6 25 144.6559


welfare <- welfare %>%
  mutate(ageg = ifelse(age < 30, "young",
                       ifelse(age <= 59, "middle", "old")))
table(welfare$ageg)
##
## middle old young
## 6049 6281 4334
qplot(welfare$ageg)

#연령대별 월급 평균표 만들기
ageg_income <- welfare %>%
  filter(!is.na(income)) %>%
  group_by(ageg) %>%
  summarise(mean_income = mean(income))
ageg_income
## # A tibble: 3 x 2
## ageg mean_income
## <chr> <dbl>
## 1 middle 281.8871
## 2 old 125.3295
## 3 young 163.5953

ggplot(data = ageg_income, aes(x = ageg, y = mean_income)) + geom_col()

ggplot(data = ageg_income, aes(x = ageg, y = mean_income)) +
  geom_col() +
  scale_x_discrete(limits = c("young", "middle", "old"))


#09-5. 연령대 및 성별 월급 차이
연령대 및 성별 월급 평균표 만들기
sex_income <- welfare %>%
  filter(!is.na(income)) %>%
  group_by(ageg, sex) %>%
  summarise(mean_income = mean(income))
sex_income
## # A tibble: 6 x 3
## # Groups: ageg [?]
## ageg sex mean_income
## <chr> <chr> <dbl>
## 1 middle female 187.97552
## 2 middle male 353.07574
## 3 old female 81.52917
## 4 old male 173.85558
## 5 young female 159.50518
## 6 young male 170.81737


ggplot(data = sex_income, aes(x = ageg, y = mean_income, fill = sex)) +
  geom_col() +
  scale_x_discrete(limits = c("young", "middle", "old"))


ggplot(data = sex_income, aes(x = ageg, y = mean_income, fill = sex)) +
  geom_col(position = "dodge") +
  scale_x_discrete(limits = c("young", "middle", "old"))


성별 연령별 월급 평균표 만들기
sex_age <- welfare %>%
  filter(!is.na(income)) %>%
  group_by(age, sex) %>%
  summarise(mean_income = mean(income))
head(sex_age)
## # A tibble: 6 x 3
## # Groups: age [3]
## age sex mean_income
## <dbl> <chr> <dbl>
## 1 20 female 147.4500
## 2 20 male 69.0000
## 3 21 female 106.9789
## 4 21 male 102.0500
## 5 22 female 139.8547
## 6 22 male 118.2379


ggplot(data = sex_age, aes(x = age, y = mean_income, col = sex)) + geom_line()

#09-6. 직업별 월급 차이

class(welfare$code_job)
## [1] "numeric"
table(welfare$code_job)

#직업분류코드 목록 불러오기
library(readxl)
list_job <- read_excel("Koweps_Codebook.xlsx", col_names = T, sheet = 2)
head(list_job)

dim(list_job)

#welfare에 직업명 결합
welfare <- left_join(welfare, list_job, id = "code_job")
## Joining, by = "code_job"
welfare %>%
  filter(!is.na(code_job)) %>%
  select(code_job, job) %>%
  head(10)

#직업별 월급 차이 분석하기
# 직업별 월급 평균표 만들기
job_income <- welfare %>%
  filter(!is.na(job) & !is.na(income)) %>%
  group_by(job) %>%
  summarise(mean_income = mean(income))
head(job_income)

#상위 10개 추출
top10 <- job_income %>%
  arrange(desc(mean_income)) %>%
  head(10)
top10

#그래프 만들기 
ggplot(data = top10, aes(x = reorder(job, mean_income), y = mean_income)) + geom_col() + coord_flip()

#하위 10위 추출 
bottom10 <- job_income %>% arrange(mean_income) %>% 
  head(10) 
bottom10

#그래프 만들기 
ggplot(data = bottom10, aes(x = reorder(job, -mean_income), y = mean_income)) + geom_col() + coord_flip() + ylim(0, 850)

#09-7. 성별 직업 빈도
성별 직업 빈도 분석하기
1. 성별 직업 빈도표 만들기 
# 남성 직업 빈도 상위 10개 추출 
job_male <- welfare %>% filter(!is.na(job) & sex == "male") %>% 
  group_by(job) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>%
head(10) 
job_male

#여성 직업 빈도 상위 10개 추출

job_female <- welfare %>% 
  filter(!is.na(job) & sex == "female") %>% 
  group_by(job) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>% 
  head(10) 
job_female

#그래프 만들기
# 남성 직업 빈도 상위 10개 직업
ggplot(data = job_male, aes(x = reorder(job, n), y = n)) + geom_col() + coord_flip()

# 여성 직업 빈도 상위 10개 직업 
ggplot(data = job_female, aes(x = reorder(job, n), y = n)) + geom_col() + coord_flip()


#09-8. 종교 유무에 따른 이혼율

#종교 변수 검토 및 전처리하기
# 변수 검토하기
class(welfare$religion) 
table(welfare$religion)

#전처리
# 종교 유무 이름 부여
welfare$religion <- ifelse(welfare$religion == 1, "yes", "no") 
table(welfare$religion)

qplot(welfare$religion)

#혼인 상태 변수 검토 및 전처리하기
# 변수 검토하기
class(welfare$marriage)
table(welfare$marriage)

#전처리 # 이혼 여부 변수 만들기
welfare$group_marriage <- ifelse(welfare$marriage == 1, "marriage", 
ifelse(welfare$marriage == 3, "divorce", NA)) 
table(welfare$group_marriage)

table(is.na(welfare$group_marriage))

qplot(welfare$group_marriage)


