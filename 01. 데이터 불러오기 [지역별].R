dir.create("./01_data")
library(tseries) ; library(dplyr) ; library(tidyr) ; library(purrr) ;
library(dplyr) ; library(factoextra) ; library(ggplot2) ; library(cluster) ;
library(rpart) ; library(rpart.plot) 

#[전국 관광객수 데이터 (지역별)]
#1. 데이터 불러오기
visitor_2018 <- read.csv("데이터/전국 관광객수/지역별, 년도별/2018_방문자수.csv", fileEncoding = "UTF-8")
visitor_2019 <- read.csv("데이터/전국 관광객수/지역별, 년도별/2019_방문자수.csv", fileEncoding = "UTF-8")
visitor_2020 <- read.csv("데이터/전국 관광객수/지역별, 년도별/2020_방문자수.csv", fileEncoding = "UTF-8")
visitor_2021 <- read.csv("데이터/전국 관광객수/지역별, 년도별/2021_방문자수.csv", fileEncoding = "UTF-8")
visitor_2022 <- read.csv("데이터/전국 관광객수/지역별, 년도별/2022_방문자수.csv", fileEncoding = "UTF-8")
visitor_2023 <- read.csv("데이터/전국 관광객수/지역별, 년도별/2023_방문자수.csv", fileEncoding = "UTF-8")

#2. 데이터 전처리하기
visitor_2018 <- visitor_2018 %>% select(-전년도.방문자수) %>% rename(방문자수_2018 = 방문자수)
visitor_2019 <- visitor_2019 %>% select(-전년도.방문자수) %>% rename(방문자수_2019 = 방문자수)
visitor_2020 <- visitor_2020 %>% select(-전년도.방문자수) %>% rename(방문자수_2020 = 방문자수)
visitor_2021 <- visitor_2021 %>% select(-전년도.방문자수) %>% rename(방문자수_2021 = 방문자수)
visitor_2022 <- visitor_2022 %>% select(-전년도.방문자수) %>% rename(방문자수_2022 = 방문자수)
visitor_2023 <- visitor_2023 %>% select(-전년도.방문자수) %>% rename(방문자수_2023 = 방문자수)
head(visitor_2018, 20)
tail(visitor_2018)

visitor_all_region <- reduce(list(visitor_2018, visitor_2019, visitor_2020, visitor_2021, visitor_2022, visitor_2023), 
                      full_join, by = "시도명")

head(visitor_all_region)
tail(visitor_all_region)

save(visitor_all_region, file = "./01_data/visitor_all_region.rdata")
write.csv(visitor_all_region, "./01_data/visitor_all_region.csv")
#─────────────────────────────────────────────────────────────────────────────────────
#[미세먼지 데이터 (지역별)]
#1. 데이터 불러오기
fd_all_region <- read.csv("./데이터/전국 미세먼지/지역별, 년도별/미세먼지 농도(2018~2023).csv", fileEncoding = "CP949")

#2. 데이터 전처리
#지역 이름 변경
old_names <- c("서울", "부산", "대구", "인천", "광주", "대전", "울산", "세종", "경기", "강원", "충북", "충남", "전북", "전남", "경북", "경남", "제주")
new_names <- c(
  "서울특별시", "부산광역시", "대구광역시", "인천광역시", "광주광역시",
  "대전광역시", "울산광역시", "세종특별자치시", "경기도", "강원특별자치도",
  "충청북도", "충청남도", "전북특별자치도", "전라남도", "경상북도", "경상남도", "제주특별자치도"
)

sum(is.na(fd_all_region$지역))
fd_all_region$지역 <- new_names[match(fd_all_region$지역, old_names)]
colnames(fd_all_region) <- c("지역",
                         "미세먼지_2018",
                         "미세먼지_2019",
                         "미세먼지_2020",
                         "미세먼지_2021",
                         "미세먼지_2022",
                         "미세먼지_2023")

colnames(fd_all_region)[1] <- "시도명" # 지역명 컬럼 이름 통일
head(fd_all_region)
tail(fd_all_region)
save(fd_all_region, file = "./01_data/fd_all_region.rdata")
write.csv(fd_all_region, "./01_data/fd_all_region.csv")

#3. 미세먼지 데이터와 방문객수 데이터 합병
merged_fine_dust <- inner_join(visitor_all_region, fd_all_region, by = "시도명") # 두 데이터 병합 (시도명 기준)
head(merged_fine_dust)

# 방문자수 long 형식으로 변환
visitor_fd_long <- merged_fine_dust %>%
  pivot_longer(cols = starts_with("방문자수_"), 
               names_to = "연도", 
               values_to = "방문자수") %>%
  mutate(연도 = as.numeric(gsub("방문자수_", "", 연도)))

# 미세먼지 long 형식으로 변환
fine_dust_long <- merged_fine_dust %>%
  pivot_longer(cols = starts_with("미세먼지_"),
               names_to = "연도",
               values_to = "미세먼지") %>%
  mutate(연도 = gsub("미세먼지_", "", 연도))

visitor_fd_long <- visitor_fd_long %>%
  mutate(연도 = as.numeric(연도))

fine_dust_long <- fine_dust_long %>%
  mutate(연도 = as.numeric(연도))

visitor_dust_long <- inner_join(visitor_fd_long, fine_dust_long, by = c("시도명", "연도"))

# 방문자수, 미세먼지 데이터를 연도와 시도명 기준으로 합침
visitor_dust_long <- visitor_fd_long %>%
  inner_join(fine_dust_long, by = c("시도명", "연도"))
head(visitor_dust_long, 10)
print(head(visitor_dust_long, 10), width = Inf)
head(visitor_dust_long$방문자수, 10)
head(visitor_dust_long$미세먼지, 10)

#4. 전체 데이터(모든 시도, 모든 연도)에서 방문자수와 미세먼지 상관 분석
cor.test(visitor_dust_long$방문자수, visitor_dust_long$미세먼지, use = "complete.obs")

# 연도별 상관계수 구하기
visitor_dust_long %>%
  group_by(연도) %>%
  summarise(correlation = cor(방문자수, 미세먼지, use = "complete.obs"))


#─────────────────────────────────────────────────────────────────────────────────────
#[기온 데이터 (지역별)]
temp_2018 <- read.csv("데이터/전국 기온/년도별_지역별_기온/지역구분별_평균기온_2018.csv", fileEncoding = "UTF-8")
temp_2019 <- read.csv("데이터/전국 기온/년도별_지역별_기온/지역구분별_평균기온_2019.csv", fileEncoding = "UTF-8")
temp_2020 <- read.csv("데이터/전국 기온/년도별_지역별_기온/지역구분별_평균기온_2020.csv", fileEncoding = "UTF-8")
temp_2021 <- read.csv("데이터/전국 기온/년도별_지역별_기온/지역구분별_평균기온_2021.csv", fileEncoding = "UTF-8")
temp_2022 <- read.csv("데이터/전국 기온/년도별_지역별_기온/지역구분별_평균기온_2022.csv", fileEncoding = "UTF-8")
temp_2023 <- read.csv("데이터/전국 기온/년도별_지역별_기온/지역구분별_평균기온_2023.csv", fileEncoding = "UTF-8")
head(temp_2018,10)

temp_all_region <- temp_2018 %>%
  full_join(temp_2019, by = "지역구분", suffix = c("_2018", "_2019")) %>%
  full_join(temp_2020, by = "지역구분", suffix = c("", "_2020")) %>%
  full_join(temp_2021, by = "지역구분", suffix = c("", "_2021")) %>%
  full_join(temp_2022, by = "지역구분", suffix = c("", "_2022")) %>%
  full_join(temp_2023, by = "지역구분", suffix = c("", "_2023"))
names(temp_all_region)[names(temp_all_region) == "평균기온"] <- "평균기온_2021"
names(temp_all_region)[names(temp_all_region) == "지역구분"] <- "시도명"
head(temp_all_region)

save(temp_all_region, file = "./01_data/temp_all_region.rdata")
write.csv(temp_all_region, "./01_data/temp_all_region.csv")

# 방문자수 + 평균기온 병합
merged_temp <- inner_join(visitor_all_region, temp_all_region, by = "시도명")

visitor_temp_long <- merged_temp %>%
  pivot_longer(cols = starts_with("방문자수_"), 
               names_to = "연도", 
               values_to = "방문자수") %>%
  mutate(연도 = as.numeric(gsub("방문자수_", "", 연도)))

tmep_long <- merged_temp %>%
  pivot_longer(cols = starts_with("평균기온_"),
               names_to = "연도",
               values_to = "평균기온") %>%
  mutate(연도 = as.numeric(gsub("평균기온_", "", 연도)))

visitor_temp_long <- inner_join(visitor_temp_long, tmep_long, by = c("시도명", "연도"))
print(visitor_temp_long)

# 전체 상관 분석
cor.test(visitor_temp_long$방문자수, visitor_temp_long$평균기온, use = "complete.obs")

# 연도별 상관계수
visitor_temp_long %>%
  group_by(연도) %>%
  summarise(correlation = cor(방문자수, 평균기온, use = "complete.obs"))

#─────────────────────────────────────────────────────────────────────────────────────
#[폭염일수 데이터 (지역별)]
heat_wave_2018 <- read.csv("데이터/전국 폭염일수/지역별, 년도별/지역구분별_평균폭염일수_2018.csv", fileEncoding = "UTF-8")
heat_wave_2019 <- read.csv("데이터/전국 폭염일수/지역별, 년도별/지역구분별_평균폭염일수_2019.csv", fileEncoding = "UTF-8")
heat_wave_2020 <- read.csv("데이터/전국 폭염일수/지역별, 년도별/지역구분별_평균폭염일수_2020.csv", fileEncoding = "UTF-8")
heat_wave_2021 <- read.csv("데이터/전국 폭염일수/지역별, 년도별/지역구분별_평균폭염일수_2021.csv", fileEncoding = "UTF-8")
heat_wave_2022 <- read.csv("데이터/전국 폭염일수/지역별, 년도별/지역구분별_평균폭염일수_2022.csv", fileEncoding = "UTF-8")
heat_wave_2023 <- read.csv("데이터/전국 폭염일수/지역별, 년도별/지역구분별_평균폭염일수_2023.csv", fileEncoding = "UTF-8")

heat_wave_2018 <- heat_wave_2018 %>% rename(평균_폭염일수_2018 = 평균.폭염일수)
heat_wave_2019 <- heat_wave_2019 %>% rename(평균_폭염일수_2019 = 평균.폭염일수)
heat_wave_2020 <- heat_wave_2020 %>% rename(평균_폭염일수_2020 = 평균.폭염일수)
heat_wave_2021 <- heat_wave_2021 %>% rename(평균_폭염일수_2021 = 평균.폭염일수)
heat_wave_2022 <- heat_wave_2022 %>% rename(평균_폭염일수_2022 = 평균.폭염일수)
heat_wave_2023 <- heat_wave_2023 %>% rename(평균_폭염일수_2023 = 평균.폭염일수)
head(heat_wave_2018)
tail(heat_wave_2018)

# 폭염일수 데이터 통합
heat_wave_all_region <- heat_wave_2018 %>%
  full_join(heat_wave_2019, by = "지역구분", suffix = c("_2018", "_2019")) %>%
  full_join(heat_wave_2020, by = "지역구분", suffix = c("", "_2020")) %>%
  full_join(heat_wave_2021, by = "지역구분", suffix = c("", "_2021")) %>%
  full_join(heat_wave_2022, by = "지역구분", suffix = c("", "_2022")) %>%
  full_join(heat_wave_2023, by = "지역구분", suffix = c("", "_2023"))

names(heat_wave_all_region)[names(heat_wave_all_region) == "지역구분"] <- "시도명"
print(heat_wave_all_region)

save(heat_wave_all_region, file = "./01_data/heat_wave_all_region.rdata")
write.csv(heat_wave_all_region, "./01_data/heat_wave_all_region.csv")

# 방문자수 + 폭염일수 병합
merged_heat_wave <- inner_join(visitor_all_region, heat_wave_all_region, by = "시도명")

visitor_heat_long <- merged_heat_wave %>%
  pivot_longer(cols = starts_with("방문자수_"), 
               names_to = "연도", 
               values_to = "방문자수") %>%
  mutate(연도 = as.numeric(gsub("방문자수_", "", 연도)))

heat_long <- merged_heat_wave %>%
  pivot_longer(cols = starts_with("평균_폭염일수_"),
               names_to = "연도",
               values_to = "평균폭염일수") %>%
  mutate(연도 = as.numeric(gsub("평균_폭염일수_", "", 연도)))

visitor_heat_long <- inner_join(visitor_heat_long, heat_long, by = c("시도명", "연도"))
print(visitor_heat_long)

# 전체 상관 분석
cor.test(visitor_heat_long$방문자수, visitor_heat_long$평균폭염일수, use = "complete.obs")

# 연도별 상관계수
visitor_heat_long %>%
  group_by(연도) %>%
  summarise(correlation = cor(방문자수, 평균폭염일수, use = "complete.obs"))


