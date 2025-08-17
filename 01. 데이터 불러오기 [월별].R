#[전국 관광객수 데이터 (월별 2018~2024)]
#1. 데이터 불러오기
visitor_monthly_2018 <- read.csv("./데이터/전국 관광객수/월별/지역 방문자수_관광지출액 추세_2018.csv", fileEncoding = "UTF-8")
visitor_monthly_2019 <- read.csv("./데이터/전국 관광객수/월별/지역 방문자수_관광지출액 추세_2019.csv", fileEncoding = "UTF-8")
visitor_monthly_2020 <- read.csv("./데이터/전국 관광객수/월별/지역 방문자수_관광지출액 추세_2020.csv", fileEncoding = "UTF-8")
visitor_monthly_2021 <- read.csv("./데이터/전국 관광객수/월별/지역 방문자수_관광지출액 추세_2021.csv", fileEncoding = "UTF-8")
visitor_monthly_2022 <- read.csv("./데이터/전국 관광객수/월별/지역 방문자수_관광지출액 추세_2022.csv", fileEncoding = "UTF-8")
visitor_monthly_2023 <- read.csv("./데이터/전국 관광객수/월별/지역 방문자수_관광지출액 추세_2023.csv", fileEncoding = "UTF-8")
visitor_monthly_2024 <- read.csv("./데이터/전국 관광객수/월별/지역 방문자수_관광지출액 추세_2024.csv", fileEncoding = "UTF-8")
head(visitor_monthly_2018)

#2. 데이터 전처리하기
visitor_monthly_2018 <- visitor_monthly_2018[,c(1,2)]
visitor_monthly_2019 <- visitor_monthly_2019[,c(1,2)]
visitor_monthly_2020 <- visitor_monthly_2020[,c(1,2)]
visitor_monthly_2021 <- visitor_monthly_2021[,c(1,2)]
visitor_monthly_2022 <- visitor_monthly_2022[,c(1,2)]
visitor_monthly_2023 <- visitor_monthly_2023[,c(1,2)]
visitor_monthly_2024 <- visitor_monthly_2024[,c(1,2)]
head(visitor_monthly_2018)

visitor_all_monthly <- bind_rows(
  visitor_monthly_2018,
  visitor_monthly_2019,
  visitor_monthly_2020,
  visitor_monthly_2021,
  visitor_monthly_2022,
  visitor_monthly_2023,
  visitor_monthly_2024
)
head(visitor_all_monthly, 10)

save(visitor_all_monthly, file = "./01_data/visitor_all_monthly.rdata")
write.csv(visitor_all_monthly, "./01_data/visitor_all_monthly.csv")


#───────────────────────────────────────────────────────────────────────────────────────────────
#[미세먼지 데이터 (월별)]
fd_all <- read.csv("./데이터/전국 미세먼지/월별/미세먼지 (2018~2024).csv", fileEncoding = "UTF-8-BOM")
head(fd_all)

# 1. 열 이름에서 X 제거
colnames(fd_all) <- gsub("^X", "", colnames(fd_all))

# 2. wide → long 변환
fd_all_monthly <- fd_all %>%
  pivot_longer(
    cols = everything(),
    names_to = "기준연월",
    values_to = "미세먼지"
  ) %>%
  mutate(
    기준연월 = gsub("\\.", "", 기준연월)  # "2018.01" → "201801"
  )

head(fd_all_monthly, 12)

save(fd_all_monthly, file = "./01_data/fd_all_monthly.rdata")
write.csv(fd_all_monthly, "./01_data/fd_all_monthly.csv")

# 2️⃣ 방문객수 데이터와 기온 데이터 합병
# visitor_monthly_all에도 '기준연월' 컬럼이 있어야 하고, 타입 확인 필요
str(fd_all_monthly$기준연월)  # integer인지 확인
fd_all_monthly$기준연월 <- as.integer(fd_all_monthly$기준연월)

merged_fd <- inner_join(visitor_all_monthly, fd_all_monthly, by = "기준연월")
head(merged_fd)
save(merged_fd, file = "./02_merged/merged_fd.rdata")

# 3️⃣ 방문객수와 평균기온 상관계수 계산
# NA 제거 및 numeric 확인
merged_fd <- merged_fd %>%
  filter(!is.na(방문자수) & !is.na(미세먼지))

merged_fd$방문자수 <- as.numeric(merged_fd$방문자수)
merged_fd$미세먼지 <- as.numeric(merged_fd$미세먼지)

cor_test_result <- cor.test(merged_fd$방문자수, merged_fd$미세먼지, use = "complete.obs")
cor_test_result


#───────────────────────────────────────────────────────────────────────────────────────────────
#[기온 데이터 불러오기 (월별)]
temp_all_monthly <- read.csv("./데이터/전국 기온/월별/전국 월별 기온데이터 (2018~2024).csv",fileEncoding = "CP949")
temp_all_monthly <- temp_all_monthly[, c(1, 3)]

temp_all_monthly$년월 <- gsub("[\t-]", "", temp_all_monthly$년월) # 연월 형식 정리: 'YYYY-MM' → 'YYYYMM' 정수로 변환
temp_all_monthly$년월 <- as.integer(temp_all_monthly$년월)
names(temp_all_monthly) <- c("기준연월", "평균기온") # 컬럼명 변경

head(temp_all_monthly, 10)

save(temp_all_monthly, file = "./01_data/temp_all_monthly.rdata")
write.csv(temp_all_monthly, "./01_data/temp_all_monthly.csv")

# 2️⃣ 방문객수 데이터와 기온 데이터 합병
# visitor_monthly_all에도 '기준연월' 컬럼이 있어야 하고, 타입 확인 필요
str(temp_all_monthly$기준연월)  # integer인지 확인
temp_all_monthly$기준연월 <- as.integer(temp_all_monthly$기준연월)

merged_temp <- inner_join(visitor_all_monthly, temp_all_monthly, by = "기준연월")
head(merged_temp)
save(merged_temp, file = "./02_merged/merged_temp.rdata")

# 3️⃣ 방문객수와 평균기온 상관계수 계산
# NA 제거 및 numeric 확인
merged_temp <- merged_temp %>%
  filter(!is.na(방문자수) & !is.na(평균기온))

merged_temp$방문자수 <- as.numeric(merged_temp$방문자수)
merged_temp$평균기온 <- as.numeric(merged_temp$평균기온)

cor_test_result <- cor.test(merged_temp$방문자수, merged_temp$평균기온, use = "complete.obs")
cor_test_result


#───────────────────────────────────────────────────────────────────────────────────────────────
#[폭염일수 데이터 (월별)]
heat_all <- read.csv("./데이터/전국 폭염일수/월별/폭염일수 (2018~2024).csv", fileEncoding = "UTF-8-BOM")
colnames(heat_all) <- gsub("^X", "", colnames(heat_all))
head(heat_all, 7)

colnames(heat_all)[1] <- "연도"
heat_all_monthly <- heat_all %>%
  pivot_longer(
    cols = -연도,                   # 이제 연도 열 제외
    names_to = "월",
    values_to = "폭염일수") %>%
  mutate(
    월 = gsub("월", "", 월),
    기준연월 = paste0(연도, sprintf("%02d", as.integer(월)))) %>%
  select(기준연월, 폭염일수)

head(heat_all_monthly, 12)

save(heat_all_monthly, file = "./01_data/heat_all_monthly.rdata")
write.csv(heat_all_monthly, "./01_data/heat_all_monthly.csv")

str(heat_all_monthly$기준연월)  # integer인지 확인
heat_all_monthly$기준연월 <- as.integer(heat_all_monthly$기준연월)

merged_heat <- inner_join(visitor_all_monthly, heat_all_monthly, by = "기준연월")
head(merged_heat)
save(merged_heat, file = "./02_merged/merged_heat.rdata")
