#[데이터 불러오기]
load("./03_cluster/merged_data.rdata")
head(merged_data,2)


merged_long <- merged_data %>%
  pivot_longer(
    cols = starts_with("방문자수"),
    names_to = "연도",
    values_to = "방문자수"
  ) %>%
  mutate(연도 = gsub("방문자수_", "", 연도))  # 연도만 남기기

merged_long <- merged_long %>%
  mutate(방문자수_그룹 = cut(
    방문자수,
    breaks = quantile(방문자수, probs = c(0, 1/3, 2/3, 1)),
    labels = c("Low", "Medium", "High"),
    include.lowest = TRUE
  ))

# 미세먼지 long 형태
fine_dust_long <- merged_data %>%
  pivot_longer(
    cols = starts_with("미세먼지"),
    names_to = "연도",
    values_to = "미세먼지"
  ) %>%
  mutate(연도 = gsub("미세먼지_", "", 연도))

# 평균기온 long 형태
temp_long <- merged_data %>%
  pivot_longer(
    cols = starts_with("평균기온"),
    names_to = "연도",
    values_to = "평균기온"
  ) %>%
  mutate(연도 = gsub("평균기온_", "", 연도))

# 폭염일수 long 형태
heat_long <- merged_data %>%
  pivot_longer(
    cols = starts_with("평균_폭염일수"),
    names_to = "연도",
    values_to = "평균_폭염일수"
  ) %>%
  mutate(연도 = gsub("평균_폭염일수_", "", 연도))

# 최종 데이터 합치기
analysis_data <- merged_long %>%
  left_join(fine_dust_long, by = c("시도명", "연도")) %>%
  left_join(temp_long, by = c("시도명", "연도")) %>%
  left_join(heat_long, by = c("시도명", "연도"))

fit <- rpart(방문자수_그룹 ~ 미세먼지 + 평균기온 + 평균_폭염일수,
             data = analysis_data, method = "class")

# 트리 시각화
rpart.plot(fit)

