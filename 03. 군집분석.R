#데이터 불러오기
load("./01_data/visitor_all_region.rdata")
load("./01_data/fine_dust_all_region.rdata")
load("./01_data/temp_all_region.rdata")
load("./01_data/heat_wave_all_region.rdata")
load("./01_data/visitor_all_monthly.rdata")
load("./01_data/temp_all_monthly.rdata")
load("./01_data/fd_all_monthly.rdata")
head(visitor_all_region)
head(fine_dust_all_region)
head(temp_all_region)
head(heat_wave_all_region)
head(visitor_all_monthly)
head(temp_all_monthly)
head(fd_all_monthly)

list_all <- list(visitor_all_region, fine_dust_all_region, temp_all_region, heat_wave_all_region)
merged_data <- reduce(list_all, ~merge(.x, .y, by = "시도명", all.x = TRUE))
head(merged_data,5)

#dir.create("./03_cluster")
save(merged_data, file = "./03_cluster/merged_data.rdata")


#[군집분석]
# 군집에 사용할 수치형 컬럼만 추출
cluster_data <- merged_data[, c(
  # 방문자수
  "방문자수_2018", "방문자수_2019", "방문자수_2020",
  "방문자수_2021", "방문자수_2022", "방문자수_2023",
  
  # 미세먼지 (X2018 ~ X2023)
  "미세먼지_2018", "미세먼지_2019", "미세먼지_2020", 
  "미세먼지_2021", "미세먼지_2022", "미세먼지_2023",
  
  # 평균기온
  "평균기온_2018", "평균기온_2019", "평균기온_2020",
  "평균기온_2021", "평균기온_2022", "평균기온_2023",
  
  # 평균 폭염일수
  "평균_폭염일수_2018", "평균_폭염일수_2019", "평균_폭염일수_2020",
  "평균_폭염일수_2021", "평균_폭염일수_2022", "평균_폭염일수_2023"
)]

# 표준화
cluster_data_scaled <- scale(cluster_data)

wss <- numeric(10)
for (k in 1:10) {
  wss[k] <- sum(kmeans(cluster_data_scaled, centers = k, nstart = 25)$withinss)
}

plot(1:10, wss, type = "b", pch = 19, frame = FALSE,
     xlab = "Number of Clusters K",
     ylab = "Total Within-Clusters Sum of Squares",
     main = "Elbow Method for Optimal K")

set.seed(123)  # 재현 가능성을 위해 고정
k_result <- kmeans(cluster_data_scaled, centers = 4, nstart = 25)
head(k_result)

# 결과 확인
table(k_result$cluster)

merged_data$cluster <- as.factor(k_result$cluster)
head(merged_data)

# 첫 두 주성분 기준으로 시각화
fviz_cluster(k_result, data = cluster_data_scaled,
             geom = "point", ellipse.type = "norm",
             palette = "jco", ggtheme = theme_minimal())


# 각 클러스터별 평균 계산
cluster_summary <- aggregate(cluster_data, 
                             by = list(cluster = merged_data$cluster), 
                             FUN = mean)

View(cluster_summary)  # 혹은 print(cluster_summary)



merged_data %>%
  group_by(cluster) %>%
  summarise(지역목록 = paste(시도명, collapse = ", "))
