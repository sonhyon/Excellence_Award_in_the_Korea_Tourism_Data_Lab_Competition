library(lmtest) ; library(forecast) ; library(tseries)

# 데이터 불러오기 
load("./02_merged/merged_fd.rdata")
load("./02_merged/merged_heat.rdata")
load("./02_merged/merged_temp.rdata")
head(merged_fd)
head(merged_heat)
head(merged_temp)



#────────────────────────────────────────────────────────────────────────
#[미세먼지]
diff_visitors <- diff(merged_fd$방문자수)

# 차분 시계열과 원래 미세먼지를 데이터프레임으로
granger_data <- data.frame(
  visitors = diff_visitors,
  dust = merged_fd$미세먼지[-1]  # diff로 길이가 1 줄어들었으므로 맞춰주기
)

# Granger 인과성 검정 (lag 1)
grangertest(visitors ~ dust, order = 1, data = granger_data)
# lag 2
grangertest(visitors ~ dust, order = 2, data = granger_data)
# lag 3
grangertest(visitors ~ dust, order = 3, data = granger_data)

# [STL 시계열 분해]
ts_fd <- ts(merged_fd$미세먼지, start = c(2018, 1), frequency = 12)
stl_fd <- stl(ts_fd, s.window = "periodic")
plot(stl_fd)

#────────────────────────────────────────────────────────────────────────
#[기온]
# 방문자수 차분, 기온 그대로 사용 가능 시
granger_data <- data.frame(
  visitors = diff(merged_temp$방문자수),  # 차분한 경우
  temp = merged_temp$평균기온[-1]        # 길이 맞추기
)

# lag 기준 검정
grangertest(visitors ~ temp, order = 1, data = granger_data)
grangertest(visitors ~ temp, order = 2, data = granger_data)
grangertest(visitors ~ temp, order = 3, data = granger_data)

# [STL 시계열 분해]
ts_temp <- ts(merged_temp$평균기온, start = c(2018, 1), frequency = 12)
stl_temp <- stl(ts_temp, s.window = "periodic")
plot(stl_temp)

#────────────────────────────────────────────────────────────────────────
#[폭염일수]
granger_data <- data.frame(
  visitors = diff(merged_heat$방문자수),  # 차분한 경우
  heatdays = merged_heat$폭염일수[-1]     # 길이 맞추기
)

# lag 1 기준 검정
grangertest(visitors ~ heatdays, order = 1, data = granger_data)
grangertest(visitors ~ heatdays, order = 2, data = granger_data)
grangertest(visitors ~ heatdays, order = 3, data = granger_data)

# [STL 시계열 분해]
# ts 객체로 변환
ts_heat <- ts(merged_heat$폭염일수, start = c(2018, 1), frequency = 12)
stl_heat <- stl(ts_heat, s.window = "periodic")
plot(stl_heat)

