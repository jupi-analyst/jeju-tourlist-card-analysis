#--------------------------------------------------------------------------------
library(tidyverse)
library(rjson)
library(jsonlite)
library(httr)
library(RCurl)
# 제주도민, 내국인관광객 수집
# getwd()
# setwd('C:/Users/USER/Documents/sh_R')
# serviceURL <- 'https://gw.jejudatahub.net/api/proxy/'
# api_key <- '113a8309e22b47faba8309e22b47fa44'
# operation <- '072040e53b1f11e9ab958f5bbd84cea8'
# startDate <- 201701
# endDate <- 201712
# limit = 100
# url <- paste0(serviceURL,
#               operation,
#               paste0('/', api_key),
#               paste0('?startDate=', startDate),
#               paste0('&endDate=', endDate),
#               paste0('&limit=', limit))
# get_url <- getURL(url)
# json_page <- fromJSON(get_url)
# numOfRows <- dim(json_page$data)[1] # 첫 페이지 데이터 수
# totalCount <- json_page$totCnt # 총 데이터 수
# 
# loopCount <- round(totalCount / numOfRows, 0)
# 
# if(loopCount * numOfRows < totalCount){
#   loopCount <- loopCount + 1
# }
# 
# tmp <- data.frame()
# for(i in 1:loopCount){
#   url <- paste0(serviceURL,
#                 operation,
#                 paste0('/', api_key),
#                 paste0('?startDate=', startDate),
#                 paste0('&endDate=', endDate),
#                 paste0('&number=', i),
#                 paste0('&limit=', limit))
#   get_url <- getURL(url)
#   json_page <- fromJSON(get_url)
#   df <- as.data.frame(json_page$data)
#   tmp <- bind_rows(tmp, df)
#   Sys.sleep(runif(1,min=0.1,max=1))
#   print(i)
# }
# write.csv(tmp, './data/관광객소비_2017.csv')

## 중국인관광객 수집
# getwd()
# setwd('C:/Users/USER/Documents/sh_R')
# serviceURL <- 'https://gw.jejudatahub.net/api/proxy/'
# api_key <- '113a8309e22b47faba8309e22b47fa44'
# operation <- '94e2c32f3b1d11e9ab958f5bbd84cea8'
# startDate <- 201801
# endDate <- 201812
# limit = 100
# url <- paste0(serviceURL,
#               operation,
#               paste0('/', api_key),
#               paste0('?startDate=', startDate),
#               paste0('&endDate=', endDate),
#               paste0('&limit=', limit))
# get_url <- getURL(url)
# json_page <- fromJSON(get_url)
# numOfRows <- dim(json_page$data)[1] # 첫 페이지 데이터 수
# totalCount <- json_page$totCnt # 총 데이터 수
# 
# loopCount <- round(totalCount / numOfRows, 0)
# 
# if(loopCount * numOfRows < totalCount){
#   loopCount <- loopCount + 1
# }
# 
# tmp <- data.frame()
# for(i in 1:loopCount){
#   url <- paste0(serviceURL,
#                 operation,
#                 paste0('/', api_key),
#                 paste0('?startDate=', startDate),
#                 paste0('&endDate=', endDate),
#                 paste0('&number=', i),
#                 paste0('&limit=', limit))
#   get_url <- getURL(url)
#   json_page <- fromJSON(get_url)
#   df <- as.data.frame(json_page$data)
#   tmp <- bind_rows(tmp, df)
#   Sys.sleep(runif(1,min=0.1,max=1))
#   print(i)
# }
# write.csv(tmp, './data/중국인관광객소비_2017.csv')
# write.csv(tmp, './data/중국인관광객소비_2018.csv') 
# ------------------------------------------------------------------------------------------------
getwd()
setwd('C:/Users/USER/Documents/sh_R')
df_2018 <- read.csv('./data/관광객소비.csv')
df_2017 <- read.csv('./data/관광객소비_2017.csv')
china_2017 <- read.csv('./data/중국인관광객소비_2017.csv')
china_2018 <- read.csv('./data/중국인관광객소비_2018.csv')
table(is.na(df_2017))
duplicated(df_2017)
df_2017 <- tmp[!duplicated(df_2017), ]
jeju <- rbind(df_2017, df_2018)
jeju <- jeju[,-1]
china <- rbind(china_2017, china_2018)
china <- china[,-1]
china$userType <- '중국인관광객'
china$ageGroup <- NA
china$gender <- NA
colnames(jeju)
colnames(china)
china <- china[,c("sido","sigungu","cityGubun","marketType","userType","ageGroup","gender","dtYearMonth","userCount","useCount","useCost")]
tourlist <- rbind(jeju,china)
tourlist$dtYearMonth <- as.character(tourlist$dtYearMonth)
tourlist$Year <- substr(tourlist$dtYearMonth, 1, 4)
tourlist$Month <- substr(tourlist$dtYearMonth, 5, 6)
str(tourlist)

library(tidyverse)
library(lubridate)
library(ggplot2)
library(reshape)
library(extrafont)
library(ggplot2)
rm(china, china_2017, china_2018, df_2017, df_2018, jeju)
str(tourlist)

tourlist$인당_평균이용금액 <- tourlist$useCost / tourlist$userCount
tourlist$건당_평균이용금액 <- tourlist$useCost / tourlist$useCount
tourlist$인당_평균이용건수 <- tourlist$useCount / tourlist$userCount
tourlist[which(tourlist$ageGroup == '20 미만'), 'ageGroup'] = '20대미만'

tourlist %>%
  group_by(Year, Month) %>%
  summarise(총이용금액 = round(sum(useCost) / 100000000,0)) -> p1
p1 %>%
  ggplot(aes(x = Month, y = 총이용금액)) +
  geom_line(aes(color = Year, group = Year), size = 0.7) +
  labs(x = '월별', y = '총이용금액(억단위)') +
  ggtitle("2017년과 2018년 총이용금액 비교") +
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 15, color = "Black"))




# 2017년, 2018년도 이용자별 총이용금액 비교
tourlist %>%
  group_by(userType, Year) %>%
  summarise(useCost_sum = round(sum(useCost) / 100000000,0)) -> p2
p2 %>%
  ggplot(aes(x = userType, y = useCost_sum, fill = Year)) +
  geom_bar(stat='identity', position = 'dodge') +
  geom_text(aes(label = useCost_sum), vjust = -0.5, position = position_dodge(0.9)) +
  labs(x = '이용자유형', y = '총이용금액(억단위)')

# 2017,2018년 업종별 총이용금액 비교
tourlist %>%
  filter(marketType != '기타') %>%
  group_by(marketType, Year) %>%
  summarise(useCost_sum = round(sum(useCost) / 100000000,0)) -> p3
p3 %>%
  ggplot(aes(x = marketType, y = useCost_sum, fill = Year)) +
  geom_bar(stat='identity', position = 'dodge') +
  geom_text(aes(label = useCost_sum), vjust = -0.5, position = position_dodge(0.9)) +
  labs(x = '업종', y = '총이용금액(억단위)')

# 2017,2018년 이용자별 총이용수
tourlist %>%
  group_by(userType, Year) %>%
  summarise(useCount_sum = round(sum(useCount)/1000000,2)) %>%
  ggplot(aes(x = userType, y = useCount_sum, fill = Year)) +
  geom_bar(stat='identity', position = 'dodge') +
  geom_text(aes(label = useCount_sum), vjust = -0.5, position = position_dodge(0.9)) +
  labs(x = '이용자유형', y = '총 이용수(백만단위)')

tourlist %>%
  filter(Year == '2018') %>%
  filter(userType != '제주도민') %>%
  group_by(userType, marketType) %>%
  summarise(총이용금액 = sum(useCost),
            총이용수 = sum(userCount),
            총이용건수 = round(sum(useCount),1),
            인당_평균이용건수 = 총이용건수 / 총이용수,
            인당_평균이용금액 = 총이용금액 / 총이용수,
            건당_평균이용금액 = 총이용금액 / 총이용건수) -> p4
p4 %>%
  ggplot(aes(x = marketType, y = 총이용금액, fill = userType)) +
  geom_bar(stat='identity')

tourlist %>% 
  filter(userType == '내국인관광객') %>%
  group_by(ageGroup, marketType) %>% 
  summarise(총이용금액 = sum(useCost),
                   총이용수 = sum(userCount),
                   총이용건수 = round(sum(useCount),1),
                   인당_평균이용건수 = 총이용건수 / 총이용수,
                   인당_평균이용금액 = 총이용금액 / 총이용수,
                   건당_평균이용금액 = 총이용금액 / 총이용건수) -> p5

# 나이대별, 월별 이용금액
tourlist %>%
  filter(Year == '2018') %>%
  group_by(ageGroup, Month) %>%
  filter(ageGroup != 'NA') %>%
  summarise(총사용금액 = round(sum(useCost) / 100000000,0)) -> p6
p6 %>%
  ggplot(aes(x = Month, y = 총사용금액)) +
  geom_line(aes(color = ageGroup, group = ageGroup), size = .7) +
  geom_point() +
  facet_wrap(~ ageGroup)

tourlist %>%
  filter(Year == '2018') %>%
  filter(userType == '중국인관광객' | userType == '내국인관광객') %>%
  group_by(userType, Month) %>%
  summarise(총이용금액 = round(sum(useCost)/100000000, 1),
                 총이용자수 = sum(userCount),
                 총이용건수 = sum(useCount),
                 인당_평균건수 = sum(useCount) / sum(userCount),
                 인당_이용금액 = sum(useCost) / sum(userCount),
                 건당_이용금액 = sum(useCost) / sum(useCount)) -> p8
p8 %>% filter(userType == '내국인관광객') %>%
  ggplot(aes(x = Month, y = 총이용금액)) +
  geom_bar(stat = 'identity', colour="black", fill = "#FF6600")
p8 %>% filter(userType == '중국인관광객') %>%
  ggplot(aes(x = Month, y = 총이용금액)) +
  geom_bar(stat = 'identity', colour="black",fill='#5CBED2')
p8 %>% 
  ggplot(aes(x = Month, y = 총이용금액)) +
  geom_bar(stat = 'identity', aes(fill = userType), position = 'dodge', colour="black")


tourlist %>%
  filter(Year == '2018')  %>%
  filter(userType == '중국인관광객' | userType == '내국인관광객') %>%
  group_by(userType, marketType) %>%
  summarise(총이용금액 = round(sum(useCost)/100000000, 1),
                 총이용자수 = sum(userCount),
                 총이용건수 = sum(useCount),
                 인당_평균건수 = sum(useCount) / sum(userCount),
                 인당_이용금액 = sum(useCost) / sum(userCount),
                 건당_이용금액 = sum(useCost) / sum(useCount)) -> p9
p9 %>% 
  ggplot(aes(x = marketType, y = 총이용금액, fill = userType)) +
  geom_bar(stat = 'identity', position = 'dodge', color = 'black')
p9 %>% filter(userType == '내국인관광객') %>% 
  ggplot(aes(x = marketType, y = 총이용금액)) +
  geom_bar(stat = 'identity', position = 'dodge', color = 'black',fill = "#FF6600")
p9 %>% filter(userType == '중국인관광객') %>% 
  ggplot(aes(x = marketType, y = 총이용금액)) +
  geom_bar(stat = 'identity', position = 'dodge', color = 'black', fill='#5CBED2')

tourlist %>%
  filter(Year == '2018')  %>%
  filter(userType == '내국인관광객') %>%
  group_by(ageGroup) %>%
  summarise(총이용금액 = round(sum(useCost)/100000000, 1),
                 총이용자수 = sum(userCount),
                 총이용건수 = sum(useCount),
                 인당_평균건수 = sum(useCount) / sum(userCount),
                 인당_이용금액 = sum(useCost) / sum(userCount),
                 건당_이용금액 = sum(useCost) / sum(useCount)) -> p10
p10 %>%
  ggplot(aes(x = ageGroup, y = 총이용금액)) +
  geom_bar(stat = 'identity')

tourlist %>%
  filter(Year == '2018')  %>%
  filter(userType == '내국인관광객') %>%
  group_by(ageGroup, marketType) %>%
  summarise(총이용금액 = round(sum(useCost)/100000000, 1),
                 총이용자수 = sum(userCount),
                 총이용건수 = sum(useCount),
                 인당_평균건수 = sum(useCount) / sum(userCount),
                 인당_이용금액 = sum(useCost) / sum(userCount),
                 건당_이용금액 = sum(useCost) / sum(useCount)) -> p11
p11 %>%
  ggplot(aes(x = ageGroup, y = 총이용금액, fill = marketType)) +
  geom_bar(stat = 'identity', position = 'dodge')

marketType_wide <- cast(p9, userType ~ marketType, value = '총이용금액')
tourlist_wide <- cast(p8, userType ~ Month, value = '총이용금액')




for (i in rev(3:ncol(tourlist_wide))){
  tourlist_wide[,i] <- round(tourlist_wide[,i] / tourlist_wide[,2],4)
}
rm(i)
#각 업종에 따른 나이대별 총이용금액
test <- function(a) {
  graph <-df %>%
    filter(marketType == a) %>%
    group_by(ageGroup) %>%
    summarise(합 = round(sum(useCost) / 100000000 , 0)) %>%
    ggplot(aes(x = ageGroup, y = 합, fill = ageGroup)) +
    geom_bar(stat = 'identity', position = 'dodge')
  graph + labs(title = paste0('업종에 따른 나이대별 총 사용금액','(', a ,')'))
}
test('소매')

df %>%
  group_by(userType, dtYearMonth) %>% 
  summarise(합 = round(sum(useCost) / 100000000 , 0)) %>%
  ggplot(aes(x = as.character(dtYearMonth), y = 합, fill = userType)) +
  geom_bar(stat = 'identity', position = 'dodge')
    # age_date_wide <- cast(age_date, ageGroup ~ dtYearMonth, value = 'useCost_mean')
