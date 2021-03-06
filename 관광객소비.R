#--------------------------------------------------------------------------------
library(tidyverse)
library(rjson)
library(jsonlite)
library(httr)
library(RCurl)
# ���ֵ���, �����ΰ����� ����
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
# numOfRows <- dim(json_page$data)[1] # ù ������ ������ ��
# totalCount <- json_page$totCnt # �� ������ ��
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
# write.csv(tmp, './data/�������Һ�_2017.csv')

## �߱��ΰ����� ����
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
# numOfRows <- dim(json_page$data)[1] # ù ������ ������ ��
# totalCount <- json_page$totCnt # �� ������ ��
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
# write.csv(tmp, './data/�߱��ΰ������Һ�_2017.csv')
# write.csv(tmp, './data/�߱��ΰ������Һ�_2018.csv') 
# ------------------------------------------------------------------------------------------------
getwd()
setwd('C:/Users/USER/Documents/sh_R')
df_2018 <- read.csv('./data/�������Һ�.csv')
df_2017 <- read.csv('./data/�������Һ�_2017.csv')
china_2017 <- read.csv('./data/�߱��ΰ������Һ�_2017.csv')
china_2018 <- read.csv('./data/�߱��ΰ������Һ�_2018.csv')
table(is.na(df_2017))
duplicated(df_2017)
df_2017 <- tmp[!duplicated(df_2017), ]
jeju <- rbind(df_2017, df_2018)
jeju <- jeju[,-1]
china <- rbind(china_2017, china_2018)
china <- china[,-1]
china$userType <- '�߱��ΰ�����'
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

tourlist$�δ�_����̿�ݾ� <- tourlist$useCost / tourlist$userCount
tourlist$�Ǵ�_����̿�ݾ� <- tourlist$useCost / tourlist$useCount
tourlist$�δ�_����̿�Ǽ� <- tourlist$useCount / tourlist$userCount
tourlist[which(tourlist$ageGroup == '20 �̸�'), 'ageGroup'] = '20��̸�'

tourlist %>%
  group_by(Year, Month) %>%
  summarise(���̿�ݾ� = round(sum(useCost) / 100000000,0)) -> p1
p1 %>%
  ggplot(aes(x = Month, y = ���̿�ݾ�)) +
  geom_line(aes(color = Year, group = Year), size = 0.7) +
  labs(x = '����', y = '���̿�ݾ�(�����)') +
  ggtitle("2017��� 2018�� ���̿�ݾ� ��") +
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 15, color = "Black"))




# 2017��, 2018�⵵ �̿��ں� ���̿�ݾ� ��
tourlist %>%
  group_by(userType, Year) %>%
  summarise(useCost_sum = round(sum(useCost) / 100000000,0)) -> p2
p2 %>%
  ggplot(aes(x = userType, y = useCost_sum, fill = Year)) +
  geom_bar(stat='identity', position = 'dodge') +
  geom_text(aes(label = useCost_sum), vjust = -0.5, position = position_dodge(0.9)) +
  labs(x = '�̿�������', y = '���̿�ݾ�(�����)')

# 2017,2018�� ������ ���̿�ݾ� ��
tourlist %>%
  filter(marketType != '��Ÿ') %>%
  group_by(marketType, Year) %>%
  summarise(useCost_sum = round(sum(useCost) / 100000000,0)) -> p3
p3 %>%
  ggplot(aes(x = marketType, y = useCost_sum, fill = Year)) +
  geom_bar(stat='identity', position = 'dodge') +
  geom_text(aes(label = useCost_sum), vjust = -0.5, position = position_dodge(0.9)) +
  labs(x = '����', y = '���̿�ݾ�(�����)')

# 2017,2018�� �̿��ں� ���̿��
tourlist %>%
  group_by(userType, Year) %>%
  summarise(useCount_sum = round(sum(useCount)/1000000,2)) %>%
  ggplot(aes(x = userType, y = useCount_sum, fill = Year)) +
  geom_bar(stat='identity', position = 'dodge') +
  geom_text(aes(label = useCount_sum), vjust = -0.5, position = position_dodge(0.9)) +
  labs(x = '�̿�������', y = '�� �̿��(�鸸����)')

tourlist %>%
  filter(Year == '2018') %>%
  filter(userType != '���ֵ���') %>%
  group_by(userType, marketType) %>%
  summarise(���̿�ݾ� = sum(useCost),
            ���̿�� = sum(userCount),
            ���̿�Ǽ� = round(sum(useCount),1),
            �δ�_����̿�Ǽ� = ���̿�Ǽ� / ���̿��,
            �δ�_����̿�ݾ� = ���̿�ݾ� / ���̿��,
            �Ǵ�_����̿�ݾ� = ���̿�ݾ� / ���̿�Ǽ�) -> p4
p4 %>%
  ggplot(aes(x = marketType, y = ���̿�ݾ�, fill = userType)) +
  geom_bar(stat='identity')

tourlist %>% 
  filter(userType == '�����ΰ�����') %>%
  group_by(ageGroup, marketType) %>% 
  summarise(���̿�ݾ� = sum(useCost),
                   ���̿�� = sum(userCount),
                   ���̿�Ǽ� = round(sum(useCount),1),
                   �δ�_����̿�Ǽ� = ���̿�Ǽ� / ���̿��,
                   �δ�_����̿�ݾ� = ���̿�ݾ� / ���̿��,
                   �Ǵ�_����̿�ݾ� = ���̿�ݾ� / ���̿�Ǽ�) -> p5

# ���̴뺰, ���� �̿�ݾ�
tourlist %>%
  filter(Year == '2018') %>%
  group_by(ageGroup, Month) %>%
  filter(ageGroup != 'NA') %>%
  summarise(�ѻ��ݾ� = round(sum(useCost) / 100000000,0)) -> p6
p6 %>%
  ggplot(aes(x = Month, y = �ѻ��ݾ�)) +
  geom_line(aes(color = ageGroup, group = ageGroup), size = .7) +
  geom_point() +
  facet_wrap(~ ageGroup)

tourlist %>%
  filter(Year == '2018') %>%
  filter(userType == '�߱��ΰ�����' | userType == '�����ΰ�����') %>%
  group_by(userType, Month) %>%
  summarise(���̿�ݾ� = round(sum(useCost)/100000000, 1),
                 ���̿��ڼ� = sum(userCount),
                 ���̿�Ǽ� = sum(useCount),
                 �δ�_��հǼ� = sum(useCount) / sum(userCount),
                 �δ�_�̿�ݾ� = sum(useCost) / sum(userCount),
                 �Ǵ�_�̿�ݾ� = sum(useCost) / sum(useCount)) -> p8
p8 %>% filter(userType == '�����ΰ�����') %>%
  ggplot(aes(x = Month, y = ���̿�ݾ�)) +
  geom_bar(stat = 'identity', colour="black", fill = "#FF6600")
p8 %>% filter(userType == '�߱��ΰ�����') %>%
  ggplot(aes(x = Month, y = ���̿�ݾ�)) +
  geom_bar(stat = 'identity', colour="black",fill='#5CBED2')
p8 %>% 
  ggplot(aes(x = Month, y = ���̿�ݾ�)) +
  geom_bar(stat = 'identity', aes(fill = userType), position = 'dodge', colour="black")


tourlist %>%
  filter(Year == '2018')  %>%
  filter(userType == '�߱��ΰ�����' | userType == '�����ΰ�����') %>%
  group_by(userType, marketType) %>%
  summarise(���̿�ݾ� = round(sum(useCost)/100000000, 1),
                 ���̿��ڼ� = sum(userCount),
                 ���̿�Ǽ� = sum(useCount),
                 �δ�_��հǼ� = sum(useCount) / sum(userCount),
                 �δ�_�̿�ݾ� = sum(useCost) / sum(userCount),
                 �Ǵ�_�̿�ݾ� = sum(useCost) / sum(useCount)) -> p9
p9 %>% 
  ggplot(aes(x = marketType, y = ���̿�ݾ�, fill = userType)) +
  geom_bar(stat = 'identity', position = 'dodge', color = 'black')
p9 %>% filter(userType == '�����ΰ�����') %>% 
  ggplot(aes(x = marketType, y = ���̿�ݾ�)) +
  geom_bar(stat = 'identity', position = 'dodge', color = 'black',fill = "#FF6600")
p9 %>% filter(userType == '�߱��ΰ�����') %>% 
  ggplot(aes(x = marketType, y = ���̿�ݾ�)) +
  geom_bar(stat = 'identity', position = 'dodge', color = 'black', fill='#5CBED2')

tourlist %>%
  filter(Year == '2018')  %>%
  filter(userType == '�����ΰ�����') %>%
  group_by(ageGroup) %>%
  summarise(���̿�ݾ� = round(sum(useCost)/100000000, 1),
                 ���̿��ڼ� = sum(userCount),
                 ���̿�Ǽ� = sum(useCount),
                 �δ�_��հǼ� = sum(useCount) / sum(userCount),
                 �δ�_�̿�ݾ� = sum(useCost) / sum(userCount),
                 �Ǵ�_�̿�ݾ� = sum(useCost) / sum(useCount)) -> p10
p10 %>%
  ggplot(aes(x = ageGroup, y = ���̿�ݾ�)) +
  geom_bar(stat = 'identity')

tourlist %>%
  filter(Year == '2018')  %>%
  filter(userType == '�����ΰ�����') %>%
  group_by(ageGroup, marketType) %>%
  summarise(���̿�ݾ� = round(sum(useCost)/100000000, 1),
                 ���̿��ڼ� = sum(userCount),
                 ���̿�Ǽ� = sum(useCount),
                 �δ�_��հǼ� = sum(useCount) / sum(userCount),
                 �δ�_�̿�ݾ� = sum(useCost) / sum(userCount),
                 �Ǵ�_�̿�ݾ� = sum(useCost) / sum(useCount)) -> p11
p11 %>%
  ggplot(aes(x = ageGroup, y = ���̿�ݾ�, fill = marketType)) +
  geom_bar(stat = 'identity', position = 'dodge')

marketType_wide <- cast(p9, userType ~ marketType, value = '���̿�ݾ�')
tourlist_wide <- cast(p8, userType ~ Month, value = '���̿�ݾ�')




for (i in rev(3:ncol(tourlist_wide))){
  tourlist_wide[,i] <- round(tourlist_wide[,i] / tourlist_wide[,2],4)
}
rm(i)
#�� ������ ���� ���̴뺰 ���̿�ݾ�
test <- function(a) {
  graph <-df %>%
    filter(marketType == a) %>%
    group_by(ageGroup) %>%
    summarise(�� = round(sum(useCost) / 100000000 , 0)) %>%
    ggplot(aes(x = ageGroup, y = ��, fill = ageGroup)) +
    geom_bar(stat = 'identity', position = 'dodge')
  graph + labs(title = paste0('������ ���� ���̴뺰 �� ���ݾ�','(', a ,')'))
}
test('�Ҹ�')

df %>%
  group_by(userType, dtYearMonth) %>% 
  summarise(�� = round(sum(useCost) / 100000000 , 0)) %>%
  ggplot(aes(x = as.character(dtYearMonth), y = ��, fill = userType)) +
  geom_bar(stat = 'identity', position = 'dodge')
    # age_date_wide <- cast(age_date, ageGroup ~ dtYearMonth, value = 'useCost_mean')
