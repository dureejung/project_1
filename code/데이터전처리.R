library(ggplot2)
library(dplyr)
library(lubridate)
library(zoo)

area<-read.csv("서울시 우리마을가게 상권분석서비스(상권영역).csv")
area<-area%>% filter(상권_코드 %in% c(1000779,1000778,1000765,1000762,1000764,1000763,1001448,
                              1000760,1000761,1000759,1000758,1001444,1001443,
                              1000754,1000756,1000747,1001450,1000750,1000751,1000753))
write.csv(area,file="상권영역.csv",row.names=F)



store3<-read.csv("서울시 우리마을가게 상권분석서비스(상권-점포).csv")
store2<-read.csv("서울시_우리마을가게_상권분석서비스(상권-점포)_2019년.csv")
store<-rbind(store,store2)

store<-store%>%
  filter(상권_코드 %in% c(1000779,1000778,1000765,1000762,1000764,1000763,1001448,
                      1000760,1000761,1000759,1000758,1001444,1001443,
                      1000754,1000756,1000747,1001450,1000750,1000751,1000753), 
         substr(store$서비스_업종_코드,1,3)=='CS1')%>%
  arrange(상권_코드_명,서비스_업종_코드,기준_년_코드,기준_분기_코드)
write.csv(store,file="점포.csv",row.names=F)



facility<-read.csv("서울시 우리마을가게 상권분석서비스(상권-집객시설).csv")
facility<-facility%>%
  filter(상권_코드 %in% c(1000779,1000778,1000765,1000762,1000764,1000763,1001448,
                      1000760,1000761,1000759,1000758,1001444,1001443,
                      1000754,1000756,1000747,1001450,1000750,1000751,1000753),
         기준_년_코드%in% c(2019,2020))%>%
  arrange(상권_코드_명,기준_년_코드,기준_분기_코드)
write.csv(facility,file="집객시설.csv",row.names=F)



sales3<-read.csv("서울시 우리마을가게 상권분석서비스(상권-추정매출).csv")
sales2<-read.csv("서울시 우리마을가게 상권분석서비스(상권-추정매출)_2019.csv")
sales<-rbind(sales3,sales2)

sales<-sales%>%
  filter(상권_코드 %in% c(1000779,1000778,1000765,1000762,1000764,1000763,1001448,
                      1000760,1000761,1000759,1000758,1001444,1001443,
                      1000754,1000756,1000747,1001450,1000750,1000751,1000753),
          substr(서비스_업종_코드,1,3)=='CS1')%>%
  arrange(상권_코드_명,서비스_업종_코드,기준_년_코드,기준_분기_코드)
write.csv(sales,file="추정매출.csv",row.names=F)



wkpop<-read.csv("서울시 우리마을가게 상권분석서비스(상권-직장인구).csv")
wkpop<-wkpop%>%
  filter(상권_코드 %in% c(1000779,1000778,1000765,1000762,1000764,1000763,1001448,
                      1000760,1000761,1000759,1000758,1001444,1001443,
                      1000754,1000756,1000747,1001450,1000750,1000751,1000753),
              기준_년월_코드 %in% c(2019,2020))%>%
  arrange(상권_코드_명,기준_년월_코드,기준_분기_코드)
write.csv(wkpop,file="직장인구.csv",row.names=F)


change<-read.csv("서울시 우리마을가게 상권분석서비스(상권-상권변화지표).csv")
change<-change%>%
  filter(상권_코드 %in% c(1000779,1000778,1000765,1000762,1000764,1000763,1001448,
                      1000760,1000761,1000759,1000758,1001444,1001443,
                      1000754,1000756,1000747,1001450,1000750,1000751,1000753),
              기준_년_코드%in% c(2019,2020))%>%
  arrange(상권_코드_명,기준_년_코드,기준_분기_코드)
write.csv(change,file="상권변화지표.csv",row.names=F)


repop<-read.csv("서울시 우리마을가게 상권분석서비스(상권_상주인구).csv")
repop<-repop%>%
  filter(상권.코드 %in% c(1000779,1000778,1000765,1000762,1000764,1000763,1001448,
                      1000760,1000761,1000759,1000758,1001444,1001443,
                      1000754,1000756,1000747,1001450,1000750,1000751,1000753),기준_년_코드%in% c(2019,2020))%>%
  arrange(상권.코드.명,기준_년_코드,기준_분기_코드)
write.csv(repop,file="상주인구.csv",row.names=F)


pop<-read.csv("서울시 우리마을가게 상권분석서비스(상권-추정유동인구).csv")
pop<-pop%>%
  filter(상권_코드 %in% c(1000779,1000778,1000765,1000762,1000764,1000763,1001448,
                      1000760,1000761,1000759,1000758,1001444,1001443,
                      1000754,1000756,1000747,1001450,1000750,1000751,1000753),기준.년코드%in% c(2019,2020))%>%
  arrange(상권_코드_명,기준.년코드,기준_분기_코드)
write.csv(pop,file="추정유동인구.csv",row.names=F)







###train/test 나누기
change<- read.csv("상권변화지표.csv")
area<- read.csv("상권영역.csv")
repop<- read.csv("상주인구.csv")
wkpop<- read.csv("직장인구.csv")
store<- read.csv("점포.csv")
facility<- read.csv("집객시설.csv")
sales<- read.csv("추정매출.csv")
pop<- read.csv("추정유동인구.csv") 

write.csv(area,file="상권영역.csv",row.names=F)

sales_data <- sales %>%
  select(-주중_매출_비율,-주말_매출_비율,-월요일_매출_비율,-화요일_매출_비율,-수요일_매출_비율,-목요일_매출_비율,
         -금요일_매출_비율,-토요일_매출_비율,-일요일_매출_비율,
         -시간대_00.06_매출_비율,-시간대_06.11_매출_비율,-시간대_11.14_매출_비율,-시간대_14.17_매출_비율,
         -시간대_17.21_매출_비율,-시간대_21.24_매출_비율,-남성_매출_비율,-여성_매출_비율,
         -연령대_10_매출_비율,-연령대_20_매출_비율,-연령대_30_매출_비율,-연령대_40_매출_비율,-연령대_50_매출_비율,-연령대_60_이상_매출_비율,
         -주중_매출_건수,-주말_매출_건수,-월요일_매출_건수,-화요일_매출_건수,-수요일_매출_건수,-목요일_매출_건수,-금요일_매출_건수,
         -토요일_매출_건수,-일요일_매출_건수,-시간대_건수.06_매출_건수,-시간대_건수.11_매출_건수,-시간대_건수.14_매출_건수,-시간대_건수.17_매출_건수,
         -시간대_건수.21_매출_건수,-시간대_건수.24_매출_건수,-남성_매출_건수,-여성_매출_건수,-연령대_10_매출_건수,-연령대_20_매출_건수,-연령대_30_매출_건수,
         -연령대_40_매출_건수,-연령대_50_매출_건수,-연령대_60_이상_매출_건수,-점포수)

test_sales_data <- sales_data %>%
  filter(기준_년_코드 == 2020, 기준_분기_코드 ==3)

data1<- sales_data %>%
  filter(기준_년_코드 == 2019)

data2<-sales_data %>%
  filter(기준_년_코드 == 2020, 기준_분기_코드 <= 2)

train_sales_data <- rbind(data1, data2)

write.csv(train_sales_data, "추정매출_train.csv",row.names=F)
write.csv(test_sales_data, "추정매출_test.csv",row.names=F)



pop_data <- pop %>%
  select(기준.년코드,기준_분기_코드,	X.상권_구분_코드,	X.상권_구분_코드_명,	상권_코드,	상권_코드_명,	총_유동인구_수,	남성_유동인구_수,	여성_유동인구_수,	
           연령대_10_유동인구_수,	연령대_20_유동인구_수,	연령대_30_유동인구_수,	연령대_40_유동인구_수,	연령대_50_유동인구_수,	연령대_60_이상_유동인구_수,
           시간대_1_유동인구_수,	시간대_2_유동인구_수,	시간대_3_유동인구_수,	시간대_4_유동인구_수,	시간대_5_유동인구_수,	시간대_6_유동인구_수,
           월요일_유동인구_수,	화요일_유동인구_수,	수요일_유동인구_수,	목요일_유동인구_수,	금요일_유동인구_수,	토요일_유동인구_수,	일요일_유동인구_수)

test_pop_data <- pop_data %>%
  filter(기준.년코드 == 2020, 기준_분기_코드 ==3)
data1<- pop_data %>%
  filter(기준.년코드 == 2019)
data2<-pop_data %>%
  filter(기준.년코드 == 2020, 기준_분기_코드 <= 2)
train_pop_data <- rbind(data1, data2)

write.csv(train_pop_data, "추정유동인구_train.csv",row.names=F)
write.csv(test_pop_data, "추정유동인구_test.csv",row.names=F)


change_data <- change %>%
  select(기준_년_코드,	기준_분기_코드,	상권_구분_코드,	상권_구분_코드_명,	상권_코드,	상권_코드_명,
                상권_변화_지표,	상권_변화_지표_명,	운영_영업_개월_평균,	폐업_영업_개월_평균,
                서울_운영_영업_개월_평균,	서울_폐업_영업_개월_평균)

test_pop_data <- change_data %>%
  filter(기준_년_코드 == 2020, 기준_분기_코드 ==3)
data1<- change_data %>%
  filter(기준_년_코드 == 2019)
data2<-change_data %>%
  filter(기준_년_코드 == 2020, 기준_분기_코드 <= 2)
train_pop_data <- rbind(data1, data2)

write.csv(train_pop_data, "상권변화지표_train.csv",row.names=F)
write.csv(test_pop_data, "상권변화지표_test.csv",row.names=F)


repop_data <- repop %>%
  select(기준_년_코드,	기준_분기_코드,	상권_구분_코드,	상권_구분_코드_명,	상권.코드,	상권.코드.명,
                총.상주인구.수,	남성.상주인구.수,	여성.상주인구.수,
                여성연령대.10.상주인구.수,	여성연령대.20.상주인구.수,	여성연령대.30.상주인구.수, 	여성연령대.40.상주인구.수,	여성연령대.50.상주인구.수,	여성연령대.60.이상.상주인구.수,
                남성연령대.10.상주인구.수, 	남성연령대.20.상주인구.수,	남성연령대.30.상주인구.수,	남성연령대.40.상주인구.수,	남성연령대.50.상주인구.수,	남성연령대.60.이상.상주인구.수)

test_pop_data <- repop_data %>%
  filter(기준_년_코드 == 2020, 기준_분기_코드 ==3)
data1<- repop_data %>%
  filter(기준_년_코드 == 2019)
data2<-repop_data %>%
  filter(기준_년_코드 == 2020, 기준_분기_코드 <= 2)
train_pop_data <- rbind(data1, data2)

write.csv(train_pop_data, "상주인구_train.csv",row.names=F)
write.csv(test_pop_data, "상주인구_test.csv",row.names=F)


store_data <- store %>%
  select(기준_년_코드,	기준_분기_코드,	상권_구분_코드,	상권_구분_코드_명,	상권_코드,	상권_코드_명,
                서비스_업종_코드,	서비스_업종_코드_명,	유사_업종_점포_수,	개업_점포_수,	폐업_점포_수,	프랜차이즈_점포_수      
                )

test_pop_data <- store_data %>%
  filter(기준_년_코드 == 2020, 기준_분기_코드 ==3)
data1<- store_data %>%
  filter(기준_년_코드 == 2019)
data2<-store_data %>%
  filter(기준_년_코드 == 2020, 기준_분기_코드 <= 2)
train_pop_data <- rbind(data1, data2)

write.csv(train_pop_data, "점포_train.csv",row.names=F)
write.csv(test_pop_data, "점포_test.csv",row.names=F)


wkpop<- rename(wkpop,상권_구분_코드=기준_분기_코드.1)
wkpop_data <- wkpop %>%
  select(기준_년월_코드,	기준_분기_코드,	상권_구분_코드,	상권_구분_코드_명,	상권_코드,	상권_코드_명,
                 총_직장_인구_수	,남성_직장_인구_수,	여성_직장_인구_수,	여성연령대_10_직장_인구_수,	여성연령대_20_직장_인구_수,	여성연령대_30_직장_인구_수,	여성연령대_40_직장_인구_수,	여성연령대_50_직장_인구_수,	 여성연령대_60_이상_직장_인구_수,
                 남성연령대_10_직장_인구_수,	남성연령대_20_직장_인구_수,	남성연령대_30_직장_인구_수,	남성연령대_40_직장_인구_수,	남성연령대_50_직장_인구_수,	남성연령대_60_이상_직장_인구_수    
                 )

test_pop_data <- wkpop_data %>%
  filter(기준_년월_코드 == 2020, 기준_분기_코드 ==3)
data1<- wkpop_data %>%
  filter(기준_년월_코드 == 2019)
data2<-wkpop_data %>%
  filter(기준_년월_코드 == 2020, 기준_분기_코드 <= 2)
train_pop_data <- rbind(data1, data2)

write.csv(train_pop_data, "직장인구_train.csv",row.names=F)
write.csv(test_pop_data, "직장인구_test.csv",row.names=F)



facility[is.na(facility)]<-0
facility<-facility%>%
  mutate(총_집객시설_수 =apply(facility[,7:26],1,sum))


facility_data <- facility %>%
  select(기준_년_코드,	기준_분기_코드,	상권_구분_코드,	상권_구분_코드_명,	상권_코드,	상권_코드_명,
                총_집객시설_수)

test_pop_data <- facility_data %>%
  filter(기준_년_코드 == 2020, 기준_분기_코드 ==3)
data1<- facility_data %>%
  filter(기준_년_코드 == 2019)
data2<-facility_data %>%
  filter(기준_년_코드 == 2020, 기준_분기_코드 <= 2)
train_pop_data <- rbind(data1, data2)

write.csv(train_pop_data, "집객시설_train.csv",row.names=F)
write.csv(test_pop_data, "집객시설_test.csv",row.names=F)


#상주인구,직장인구 성별/연령별 묶기
repop$연령대.10.상주인구수<-repop$여성연령대.10.상주인구.수+repop$남성연령대.10.상주인구.수
repop$연령대.20.상주인구수<-repop$여성연령대.20.상주인구.수+repop$남성연령대.20.상주인구.수
repop$연령대.30.상주인구수<-repop$여성연령대.30.상주인구.수+repop$남성연령대.30.상주인구.수
repop$연령대.40.상주인구수<-repop$여성연령대.40.상주인구.수+repop$남성연령대.40.상주인구.수
repop$연령대.50.상주인구수<-repop$여성연령대.50.상주인구.수+repop$남성연령대.50.상주인구.수
repop$연령대.60.이상.상주인구수<-repop$여성연령대.60.이상.상주인구.수+repop$남성연령대.60.이상.상주인구.수
repop<-repop[,-c(10:21)]

wkpop$연령대_10_직장인구_수<-wkpop$여성연령대_10_직장_인구_수+wkpop$남성연령대_10_직장_인구_수
wkpop$연령대_20_직장인구_수<-wkpop$여성연령대_20_직장_인구_수+wkpop$남성연령대_20_직장_인구_수
wkpop$연령대_30_직장인구_수<-wkpop$여성연령대_30_직장_인구_수+wkpop$남성연령대_30_직장_인구_수
wkpop$연령대_40_직장인구_수<-wkpop$여성연령대_40_직장_인구_수+wkpop$남성연령대_40_직장_인구_수
wkpop$연령대_50_직장인구_수<-wkpop$여성연령대_50_직장_인구_수+wkpop$남성연령대_50_직장_인구_수
wkpop$연령대_60_이상_직장인구_수<-wkpop$여성연령대_60_이상_직장_인구_수+wkpop$남성연령대_60_이상_직장_인구_수
wkpop<-wkpop[,-c(10:21)]

#merge
#1. sales 데이터 중분류로 묶기 
data1 <- sales %>% group_by(기준_년_코드,기준_분기_코드,상권_구분_코드,상권_구분_코드_명,상권_코드,상권_코드_명,서비스_업종_코드,서비스_업종_코드_명) %>% 
  summarise(분기_매출_금액 = sum(당월_매출_금액),
            분기_매출_건수 = sum(당월_매출_건수))
#2. sales + pop + repop + wkpop = merge_data
#2-1 pop
merge_data <- merge(data1, pop,
                    by.x=c('기준_년_코드','기준_분기_코드','상권_구분_코드','상권_구분_코드_명','상권_코드','상권_코드_명'),
                    by.y=c('기준.년코드','기준_분기_코드','X.상권_구분_코드','X.상권_구분_코드_명','상권_코드','상권_코드_명'),
                    all.x=T)

#2-2 repop
merge_data<-merge(merge_data, repop,
                  by.x=c('기준_년_코드','기준_분기_코드','상권_구분_코드','상권_구분_코드_명','상권_코드','상권_코드_명'),
                  by.y=c('기준_년_코드','기준_분기_코드','상권_구분_코드','상권_구분_코드_명','상권.코드','상권.코드.명'),
                  all.x=T)

##2_3 wkpop

merge_data<-merge(merge_data, wkpop,
                  by.x=c('기준_년_코드','기준_분기_코드','상권_구분_코드','상권_구분_코드_명','상권_코드','상권_코드_명'),
                  by.y=c('기준_년월_코드','기준_분기_코드','상권_구분_코드','상권_구분_코드_명','상권_코드','상권_코드_명'),
                  all.x = TRUE)


#3. merge_data + facilty
merge_data <- merge(merge_data, facility,
                    by.x=c('기준_년_코드','기준_분기_코드','상권_구분_코드','상권_구분_코드_명','상권_코드','상권_코드_명'),
                    by.y=c('기준_년_코드','기준_분기_코드','상권_구분_코드','상권_구분_코드_명','상권_코드','상권_코드_명'),
                    all.x = TRUE)


#4.merge_data + store, 유사업종_점포수 merge 
data1<-store %>% 
  group_by(기준_년_코드,기준_분기_코드,상권_구분_코드,상권_구분_코드_명,상권_코드,상권_코드_명,서비스_업종_코드,서비스_업종_코드_명) %>% 
  summarise(점포_수 = sum(유사_업종_점포_수),
                개업_점포_수 = sum(개업_점포_수),
                폐업_점포_수 = sum(폐업_점포_수),
                프랜차이즈_점포_수 = sum(프랜차이즈_점포_수))

merge_data <- merge(merge_data, data1,                
                    by.x=c('기준_년_코드','기준_분기_코드','상권_구분_코드','상권_구분_코드_명','상권_코드','상권_코드_명','서비스_업종_코드',"서비스_업종_코드_명"),                 
                    by.y=c('기준_년_코드','기준_분기_코드','상권_구분_코드','상권_구분_코드_명','상권_코드','상권_코드_명','서비스_업종_코드',"서비스_업종_코드_명"),
                    all.x=T)
rm(data1)

###merge한 데이터 정리
#필요한 상권만 남기기
merge_data<-merge_data%>%
  filter(상권_코드 %in% c(1000779,1000778,1000765,1000764,1000763,1001448,
                      1000760,1000761,1000759,1000758,1001444,
                      1000754,1000756,1000747,1001450,1000753))

#필요없는 컬럼삭제
merge_data<-merge_data[,-c(3,4)]


#중복값확인
merge_data%>%duplicated()%>%table()



#컬럼추가_코로나_total
library(zoo)
covid<-read.csv("covid19.csv")
covid<- covid%>%
  filter(ISO_CODE=='KOR')
covid$quarterYear <-as.yearqtr(as.Date(covid$DATES))   
covid<-covid%>% 
  mutate(
    기준_분기_코드 = case_when(
      quarterYear =='2020 Q1' ~ "1",
      quarterYear =='2020 Q2' ~ "2",
      quarterYear =='2020 Q3' ~ "3",
      quarterYear =='2020 Q4' ~ "4",
      quarterYear =='2019 Q4' ~ "4"
    )
  )
covid<-covid%>% 
  mutate(
    기준_년_코드 = case_when(
      quarterYear =='2020 Q1' ~ "2020",
      quarterYear =='2020 Q2' ~ "2020",
      quarterYear =='2020 Q3' ~ "2020",
      quarterYear =='2020 Q4' ~ "2020",
      TRUE ~ "2019"
    )
  )
covid[is.na(covid)]<-0
covid<-covid%>%
  group_by(기준_년_코드,기준_분기_코드)%>%
  mutate(총_확진자_수=sum(NEW_CASES))
covid<-covid%>%
  select(기준_년_코드,기준_분기_코드,총_확진자_수)
covid<-covid%>%
  mutate(기준_년_코드=factor(x=기준_년_코드),
                기준_분기_코드=factor(x=기준_분기_코드))
#covid의 중복값 찾고 제거
covid%>%duplicated()%>%table()
unique(covid)
covid<-unique(covid)

merge_data<-merge(merge_data, covid,                
                  by.x=c('기준_년_코드','기준_분기_코드'),
                  by.y=c('기준_년_코드','기준_분기_코드'),all.x=T)  


#컬럼추가_코로나_서울
library(lubridate)
list.files()
covid_seoul<-read.csv("서울시 코로나19 확진자 현황.csv")
covid_seoul$quarterYear <-as.yearqtr(ymd(covid_seoul$확진일)) 
covid_seoul$quarterYear<- as.factor(covid_seoul$quarterYear)
covid_seoul<-covid_seoul%>% filter(quarterYear %in% c('2020 Q1','2020 Q2'))
covid_seoul<-covid_seoul%>% 
  mutate(
    기준_분기_코드 = case_when(
      quarterYear =='2020 Q1' ~ "1",
      quarterYear =='2020 Q2' ~ "2"))
covid_seoul<-covid_seoul%>% 
  mutate(
    기준_년_코드 = case_when(
      quarterYear =='2020 Q1' ~ "2020",
      quarterYear =='2020 Q2' ~ "2020"))
count(covid_seoul,기준_분기_코드)
covid_seoul<-covid_seoul%>% 
  mutate(
    서울_확진자_수 = case_when(
      quarterYear =='2020 Q1' ~ 478,
      quarterYear =='2020 Q2' ~ 844))
covid_seoul<-unique(covid_seoul[,16:18])
merge_data<-merge(merge_data, covid_seoul,                
                  by.x=c('기준_년_코드','기준_분기_코드'),
                  by.y=c('기준_년_코드','기준_분기_코드'),all.x=T) 

merge_data[, c("총_확진자_수", "서울_확진자_수")][is.na(merge_data[, c("총_확진자_수", "서울_확진자_수")])]<-0;


#컬럼추가_방학개월수
merge_data<-merge_data%>% 
  mutate(
    방학_개월수 = case_when(
      기준_분기_코드==1 ~2,
      기준_분기_코드==2 ~0,
      기준_분기_코드==3 ~2,
      기준_분기_코드==4 ~0))

#컬럼추가_상권명
merge_data<-merge_data%>% 
  mutate(
    상권명 = case_when(
      상권_코드_명 %in% c('흑석로9길', '흑석로13길',
                     '서달로8가길', '서달로15길',
                     '서달로14길','흑석시장') ~ "중앙대",
      상권_코드_명 %in% c('상도로61길','상도로62길',
                     '상도로47길','상도로37길','상도전통시장') ~ "숭실대", 
      TRUE ~ "총신대"
    )
  )


#범주형으로 변환
merge_data<- merge_data%>%
  mutate(서비스_업종_코드=factor(x=서비스_업종_코드),
         상권명=factor(x=상권명))


#컬럼명 변경
merge_data <- rename(merge_data, 
                     "시간대_00_06_유동인구_수" = "시간대_1_유동인구_수",
                     "시간대_06_11_유동인구_수" = "시간대_2_유동인구_수",
                     "시간대_11_14_유동인구_수" = "시간대_3_유동인구_수",
                     "시간대_14_17_유동인구_수" = "시간대_4_유동인구_수",
                     "시간대_17_21_유동인구_수" = "시간대_5_유동인구_수",
                     "시간대_21_24_유동인구_수" = "시간대_6_유동인구_수")


#점포수 0->1로 바꿔주기
summary(merge_data)
merge_data[is.na(merge_data)]<-0

merge_data$점포_수<-ifelse( merge_data$점포_수==0,1,merge_data$점포_수)




# ##저장
# write.csv(merge_data,"merge_data.csv",row.names = F)




train<- train%>%
  mutate(서비스_업종_코드=factor(x=서비스_업종_코드),
                  상권명=factor(x=상권명))
train_mod<- train_mod%>%
  mutate(서비스_업종_코드=factor(x=서비스_업종_코드),
                  상권명=factor(x=상권명))
test<- test%>%
  mutate(서비스_업종_코드=factor(x=서비스_업종_코드),
                  상권명=factor(x=상권명))
test_mod<- test_mod%>%
  mutate(서비스_업종_코드=factor(x=서비스_업종_코드),
                  상권명=factor(x=상권명))


# 
# write.csv(train,"train2015.csv",row.names = F)
# write.csv(test,"test2015.csv",row.names = F)
# write.csv(train_mod,"train_mod.csv",row.names = F)
# write.csv(test_mod,"test_mod.csv",row.names = F)
