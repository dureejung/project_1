list.files()
merge_data<-read.csv("merge_data.csv")

 
#################################
#대학상권별 특징
#################################
options(scipen = 100) #지수->정수로

# 1. 매출액
# 1-1. 매출액 추이
merge_data%>%
  group_by(상권명,분기별=paste(기준_년_코드,기준_분기_코드))%>%
  summarise(매출액=sum(분기_매출_금액))%>%
  ggplot(aes(x=분기별,y=매출액,color=상권명,group=상권명))+
  scale_y_continuous( labels = scales::comma)+
  geom_line(size=2)+
  geom_point(size=2.5)+
  ggtitle('시간에 따른 상권별 매출액 추이')+
  theme(axis.title = element_text(size=20),
        title = element_text(size=20))


# 1-2. (매출액/점포수) 추이
merge_data%>%
  group_by(상권명,분기별=paste(기준_년_코드,기준_분기_코드))%>%
  summarise(매출액=sum(분기_매출_금액)/sum(점포_수))%>%
  ggplot(aes(x=분기별,y=매출액,color=상권명,group=상권명))+
  scale_y_continuous( labels = scales::comma)+
  geom_line(size=2)+
  geom_point(size=2.5)+
  ggtitle('시간에 따른 상권별 (매출액/점포수)추이')+
  theme(axis.title = element_text(size=20),
        title = element_text(size=20))


# 2. 유동인구
# 2-1. 유동인구 추이
merge_data%>%
  group_by(기준_년_코드,기준_분기_코드,상권명,상권_코드)%>%
  summarise(유동인구=mean(총_유동인구_수))%>%
  ungroup()%>%
  group_by(분기별=paste(기준_년_코드,기준_분기_코드),상권명)%>%
  summarise(유동인구=sum(유동인구))%>%
  ggplot(aes(x=분기별,y=유동인구,color=상권명,group=상권명))+
  scale_y_continuous( labels = scales::comma)+
  geom_line()+
  geom_point(size=2)+
  ggtitle('시간에 따른 상권별 유동인구 추이')

# 2-2. 성별 비율
merge_data%>%  
  group_by(기준_년_코드,기준_분기_코드,상권명,상권_코드)%>%
  summarise(남성=mean(남성_유동인구_수),
            여성=mean(여성_유동인구_수))%>%
  gather("성별","인구수",남성,여성) %>%
  group_by(분기별=paste(기준_년_코드, 기준_분기_코드),상권명,성별) %>% 
  summarise(성별유동인구 = sum(인구수)) %>%
  mutate(전체_유동인구 = sum(성별유동인구)) %>%
  mutate(비율 = round(성별유동인구 / 전체_유동인구,2)*100) %>% 
  ggplot(aes(x=분기별, y= 성별유동인구, fill = 성별))+
  geom_bar(stat = "identity")+
  facet_grid(~상권명)+  
  geom_text(stat = "sum",aes(label = paste0(비율,"%")),
            position = position_stack(vjust = 0.5))+
  scale_fill_brewer(palette="Pastel1")+
  ggtitle("유동인구 성별비율")+
  theme(axis.title=element_text(size=15),title = element_text(size=13))

# 2-3. 연령별 비율
merge_data%>%  
  group_by(기준_년_코드,기준_분기_코드,상권명,상권_코드)%>%
  summarise(연령대_10=mean(연령대_10_유동인구_수),
            연령대_20=mean(연령대_20_유동인구_수),
            연령대_30=mean(연령대_30_유동인구_수),
            연령대_40=mean(연령대_40_유동인구_수),
            연령대_50=mean(연령대_50_유동인구_수),
            연령대_60_이상=mean(연령대_60_이상_유동인구_수))%>%
  gather("연령별","인구수",연령대_10:연령대_60_이상) %>%
  group_by(분기별=paste(기준_년_코드, 기준_분기_코드),상권명,연령별) %>% 
  summarise(연령별유동인구 = sum(인구수)) %>%
  mutate(전체_유동인구 = sum(연령별유동인구)) %>%
  mutate(비율 = round(연령별유동인구 / 전체_유동인구,2)*100) %>% 
  ggplot(aes(x=분기별, y= 연령별유동인구, fill = 연령별))+
  geom_bar(stat = "identity")+
  facet_wrap(~상권명)+  
  geom_text(stat = "sum",aes(label = paste0(비율,"%")),
            position = position_stack(vjust = 0.5))+
  scale_fill_brewer(palette="Pastel1")+
  ggtitle("유동인구 연령별비율")+
  theme(axis.title=element_text(size=15),title = element_text(size=13))

# 2-4. 시간대별 비율
merge_data %>%  
  group_by(기준_년_코드,기준_분기_코드,상권명,상권_코드)%>%
  summarise(시간대_00_06=mean(시간대_00_06_유동인구_수),
            시간대_06_11=mean(시간대_06_11_유동인구_수),
            시간대_11_14=mean(시간대_11_14_유동인구_수),
            시간대_14_17=mean(시간대_14_17_유동인구_수),
            시간대_17_21=mean(시간대_17_21_유동인구_수),
            시간대_21_24=mean(시간대_21_24_유동인구_수))%>%
  gather("시간별","인구수",시간대_00_06:시간대_21_24) %>%
  group_by(분기별=paste(기준_년_코드, 기준_분기_코드),상권명,시간별) %>% 
  summarise(시간별유동인구 = sum(인구수)) %>%
  mutate(전체_유동인구 = sum(시간별유동인구)) %>%
  mutate(비율 = round(시간별유동인구 / 전체_유동인구,2)*100) %>% 
  ggplot(aes(x=분기별, y= 시간별유동인구, fill = 시간별))+
  geom_bar(stat = "identity")+
  facet_wrap(~상권명)+  
  geom_text(stat = "sum",aes(label = paste0(비율,"%")),
            position = position_stack(vjust = 0.5))+
  scale_fill_brewer(palette="Pastel1")+
  ggtitle("유동인구 시간별비율")+
  theme(axis.title=element_text(size=15),title = element_text(size=13))

# 2-5. 요일별 비율
merge_data%>%  
  group_by(기준_년_코드,기준_분기_코드,상권명,상권_코드)%>%
  summarise(월요일=mean(월요일_유동인구_수),
            화요일=mean(화요일_유동인구_수),
            수요일=mean(수요일_유동인구_수),
            목요일=mean(목요일_유동인구_수),
            금요일=mean(금요일_유동인구_수),
            토요일=mean(토요일_유동인구_수),
            일요일=mean(일요일_유동인구_수))%>%
  gather("요일별","인구수",월요일:일요일) %>%
  group_by(분기별=paste(기준_년_코드, 기준_분기_코드),상권명,요일별) %>% 
  summarise(요일별유동인구 = sum(인구수)) %>%
  mutate(전체_유동인구 = sum(요일별유동인구)) %>%
  mutate(비율 = round(요일별유동인구 / 전체_유동인구,2)*100) %>% 
  transform(요일별 = factor(요일별, levels = c("월요일","화요일","수요일","목요일","금요일","토요일","일요일")))%>%
  ggplot(aes(x=분기별, y= 요일별유동인구, fill = 요일별))+
  geom_bar(stat = "identity")+
  facet_wrap(~상권명)+  
  geom_text(stat = "sum",aes(label = paste0(비율,"%")),
            position = position_stack(vjust = 0.5))+
  scale_fill_brewer(palette="Pastel1")+
  ggtitle("유동인구 요일별비율")+
  theme(axis.title=element_text(size=15),title = element_text(size=13))



# 3. 상주인구
# 3-1. 상주인구 추이
merge_data%>%
  group_by(기준_년_코드,기준_분기_코드,상권명,상권_코드)%>%
  summarise(상주인구=mean(총.상주인구.수))%>%
  ungroup()%>%
  group_by(분기별=paste(기준_년_코드,기준_분기_코드),상권명)%>%
  summarise(상주인구=sum(상주인구))%>%
  ggplot(aes(x=분기별,y=상주인구,color=상권명,group=상권명))+
  scale_y_continuous( labels = scales::comma)+
  geom_line()+
  geom_point(size=2)+
  ggtitle('시간에 따른 상권별 상주인구수 추이')

# 3-2. 성별 비율
merge_data%>%  
  group_by(기준_년_코드,기준_분기_코드,상권명,상권_코드)%>%
  summarise(남성=mean(남성.상주인구.수),
            여성=mean(여성.상주인구.수))%>%
  gather("성별","인구수",남성,여성) %>%
  group_by(분기별=paste(기준_년_코드, 기준_분기_코드),상권명,성별) %>% 
  summarise(성별상주인구 = sum(인구수)) %>%
  mutate(전체_상주인구 = sum(성별상주인구)) %>%
  mutate(비율 = round(성별상주인구 / 전체_상주인구,2)*100) %>% 
  ggplot(aes(x=분기별, y= 성별상주인구, fill = 성별))+
  geom_bar(stat = "identity")+
  facet_wrap(~상권명)+  
  geom_text(stat = "sum",aes(label = paste0(비율,"%")),
            position = position_stack(vjust = 0.5))+
  scale_fill_brewer(palette="Pastel1")+
  ggtitle("상주인구 성별비율")+
  theme(axis.title=element_text(size=15),title = element_text(size=13))

# 3-3. 연령별 비율
merge_data%>%  
  group_by(기준_년_코드,기준_분기_코드,상권명,상권_코드)%>%
  summarise(연령대_10=mean(연령대.10.상주인구수),
                연령대_20=mean(연령대.20.상주인구수),
                연령대_30=mean(연령대.30.상주인구수),
                연령대_40=mean(연령대.40.상주인구수),
                연령대_50=mean(연령대.50.상주인구수),
                연령대_60_이상=mean(연령대.60.이상.상주인구수))%>%
  gather("연령별","인구수",연령대_10:연령대_60_이상) %>%
  group_by(분기별=paste(기준_년_코드, 기준_분기_코드),상권명,연령별) %>% 
  summarise(연령별상주인구 = sum(인구수)) %>%
  mutate(전체_상주인구 = sum(연령별상주인구)) %>%
  mutate(비율 = round(연령별상주인구 / 전체_상주인구,2)*100) %>% 
  ggplot(aes(x=분기별, y= 연령별상주인구, fill =연령별 ))+
  geom_bar(stat = "identity")+
  facet_grid(~상권명)+  
  geom_text(stat = "sum",aes(label = paste0(비율,"%")),
            position = position_stack(vjust = 0.5))+
  scale_fill_brewer(palette="Pastel1")+
  ggtitle("상주인구 연령별비율")+
  theme(axis.title=element_text(size=15),title = element_text(size=13))


# 4. 직장인구
# 4-1. 직장인구 추이
merge_data%>%
  group_by(기준_년_코드,기준_분기_코드,상권명,상권_코드)%>%
  summarise(직장인구=mean(총_직장_인구_수))%>%
  ungroup()%>%
  group_by(분기별=paste(기준_년_코드,기준_분기_코드),상권명)%>%
  summarise(직장인구=sum(직장인구))%>%
  ggplot(aes(x=분기별,y=직장인구,color=상권명,group=상권명))+
  scale_y_continuous( labels = scales::comma)+
  geom_line()+
  geom_point(size=2)+
  ggtitle('시간에 따른 상권별 직장인구수 추이')

# 4-2. 성별 비율
merge_data%>%  
  group_by(기준_년_코드,기준_분기_코드,상권명,상권_코드)%>%
  summarise(남성=mean(남성_직장_인구_수),
              여성=mean(여성_직장_인구_수))%>%
  gather("성별","인구수",남성,여성) %>%
  group_by(분기별=paste(기준_년_코드, 기준_분기_코드),상권명,성별) %>% 
  summarise(성별직장인구 = sum(인구수)) %>%
  mutate(전체_직장인구 = sum(성별직장인구)) %>%
  mutate(비율 = round(성별직장인구 / 전체_직장인구,2)*100) %>% 
  ggplot(aes(x=분기별, y= 성별직장인구, fill = 성별))+
  geom_bar(stat = "identity")+
  facet_wrap(~상권명)+  
  geom_text(stat = "sum",aes(label = paste0(비율,"%")),
            position = position_stack(vjust = 0.5))+
  scale_fill_brewer(palette="Pastel1")+
  ggtitle("직장인구 성별비율")+
  theme(axis.title=element_text(size=15),title = element_text(size=13))

# 4-3. 연령별 비율
merge_data%>%  
  group_by(기준_년_코드,기준_분기_코드,상권명,상권_코드)%>%
  summarise(연령대_10=mean(연령대_10_직장인구_수),
                연령대_20=mean(연령대_20_직장인구_수),
                연령대_30=mean(연령대_30_직장인구_수),
                연령대_40=mean(연령대_40_직장인구_수),
                연령대_50=mean(연령대_50_직장인구_수),
                연령대_60_이상=mean(연령대_60_이상_직장인구_수))%>%
  gather("연령별","인구수",연령대_10:연령대_60_이상) %>%
  group_by(분기별=paste(기준_년_코드, 기준_분기_코드),상권명,연령별) %>% 
  summarise(연령별직장인구 = sum(인구수)) %>%
  mutate(전체_직장인구 = sum(연령별직장인구)) %>%
  mutate(비율 = round(연령별직장인구 / 전체_직장인구,2)*100) %>% 
  ggplot(aes(x=분기별, y= 연령별직장인구, fill =연령별 ))+
  geom_bar(stat = "identity")+
  facet_grid(~상권명)+  
  geom_text(stat = "sum",aes(label = paste0(비율,"%")),
            position = position_stack(vjust = 0.5))+
  scale_fill_brewer(palette="Pastel1")+
  ggtitle("직장인구 연령별비율")+
  theme(axis.title=element_text(size=15),title = element_text(size=13))


# 5. 점포
# 5-1. 점포수 추이
merge_data%>%
  group_by(기준_년_코드,기준_분기_코드,상권명,상권_코드)%>%
  summarise(점포수=sum(점포_수))%>%
  ungroup()%>%
  group_by(분기별=paste(기준_년_코드,기준_분기_코드),상권명)%>%
  summarise(점포수=sum(점포수))%>%
  ggplot(aes(x=분기별,y=점포수,color=상권명,group=상권명))+
  scale_y_continuous( labels = scales::comma)+
  geom_line()+
  geom_point(size=2)+
  ggtitle('시간에 따른 상권별 점포수 추이')

# 5-2. 업종구성비율
merge_data%>%  
  group_by(기준_년_코드,기준_분기_코드,상권명,상권_코드,서비스_업종_코드)%>%
  summarise(한식음식점=sum(서비스_업종_코드=='CS100001'),
            중식음식점=sum(서비스_업종_코드=='CS100002'),
            일식음식점=sum(서비스_업종_코드=='CS100003'),
            양식음식점=sum(서비스_업종_코드=='CS100004'),
            제과점=sum(서비스_업종_코드=='CS100005'),
            패스트푸드점=sum(서비스_업종_코드=='CS100006'),
            치킨전문점=sum(서비스_업종_코드=='CS100007'),
            분식전문점=sum(서비스_업종_코드=='CS100008'),
            호프_간이주점=sum(서비스_업종_코드=='CS100009'),
            커피_음료=sum(서비스_업종_코드=='CS100010'))%>%
  gather("서비스업종","점포수",한식음식점:커피_음료) %>%
  group_by(분기별=paste(기준_년_코드, 기준_분기_코드),상권명,서비스업종) %>% 
  summarise(업종별점포수 = sum(점포수)) %>%
  mutate(총점포수 = sum(업종별점포수)) %>%
  mutate(비율 = round(업종별점포수 / 총점포수,2)*100) %>% 
  ggplot(aes(x=분기별, y= 업종별점포수, fill = 서비스업종))+
  geom_bar(stat = "identity")+
  facet_wrap(~상권명)+  
  geom_text(stat = "sum",aes(label = paste0(비율,"%")),
            position = position_stack(vjust = 0.5))+
  scale_fill_brewer(palette="Pastel1")+
  ggtitle("상권별 업종구성비율")+
  theme(axis.title=element_text(size=15),title = element_text(size=13))

# 5-3. 프랜차이즈와 일반업소 비율
merge_data%>%  
  group_by(기준_년_코드,기준_분기_코드,상권명,상권_코드,서비스_업종_코드)%>%
  summarise(프랜차이즈=sum(프랜차이즈_점포_수),
            일반업소=(sum(점포_수)-sum(프랜차이즈_점포_수)))%>%
  gather("업소종류","점포수",프랜차이즈,일반업소) %>%
  group_by(분기별=paste(기준_년_코드, 기준_분기_코드),상권명,업소종류) %>% 
  summarise(업소종류별점포수 = sum(점포수)) %>%
  mutate(총점포수 = sum(업소종류별점포수)) %>%
  mutate(비율 = round(업소종류별점포수 / 총점포수,2)*100) %>% 
  ggplot(aes(x=분기별, y= 업소종류별점포수, fill = 업소종류))+
  geom_bar(stat = "identity")+
  facet_grid(~상권명)+  
  geom_text(stat = "sum",aes(label = paste0(비율,"%")),
            position = position_stack(vjust = 0.5))+
  scale_fill_brewer(palette="Pastel1")+
  ggtitle("상권별 업소종류 비율")+
  theme(axis.title=element_text(size=15),title = element_text(size=13))


# 6. 집객시설 수
merge_data%>%
  group_by(기준_년_코드,기준_분기_코드,상권명,상권_코드)%>%
  summarise(집객시설수=mean(총_집객시설_수))%>%
  ungroup()%>%
  group_by(상권명)%>%
  summarise(집객시설수=sum(집객시설수)/6)%>%
  ggplot(aes(x=상권명,y=집객시설수,fill=집객시설수))+
  geom_bar(stat = 'identity')+
  ggtitle('상권별 집객시설 수')


# 1. 매출액 연도별(2015~)
# 1-1. 매출액 추이
merge_train%>%
  group_by(상권명,분기별=paste(기준_년_코드,기준_분기_코드))%>%
  summarise(매출액=sum(분기_매출_금액))%>%
  ggplot(aes(x=분기별,y=매출액,color=상권명,group=상권명))+
  scale_y_continuous( labels = scales::comma)+
  geom_line(size=2)+
  geom_point(size=2.5)+
  ggtitle('시간에 따른 상권별 매출액 추이')+
  theme(axis.title = element_text(size=20),
        title = element_text(size=20))


# 1-2. (매출액/점포수) 추이
merge_data%>%
  group_by(상권명,기준_년_코드)%>%
  summarise(매출액=sum(분기_매출_금액)/sum(점포_수))%>%
  ggplot(aes(x=기준_년_코드,y=매출액,color=상권명,group=상권명))+
  scale_y_continuous( labels = scales::comma)+
  geom_line(size=2)+
  geom_point(size=2.5)+
  ggtitle('시간에 따른 상권별 (매출액/점포수)추이')+
  theme(axis.title = element_text(size=20),
        title = element_text(size=20))


list.files()
merge_data<-read.csv("train2015.csv")
merge_data%>%  
  group_by(기준_년_코드,기준_분기_코드,상권명,상권_코드)%>%
  summarise(연령대_10=mean(연령대_10_유동인구_수),
                연령대_20=mean(연령대_20_유동인구_수),
                연령대_30=mean(연령대_30_유동인구_수),
                연령대_40=mean(연령대_40_유동인구_수),
                연령대_50=mean(연령대_50_유동인구_수),
                연령대_60_이상=mean(연령대_60_이상_유동인구_수))%>%
  gather("연령별","인구수",연령대_10:연령대_60_이상) %>%
  group_by(분기별=paste(기준_년_코드, 기준_분기_코드),상권명,연령별) %>% 
  summarise(연령별유동인구 = sum(인구수)) %>%
  mutate(전체_유동인구 = sum(연령별유동인구)) %>%
  mutate(비율 = round(연령별유동인구 / 전체_유동인구,2)*100) %>% 
  ggplot(aes(x=분기별, y= 연령별유동인구, fill = 연령별))+
  geom_bar(stat = "identity")+
  facet_wrap(~상권명)+  
  geom_text(stat = "sum",aes(label = paste0(비율,"%")),
            position = position_stack(vjust = 0.5))+
  scale_fill_brewer(palette="Pastel1")+
  ggtitle("유동인구 연령별비율")+
  theme(axis.title=element_text(size=15),title = element_text(size=13))










sales<-sales%>%
  filter(상권_코드 %in% c(1000779,1000778,1000765,1000764,1000763,1001448,
                      1000760,1000761,1000759,1000758,1001444,
                      1000754,1000756,1000747,1001450,1000753),
              substr(sales$서비스_업종_코드,1,3)=='CS1')%>%
  arrange(상권_코드_명,서비스_업종_코드,기준_년_코드,기준_분기_코드)

data <- merge(merge_data, sales,
              by.all = c("기준_년_코드","기준_분기_코드","상권_코드","상권_코드_명","서비스_업종_코드","서비스_업종_코드_명"),
)
sales <- sales[,c(1:8,51:56)]
data <- merge(merge_data, sales,
              by.all = c("기준_년_코드","기준_분기_코드","상권_코드","상권_코드_명","서비스_업종_코드","서비스_업종_코드_명"),
)

data%>%
  group_by(기준_년_코드,기준_분기_코드,상권명,상권_코드_명,서비스_업종_코드_명)%>%
  summarise(연령대_10=sum(연령대_10_매출_금액),
                연령대_20=sum(연령대_20_매출_금액),
                연령대_30=sum(연령대_30_매출_금액),
                연령대_40=sum(연령대_40_매출_금액),
                연령대_50=sum(연령대_50_매출_금액),
                연령대_60_이상=sum(연령대_60_이상_매출_금액))%>%
  gather("연령별","매출액",연령대_10:연령대_60_이상) %>%
  group_by(분기별=paste(기준_년_코드, 기준_분기_코드),상권명,연령별) %>% 
  summarise(연령별매출 = sum(매출액)) %>%
  mutate(전체_매출액 = sum(연령별매출)) %>%
  mutate(비율 = round(연령별매출 / 전체_매출액,2)*100)



merge_data%>%
  group_by(상권명,기준_년_코드,기준_분기_코드)%>%
  summarise(매출액=sum(분기_매출_금액))

