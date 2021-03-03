covid<-read.csv("covid19.csv")
covid<- covid%>%
  filter(ISO_CODE=='KOR')
# 
# 
# covid%>%
#   ggplot()+
#   geom_bar(aes(x=DATES,y=TOTAL_CASES,fill=TOTAL_CASES),stat='identity')
# 
# covid %>%
#   group_by(DATES)%>%
#   summarise(TOTAL_CASES)%>%
#   ggplot()+
#   geom_line(aes(x=DATES,y=TOTAL_CASES,group=1))



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
  #summarise(총_확진자_수=sum(TOTAL_CASES))%>%
  mutate(총_확진자_수=sum(NEW_CASES))


write.csv(covid,"covid.csv",row.names = F)
c<-read.csv("covid.csv")
c<-c%>%
  select(기준_년_코드,기준_분기_코드,총_확진자_수)


c<-c%>%
  mutate(기준_년_코드=factor(x=기준_년_코드),
                기준_분기_코드=factor(x=기준_분기_코드))

#c의 중복값 찾고 제거
c%>%duplicated()%>%table()
unique(c)
c<-unique(c)

c<-c%>%
arrange(기준_년_코드,기준_분기_코드)


