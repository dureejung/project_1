# #최종정리데이터
# #2015~2020
# train<-read.csv("train2015.csv")
# test<-read.csv("test2015.csv")
# #상관분석후
# train_mod<-read.csv("train_mod.csv")
# test_mod<-read.csv("test_mod.csv")
library(rpart)
library(rpart.plot)


##################################################
#2015~데이터
##################################################

ctrl<- rpart.control(minsplit = 10,
                     cp = 0.001,
                     maxdepth = 30)

#모형적합
set.seed(1234)
fit1<-rpart(formula = (분기_매출_금액/점포_수)~.,
            data = train_mod,
            control = ctrl)
printcp(fit1)
#xerror가 최소값인지 확인(최소면 가지치기 필요없음)
plotcp(fit1) #역시 현재가 가장 작음->가지치기 필요없음


#xerror최소값 찾는법
fit1$cptable[,4] #xerror만 벡터로 가져오기

which.min(fit1$cptable[,4]) #최소값 위치 찾기
cp<- fit1$cptable[62,1] 
print(cp)

#가지치기
fit2<- prune.rpart(tree=fit1,cp=cp)
summary(fit2)

#결과 길어서 console에 다 안담길 때
sink("회귀나무.txt")
summary(fit2)
sink()

# Variable importance

# 연령대.30.상주인구수       연령대.10.상주인구수       연령대.20.상주인구수       서비스_업종_코드 
# 12                         12                         10                         9 
# 상권_코드                  총_집객시설_수             총_유동인구_수             연령대_30_유동인구_수 
# 7                          6                          6                          5 
# 연령대_60_이상_직장인구_수 시간대_17_21_유동인구_수   여성_유동인구_수           남성_유동인구_수 
# 4                          3                          3                          3 
# 시간대_11_14_유동인구_수   시간대_14_17_유동인구_수   연령대_40_유동인구_수      연령대_20_유동인구_수 
# 3                          3                          3                          2 
# 기준_년_코드               연령대_50_유동인구_수      화요일_유동인구_수         월요일_유동인구_수 
# 2                          2                          2                          1 
# 수요일_유동인구_수         상권명                     금요일_유동인구_수 
# 1                          1                          1 

#실제값 할당
real<- (test_mod$분기_매출_금액/test_mod$점포_수)
pred2<- predict(fit2, newdata = test_mod, type = 'vector')

#사용자정의함수 불러오기
source("Myfunction.R")
regMeasure(real = real, pred = pred2)

# MSE     RMSE      MAE     MAPE
# 1 9.265346e+14 30439031 18553127 2.181767



#################################################
# 연령대_20_유동인구_수 빼보기
#################################################
trainA<-train_mod[,-9]
testA<-test_mod[,-9]

ctrl<- rpart.control(minsplit = 10,
                     cp = 0.001,
                     maxdepth = 30)

#모형적합
set.seed(1234)
fit1<-rpart(formula = (분기_매출_금액/점포_수)~.,
            data = trainA,
            control = ctrl)
printcp(fit1)
#xerror가 최소값인지 확인(최소면 가지치기 필요없음)
which.min(fit1$cptable[,4]) #최소값 위치 찾기

#실제값할당
real<- (testA$분기_매출_금액/testA$점포_수)
pred1<- predict(fit1, newdata = testA, type = 'vector')

#사용자정의함수 불러오기
source("Myfunction.R")
regMeasure(real = real, pred = pred1)

# MSE     RMSE      MAE     MAPE
# 1 5.848701e+14 24184088 16201713 2.128253


#################################################
# 연령대20유동인구수,연령대20상주인구수 빼보기
#################################################
trainB<-train_mod[,-c(9,22)]
testB<-test_mod[,-c(9,22)]

ctrl<- rpart.control(minsplit = 10,
                     cp = 0.001,
                     maxdepth = 30)

#모형적합
set.seed(1234)
fit1<-rpart(formula = (분기_매출_금액/점포_수)~.,
            data = trainB,
            control = ctrl)
printcp(fit1)
#xerror가 최소값인지 확인(최소면 가지치기 필요없음)
which.min(fit1$cptable[,4]) #최소값 위치 찾기

#실제값할당
real<- (testB$분기_매출_금액/testB$점포_수)
pred1<- predict(fit1, newdata = testB, type = 'vector')

#사용자정의함수 불러오기
source("Myfunction.R")
regMeasure(real = real, pred = pred1)

# MSE     RMSE      MAE     MAPE
# 1 5.905601e+14 24301442 16177148 2.118678


#################################################
# 연령대 20/나머지연령대 묶어보기(유동인구)
#################################################
trainC<- train_mod%>%
  mutate('연령대_30~50_유동인구_수'=연령대_30_유동인구_수+연령대_40_유동인구_수+연령대_50_유동인구_수)%>%
  select(-c(연령대_30_유동인구_수,연령대_40_유동인구_수,연령대_50_유동인구_수))
testC<- test_mod%>%
  mutate('연령대_30~50_유동인구_수'=연령대_30_유동인구_수+연령대_40_유동인구_수+연령대_50_유동인구_수)%>%
  select(-c(연령대_30_유동인구_수,연령대_40_유동인구_수,연령대_50_유동인구_수))

ctrl<- rpart.control(minsplit = 10,
                     cp = 0.001,
                     maxdepth = 30)

#모형적합
set.seed(1234)
fit1<-rpart(formula = (분기_매출_금액/점포_수)~.,
            data = trainC,
            control = ctrl)
printcp(fit1)
#xerror가 최소값인지 확인(최소면 가지치기 필요없음)
which.min(fit1$cptable[,4]) #최소값 위치 찾기


#실제값할당
real<- (testC$분기_매출_금액/testC$점포_수)
pred1<- predict(fit1, newdata = testC, type = 'vector')

#사용자정의함수 불러오기
source("Myfunction.R")
regMeasure(real = real, pred = pred1)

# MSE     RMSE      MAE     MAPE
# 1 8.404296e+14 28990164 17830356 2.153555

#################################################
# 연령대 20/나머지연령대 묶어보기(유동인구+상주인구)
#################################################
trainD<- train_mod%>%
  mutate('연령대_30~50_유동인구_수'=연령대_30_유동인구_수+연령대_40_유동인구_수+연령대_50_유동인구_수)%>%
  mutate('연령대_10,30_상주인구_수'=연령대.10.상주인구수+연령대.30.상주인구수)%>%
  select(-c(연령대_30_유동인구_수,연령대_40_유동인구_수,연령대_50_유동인구_수,연령대.10.상주인구수,연령대.30.상주인구수))
testD<- test_mod%>%
  mutate('연령대_30~50_유동인구_수'=연령대_30_유동인구_수+연령대_40_유동인구_수+연령대_50_유동인구_수)%>%
  mutate('연령대_10,30_상주인구_수'=연령대.10.상주인구수+연령대.30.상주인구수)%>%
  select(-c(연령대_30_유동인구_수,연령대_40_유동인구_수,연령대_50_유동인구_수,연령대.10.상주인구수,연령대.30.상주인구수))

ctrl<- rpart.control(minsplit = 10,
                     cp = 0.001,
                     maxdepth = 30)

#모형적합
set.seed(1234)
fit1<-rpart(formula = (분기_매출_금액/점포_수)~.,
            data = trainD,
            control = ctrl)
printcp(fit1)
#xerror가 최소값인지 확인(최소면 가지치기 필요없음)
which.min(fit1$cptable[,4]) #최소값 위치 찾기


#실제값할당
real<- (testD$분기_매출_금액/testD$점포_수)
pred1<- predict(fit1, newdata = testD, type = 'vector')

#사용자정의함수 불러오기
regMeasure(real = real, pred = pred1)

# MSE     RMSE      MAE     MAPE
# 1 4.252098e+14 20620617 15457198 2.029574


#################################################
# 연령대 20/나머지연령대 묶어보기(유동인구+상주인구)
#  -연령대60이상직장인구수
#################################################
trainE<- train_mod%>%
  mutate('연령대_30~50_유동인구_수'=연령대_30_유동인구_수+연령대_40_유동인구_수+연령대_50_유동인구_수)%>%
  mutate('연령대_10,30_상주인구_수'=연령대.10.상주인구수+연령대.30.상주인구수)%>%
  select(-c(연령대_30_유동인구_수,연령대_40_유동인구_수,연령대_50_유동인구_수,연령대.10.상주인구수,연령대.30.상주인구수,연령대_60_이상_직장인구_수))
testE<- test_mod%>%
  mutate('연령대_30~50_유동인구_수'=연령대_30_유동인구_수+연령대_40_유동인구_수+연령대_50_유동인구_수)%>%
  mutate('연령대_10,30_상주인구_수'=연령대.10.상주인구수+연령대.30.상주인구수)%>%
  select(-c(연령대_30_유동인구_수,연령대_40_유동인구_수,연령대_50_유동인구_수,연령대.10.상주인구수,연령대.30.상주인구수,연령대_60_이상_직장인구_수))

ctrl<- rpart.control(minsplit = 10,
                     cp = 0.001,
                     maxdepth = 30)

#모형적합
set.seed(1234)
fit1<-rpart(formula = (분기_매출_금액/점포_수)~.,
            data = trainE,
            control = ctrl)
printcp(fit1)
#xerror가 최소값인지 확인(최소면 가지치기 필요없음)
which.min(fit1$cptable[,4]) #최소값 위치 찾기


#실제값할당
real<- (testE$분기_매출_금액/testE$점포_수)
pred1<- predict(fit1, newdata = testE, type = 'vector')

#사용자정의함수 불러오기
regMeasure(real = real, pred = pred1)

# MSE     RMSE      MAE     MAPE
# 1 5.848565e+14 24183806 15911637 2.070734

#################################################
# 연령대 20/나머지연령대 묶어보기(유동인구+상주인구)
#  -연령대60이상직장인구수
#################################################
trainE<- train_mod%>%
  mutate('연령대_30~50_유동인구_수'=연령대_30_유동인구_수+연령대_40_유동인구_수+연령대_50_유동인구_수)%>%
  mutate('연령대_10,30_상주인구_수'=연령대.10.상주인구수+연령대.30.상주인구수)%>%
  select(-c(연령대_30_유동인구_수,연령대_40_유동인구_수,연령대_50_유동인구_수,연령대.10.상주인구수,연령대.30.상주인구수,연령대_60_이상_직장인구_수))
testE<- test_mod%>%
  mutate('연령대_30~50_유동인구_수'=연령대_30_유동인구_수+연령대_40_유동인구_수+연령대_50_유동인구_수)%>%
  mutate('연령대_10,30_상주인구_수'=연령대.10.상주인구수+연령대.30.상주인구수)%>%
  select(-c(연령대_30_유동인구_수,연령대_40_유동인구_수,연령대_50_유동인구_수,연령대.10.상주인구수,연령대.30.상주인구수,연령대_60_이상_직장인구_수))

ctrl<- rpart.control(minsplit = 10,
                     cp = 0.001,
                     maxdepth = 30)

#모형적합
set.seed(1234)
fit1<-rpart(formula = (분기_매출_금액/점포_수)~.,
            data = trainE,
            control = ctrl)
printcp(fit1)
#xerror가 최소값인지 확인(최소면 가지치기 필요없음)
which.min(fit1$cptable[,4]) #최소값 위치 찾기


#실제값할당
real<- (testE$분기_매출_금액/testE$점포_수)
pred1<- predict(fit1, newdata = testE, type = 'vector')

#사용자정의함수 불러오기
regMeasure(real = real, pred = pred1)

# MSE     RMSE      MAE     MAPE
# 1 5.848565e+14 24183806 15911637 2.070734
