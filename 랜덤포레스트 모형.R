

library(randomForest)
set.seed(1234)
fit1<-randomForest(formula = (분기_매출_금액/점포_수)~.,
                   data = train_mod,
                   ntree = 1000,
                   mtry = 10,
                   importance = TRUE,
                   do.trace = 50,
                   keep.forest = T)
print(fit1)


print(fit1$err.rate)
print(fit1$err.rate[,1])
tail(fit1$err.rate[,1],n=1L)
plot(fit1)

importance(fit1,type = 1)
varImpPlot(fit1,type = 1)


real<-(test_mod$분기_매출_금액/test_mod$점포_수)
pred1<-predict(fit1,newdata = test_mod,type = 'response')


#사용자정의함수 불러오기
source("Myfunction.R")

regMeasure(real = real, pred = pred1)
# MSE     RMSE      MAE     MAPE
# 1 6.144392e+14 24787885 13337171 1.195882



##그리드생성
grid<-expand.grid(ntree=c(300,500,700,1000),
                  mtry=c(3:10),
                  error=NA)
print(grid) 

#반복문 사용한 모형 튜닝
n<- nrow(grid)
for(i in 1:n){
  ntree<- grid$ntree[i]
  mtry<- grid$mtry[i]
  disp<- str_glue('현재 {i}번째 행 실행중! [ntree: {ntree},
                  mtry : {mtry}]')
  cat(disp,'\n\n')
  set.seed(1234)
  fit<- randomForest(formula =(분기_매출_금액/점포_수)~.,
                     data = train_mod,
                     ntree = ntree,
                     mtry = mtry)
  grid$error[i] <- tail(x = fit$mse, n = 1)
}



#보기쉽게 그래프로 그리기
plot(grid$error,type = 'b',pch=19,col='red',
     main = 'Grid Search Result')

#최소값 직선 추가
abline(h=min(grid$error),col='red',lty=2)

#최적의 파라미터 설정
loc<- which.min(grid$error)
print(loc)
bestPara<- grid[loc,]
print(bestPara)

#OOB오차가 최소인 하이퍼파라미터로 모형 적합
set.seed(1234)
best<-randomForest(formula = (분기_매출_금액/점포_수)~.,
                   data = train_mod,
                   ntree = bestPara$ntree,
                   mtry = bestPara$mtry,
                   importance = TRUE)
print(best)


plot(best,main='Best Fit')
importance(best,type = 1)
varImpPlot(best,main = 'Variable Importance',type = 1)




pred2<-predict(best,newdata = test_mod,type = 'response')


#사용자정의함수 불러오기
regMeasure(real = real, pred = pred2)

# MSE     RMSE      MAE     MAPE
# 1 5.867377e+14 24222670 13294579 1.219518




#####################################################
# 연령대 20/나머지연령대 묶어보기(유동인구+상주인구)
#####################################################
trainD<-data.frame(trainD)
testD<-data.frame(testD)

library(randomForest)
set.seed(1234)
fit1<-randomForest(formula = (분기_매출_금액/점포_수)~.,
                   data = trainD,
                   ntree = 1000,
                   mtry = 9,
                   importance = TRUE,
                   do.trace = 50,
                   keep.forest = T)
print(fit1)


print(fit1$err.rate)
print(fit1$err.rate[,1])
tail(fit1$err.rate[,1],n=1L)
plot(fit1)

importance(fit1,type = 1)
varImpPlot(fit1,type = 1)

#실제값할당
real<-(testD$분기_매출_금액/testD$점포_수)
pred1<-predict(fit1,newdata = testD,type = 'response')


#사용자정의함수 불러오기
source("Myfunction.R")

regMeasure(real = real, pred = pred1)
# MSE     RMSE      MAE     MAPE
# 1 6.003365e+14 24501765 13533371 1.255849



##그리드생성
grid<-expand.grid(ntree=c(300,500,700,1000),
                  mtry=c(3:9),
                  error=NA)
print(grid) 

#반복문 사용한 모형 튜닝
n<- nrow(grid)
for(i in 1:n){
  ntree<- grid$ntree[i]
  mtry<- grid$mtry[i]
  disp<- str_glue('현재 {i}번째 행 실행중! [ntree: {ntree},
                  mtry : {mtry}]')
  cat(disp,'\n\n')
  set.seed(1234)
  fit<- randomForest(formula =(분기_매출_금액/점포_수)~.,
                     data = trainD,
                     ntree = ntree,
                     mtry = mtry)
  grid$error[i] <- tail(x = fit$mse, n = 1)
}



#최적의 파라미터 설정
loc<- which.min(grid$error)
print(loc)
bestPara<- grid[loc,]
print(bestPara)

#OOB오차가 최소인 하이퍼파라미터로 모형 적합
set.seed(1234)
best<-randomForest(formula = (분기_매출_금액/점포_수)~.,
                   data = trainD,
                   ntree = bestPara$ntree,
                   mtry = bestPara$mtry,
                   importance = TRUE)
print(best)


plot(best,main='Best Fit')
importance(best,type = 1)
varImpPlot(best,main = 'Variable Importance',type = 1)



pred2<-predict(best,newdata = testD,type = 'response')


#사용자정의함수 불러오기
regMeasure(real = real, pred = pred2)
# MSE     RMSE      MAE     MAPE
# 1 6.003365e+14 24501765 13533371 1.255849




#################################################
# 연령대 20/나머지연령대 묶어보기(유동인구)
#################################################
trainC<-data.frame(trainC)
testC<-data.frame(testC)

library(randomForest)
set.seed(1234)
fit1<-randomForest(formula = (분기_매출_금액/점포_수)~.,
                   data = trainC,
                   ntree = 1000,
                   mtry = 9,
                   importance = TRUE,
                   do.trace = 50,
                   keep.forest = T)
print(fit1)


print(fit1$err.rate)
print(fit1$err.rate[,1])
tail(fit1$err.rate[,1],n=1L)
plot(fit1)

importance(fit1,type = 1)
varImpPlot(fit1,type = 1)

#실제값할당
real<-(testC$분기_매출_금액/testC$점포_수)
pred1<-predict(fit1,newdata = testC,type = 'response')


#사용자정의함수 불러오기
source("Myfunction.R")

regMeasure(real = real, pred = pred1)
# MSE     RMSE      MAE     MAPE
# 1 5.899445e+14 24288773 13717904 1.248633



##그리드생성
grid<-expand.grid(ntree=c(300,500,700,1000),
                  mtry=c(3:9),
                  error=NA)
print(grid) 

#반복문 사용한 모형 튜닝
n<- nrow(grid)
for(i in 1:n){
  ntree<- grid$ntree[i]
  mtry<- grid$mtry[i]
  disp<- str_glue('현재 {i}번째 행 실행중! [ntree: {ntree},
                  mtry : {mtry}]')
  cat(disp,'\n\n')
  set.seed(1234)
  fit<- randomForest(formula =(분기_매출_금액/점포_수)~.,
                     data = trainC,
                     ntree = ntree,
                     mtry = mtry)
  grid$error[i] <- tail(x = fit$mse, n = 1)
}



#최적의 파라미터 설정
loc<- which.min(grid$error)
print(loc)
bestPara<- grid[loc,]
print(bestPara)

#OOB오차가 최소인 하이퍼파라미터로 모형 적합
set.seed(1234)
best<-randomForest(formula = (분기_매출_금액/점포_수)~.,
                   data = trainC,
                   ntree = bestPara$ntree,
                   mtry = bestPara$mtry,
                   importance = TRUE)
print(best)


plot(best,main='Best Fit')
importance(best,type = 1)
varImpPlot(best,main = 'Variable Importance',type = 1)



pred2<-predict(best,newdata = testC,type = 'response')


#사용자정의함수 불러오기
regMeasure(real = real, pred = pred2)
# MSE     RMSE      MAE     MAPE
# 1 5.899445e+14 24288773 13717904 1.248633


#################################################
# 연령대_20_유동인구_수 빼보기
#################################################
library(randomForest)
set.seed(1234)
fit1<-randomForest(formula = (분기_매출_금액/점포_수)~.,
                   data = trainA,
                   ntree = 1000,
                   mtry = 10,
                   importance = TRUE,
                   do.trace = 50,
                   keep.forest = T)

#실제값할당
real<-(testA$분기_매출_금액/testA$점포_수)
pred1<-predict(fit1,newdata = testA,type = 'response')


#사용자정의함수 불러오기
regMeasure(real = real, pred = pred1)
# MSE     RMSE      MAE    MAPE
# 1 596718524049763 24427823 13340516 1.22596


##그리드생성
grid<-expand.grid(ntree=c(300,500,700,1000),
                  mtry=c(3:10),
                  error=NA)
print(grid) 

#반복문 사용한 모형 튜닝
n<- nrow(grid)
for(i in 1:n){
  ntree<- grid$ntree[i]
  mtry<- grid$mtry[i]
  disp<- str_glue('현재 {i}번째 행 실행중! [ntree: {ntree},
                  mtry : {mtry}]')
  cat(disp,'\n\n')
  set.seed(1234)
  fit<- randomForest(formula =(분기_매출_금액/점포_수)~.,
                     data = trainA,
                     ntree = ntree,
                     mtry = mtry)
  grid$error[i] <- tail(x = fit$mse, n = 1)
}



#최적의 파라미터 설정
loc<- which.min(grid$error)
print(loc)
bestPara<- grid[loc,]
print(bestPara)

#OOB오차가 최소인 하이퍼파라미터로 모형 적합
set.seed(1234)
best<-randomForest(formula = (분기_매출_금액/점포_수)~.,
                   data = trainA,
                   ntree = bestPara$ntree,
                   mtry = bestPara$mtry,
                   importance = TRUE)

varImpPlot(best,main = 'Variable Importance',type = 1)

pred2<-predict(best,newdata = testA,type = 'response')


#사용자정의함수 불러오기
regMeasure(real = real, pred = pred2)
# MSE     RMSE      MAE     MAPE
# 1 591113236798606 24312820 13318448 1.243877



#################################################
# 연령대20유동인구수,연령대20상주인구수 빼보기
#################################################
library(randomForest)
set.seed(1234)
fit1<-randomForest(formula = (분기_매출_금액/점포_수)~.,
                   data = trainB,
                   ntree = 1000,
                   mtry = 10,
                   importance = TRUE,
                   do.trace = 50,
                   keep.forest = T)

#실제값할당
real<-(testB$분기_매출_금액/testB$점포_수)
pred1<-predict(fit1,newdata = testB,type = 'response')


#사용자정의함수 불러오기
regMeasure(real = real, pred = pred1)
# MSE     RMSE      MAE     MAPE
# 1 597661817973563 24447123 13233972 1.228281


##그리드생성
grid<-expand.grid(ntree=c(300,500,700,1000),
                  mtry=c(3:10),
                  error=NA)
print(grid) 

#반복문 사용한 모형 튜닝
n<- nrow(grid)
for(i in 1:n){
  ntree<- grid$ntree[i]
  mtry<- grid$mtry[i]
  disp<- str_glue('현재 {i}번째 행 실행중! [ntree: {ntree},
                  mtry : {mtry}]')
  cat(disp,'\n\n')
  set.seed(1234)
  fit<- randomForest(formula =(분기_매출_금액/점포_수)~.,
                     data = trainB,
                     ntree = ntree,
                     mtry = mtry)
  grid$error[i] <- tail(x = fit$mse, n = 1)
}



#최적의 파라미터 설정
loc<- which.min(grid$error)
print(loc)
bestPara<- grid[loc,]
print(bestPara)

#OOB오차가 최소인 하이퍼파라미터로 모형 적합
set.seed(1234)
best<-randomForest(formula = (분기_매출_금액/점포_수)~.,
                   data = trainB,
                   ntree = bestPara$ntree,
                   mtry = bestPara$mtry,
                   importance = TRUE)

varImpPlot(best,main = 'Variable Importance',type = 1)

pred2<-predict(best,newdata = testB,type = 'response')


#사용자정의함수 불러오기
regMeasure(real = real, pred = pred2)
# MSE     RMSE      MAE     MAPE
# 1 629055400486736 25080977 13254419 1.237042




###################################################################
#코로나관련변수 제거
###################################################################

trainF<- train_mod%>%
select(-c(총_확진자_수,서울_확진자_수))
testF<- test_mod%>%
  select(-c(총_확진자_수,서울_확진자_수))


library(randomForest)
set.seed(1234)
fit1<-randomForest(formula = (분기_매출_금액/점포_수)~.,
                   data = trainF,
                   ntree = 1000,
                   mtry = 10,
                   importance = TRUE,
                   do.trace = 50,
                   keep.forest = T)

#실제값할당
real<-(testF$분기_매출_금액/testF$점포_수)
pred1<-predict(fit1,newdata = testF,type = 'response')


#사용자정의함수 불러오기
regMeasure(real = real, pred = pred1)
# MSE     RMSE      MAE     MAPE
# 1 542471240193804 23291012 12637922 1.109185


##그리드생성
grid<-expand.grid(ntree=c(300,500,700,1000),
                  mtry=c(3:10),
                  error=NA)
print(grid) 

#반복문 사용한 모형 튜닝
n<- nrow(grid)
for(i in 1:n){
  ntree<- grid$ntree[i]
  mtry<- grid$mtry[i]
  disp<- str_glue('현재 {i}번째 행 실행중! [ntree: {ntree},
                  mtry : {mtry}]')
  cat(disp,'\n\n')
  set.seed(1234)
  fit<- randomForest(formula =(분기_매출_금액/점포_수)~.,
                     data = trainF,
                     ntree = ntree,
                     mtry = mtry)
  grid$error[i] <- tail(x = fit$mse, n = 1)
}



#최적의 파라미터 설정
loc<- which.min(grid$error)
print(loc)
bestPara<- grid[loc,]
print(bestPara)

#OOB오차가 최소인 하이퍼파라미터로 모형 적합
set.seed(1234)
best<-randomForest(formula = (분기_매출_금액/점포_수)~.,
                   data = trainF,
                   ntree = bestPara$ntree,
                   mtry = bestPara$mtry,
                   importance = TRUE)

varImpPlot(best,main = 'Variable Importance F',type = 1)

pred2<-predict(best,newdata = testF,type = 'response')


#사용자정의함수 불러오기
regMeasure(real = real, pred = pred2)
# MSE     RMSE      MAE     MAPE
# 1 542471240193804 23291012 12637922 1.109185













