#TS 객체 만들기, 토요일자 제외
library(fpp2)
library(lubridate)
library(tseries)

PAPER_OFF_ON_TS<-PAPER_OFF_ON %>% filter(year(POST_DAY)>2010,wday(POST_DAY,label = T)!='토') %>%
  group_by(YEAR=year(POST_DAY),MONTH=month(POST_DAY)) %>% 
  summarise(SUM=sum(AMOUNT)) %>% 
  ungroup() %>% 
  select(SUM) %>%
  ts(start=2011,frequency = 12)

#EDA: 시계열 분해 
plot(decompose(PAPER_OFF_ON_TS))

#계절성 그래프

PAPER_OFF_ON_TS %>% ggseasonplot(year.labels = T,year.labels.left = T) +
  ggtitle("2011~2020 Monthly Sales") + 
  xlab("YEAR")+ 
  ylab("AMOUNT(백만)")

PAPER_OFF_ON_TS %>% ggseasonplot(polar = T) +
  ggtitle("2011~2020 Monthly Sales") + 
  xlab("YEAR")+ 
  ylab("AMOUNT(백만)")

#계절성 부시계열 그래프
PAPER_OFF_ON_TS %>% ggsubseriesplot() +
  ggtitle("2011~2020 Monthly Sales") + 
  xlab("YEAR")+ 
  ylab("AMOUNT(백만)")


#디키-풀러테스트:  p-value가 0.05보다 작기 때문에 정상성을 갖는다고 볼 수 있음

adf.test(PAPER_OFF_ON_TS)

#박스-칵스 변환가설검정:  정상성 테스트
library(TSA)
BoxCox.ar(PAPER_OFF_ON_TS)
BoxCox.ar(log(PAPER_OFF_ON_TS))

#시간 그래프
PAPER_OFF_ON_TS %>% autoplot() +
  ggtitle("2011~2020 Monthly Sales") + 
  xlab("YEAR")+ 
  ylab("AMOUNT(백만)")
  scale_x_continuous(breaks=seq(2011,2020,1))

#ARIMA Model: 로그 변환된 자료를 가지고 auto.arima를 돌림, 정확성을 높이기 위해 approximation=FALSE,stepwise=FALSE로 둠 
fit<-PAPER_OFF_ON_TS %>%log() %>% auto.arima(approximation=FALSE,stepwise=FALSE) 
fit1<-PAPER_OFF_ON_TS %>% auto.arima() 
fit2<-PAPER_OFF_ON_TS %>%log() %>% diff(12) %>% auto.arima() 

fit %>% forecast(h=12) %>% as_tibble() %>% map_df(.,sum)
fit %>% forecast(h=12) %>% 
  autoplot() + 
  ggtitle("Forecasting")+
  xlab("YEAR")+ 
  ylab("AMOUNT(백만)")+
  scale_x_continuous(breaks=seq(2011,2020,1))


#검증 

accuracy(PAPER_OFF_ON_TS,exp(fitted(fit)))
accuracy(PAPER_OFF_ON_TS,fitted(fit1))
accuracy(PAPER_OFF_ON_TS,exp(fitted(fit2)))

tsCV(PAPER_OFF_ON_TS, forecastfunction=auto.arima, h=8)


