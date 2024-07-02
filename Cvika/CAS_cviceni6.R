library(forecast)

############################
### Hledani modelu arima

# pro radu Nile, lynx
plot(Nile)
  # Flow of the River Nile
  # rada, ktera alespon v uvodu stacionarni neni
par(mfrow=c(2,1))
acf(Nile,na.action = na.pass);pacf(Nile,na.action = na.pass)
par(mfrow=c(1,1))
  # autokorelacni funkce klesa moc pomalu
acf(Nile,na.action = na.pass,type="covariance",plot=F)
  # prvni z uvedenych hodnot je rozptyl rady

d1<-diff(Nile)
  # rada prvnich diferenci
plot(d1)
par(mfrow=c(2,1))
acf(d1,na.action = na.pass);pacf(d1,na.action = na.pass)
par(mfrow=c(1,1))
  # rada se jevi jako stacionarni
  # pro radu diferenci se jevi jako optimalni model MA(1)
acf(d1,na.action = na.pass,type="covariance",plot=F)
  # rozptyl se zmensil

d2<-diff(Nile,differences = 2)
  # rada druhych diferenci
plot(d2)
par(mfrow=c(2,1))
acf(d2,na.action = na.pass);pacf(d2,na.action = na.pass)
par(mfrow=c(1,1))
acf(d2,na.action = na.pass,type="covariance",plot=F)
  # rozptyl se zvetsil - druha diference uz je spatna

(fit1<-arima(Nile,order=c(1,1,0)))
  # ARIMA(1,1,0)
(fit2<-arima(Nile,order=c(2,1,0)))
  # ARIMA(2,1,0)
(fit3<-arima(Nile,order=c(0,1,1)))
  # ARIMA(0,1,1)
(fit4<-arima(Nile,order=c(1,1,1)))
  # ARIMA(1,1,1)
  # podle AIC kriteria je optimalni rada ARIMA(1,1,1)
BIC(fit3)
BIC(fit4)
  # podle BIC kriteria rada ARIMA(0,1,1)

(fit.a<-auto.arima(Nile))
  # automaticky se voli na zaklade AIC kriteria

# predpoved
(for.fit2<-forecast(fit2,10))
  # predpoved s intervalem spolehlivosti
predict(fit2,10)
  # predpoved bez intervalu spolehlivosti pouze se stredni chybou
plot(for.fit2)
  # zakresneni predpovedi do grafu

# simulace budoucich hodnot
s<-matrix(0,10,10)
(s[1,]<-simulate(fit2,nsim=10,future=T))
  # simulovane pokracovani rady
plot(Nile,xlim=c(1871,1980),ylim=c(100,1700))
lines(1971:1980,s[1,],col=3)
  # zakresleni simulovane rady do grafu
for(i in 2:10){
  s[i,]<-simulate(fit2,nsim=10,future=T)
  lines(1971:1980,s[i,],col=3)
}
  # pripocitani a prikresleni dalsich moznych deviti pruchodu

############################
### Hledani modelu SARIMA

# Zkusime najit optimalni model pro rady nottem, UKDriverDeaths

plot(nottem)
  # Average Monthly Temperatures at Nottingham, 1920–1939
  # evidentne sezonni rada
par(mfrow=c(2,1))
acf(nottem,na.action = na.pass);pacf(nottem,na.action = na.pass)
par(mfrow=c(1,1))
  # sezonnost je videt v autokorelacni funkci

d1<-diff(nottem,lag=12)
# rada prvnich sezonnich diferenci
plot(d1)
par(mfrow=c(2,1))
acf(d1,na.action = na.pass);pacf(d1,na.action = na.pass)
par(mfrow=c(1,1))
  # autokorelacni i parcialni autokorelacni funkce stale ukazuji vysoke hodnoty na delce sezony,
  #   ale jinde jiz ne
  # jevi se jako MA(1) model na sezonnich datech
acf(d1,na.action = na.pass,type="covariance",plot=F)
  # rozptyl se zmensil

fit1<-arima(nottem,order=c(1,0,0),seasonal=list(order=c(0,1,0)))
fit2<-arima(nottem,order=c(0,0,1),seasonal=list(order=c(0,1,0)))
fit3<-arima(nottem,order=c(1,0,1),seasonal=list(order=c(0,1,0)))
fit4<-arima(nottem,order=c(0,0,0),seasonal=list(order=c(1,1,0)))
fit5<-arima(nottem,order=c(0,0,0),seasonal=list(order=c(0,1,1)))
fit6<-arima(nottem,order=c(0,0,0),seasonal=list(order=c(1,1,1)))
fit7<-arima(nottem,order=c(1,0,0),seasonal=list(order=c(1,1,1)))
BIC(fit1)
BIC(fit2)
BIC(fit3)
BIC(fit4)
BIC(fit5)
BIC(fit6)
BIC(fit7)
  # jako optimalni se jevi posledni model
  # SARIMA(1,0,0)x(1,1,1)[12]

(fit.a<-auto.arima(nottem))
BIC(fit.a)
  # automaticky model rozhodne optimalni neni - je prilis komplikovany

# autokorelacni a parcialni autokorelacni funkce optimalniho modelu
par(mfrow=c(2,1))
acf(residuals(fit7),na.action = na.pass);pacf(residuals(fit7),na.action = na.pass)
par(mfrow=c(1,1))
  # zavislosti jiz v datech nejsou

#############################
### najdete optimalni model a spocitejte 15 predpovedi pro radu lynx
### najdete optimalni model pro radu BJsales, UKDriverDeaths

### predikce pro kombinovanou radu

# pracujte s daty airquality (nejsou zadany jako casove rady, je treba je transformovat)
# vytvorte novou promennou, ktera bude pocitana jako Temp-0.2*Wind
# predpovidejte nove hodnoty (10 hodnot) temp.ad z dat temp a wind vcetne intervalu spolehlivosti

# predpovedi je mozne pocitat, jako predpovedi pro kazdou radu zvlast 
#   a vysledek zkombinovat dle daneho vzorce
# kombinovany interval spolehlivosti nebude fungovat
# pro interval spolehlivosti je nutne nasimulovat velke mnozstvi budoucich pruchodu obou rad
#   zkombinovat je dle daneho vzorce 
#   a interval spolehlivosti vzit jako pozadovane kvantily ze simulovanych dat

# kvantily z matice s, kde bude 10000 radku se simulovanymi pruchody rady
quantile(s[,1],c(0.05,0.95))
  # meze 90%-ni interval spolehlivosti v prvnim kroku

# pro kazdou radu tedy najdete optimalni model, simulujte budouci pruchody,
#   zkombinujte je a najdete kvantily intervalu spolehlivosti