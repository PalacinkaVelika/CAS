#######################
### knihovny
library(forecast)

#######################
# Autokorelacni a parcialni autokorelacni funkce 
data(lh)
  # Luteinizing Hormone in Blood Samples
plot(lh)
  # v rade neni evidentni zadny trend ani sezonnost

# autokovariancni funkce
acf(lh,type="covariance")
acf(lh,type="covariance",plot=F)
  # vypis hodnot, prvni z nich je rozptyl rady
# autokorelacni funkce
acf(lh)
acf(lh,plot=F)
  # opet si muzeme nechat hodnoty vypsat
# parcialni autokorelacni funkce
pacf(lh)
  # parcialni autokorelacni funkce
pacf(lh,plot=F)
  # vypis hodnot

# ktere korelace jsou nenulove? Ktare modely pripadaji pro radu v uvahu?
par(mfrow=c(2,1))
acf(lh);pacf(lh)
par(mfrow=c(1,1))
  # nakresleni obou funkci pod sebe

# test na nulovost autokorelacni funkce
Box.test(lh, lag=2, type="Ljung-Box")
  # rucne najdete hodnotu, kterou by nemela prekrocit druha autokorelace, pokud vime, ze prvni je nulova

# nulovost autokorelacni funkce residui znamena, ze mame spravny model

# pomoci funkce arima.sim(n = ,list(ar = ,ma = )) nasimulujte rady nasledujicich typu
# podivejte se, jak rady vypadaji a jak vypadaji jejich autokoelacni a parcialni autokorelacni funkce
#   MA(1) s parametrem theta1 = 0.75
rada<-arima.sim(n = 100,list(ma = 0.75))
plot(rada)
par(mfrow=c(2,1))
acf(rada);pacf(rada)
par(mfrow=c(1,1))

#   MA(1) s parametrem theta1 = -0.75
#   MA(1) s parametrem theta1 = 1.5
#   AR(1) s parametrem phi1 = 0.75
#   AR(1) s parametrem phi1 = - 0.75
#   AR(1) s parametrem phi1 = 1.5
x<-rep(0,100)
bs<-rnorm(100)
for(i in 2:100){x[i]<-1.5*x[i-1]+bs[i]}
rada<-ts(x)
plot(rada)
	# explozivni proces
	
########################
## Hledani modelu ARMA

# rada presidents
plot(presidents)
  # Quarterly Approval Ratings of US Presidents
  # rada bez zjevneho trendu, budu na ni hledat model ARMA
par(mfrow=c(2,1))
acf(presidents,na.action = na.pass);pacf(presidents,na.action = na.pass)
par(mfrow=c(1,1))
  # autokorelacni funkce nema bod useknuti, parcialni autokorelacni funkce ho ma, k0 = 1
  # optimalni model by mel byt AR(1)

# porovnani vice modelu
(fit1<-arima(presidents,order=c(1,0,0)))
	# AR(1)
(fit2<-arima(presidents,order=c(2,0,0)))
	# AR(2)
(fit3<-arima(presidents,order=c(0,0,1)))
	# MA(1)
(fit4<-arima(presidents,order=c(1,0,1)))
	# ARMA(1,1)

# Bayesovske informacni kriterium
BIC(fit1)
BIC(fit2)
BIC(fit3)
BIC(fit4)
  # optimalni je model AR(1)

# existuje i funkce, ktera hleda idealni model automaticky }v knihovne forecast)
(fit.a<-auto.arima(presidents))
  # pokud mam data s nejakou frekvenci, pak mi automaticky hleda sezonni model
  # na zaklade BIC kriteria bych radeji volila jednodussi model AR(1)

# vykresleni obou modelu proti sobe
plot(presidents)
lines(fitted(fit1),col=2)
lines(fitted(fit.a),col=3)

# hledejte optimalni model pro rady 
#	  airquality$Wind, discoveries, treering, nhtemp

## Jak ziskat predikce
(p<-predict(fit1,3))
  # vysledkem je predikce a jejic stredni chyba
(f<-forecast(fit1))
	# doda i interval spolehlivosti
plot(f)
	# graf i s predikcnim intervalem
  # pomoci ARIMA modelu se ziskaji kvalitni predikce pro maly pocet kroku dopredu
  # pro dlouhodobe predikce je lepe pouzit dekompozicni modely
	
############################
### Hledani modelu arima

# pro radu Nile, lynx
plot(Nile)
  # Flow of the River Nile
  # rada, ktera alespon v uvodu stacionarni neni
par(mfrow=c(2,1))
acf(Nile,na.action = na.pass);pacf(Nile,na.action = na.pass)
par(mfrow=c(1,1))
  # zejmena autokorelacni funkce klesa moc pomalu
acf(Nile,na.action = na.pass,type="covariance",plot=F)
  # prvni z uvedenych hodnot je rozptyl rady

d1<-diff(Nile,lag=1)
  # rada prvnich diferenci
plot(d1)
par(mfrow=c(2,1))
acf(d1,na.action = na.pass);pacf(d1,na.action = na.pass)
par(mfrow=c(1,1))
  # rada se jevi jako stacionarni
acf(d1,na.action = na.pass,type="covariance",plot=F)
  # rozptyl se zmensil

d2<-diff(d1,lag=1)
  # rada druhych diferenci
plot(d2)
par(mfrow=c(2,1))
acf(d2,na.action = na.pass);pacf(d2,na.action = na.pass)
par(mfrow=c(1,1))
  # pro radu diferenci se jevi jako optimalni model MA(1)
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
  # autokorelacni i parcialni autokorelacni funkce stale ukazuji vysoke hodnoty na delce sezony
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

fit.a<-auto.arima(nottem)
BIC(fit.a)
  # automaticky model rozhodne optimalni neni - je prilis komplikovany

# autokorelacni a parcialni autokorelacni funkce optimalniho modelu
par(mfrow=c(2,1))
acf(residuals(fit7),na.action = na.pass);pacf(residuals(fit7),na.action = na.pass)
par(mfrow=c(1,1))

############################
### prozkoumejme vzajemne vztahy mezi radami souboru airquality
ccf(ts(airquality$Wind),ts(airquality$Solar.R),na.action = na.pass)
	# nejsou zavisle
ccf(ts(airquality$Wind),ts(airquality$Ozone),na.action = na.pass)
	# okamzita zavislost

# dynamicke linearni modely
library(dynlm)
data("UKDriverDeaths", package = "datasets")
plot(UKDriverDeaths)
uk <- log10(UKDriverDeaths)
(dfm <- dynlm(uk ~ L(uk, 1) + L(uk, 12)))
summary(dfm)

mod1<-dynlm(airquality$Ozone ~ L(airquality$Ozone,1)+airquality$Wind+airquality$Solar.R+airquality$Temp)
summary(mod1)
