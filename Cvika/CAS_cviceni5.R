#######################
### knihovny
library(forecast)

#######################
### Opakovani
# Autokorelacni a parcialni autokorelacni funkce 
# vyzkousime pro radu nhtemp
plot(nhtemp)
  # prumerna rocni teplota ve stupnich Fahrenheita v New Haven z let 1912 az 1971
  # rada bez zasadniho trendu ci sezonnosti

# autokorelacni funkce
acf(nhtemp)
acf(nhtemp,plot=F)
  # konkretni hodnoty
# parcialni autokorelacni funkce
pacf(nhtemp)
pacf(nhtemp,plot=F)
  # vypis hodnot
# Ktere hodnoty jsou nenulove? Muzeme jednoznacne urcit model?

# A co rada LakeHuron
plot(LakeHuron)
  # rocni hodnoty vysky hladiny Huronskeho jezera ve stopach z let 1875-1972

# autokorelacni a parcialni autokorelacni funkce
acf(LakeHuron)
pacf(LakeHuron)
  # ktere korelace jsou nenulove zde?

########################
## Hledani modelu ARMA
# rada presidents
plot(presidents)
  # ctvrtletni hodnoceniamerickych prezidentu
  # rada bez zjevneho trendu, budu na ni hledat model ARMA

# autokorelacni a parcialni autokorelacni funkce
par(mfrow=c(2,1))
acf(presidents,na.action = na.pass);pacf(presidents,na.action = na.pass)
par(mfrow=c(1,1))
  # autokorelacni funkce nema bod useknuti, parcialni autokorelacni funkce ho ma, k0 = 1
  # optimalni model by mel byt AR(1)

# odhadnu vice jednoduchych modelu
(fit1<-arima(presidents,order=c(1,0,0)))
  # AR(1)
(fit2<-arima(presidents,order=c(2,0,0)))
  # AR(2)
(fit3<-arima(presidents,order=c(0,0,1)))
  # MA(1)
(fit4<-arima(presidents,order=c(1,0,1)))
  # ARMA(1,1)

# ke kazdemu modelu vidim Akaikeho informacni kriterium
#   podle nej by mel byt nejlepsi model AR(1) : aic = 839.78

# Bayesovske informacni kriterium - pro vyber optimalniho modelu je lepsi
BIC(fit1)
BIC(fit2)
BIC(fit3)
BIC(fit4)
  # i timto kriteriem vyberu model AR(1)

# v knihovne forecast existuje i funkce, ktera hleda idealni model automaticky
(fit.a<-auto.arima(presidents))
  # pokud mam data s nejakou frekvenci, pak mi automaticky hleda sezonni model
  # vybira model podle Akaikeho kriteria
  # na zaklade BIC kriteria bych radeji volila jednodussi model AR(1)

# vykresleni obou modelu proti sobe
plot(presidents)
lines(fitted(fit1),col=2)
lines(fitted(fit.a),col=3)
  # rozdil neni prilis velky

### Dalsi kriteria pro urceni idealniho modelu:
  # hodnoty parametru by mely byt alespon 2x vetsi nez jejich stredni chyba
  # autokorelacni funkce residui modelu by mely byt male
# AR(1) model
par(mfrow=c(2,1))
acf(residuals(fit1),na.action = na.pass);pacf(residuals(fit1),na.action = na.pass)
par(mfrow=c(1,1))
# AR(1) model se sezonni slozkou
par(mfrow=c(2,1))
acf(residuals(fit.a),na.action = na.pass);pacf(residuals(fit.a),na.action = na.pass)
par(mfrow=c(1,1))
  # u obou modelu vychazi hodnoty autokorelacni i parcialni autokorelacni funkce male

## test na nulovost autokorelacni funkce
#   nulovost autokorelacni funkce residui znamena, ze mame spravny model
Box.test(residuals(fit1), lag=5, type="Ljung-Box")
Box.test(residuals(fit.a), lag=5, type="Ljung-Box")
  # u sezonniho modelu vychazi autokorelacni funkce primo nulova
  #   u modelu AR(1) je mirna odchylka

# Jaky model tedy vybrat? - subjektivni rozhodnuti :)

#############################
### Samostatne

# hledejte optimalni model pro rady 
#	  nhtemp, LakeHuron, airquality$Wind, discoveries, treering, 
