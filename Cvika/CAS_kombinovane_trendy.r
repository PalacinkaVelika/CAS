#############################
### Linearni trend
time<-1971:2000

rada<-Examples1[,1]
rada.ts<-ts(rada,start=1971)
plot(rada.ts)

mod1<-lm(rada.ts~time)
summary(mod1)
coef(mod1)
AIC(mod1)
lines(time,fitted(mod1),col=2)

############################
## Kvadraticky trend

rada<-Examples1[,2]
rada.ts<-ts(rada,start=1971)
plot(rada.ts)

mod1<-lm(rada.ts~time+time2)
summary(mod1)
coef(mod1)
AIC(mod1)
lines(time,fitted(mod1),col=2)

mod1<-lm(rada.ts~time)
summary(mod1)
coef(mod1)
AIC(mod1)
lines(time,fitted(mod1),col=3)

############################
## Exponencialni trend

rada<-Examples1[,3]
rada.ts<-ts(rada,start=1971)
plot(rada.ts)

ln.rada.ts<-log(rada.ts)
mod1<-lm(ln.rada.ts~time)
summary(mod1)
coef(mod1)
exp(coef(mod1))
AIC(mod1)
lines(time,exp(fitted(mod1)),col=2)

mod1<-lm(rada.ts~time+time2)
summary(mod1)
coef(mod1)
AIC(mod1)
lines(time,fitted(mod1),col=3)

mod1<-lm(rada.ts~time)
summary(mod1)
coef(mod1)
AIC(mod1)
lines(time,fitted(mod1),col=4)

##############################
## Logisticky trend
x<-1:30

rada<-Examples1[,4]
rada.ts<-ts(rada,start=1971)
plot(rada.ts)
-(log(120))/log(0.7)

mod1<-nls(rada.ts ~ SSlogis(time, Asym, xmid, scal))
mod1<-nls(rada.ts ~ SSlogis(x, Asym, xmid, scal))
summary(mod1)
coef(mod1)
AIC(mod1)

mod1<-nls(rada.ts ~ SSgompertz(time, Asym, b2, b3))
mod1<-nls(rada.ts ~ SSgompertz(x, Asym, b2, b3))
summary(mod1)
coef(mod1)
AIC(mod1)

##############################
## Gompertzova krivka

rada<-Examples[,5]
rada.ts<-ts(rada,start=1971)
plot(rada.ts)
-(log(13.2))/log(0.8)

mod1<-nls(rada.ts ~ SSgompertz(time, Asym, b2, b3))
mod1<-nls(rada.ts ~ SSgompertz(x, Asym, b2, b3))
summary(mod1)
coef(mod1)
AIC(mod1)

mod1<-nls(rada.ts ~ SSlogis(time, Asym, xmid, scal))
mod1<-nls(rada.ts ~ SSlogis(x, Asym, xmid, scal))
summary(mod1)
coef(mod1)
AIC(mod1)

################################
## Vyzkousejte na dalsich radach ze souboru Examples1

################################
############################
### nacteni knihoven, ktere pracuji s casovymi radami
library(TTR)
library(zoo)
library(forecast)
library(randtests)
  # dalsi uzitecne funkce obsazene primo v knihovne stats

############################
### Vyhlazeni casovych rad

### Vyhlaeni pomoci klouzavych prumeru 

kings <- scan("http://robjhyndman.com/tsdldata/misc/kings.dat",skip=3)
  # veky, ve kterych zemreli anglicti kralove
kings.ts <- ts(kings)
plot(kings.ts)
  # trendova slozka bez sezonnosti

# klouzave prumery prvniho radu ruzne delky
kings.rm3<-rollmean(kings.ts,3)
kings.rm5<-rollmean(kings.ts,5)
kings.rm7<-rollmean(kings.ts,7)
kings.rm9<-rollmean(kings.ts,9)
kings.rm11<-rollmean(kings.ts,11)
  # pro krajni body odhady nejsou

plot(kings.ts)
lines(kings.rm3,col=2)
lines(kings.rm5,col=3)
lines(kings.rm7,col=4)
lines(kings.rm9,col=5)
lines(kings.rm11,col=6)
legend(34,40,legend=c("puvodni rada","KP delky 3","KP delky 5","KP delky 7","KP delky 9","KP delky 11"),
       lty=1,col=1:6,cex=0.8)
  # podivame se na vysledky a podle toho, jak detailni vyhlazeni chceme, volime delku klouzaveho prumeru

# klouzave prumery tretiho radu
vahy<-(1/35)*c(-3,12,17,12,-3)
kings.rm3.5<-rollapply(kings.ts, 5, 
                    function(z){return(weighted_mean = weighted.mean(z,vahy))})
vahy<-(1/231)*c(-21,14,39,54,59,54,39,14,-21)
kings.rm3.9<-rollapply(kings.ts, 9, 
                    function(z){return(weighted_mean = weighted.mean(z,vahy))})

plot(kings.ts)
lines(kings.rm3.5,col=2)
lines(kings.rm3.9,col=3)
  # mame lepsi vysledky nez obycejnymi klouzavymi prumery?

###################################
### Vyhlazeni pomoci exponencialniho vyrovnani

## Jednoduche exponencialni vyhlazovani - vhodne pro lokalne konstantni trend
rain <- scan("http://robjhyndman.com/tsdldata/hurst/precip1.dat",skip=1)
  # rocni srazky v Londyne merene v palcich z let 1813 az 1912
rain.ts <- ts(rain,start=c(1813))
plot(rain.ts)
  # aditivni rada s konstantnim trendem

rain.exp<-HoltWinters(rain.ts, beta=FALSE, gamma=FALSE)
  # jednoduche exponencialni vyrovnani
plot(rain.exp)
  # vykresli data i s odhadnutym trendem
rain.exp$fitted
  # predpovedi, neboli hodnoty trendu
rain.exp$SSE
  # sum of squared errors - soucet druhych mocnin chyb odhadu
  # pri exponencialnim vyhlazovani se bezne bere jako pocatek trendu prvni merena hodnota
  # pokud chceme trend zacit v jine hodnote, je mozne vyuzit parametr l.start
plot(HoltWinters(rain.ts, beta=FALSE, gamma=FALSE,l.start=25))
  # jak vypada odhad trendu zacinajiciho na hodnote 25

## Predpovedi z exponencialniho vyhlazovani
rain.exp.for<-forecast(rain.exp, h=10)
  # vypocte 10 predpovedi na budouci uhrny srazek
plot(rain.exp.for)
lines(1814:1912,rain.exp$fitted[,1],col=3)
  # zakresleni predpovedi do grafu
  # modra cara je bodova predpoved, tmave seda plocha 80% interval spolehlivosti
  #   a svetle seda plocha 95% interval spolehlivosti

## Dvojite exponencialni vyhlazovani - pri lokalne linearnim trendu
skirts <- scan("http://robjhyndman.com/tsdldata/roberts/skirts.dat",skip=5)	
  # rocni data o prumeru lemu sukni z let 1866 az 1911
skirts.ts<-ts(skirts,start=c(1866))
plot(skirts.ts)
  # je videt nejprve rostouci a pak klesajici trend bez sezonni slozky s minimalni chybou

skirts.exp<-HoltWinters(skirts.ts, gamma=FALSE)
  # Holtova metoda - zobecnene dvojite exponencialni vyrovnani
  # odhadnute hodnoty parametru alfa a beta nam rikaji, ze odhady jsou zalozene 
  #   predevsim na nedavnych hodnotach (zavislost nejde daleko do minulosti)
plot(skirts.exp)
  # vykresleni odhadnuteho trendu
skirts.exp$SSE
  # Soucet druhych mocnin chyb odhadu

## Predpovedi z Holtova vyhlazovani
skirts.exp.for<-forecast(skirts.exp, h=20)
  # vypocte 20 predpovedi
plot(skirts.exp.for)
lines(1868:1911,skirts.exp$fitted[,1],col=3)
  # zakresleni predpovedi do grafu
  # modra cara je bodova predpoved, tmave seda plocha 80% interval spolehlivosti
  #   a svetle seda plocha 95% interval spolehlivosti

###############################
### Holt-Wintersova metoda pro sezonni rady

births <- scan("http://robjhyndman.com/tsdldata/data/nybirths.dat")
  # mesicni pocty narozenych v New Yorku od ledna 1946 do prosince 1959
births.ts <- ts(births, frequency=12, start=c(1946,1))
  # prevedeni na casovou radu
plot(births.ts)
  # vykresleni rady

births.hw<-HoltWinters(births.ts)
  # podobna myslenka jako u dvojiteho exponencialniho vyhlazeni, 
  #   ale pridam sezonnost
plot(births.hw)
  # vykresleni odhadnuteho trendu
  # na data sedi mnohem lepe 
births.hw
  # vidim i vykyvy pro jenotlive mesice

souvenir <- scan("http://robjhyndman.com/tsdldata/data/fancy.dat")
  # mesicni data o prodeji suvenyru v Queendslandu v Australii od ledna 1987 do prosince 1993
souvenir.ts <- ts(souvenir, frequency=12, start=c(1987,1))	
  # prevedeni na casovou radu
plot(souvenir.ts)
  # vykresleni rady
  # vykyvy se zvetsuji -> multplikativni sezonni rada

# Vyzkousim bezny HoltWintersuv model
souvenir.hw<-HoltWinters(souvenir.ts)
plot(souvenir.hw)
  # vykresleni odhadnuteho trendu
souvenir.hw$SSE
  # nic moc

# Ale existuje i Holt-Wintersova metoda pro multiplikativni rady
souvenir.hwm<-HoltWinters(souvenir.ts,seasonal = "multiplicative")
plot(souvenir.hwm)
  # vykresleni odhadnuteho trendu
souvenir.hwm$SSE
  # vyrazne lepsi
# druha moznost je logaritmovat
ln.souvenir.ts <- log(souvenir.ts)
  # logaritmem casto prevedu multiplikativni model na aditivni
  # oduvodneni: logaritmus soucinu rovna se soucet logaritmus
plot(ln.souvenir.ts)

ln.souvenir.exp<- HoltWinters(ln.souvenir.ts)
ln.souvenir.exp
  # nizka hodnota koeficientu alpha ukazuje, ze uroven se meni v zavislosti na 
  #   nedavnych i davnych datech
  # beta = 0 rika, ze sklon trendu se v case nemeni
  # vysoka hodnota gamma rika, ze sezonnost je zalozena na nedavnych/poslednich datech
plot(ln.souvenir.exp)
  # odhad je velmi dobry

########################
## Obecna dekompozice rady se sezonni slozkou 
births.dec<-decompose(births.ts)
  # ma i verzi pro multiplikativni radu type = "multiplicative"
plot(births.dec)
  # rada byla rozlozena na trend, sezonni slozku a nahodnou chybu
births.dec$seasonal
  # sezonni slozka
births.dec$trend
  # je mozne odhadnout krivkou logistickeho trendu
(ns<-births.dec$random)
  # nahodna slozka

############################
## Najdete optimalni zpusob vyhlazeni u nasledujicich rad

## rada hlavniho indikatoru prodeje
plot(BJsales.lead)
## koncentrace CO2 v ovydusi v Mauna Loa
plot(co2)
## rocni hodnoty vysky hladiny Huronskeho jezera
plot(LakeHuron)
## rocni prumerne teploty v New Haven
plot(nhtemp)
## rocni prutoky Nilu
plot(Nile)
## kvartalni hodnoceni americkych presidentu
plot(presidents)
## mesicni pocty vaznych nehod Britanii
plot(UKDriverDeaths)
## pocty lidi pripojenych k internetu za minutu
plot(WWWusage)
lines(rollmean(presidents,5),col=2)

# popiste + odhadnete radu AirPassengers
  # Monthly Airline Passenger Numbers 1949-1960
# popiste + odhadnete radu fdeaths
  # Monthly Deaths from Lung Diseases in the UK

#######################
## Testy nahodnosti pro radu typu bily sum
plot(discoveries)

# Je tato rada nahodna?
difference.sign.test(discoveries)
  # test zalozeny na znamenkach diferenci
diff(discoveries,1)
  # takto vypadaji diference
sign(diff(discoveries,1))
  # takto vypadaji znamenka diferenci
table(sign(diff(discoveries,1)))
  # a tady vidime pocty kladnych a zapornych diferenci
  # podle tohoto testu je rada nahodna

# pro testy zalozene na korelacnich koeficientech potrebuji poradove cislo hodnot
index<-1:length(discoveries)
cor.test(discoveries,index,method="kendall")
  # test zalozeny na Kandallove korelacnim koeficientu
  # korelacni koeficient pro usporadane promenne
cor.test(discoveries,index,method="spearman")
  # test zalozeny na Spearmanove korelacnim koeficientu
  # test pro spojite promenne zalozeny na poradich
  # oba tyto testy ukazuji mirnou zapornou korelaci (rada klesa),
  #   ktera je na hladine vyznamnosti 5% vyznamna

# dalsi testy
runs.test(discoveries)
  # Wald-Wolfowitz Runs Test
discoveries-median(discoveries)
  # rada odchylek od medianu
sign(discoveries-median(discoveries))
  # znamenka odchylek
(zn<-sign(discoveries-median(discoveries))[sign(discoveries-median(discoveries))!=0])
  # znamenka s vynechanymi nulami
abs(diff(zn))
  # body, kde se mi meni znamenko
sum(abs(diff(zn)))/2
  # pocet useku se stejnymi znamenky
  # rada se tvari nahodne

# pro zajemce
cox.stuart.test(discoveries)
  # znamenkovy test porovnavajici dve poloviny casove rady proti sobe
bartels.rank.test(discoveries)
  # poradova verze Neumannova podiloveho testu

###############################

# je rada LakeHuron nahodna?
  # hladina Huronskeho jezera
# je rada lh nahodna?
  # Luteinizing Hormone in Blood Samples
# je rada nhtemp nahodna?
  # Average Yearly Temperatures in New Haven