############################
### nacteni knihoven, ktere pracuji s casovymi radami
install.packages(c('TTR', 'zoo', 'forecast'))
library(TTR)
library(zoo)
library(forecast)
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
### funkcne zapsatelny trend
# pokud jsme tvar trendu schopni odhadnout z dat, je mozne pocitat primo model zavislosti na case
cas<-1:42
cas2<-cas*cas
cas3<-cas2*cas
lin.trend<-lm(kings~cas)
kv.trend<-lm(kings~cas+cas2)
kub.trend<-lm(kings~cas+cas2+cas3)
plot(kings.ts)
abline(lin.trend,col=2)
lines(cas,fitted(kv.trend),col=3)
lines(cas,fitted(kub.trend),col=4)
legend(34,40,legend=c("puvodni rada","linearni trend","kvadraticky trend","kubicky trend"),lty=1,col=1:4,cex=0.7)

# porovnani, ktery trend je lepsi
summary(lin.trend)$r.squared
summary(kv.trend)$r.squared
summary(kub.trend)$r.squared
  # procento variability vysvetlene modelem

AIC(lin.trend)
AIC(kv.trend)
AIC(kub.trend)
  # Akaikeho informacni kriterium - cim mensi tim lepsi
BIC(lin.trend)
BIC(kv.trend)
BIC(kub.trend)
  # Bayesovske informacni kriterium - cim mensi tim lepsi
  # idealni model se jevi kvadraticky

#############################
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

############################
## Najdete optimalni zpusob vyhlazeni u nasledujicich rad

## rada hlavniho indikatoru prodeje
bb = ts(BJsales.lead)
plot(bb)

bbExp<-HoltWinters(bb, gamma=FALSE, alpha = 0.3)
plot(bbExp)

# vypocte 20 predpovedi
bbExp.for<-forecast(bbExp, h=20)
plot(bbExp.for)
lines(1868:1911,bbExp$fitted[,1],col=3)


## koncentrace CO2 v ovydusi v Mauna Loa
plot(co2)

cc = ts(co2)
plot(cc)
cc.rm<-rollmean(cc,12)
lines(cc.rm,col=2)


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
