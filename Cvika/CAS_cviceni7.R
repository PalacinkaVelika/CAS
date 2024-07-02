############################
library(forecast)
library(dynlm)
library(lmtest)
  # nacteni knihoven

############################
### prozkoumejme vzajemne vztahy mezi radami souboru airquality
# kroskorelacni funkce
ccf(ts(airquality$Wind),ts(airquality$Solar.R),na.action = na.pass)
  # nejsou zavisle
ccf(ts(airquality$Wind),ts(airquality$Ozone),na.action = na.pass)
  # jsou zavisle, nejvyssi korelace je s nulovym zpozdenim

###########################
### dynamicke linearni modely
# jak modelovat zavislost mezi casovymi radami

# generovani dat
set.seed(999)
x_series <- arima.sim(n = 200, list(order = c(1,0,0), ar = 0.7, sd=1))
  # nasimulovany AR(1) proces
z <- ts.intersect(stats::lag(x_series, -1), stats::lag(x_series, -2)) 
  # hodnoty rady x_series v case t a t-1
y_series <- 15 + 0.8*z[,1] + 1.5*z[,2] + rnorm(199,0,1)
  # kombinace aktualni a zpozdene hodnoty + nahodne chyby
xy_series <- ts.intersect(y_series, z)
  # datovy soubor s jednou zavislou a dvema znezavislymi promennymi

# model pomoci bezne linearni regrese
lm1 <- lm(xy_series[,1] ~ xy_series[,2] + xy_series[,3])
summary(lm1)
  # popis modelu

checkresiduals(lm1)
  # popis residui modelu
  # abychom mohli pouzit klasicky linearni model, musi byt residua normalni, 
  #   s konstantnim rozptylem a NEKORELOVANA
  # nekorelovanosti residui se tyka i vypsany test (Breusch-Godfrey)
  #   je-li p-hodnota > alfa 0.05, pak residua jsou nekorelovana az do radu 10

# model pomoci dynamickeho modelovani
dlm1 <- dynlm(y_series ~ L(x_series, 1) + L(x_series, 2))
summary(dlm1)
  # stejny vysledek

### pouziti dynamickeho modelovani k odhaleni trendu a sezonnosti
plot(AirPassengers)
  # multiplikativni rada, udelam z ni logaritmus
ap <- log(AirPassengers)
plot(ap)
  # uz mam radu aditivni

# pridam zavislost na tretim pozorovani dozadu
set.seed(123)
ap_x <- 2 * stats::lag(ap, -3) + rnorm(length(ap), 0, 0.2)

# dynamicky linearni model
dlm2 <- dynlm(ap ~ trend(ap) + season(ap) + L(ap_x, 3))
summary(dlm2)
  # vyznamnost koeficientu

dlm2.o <- ts.intersect(ap, dlm2$fitted.values)
plot.ts(dlm2.o, plot.type = "single", col=c("orange","blue"), 
        lty=c(1,4), lwd=c(1,1),
        main = "Dynamic Linear Model - Original (orange) and Fitted series (blue)") 
  # jak mi odhadnuty model sedi na data

# model vypada hezky, jeste zkontrolujem residua
checkresiduals(dlm2)
# vidim autoregresni zavislost - model neni dobre!

### Jak pracovat s korelovanymi residui 
# generovani dat
set.seed(999)
x2_series <- arima.sim(n = 200, list(order = c(1,0,0), ar = 0.7, sd=1))
z2 <- ts.intersect(x2_series, stats::lag(x2_series, -3), stats::lag(x2_series, -4)) 
y2_series <- 15 + 0.8*z2[,2] + 1.5*z2[,3] 
y2_errors <- arima.sim(n = 196, list(order = c(1,0,1), ar = 0.6, ma = 0.6), sd=1)
y2_series <- y2_series + y2_errors
plot(y2_series)
  # zavisle promenna - casova rada

# chceme modelovat zavislost y2 na x2
# nejprve hledame, zda je zavislost v case t nebo se zpozdenim
ccf(x2_series,y2_series)
which.max(ccf(x2_series,y2_series,plot=F)$acf)
  # maximalni korelace na 16 pozici
ccf(x2_series,y2_series,plot=F)$lag[16]
  # se zpozdenim 4 kroku

# dynamicky model
dlm3 <- dynlm(y2_series ~ L(x2_series, 3) + L(x2_series, 4))
summary(dlm3)
  # vyznamnost koeficientu
lm2d <- ts.intersect(y2_series, dlm3$fitted.values)
plot.ts(lm2d, plot.type = "single", col=c("orange","blue"), 
        lty=c(1,4), lwd=c(1,1),
        main = "'Classic' Linear Model - Original (orange) and Fitted series (blue)") 
  # jak model sedi na data
# kontrola residui
checkresiduals(lm2)
  # autokorelovana residua

# do modelu pridame autokorelovana residua
x2Lagged <- cbind(
  xLag0 = x2_series,
  xLag3 = stats::lag(x2_series,-3),
  xLag4 = stats::lag(x2_series,-4))
xy2_series <- ts.union(y2_series, x2Lagged)
  # matice se zavisle promennou a posunutymi nezavisle promennymi

# do prikazu auto.arima muzeme pridat i matici regresoru
arima1 <- auto.arima(xy2_series[,1], xreg = xy2_series[,3:4])
arima1
# jak vypadaji residua tohoto modelu
checkresiduals(arima1)
  # ok

# zakresleni modelu do dat
arima1d <- ts.intersect(na.omit(xy2_series[,1]), arima1$fitted)
plot.ts(arima1d, plot.type = "single", col=c("orange","blue"), 
        lty=c(1,4), lwd=c(1,1),
        main = "ARIMA errors model - Original (orange) and Fitted series (blue)") 

# vyznamnost koeficientu ARIMA modelu
coeftest(arima1)
  # prikaz z knihovny lmtest

########################
### Samostatne

# najdete optimalni model pro radu co2
# hledejte optimalni model pro radu ozonu v datech airquality v zavislosti na ostatnich promennych
