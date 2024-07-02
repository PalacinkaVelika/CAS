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

# pomoci funkce arima.sim(n = ,list(ar = ,ma = )) nasimulujte rady typu
#   MA(1) s parametrem theta1 = 0.75
#   MA(1) s parametrem theta1 = -0.75
#   MA(1) s parametrem theta1 = 1.5
#   AR(1) s parametrem phi1 = 0.75
#   AR(1) s parametrem phi1 = - 0.75
#   AR(1) s parametrem phi1 = 1.5
# podivejte se, jak rady vypadaji a jak vypadaji jejich autokoelacni a parcialni autokorelacni funkce

# podivejte se na radu ldeaths
#   vypoctete jeji autokorelacni a parcialni autokorelacni funkci
#   ocistete ji od trendu a seyonnosti a podivejte se na autokorelacni funkci 
#     a parcialni autokorelacni funkci jejich residui
