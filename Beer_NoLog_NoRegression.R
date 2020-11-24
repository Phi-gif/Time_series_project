library(graphics)
library(ggplot2)
library(dplyr)
library(moments)
library(forecast)
library(tseries)

elvi_path= '~/Documents/M2-Angers/temporel/BeerProd.csv'

beer<-read.csv(file = elvi_path,sep = ';')
class(beer) # sous format data.frame
# Transformation en time series
beer_ts<-ts(data=beer,start=c(1956,01),frequency = 12)
class(beer_ts)
start(beer_ts) # on commence bien à 1956-01

#### Plot ####
plot(beer_ts,type='l')

# Décomposition 
beer_ts1 <- ts(beer$BeerProd, start = 1956, end = 1995, freq = 12) 
beer_stl <- stl(beer_ts1, s.window = "period")
plot(beer_stl) 
# Nous remarquons qu'il y a saisonnalité, et une tendance quadratique

# Modèle additif: pas de transformation en log

# La série n'est pas stationnaire
beer_diff<-as.vector(diff(beer_ts)) #d=1 et D=0
beer_diff_12<-as.vector(diff(beer_ts,12)) # d=0 et D=1 
beer_diff_6<-as.vector(diff(beer_ts,6)) # d=0 et D=1/2
beer_dd12<-as.vector(diff(beer_diff_12,12)) # d=0 et D=2 # Nous dérivons à nouveau beer_d12
beer_diff_d12<-as.vector(diff(diff(beer_ts,12))) # d=1, et D=1
beer_diff_dd12<-as.vector(diff(beer_diff_d12,12)) # d=1, et D=2 
beer_diff_diff_dd12<-as.vector(diff(beer_diff_dd12)) # d=2, et D=2 


# Vérification de stationnarité: 
# Pour beer_diff: 
# Avec tests adf et kpss
adf.test(beer_diff) # ADF rejète hypothèse de non stationnarité  (H1) car p=0.01 < 5%
kpss.test(beer_diff) # KPSS ne rejète pas l'hypothèse de stationnarité (H1) car p=0.1 > 5%
# Modèle stationnaire selon les tests
# Visuel
ts.plot(diff(beer_ts),type='l')
ACF_diff<-acf(beer_diff) # Nous voyons des pics à 2, 6, 12, 18, 24,..
PACF_diff<-pacf(beer_diff) # Il y a plusieurs pics
# Nous cherchons à éliminer la saisonnalité


# Pour beer_diff_12: 
# Avec tests adf et kpss
adf.test(beer_diff_12) # ADF rejète hypothèse de non stationnarité  (H1) car p=0.01 < 5%
kpss.test(beer_diff_12) # KPSS rejète hypothèse de stationnarité (H1) car p=0.01 < 5%
# ADF et KPSS se contredisent. Pas concluant
# Visuel
ts.plot(diff(beer_ts,12),type='l')
ACF_diff_12<-acf(beer_diff_12) # plusieurs pics significatifs
PACF_diff_12<-pacf(beer_diff_12) # plusieurs pics significatifs


# Pour beer_diff_6: 
# Avec tests adf et kpss
adf.test(beer_diff_6) # ADF rejète hypothèse de non stationnarité  (H1) car p=0.01 < 5%
kpss.test(beer_diff_6) # KPSS rejète hypothèse de stationnarité (H1) car p=0.01 < 5%
# ADF et KPSS se contredisent. Pas concluant
# Visuel
ACF_diff_6<-acf(beer_diff_6) 
PACF_diff_6<-pacf(beer_diff_6) 
# Nous allons nous fier sur l'ACF et PACF - qui montre présence saisonnalité


# Pour beer_diff_d12: 
# Avec tests adf et kpss
adf.test(beer_diff_d12) # ADF rejète hypothèse de non stationnarité  (H1) car p=0.01 < 5%
kpss.test(beer_diff_d12) # KPSS ne rejète pas l'hypothèse de stationnarité (H1) car p=0.1 > 5%
# Modèle stationnaire selon les tests

# Visuel
ACF_diff_d12<-acf(beer_diff_d12) # Nous voyons toujours des pics à 12, 24, ...
PACF_diff_d12<-pacf(beer_diff_d12)



# Pour beer_dd12: Je ne teste pas, car il nous faut au moins d=1



# Pour beer_diff_dd12: 
# Avec tests adf et kpss
adf.test(beer_diff_dd12)# p=0.01, ADF rejète l'hypothese de non stationnarité (H1) car p < 5%
kpss.test(beer_diff_dd12)# p=0.1, KPSS ne rejète pas hypothèse de stationnarité (H1) car p > 5%
# Modèle stationnaire selon les tests
# Visuel
ACF_diff_dd12<-acf(beer_diff_dd12)
PACF_diff_dd12<-pacf(beer_diff_dd12)
# Stationnaire selon les tests
# Mais pics significatifs périodiques à 11-23.


# Pour beer_diff_diff_dd12: 
# Avec tests adf et kpss
adf.test(beer_diff_diff_dd12)# p=0.01, ADF rejète l'hypothese de non stationnarité (H1) car p < 5%
kpss.test(beer_diff_diff_dd12)# p=0.1, KPSS ne rejète pas hypothèse de stationnarité (H1) car p > 5%
# Modèle stationnaire selon les tests
# Visuel
ACF_diff_diff_dd12<-acf(beer_diff_diff_dd12)
PACF_diff_diff_dd12<-pacf(beer_diff_diff_dd12)
# Stationnaire selon les tests
# Mais pics significatifs périodiques à 11-23.


