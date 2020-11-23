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
# Nous remarquons qu'il y a saisonnalité. 

# Modèle multiplicatif?

# Si on fait une transformation en log
l_beer<-log(beer_ts)
plot(l_beer)

# La série n'est pas stationnaire
l_beer_diff<-as.vector(diff(l_beer)) #d=1 et D=0
l_beer_diff_12<-as.vector(diff(l_beer,12)) # d=0 et D=1 ###### C'EST PEUT ETRE LE BON! AVEC AR(3)
l_beer_diff_6<-as.vector(diff(l_beer,6)) # d=0 et D=1/2
l_beer_dd12<-as.vector(diff(l_beer_diff_12,12)) # d=0 et D=2 # Nous dérivons à nouveau l_beer_d12
l_beer_diff_d12<-as.vector(diff(diff(l_beer,12))) # d=1, et D=1
l_beer_diff_dd12<-as.vector(diff(l_beer_diff_d12,12)) # d=1, et D=2 ###### C'EST PEUT ETRE LE BON!
# l_beer_diff_diff_dd12<-as.vector(diff(l_beer_diff_dd12)) # d=2, et D=2


# Vérification de stationnarité: 
  # Pour l_beer_diff: 
  # Avec tests adf et kpss
adf.test(l_beer_diff) # ADF rejète hypothèse de non stationnarité  (H1) car p=0.01 < 5%
kpss.test(l_beer_diff) # KPSS rejète hypothèse de stationnarité (H1) car p=0.01 < 5%
    # Visuel
ACF_l_diff<-acf(l_beer_diff) # Nous voyons des pics à 6, 12, 18, 24,..
PACF_l_diff<-pacf(l_beer_diff) # Nous voyons des pics à 6, 12, 18, 24...

# ADF et KPSS se contredisent. Pas concluant.
# Nous allons nous fier sur l'ACF et PACF - qui montre présence saisonnalité


  # Pour l_beer_diff_12: # POSSIBLE!! AR(3)
  # Avec tests adf et kpss
adf.test(l_beer_diff_12) # ADF rejète hypothèse de non stationnarité  (H1) car p=0.01 < 5%
kpss.test(l_beer_diff_12) # KPSS rejète hypothèse de stationnarité (H1) car p=0.01 < 5%
    # Visuel
ACF_l_diff_12<-acf(l_beer_diff_12) # 
PACF_l_diff_12<-pacf(l_beer_diff_12) 
# ADF et KPSS se contredisent. Pas concluant
# Nous allons nous fier sur l'ACF et PACF - qui montre présence saisonnalité


# Pour l_beer_diff_6: 
# Avec tests adf et kpss
adf.test(l_beer_diff_6) # ADF rejète hypothèse de non stationnarité  (H1) car p=0.01 < 5%
kpss.test(l_beer_diff_6) # KPSS rejète hypothèse de stationnarité (H1) car p=0.01 < 5%
# Visuel
ACF_l_diff_6<-acf(l_beer_diff_6) # 
PACF_l_diff_6<-pacf(l_beer_diff_6) 
# ADF et KPSS se contredisent. Pas concluant
# Nous allons nous fier sur l'ACF et PACF - qui montre présence saisonnalité





  # Pour l_beer_dd12: 
  # Avec tests adf et kpss
adf.test(l_beer_dd12) # ADF rejète hypothèse de non stationnarité  (H1) car p=0.01 < 5%
kpss.test(l_beer_dd12) # KPSS rejète hypothèse de stationnarité (H1) car p=0.01 < 5%
# ADF et KPSS se contredisent. Pas concluant.
# Nous allons nous fier sur l'ACF et PACF - qui montre présence saisonnalité

    # Visuel
ACF_l_dd12<-acf(l_beer_dd12) # 
PACF_l_dd12<-pacf(l_beer_dd12) # pic à 12


  # Pour l_beer_diff_d12: # Pas stationnaire
  # Avec tests adf et kpss
adf.test(l_beer_diff_d12)# p=0.01, ADF rejète l'hypothese de non stationnarité (H1) car p < 5%
kpss.test(l_beer_diff_d12)# p=0.02096, KPSS rejète hypothèse de stationnarité (H1) car p < 5%
# ADF et KPSS se contredisent. Nous allons nous fier sur l'ACF et PACF - qui montre présence saisonnalité

    # Visuel
ACF_l_diff_d12<-acf(l_beer_diff_d12)
PACF_l_diff_d12<-pacf(l_beer_diff_d12)


  # Pour l_beer_diff_dd12: # Pas stationnaire
  # Avec tests adf et kpss
adf.test(l_beer_diff_dd12)# p=0.01, ADF rejète l'hypothese de non stationnarité (H1) car p < 5%
kpss.test(l_beer_diff_dd12)# p=0.1, KPSS ne rejète pas hypothèse de stationnarité (H1) car p > 5%
  # Visuel
ACF_l_diff_dd12<-acf(l_beer_diff_dd12)
PACF_l_diff_dd12<-pacf(l_beer_diff_dd12)
# Stationnaire selon les tests
# Mais pics significatifs périodiques selon les corrélogrammes.


