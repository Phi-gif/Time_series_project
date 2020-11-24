library(graphics)
library(ggplot2)
library(dplyr)
library(moments)
library(forecast)
library(tseries)

elvi_path= '~/Documents/M2-Angers/temporel/BeerProd.csv'

beer<-read.csv(file = elvi_path,sep = ';')
class(beer) # sous format data.frame
tps<-time(beer$Month)
tps


beer$Month<-as.Date(beer$Month)
# Transformation en time series
#beer_ts<-ts(data=beer,start=c(1956,01),frequency = 12)
#class(beer_ts)
#start(beer_ts) # on commence bien à 1956-01

#beer_ts<-as.data.frame(beer_ts)


# Plot
#plot(beer_ts,type='l')

# Décomposition 
beer_ts1 <- ts(beer$BeerProd, start = 1956, end = 1995, freq = 12) 
plot.ts(beer_ts1)
beer_stl <- stl(beer_ts1, s.window = "period")
plot(beer_stl)

# Nous avons une tendance quadratique, de la saisonnalité à chaque 12 mois. Le modèle est également possiblement multiplicatif

# Nous faisons le log afin de rendre le modèle additif:
l_beer<-log(beer_ts1)
plot(l_beer)

# Nous éliminons la saisonnalité:
l_beer_d12<-diff(l_beer,12) # d=0 et D=1
plot(l_beer_d12)

l_beer_d_d12<-diff(l_beer_d12,12) # d=0 et D=2
plot(l_beer_d_d12)
tps_1<-time(l_beer_d_d12)

# Nous différencions pour éliminier la tendance:
d_l_beer_d12<-diff(l_beer_d12) # d=1 et D=1
plot(d_l_beer_d12)
tps_d1<-time(d_l_beer_d12)

d_l_beer_d_d12<-diff(l_beer_d_d12) # d=1 et D=2
plot(d_l_beer_d_d12)
tps_d2<-time(d_l_beer_d_d12)

# Nous faisons une régression linéaire:
### 1) Nous commencons par tester l_beer_d12, D=1
reglm<-lm(l_beer_d12 ~ tps)
summary(reglm) # R=0.5344 bien

tps2<-tps^2
reglm2<-lm(l_beer_d12 ~ tps + tps2)
summary(reglm2) # R2 = 0.7176 bien

anova(reglm,reglm2) # par les résultats du test, tps2 a une influence significative  (p valeur très petit)
# reglm2 est meilleure

# ACF/PACF sur les résidues de reglm
acf(reglm$residuals) # Nous voyons la présence de saisonnalité
pacf(reglm$residuals)

# ACF/PACF sur les résidues de reglm2
acf(reglm2$residuals) # Nous voyons la présence de saisonnalité
pacf(reglm2$residuals)


#### 2) Nous testons l_beer_d_d12, D=2
reglm_1<-lm(l_beer_d_d12 ~ tps_1)
summary(reglm_1) # R2= 0.0001252 très mauvais

tps2_1<-tps_1^2
reglm2_1<-lm(l_beer_d_d12 ~ tps_1 + tps2_1)
summary(reglm2_1) # R2 = 0.0002571 très mauvais

anova(reglm_1,reglm2_1) # par les résultats du test, tps2_1 n'a d'influence significative 
#  reglm_1 est meilleure 

#### POSSIBLEMENT LE MEILLEUR RESULTAT - ARTEFACT À 7, 13 ???
# ACF/PACF sur les résidues de reglm
acf(reglm_1$residuals) # Nous voyons la présence de saisonnalité
pacf(reglm_1$residuals)
adf.test(reglm_1$residuals) # ADF rejète hypothèse de non stationnarité  (H1) car p=0.01 < 5%
kpss.test(reglm_1$residuals) # p=0.1, KPSS ne rejète pas hypothèse de stationnarité (H1)

# ACF/PACF sur les résidues de reglm2
acf(reglm2_1$residuals) # Nous voyons la présence de saisonnalité
pacf(reglm2_1$residuals)
adf.test(reglm2_1$residuals) # ADF rejète hypothèse de non stationnarité  (H1) car p=0.01 < 5%
kpss.test(reglm2_1$residuals) # p=0.1, KPSS ne rejète pas hypothèse de stationnarité (H1)



### 3) Nous testons d_l_beer_d12 # d=1,D=1

reglm_d1<-lm(d_l_beer_d12 ~ tps_d1)
summary(reglm_d1) 

tps2_d1<-tps_d1^2
reglm2_d1<-lm(d_l_beer_d12 ~ tps_d1 + tps2_d1)
summary(reglm2_d1) 

anova(reglm_d1,reglm2_d1) # par les résultats du test, tps_d1 a une influence significative  (p valeur très petit)
# reglm_d1 est meilleure

# ACF/PACF sur les résidues de reglm
acf(reglm_d1$residuals) # Nous voyons la présence de saisonnalité
pacf(reglm_d1$residuals)

# ACF/PACF sur les résidues de reglm2
acf(reglm2_d1$residuals) # Nous voyons la présence de saisonnalité
pacf(reglm2_d1$residuals)


### 4) Nous testons d_l_beer_d_d12 # d=1,D=2
reglm_d2<-lm(d_l_beer_d_d12 ~ tps_d2)
summary(reglm_d2) 

tps2_d2<-tps_d2^2
reglm2_d2<-lm(d_l_beer_d_d12 ~ tps_d2 + tps2_d2)
summary(reglm2_d2) 

anova(reglm_d2,reglm2_d2) # par les résultats du test, tps_d2 a une influence significative  (p valeur très petit)
# reglm_d1 est meilleure

# ACF/PACF sur les résidues de reglm
acf(reglm_d2$residuals) # Nous voyons la présence de saisonnalité
pacf(reglm_d2$residuals)

# ACF/PACF sur les résidues de reglm2
acf(reglm2_d2$residuals) # Nous voyons la présence de saisonnalité
pacf(reglm2_d2$residuals)



