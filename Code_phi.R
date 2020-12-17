source('CheckupRes.R')

library(forecast)
library(tseries)
library(TSA)

#Récupération des données
data = read.csv('BeerProd.csv', sep =';')
attach(data)

Months = data$Month
##en 1995, n'a que les valeurs jusqu'au mois d'aout --> Que faire ?

#Première visualisation des données
plot(BeerProd, type='l')

##Observations : tendance (linéaire ? quadratique ?, rupture ?)
##               périodicité 
##               amplification des périodes ? --> modèle multiplicatif/transformation log ?

acf(BeerProd, lag.max = 40)
pacf(BeerProd, lag.max = 40)
##période de 12

# BeerProd = ts(BeerProd, frequency = 12)
# 
# ##Pour identifier la tendance, la périodicité
# analyse = decompose(BeerProd)
# 
# tendance = analyse$trend
# saisonnalite = analyse$seasonal
# fluctuations = analyse$random
# plot(tendance)      ##tendance plutôt quadratique que linéaire, rupture en tendance ?
# plot(saisonnalite)  #période : pas un nombre entier de période (comment gérer ?)
# plot(fluctuations)  #N'ont pas l'air stationnaires

## 1ère approche : on différencie la série pour obtenir des modèles SARIMA (car périodicité)

BeerProd_diff = diff(BeerProd) # d=1, D=0
BeerProd_diff2 = diff(diff(BeerProd)) # d=2, D=0
BeerProd_diff12 = diff(BeerProd, 12) # d=0, D=1
BeerProd_diff12d = diff(diff(BeerProd, 12)) # d=1, D=1

plot(BeerProd_diff, type='l') #plus de tendance, plus trop de périodicité
acf(as.numeric(BeerProd_diff)) #pics à 12 et 24 --> encore de la périodicité
pacf(as.numeric(BeerProd_diff)) #partie AR importante

adf.test(BeerProd_diff)
kpss.test(BeerProd_diff)
#Cette série semble stationnaire même si à vu d'oeil la variance est trop changeante..
#Douteux, peu aller plus loin pour se défaire de la périodicité

plot(BeerProd_diff2, type='l') #plus de tendance, plus trop de périodicité
acf(as.numeric(BeerProd_diff2)) #pics à 12 --> encore de la périodicité
pacf(as.numeric(BeerProd_diff2)) #pics à 12 et 24 + partie AR importante

adf.test(BeerProd_diff2)
kpss.test(BeerProd_diff2)
#Cette série semble stationnaire même si à vu d'oeil la variance est trop changeante..
#Douteux, peu aller plus loin pour se défaire de la périodicité

plot(BeerProd_diff12, type='l') #plus trop de tendance mais encore un peu quand même, plus trop de périodicité
acf(as.numeric(BeerProd_diff12)) #pics à 12 --> encore de la périodicité (?), partie MA importante
pacf(as.numeric(BeerProd_diff12)) #pics à 12 et 24 + partie AR importante

adf.test(BeerProd_diff12)
kpss.test(BeerProd_diff12)
#Les tests ne sont pas d'accord.

plot(BeerProd_diff12d, type='l') #plus trop de tendance mais encore un peu quand même, plus trop de périodicité
acf(as.numeric(BeerProd_diff12d)) #pics à 12 --> encore de la périodicité (?), partie MA importante
pacf(as.numeric(BeerProd_diff12d)) #partie AR importante

adf.test(BeerProd_diff12d)
kpss.test(BeerProd_diff12d)
#Cette série semble stationnaire.

##On part sur un modèle SARIMA avec d=1, D=1, éventuellement Q=1 et P=0

auto.arima(ts(BeerProd, frequency = 12), d=1, D=1, ic='bic', allowdrift = T)
#SARIMA (0,1,3)x(0,1,2)[12] 

Mod1 = Arima(BeerProd, order = c(4,1,4), seasonal = list(order=c(0,1,1), period =12))
###SARIMA (.,1,.)x(.,1,1)[12] --> pas de modèle qui convient

## 2ème approche : on prend en compte une variable endogène extérieure (SARIMAX)

Tps = time(BeerProd)
n = length(BeerProd)

Tps2 = Tps^2
RegL = lm(BeerProd ~ Tps)
RegQ = lm(BeerProd ~ Tps + Tps2)
summary(RegL)
summary(RegQ)

plot(BeerProd, type='l')
lines(Tps, RegL$fitted.values, lwd = 2, col = 'blue')
lines(Tps, RegQ$fitted.values, lwd = 2, col = 'red')
##colle plutôt très bien

anova(RegL, RegQ)
#modèle quadratique semble meilleur
acf(RegQ$residuals)
pacf(RegQ$residuals)
#les résidus sont périodiques

ResRegQ = ts(RegQ$residuals, frequency = 12)

auto.arima(ResRegQ, allowmean = T, allowdrift = T, ic='bic')
#SARIMA (2,0,2)x(0,1,2)[12]

Mod2 = Arima(BeerProd, order = c(4,0,4), seasonal = list(order=c(0,1,1), period=12), xreg = cbind(Tps,Tps2))


##3ème approche : rupture (en tendance)
##Recherche du point de rupture

MSE = rep(0,(n-1))
X = BeerProd

for (R in 1:(n-1)){
  X1 = X[1:R]
  X2 = X[(R+1):n]
  Tp = 1:R
  Tp2 = (R+1):n
  
  #Revérifier avec le prof
  RL1 = lm(X1 ~ Tp)
  RL2 = lm(X2 ~ Tp2)
  MSE[R] = (sum((RL1$residuals)^2) + sum((RL2$residuals)^2))/n
}

plot(MSE, type = 'l', main ="MSE obtenus", xlab = 'R')
which.min(MSE)
R = 213

#on sépare la série en 2 séries
X1 = X[1:R]
X2 = X[(R+1):n]
Tp1 = 1:R
Tp2 = (R+1):n

RL1 = lm(X1 ~ Tp1)
RL2 = lm(X2 ~ Tp2)

plot(Tp1, X1, type = 'l', col ='red', xlim = c(1,n), ylim = c(min(X),max(X)))
lines((R+1):n, X2, type ='l', col = 'blue')
abline(v=R+0.5, lty = 2)
abline(RL1, col='red')
abline(RL2, col='blue')

X = X2
nT = length(X)

auto.arima(ts(X, frequency = 12),d=1, allowdrift = T, seasonal = T, ic='bic')
#SARIMA (0,0,0)x(1,1,1)[12]

Mod3 = Arima(X, order = c(4,1,3), seasonal = list(order=c(0,1,1), period =12))

## Première visualisation de la qualité des modèles 

#Modèle 1
plot(BeerProd, type='l',lwd = 1)
lines(Mod1$fitted, col = 'red')
checkupRes(Mod1$residuals)
shapiro.test(Mod1$residuals) #on rejette le caractère gaussien des résidus
Box.test(Mod1$residuals, type = "Ljung-Box", lag = 12) # bruit blanc

# lines(Mod1$fitted - 1.96*sqrt(Mod1$sigma2), lty = 2, col = 'blue')
# lines(Mod1$fitted + 1.96*sqrt(Mod1$sigma2), lty = 2, col = 'blue')

##Les grands pics ne sont pas forcément bien modélisés


#Modèle 2
dev.off()
plot(BeerProd, type='l',lwd = 1)
lines(Mod2$fitted, col = 'red')
checkupRes(Mod2$residuals)
shapiro.test(Mod2$residuals) #on rejette le caractère gaussien des résidus
Box.test(Mod2$residuals, type = "Ljung-Box", lag = 12) #bruit blanc
##Mêmes remarques

#Modèle 3
dev.off()
plot(X, type='l',lwd = 1)
lines(Mod3$fitted, col = 'red')
checkupRes(Mod3$residuals)
shapiro.test(Mod3$residuals) # tendancieux
Box.test(Mod3$residuals, type = "Ljung-Box", lag = 12) #bruit blanc
##Mêmes remarques


################ PASSAGE AU LOG ########################################################

LBeerProd = log(BeerProd)
plot(LBeerProd, type='l')

acf(LBeerProd, lag.max = 40)
pacf(LBeerProd, lag.max = 40)
##périodique de période 12, tendance linéaire ou quadratique, rupture ?

LBeerProd_diff = diff(LBeerProd) # d=1, D=0
LBeerProd_diff2 = diff(diff(LBeerProd)) # d=2, D=0
LBeerProd_diff12 = diff(LBeerProd, 12) # d=0, D=1
LBeerProd_diff12d = diff(diff(LBeerProd, 12)) # d=1, D=1

plot(LBeerProd_diff, type='l') #plus de tendance, encore un peu de périodicité
acf(LBeerProd_diff, lag.max = 40) #pics à 12, 24, 36--> encore de la périodicité
pacf(LBeerProd_diff, lag.max = 40) #partie AR importante

adf.test(LBeerProd_diff)
kpss.test(LBeerProd_diff)
#Cette série semble stationnaire 
#par contre on a de la périodicité

plot(LBeerProd_diff2, type='l') #plus de tendance, plus trop de périodicité
acf(LBeerProd_diff2, lag.max = 40) #pics à 12,4,36 --> encore de la périodicité
pacf(LBeerProd_diff2, lag.max = 40) #pics à 12 et 24 + partie AR importante

adf.test(LBeerProd_diff2)
kpss.test(LBeerProd_diff2)
#Cette série semble stationnaire mais toujours de la périodicité

plot(LBeerProd_diff12, type='l') #plus trop de tendance, plus trop de périodicité
acf(LBeerProd_diff12, lag.max = 40) #pics à 12 --> encore de la périodicité (?) ou peut-être différencier en local, partie MA importante
pacf(LBeerProd_diff12, lag.max = 40) #pics à 12, 24 et 36

adf.test(LBeerProd_diff12)
kpss.test(LBeerProd_diff12)
#Les tests ne sont pas d'accord, la série ne semble pas stationnaire

plot(LBeerProd_diff12d, type='l') #plus trop de tendance mais encore un peu quand même, plus trop de périodicité
acf(LBeerProd_diff12d, lag.max = 40) #pics à 12 puis plus rien (Q=1 ?), partie MA importante
pacf(LBeerProd_diff12d, lag.max = 40) #partie AR importante

adf.test(LBeerProd_diff12d)
kpss.test(LBeerProd_diff12d)
#Cette série semble stationnaire, elle a fait disparaitre la périodicité donc :

##On part sur un modèle SARIMA avec d=1, D=1, éventuellement Q=1 et P=0

auto.arima(ts(LBeerProd, frequency = 12), d=1, D=1, ic='bic', allowdrift = T)
#SARIMA (2,1,2)x(1,1,2)[12] 

ModL1 = Arima(LBeerProd, order = c(3,1,4), seasonal = list(order=c(0,1,1), period =12))
###SARIMA (.,1,.)x(.,1,1)[12] --> pas de modèle qui convient

## 2ème approche : on prend en compte une variable endogène extérieure (SARIMAX)

RegLL = lm(LBeerProd ~ Tps)
RegLQ = lm(LBeerProd ~ Tps + Tps2)
summary(RegLL)
summary(RegLQ)

plot(LBeerProd, type='l')
lines(Tps, RegLL$fitted.values, lwd = 2, col = 'blue')
lines(Tps, RegLQ$fitted.values, lwd = 2, col = 'red')
## régression quadratique colle plutôt très bien

anova(RegLL, RegLQ)
#modèle quadratique semble meilleur

acf(RegLQ$residuals, lag.max = 40)
pacf(RegLQ$residuals, lag.max = 40)
#les résidus sont périodiques

ResRegLQ = ts(RegLQ$residuals, frequency = 12)

auto.arima(ResRegLQ, allowmean = T, allowdrift = T, ic='bic')
#SARIMA (0,0,0)(0,1,2)[12] 

ModL2 = Arima(LBeerProd, order = c(4,0,4), seasonal = list(order=c(0,1,1), period=12), xreg = cbind(Tps,Tps2))


##3ème approche : rupture (en tendance)
##Recherche du point de rupture

MSE = rep(0,(n-1))
XL = LBeerProd

for (R in 1:(n-1)){
  X1L = XL[1:R]
  X2L = XL[(R+1):n]
  TpL = 1:R
  TpL2 = (R+1):n
  
  #Revérifier avec le prof
  RLL1 = lm(X1L ~ TpL)
  RLL2 = lm(X2L ~ TpL2)
  MSE[R] = (sum((RLL1$residuals)^2) + sum((RLL2$residuals)^2))/n
}

plot(MSE, type = 'l', main ="MSE obtenus", xlab = 'R')
which.min(MSE)
R = 213

#on sépare la série en 2 séries
X1L = XL[1:R]
X2L = XL[(R+1):n]
TpL1 = 1:R
TpL2 = (R+1):n

RLL1 = lm(X1L ~ TpL1)
RLL2 = lm(X2L ~ TpL2)

plot(TpL1, X1L, type = 'l', col ='red', xlim = c(1,n), ylim = c(min(XL),max(XL)))
lines((R+1):n, X2L, type ='l', col = 'blue')
abline(v=R+0.5, lty = 2)
abline(RLL1, col='red')
abline(RLL2, col='blue')

XL = X2L

auto.arima(ts(XL, frequency = 12), allowdrift = T, seasonal = T, ic='bic')
#SARIMA (0,0,0)x(1,1,1)[12]

ModL3 = Arima(XL, order = c(4,0,4), seasonal = list(order=c(0,1,1), period =12))

## Première visualisation de la qualité des modèles 

#Modèle L1
plot(LBeerProd, type='l',lwd = 1)
lines(ModL1$fitted, col = 'red')
checkupRes(ModL1$residuals)
shapiro.test(ModL1$residuals) #on rejette le caractère gaussien des résidus
Box.test(ModL1$residuals, type = "Ljung-Box", lag = 12) # bruit blanc

# lines(Mod1$fitted - 1.96*sqrt(Mod1$sigma2), lty = 2, col = 'blue')
# lines(Mod1$fitted + 1.96*sqrt(Mod1$sigma2), lty = 2, col = 'blue')

#Modèle L2
dev.off()
plot(LBeerProd, type='l',lwd = 1)
lines(ModL2$fitted, col = 'red')
checkupRes(ModL2$residuals)
shapiro.test(ModL2$residuals) #on rejette le caractère gaussien des résidus
Box.test(ModL2$residuals, type = "Ljung-Box", lag = 12) # bruit blanc


#Modèle L3
dev.off()
plot(XL, type='l',lwd = 1)
lines(ModL3$fitted, col = 'red')
checkupRes(ModL3$residuals)
shapiro.test(ModL3$residuals) # pas gaussien
Box.test(ModL3$residuals, type = "Ljung-Box", lag = 12) # bruit blanc


### Comparaison sur un critère prédictif

BeerT = BeerProd[1:(n-12)]
TpsT = Tps[1:(n-12)]
Tps2T = TpsT^2
NTps = Tps[(n-11):n]
NTps2 = NTps^2
XT = X[1:(nT-12)]

Mod1T = Arima(BeerT, order = c(4,1,4), seasonal = list(order=c(0,1,1), period =12))
Mod2T = Arima(BeerT, order = c(4,0,4), seasonal = list(order=c(0,1,1), period=12), xreg = cbind(TpsT,Tps2T))
Mod3T = Arima(XT, order = c(4,1,3), seasonal = list(order=c(0,1,1), period =12))

pred1T = forecast(Mod1T, h=12)$mean
pred2T = forecast(Mod2T, h=12, xreg = cbind(NTps,NTps2))$mean
pred3T = forecast(Mod3T, h=12)$mean

BeerProd_DP = BeerProd[(n-11):n]
plot(BeerProd_DP, type = 'l')
lines(as.numeric(pred1T), type = 'l', col = 'red')
lines(as.numeric(pred2T), type = 'l', col = 'blue')
lines(as.numeric(pred3T), type = 'l', col = 'green')

LBeerT = LBeerProd[1:(n-12)]
XLT = XL[1:(nT-12)]

ModL1T = Arima(LBeerT, order = c(3,1,4), seasonal = list(order=c(0,1,1), period =12))
ModL2T = Arima(LBeerT, order = c(4,0,4), seasonal = list(order=c(0,1,1), period=12), xreg = cbind(TpsT,Tps2T))
ModL3T = Arima(XLT, order = c(4,0,4), seasonal = list(order=c(0,1,1), period =12))

predL1T = forecast(ModL1T, h=12)$mean
predL2T = forecast(ModL2T, h=12, xreg = cbind(NTps,NTps2))$mean
predL3T = forecast(ModL3T, h=12)$mean

LBeerProd_DP = LBeerProd[(n-11):n]
plot(LBeerProd_DP, type = 'l')
lines(as.numeric(predL1T), type = 'l', col = 'red')
lines(as.numeric(predL2T), type = 'l', col = 'blue')
lines(as.numeric(predL3T), type = 'l', col = 'green')

#Calcul de l'erreur MSE pour chaque série (log et non log)

MSE1 = sum((BeerProd_DP - pred1T)^2)/12 ; MSE1
MSE2 = sum((BeerProd_DP - pred2T)^2)/12 ; MSE2 # meilleur
MSE3 = sum((BeerProd_DP - pred3T)^2)/12 ; MSE3

MSE4 = sum((LBeerProd_DP - predL1T)^2)/12 ; MSE4
MSE5 = sum((LBeerProd_DP - predL2T)^2)/12 ; MSE5
MSE6 = sum((LBeerProd_DP - predL3T)^2)/12 ; MSE6 # meilleur

## --> on garde donc deux modèles : un modèle log qui est le modèle ModL3 et un modèle non log
## qui est le modèle Mod2
## on comparera ces deux modèles sur leur intervalles de prédiction 

###Prédictions et intervalles de confiance

NTs = 477:512
NTs2 = NTps^2
corr = mean(exp(ModL3$residuals))

pred1 = forecast(Mod2, h=36, xreg=cbind(NTs,NTs2))
predL2 = forecast(ModL3, h=36)
pred2 = exp(predL2$mean)*corr

plot(BeerProd, xlim=c(1,512), type='l')
lines(NTs, pred1$mean, type = 'l', col='blue')
lines(NTs, pred2, col='red')
plot(1:36, pred1$lower[,2], type = 'l', col='cyan', ylim =c(100,220))
lines(1:36, pred1$upper[,2], type = 'l', col='cyan')
lines(1:36, exp(predL2$upper[,2]), col='orange')
lines(1:36, exp(predL2$lower[,2]), col='orange')

##Au vu de la taille des intervalles de prédiction, le modèle sans passage au log semble meilleur

##A affiner : 1. Comment gérer la non normalité des résidus quand pas de passage au log ?
##            2. polygone pour affichage des prédictions (jolie)
##            3. Titres aux graphiques et réagencement pour en avoir plusieurs sur un même