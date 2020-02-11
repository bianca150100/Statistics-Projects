data("quakes")
head(quakes)
str(quakes)

#remove.packages("ggplot2") # Unisntall ggplot
install.packages("ggplot2") # Install it again
library(ggplot2)

#Calculam corelatiile intre variabile 
corr <- cor(quakes$stations, quakes$mag)
print(corr)
corr <- cor(quakes$mag, quakes$long)
print(corr)
corr <- cor(quakes$mag, quakes$depth)
print(corr)

par(mfrow = c(3, 1))

attach(quakes)
plot(mag, stations, 
     ylab = "# of Stations Reporting",
     xlab = "Magnitude",
     main = "Fiji Earthquakes Magnitude and Reporting")


plot(jitter(mag, amount = 0.05), stations, 
     pch = 20,
     ylab = "# of Stations Reporting",
     xlab = "Magnitude",
     main = "Fiji Earthquakes Magnitude and Reporting(jitter)",
     col = rgb(0.3, 0.5, 0.4, 0.2))

#construim modelul de regresie liniara simpla intre nr de statii(raspuns) si magnitudine(predicator)
Quake.mod <- lm(stations ~ mag)
Quake.mod

plot(jitter(mag, amount = 0.05), stations, 
     pch = 20,
     ylab = "# of Stations Reporting",
     xlab = "Magnitude",
     main = "Fiji Earthquakes Magnitude & Stations",
     col = rgb(0.3, 0.5, 0.4, 0.2))
#trasam dreapta de regresie liniara
abline(-180.42, 46.28, col="magenta", lwd = 3)
summary(Quake.mod)


#Realizam un model de regresie simpla intre variabile magnitudine(raspuns) si adancime(predicator)


par(mfrow = c(2, 1))
plot(depth, mag, 
     ylab = "Magnitude of Earthquake",
     xlab = " Depth of Earthquake(km)",
     main = "Fiji Earthquakes Magnitude and Depth")


plot(jitter(depth, amount = 0.05), mag, 
     pch = 20,
     ylab = "Magnitude of Earthquake", 
     xlab = "Depth of Earthquake(km)", 
     main = "Fiji Earthquakes Magnitude & Depth",
     col = rgb(0.1, 0.2, 0.8, 0.3))

Quake.mod2 <- lm(mag ~depth)
Quake.mod2
summary(Quake.mod2)
#trasam dreapta de regresie liniara
abline(4.754599, - .000431, col="red", lwd = 2)


#Regresie MULTIPLA

#Construim regresie multipla cu varibilele:
# raspuns: magnitudine
# predicator: adancime + longitudine

modelm1 <- lm(mag ~ long + depth)
modelm1
summary(modelm1)


equation2=function(x){coef(fit1)[2]*x+coef(fit1)[1]+coef(fit1)[3]}

ggplot(quakes,aes(y=mag,x=long,color=depth))+geom_point()+
  stat_function(fun=equation2,geom="line",color=scales::hue_pal()(2)[2])

#Construim un nou model de regresie multipla cu variabilele:
# raspuns: magnitudine
# predicator: adancime + viteza undelor seismice(variabila pe care o vom crea)

#Generam un vector de 1000 valori repr o repartitie normala
v <- rnorm(1000,5.5,0.5)

quakes$waves = v
head(quakes)

#Calculam corelatia intre magnitudine si waves - chiar daca avem o corelatie mica, incercam totusi sa contruim un model de regresie liniara multipla
corr <- cor(quakes$mag, quakes$waves)
print(corr)

#Construim modelul de regresie multipla
model <- lm(quakes$mag ~ quakes$waves+ quakes$depth   , data=quakes)
summary(model)
summary(model)$coefficient
require(ggplot)

#Trasam grafic
equation=function(x){coef(model)[2]*x+coef(model)[1]+coef(model)[3]}

ggplot(quakes,aes(y=quakes$mag,x=quakes$waves,color=quakes$depth))+geom_point()+
  stat_function(fun=equation,geom="line",color=scales::hue_pal()(2)[2])


