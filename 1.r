data("quakes")
head(quakes)
str(quakes)


par(mfrow = c(3, 2))
plot(quakes$lat,main="Latitudine")
plot(quakes$long, main="Longitudine")
plot(quakes$depth, main="Adancime")
plot(quakes$mag, main="Magnitudine")
plot(quakes$stations, main="Statii")

#Gasim media pentru fiecare variabila
result.mean.lat <- mean(quakes$lat)
result.mean.long <- mean(quakes$long)
result.mean.depth <- mean(quakes$depth)
result.mean.mag <- mean(quakes$mag)
print(result.mean.lat)

#Gasim mediana fiecarei variabile pentru a putea analiza
result.median.lat <- median(quakes$lat)
result.median.long <- median(quakes$long)
result.median.depth <- median(quakes$depth)
result.median.mag <- median(quakes$mag)
print(result.median.lat)

summary(quakes)

#Gasim varianta fiecarei variabile
result.variance.lat <- var(quakes$lat)
result.variance.long <- var(quakes$long)
result.variance.depth <- var(quakes$depth)
result.variance.mag <- var(quakes$mag)
result.variance.stations <- var(quakes$stations)

#Gasim cuantilele fiecarei variabile
result.quantile.lat <- quantile(quakes$lat)
result.quantile.long <- quantile(quakes$long)
result.quantile.depth <- quantile(quakes$depth)
result.quantile.mag <- quantile(quakes$mag)
result.quantile.stations <- quantile(quakes$stations)

#Boxplots si valori aberante
par(mfrow = c(3, 2))
boxplot(quakes$lat, main="Latitudinea")
boxplot(quakes$long, main="Longitudinea")
boxplot(quakes$depth, main="Adancimea")
boxplot(quakes$mag, main="Magnitudea")
boxplot(quakes$stations, main="Raportul statiilor")


require(graphics)
pairs(quakes, main = "Fiji Earthquakes, N = 1000", cex.main = 1.2, pch = ".")