
library(dplyr)
library(foreign)
library(RCurl)
library(foreign)

#1. Descargar la base y obtener el ancho del intervalo
url <- "https://raw.githubusercontent.com/jcms2665/DemographyCourse/master/Data/tabla.csv"
tabla <- read.csv(textConnection(getURL(url)))
tabla <- mutate(tabla, n = c(diff(edad), 0))
rm("url")  #Quitamos la url


#2. Calcular las tasas específicas de mortalidad entre la edad "x" y "x+n" de la tabla de vida
tabla <- mutate(tabla, nmx = D/N)


#3. Obtener el número promedio de años persona vividos en el intervalo por aquellos que murieron
#los vamos a tomar prestados de Keyfitz y Flieger (1971)
kfnax <- read.dta("http://data.princeton.edu/eco572/datasets/kfnax.dta")
names(kfnax)[1]<-"edad"


#4. Unimos los años persona vividos a la tabla que teníamos usando la edad como pivote  
tabla <- inner_join(tabla, kfnax, by="edad")

#5. Checamos los años persona vividos para las edades de 0-1 y 1-4

cond <- rep(tabla[1,"nmx"] >= 0.107, 2) # Usamos una expresión como ayuda
tabla[1:2,"nax"] <- ifelse(cond, c(0.330, 1.352), tabla[1:2,"nax"] <- c(0.045, 1.651) + c(2.684, -2.816)* tabla[1,"nmx"])

last <- nrow(tabla) 
tabla[last,"nax"] <- 1/ tabla[last,"nmx"]

#6. Calculamos las qx y px

tabla <- mutate(tabla, q = n * nmx/(1 + (n - nax) * nmx), p = 1 - q)

#7. Bajo el supuesto de tener una tabla cerrada, consideramos que todos mueren q=1 y nadie sobrevive p=0
tabla[last, c("q","p")] = c(1, 0)

#8.Elegimos un radix lx

tabla <- mutate(tabla, lx = 100000 * cumprod( c(1, p[-last])))

#9. Calcular el número de personas que murieron entre las edades x y x+n
tabla <- mutate(tabla, d = c(-diff(lx), lx[last]))

#10. Calcular los sobrevivientes y Años Persona Vividos (T) arriba de la edad x
tabla <- mutate(tabla, L =  (lx - d) * n +  d * nax,T = sum(L) - cumsum(L) + L)

#11. Esperanza de vida
tabla <- mutate(tabla, e = T/lx)


select(tabla, edad, N, D, nmx, nax, q, p,lx, d, L, T, e)
