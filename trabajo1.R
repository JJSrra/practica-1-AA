## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(knitr)

## ----Funciones aportadas-------------------------------------------------
## ------------------------------------------------------------------------
# por defecto genera 2 puntos entre [0,1] de 2 dimensiones

simula_unif = function (N=2,dims=2, rango = c(0,1)){
 m = matrix(runif(N*dims, min=rango[1], max=rango[2]),
 nrow = N, ncol=dims, byrow=T)
 m
}

## -----------------------------------------------------------------------

# función simula_gaus(N, dim, sigma) que genera un
# conjunto de longitud N de vectores de dimensión dim, conteniendo números
# aleatorios gaussianos de media 0 y varianzas dadas por el vector sigma.
# por defecto genera 2 puntos de 2 dimensiones

simula_gaus = function(N=2,dim=2,sigma){

  if (missing(sigma)) stop("Debe dar un vector de varianzas")
  sigma = sqrt(sigma)  # para la generación se usa sd, y no la varianza
  if(dim != length(sigma)) stop ("El numero de varianzas es distinto de la dimensión")

  simula_gauss1 = function() rnorm(dim, sd = sigma) # genera 1 muestra, con las desviaciones especificadas
  m = t(replicate(N,simula_gauss1())) # repite N veces, simula_gauss1 y se hace la traspuesta
  m
}

## ------------------------------------------------------------------------
#  simula_recta(intervalo) una funcion que calcula los parámetros
#  de una recta aleatoria, y = ax + b, que corte al cuadrado [-50,50]x[-50,50]
#  (Para calcular la recta se simulan las coordenadas de 2 ptos dentro del
#  cuadrado y se calcula la recta que pasa por ellos),
#  se pinta o no segun el valor de parametro visible

simula_recta = function (intervalo = c(-1,1), visible=F){

  ptos = simula_unif(2,2,intervalo) # se generan 2 puntos
   a = (ptos[1,2] - ptos[2,2]) / (ptos[1,1]-ptos[2,1]) # calculo de la pendiente
   b = ptos[1,2]-a*ptos[1,1]  # calculo del punto de corte

   if (visible) {  # pinta la recta y los 2 puntos
       if (dev.cur()==1) # no esta abierto el dispositivo lo abre con plot
           plot(1, type="n", xlim=intervalo, ylim=intervalo)
       points(ptos,col=3)  #pinta en verde los puntos
       abline(b,a,col=3)   # y la recta
   }
   c(a,b) # devuelve el par pendiente y punto de corte
}

# Para el apartado 3 del Ejercicio 1
#-------------------------------------------------------------------------------
## funcion para pintar la frontera de la función
# a la que se pueden añadir puntos, y etiquetas

pintar_frontera = function(f,rango=c(-50,50)) {
   x=y=seq(rango[1],rango[2],length.out = 400)
   z = outer(x,y,FUN=f)
  if (dev.cur()==1) # no esta abierto el dispositivo lo abre con plot
          plot(1, type="n", xlim=rango, ylim=rango)
   contour(x,y,z, levels = 0, drawlabels = FALSE,xlim =rango, ylim=rango, xlab = "x", ylab = "y")
}

## ----Ejercicio 1.1a------------------------------------------------------
set.seed(100)
datos50 = simula_unif(N=50, dim=2, rango=c(-50,50))
plot(datos50)

## ----Ejercicio 1.1b------------------------------------------------------
set.seed(100)
datosgauss = simula_gaus(N=50, dim=2, sigma=c(5,7))
plot(datosgauss)

## ----Ejercicio 1.2a------------------------------------------------------
# Asigna etiquetas a unos puntos en relación a si están por encima o por debajo de una recta,
# es decir, si la y para ese punto es mayor o menor que la y de la recta en la x del punto.
asignarEtiquetasSegunRecta = function(recta,puntos){
	etiquetas = sign(puntos[,2] - (recta[1]*puntos[,1]+recta[2]))
}

set.seed(100)
datos2 = simula_unif(50, 2, c(-50,50))
recta = simula_recta(c(-50,50))

etiquetas2 = asignarEtiquetasSegunRecta(recta, datos2)
plot(datos2, col=etiquetas2+3)
abline(recta[2], recta[1])


## ----Ejercicio 1.2b------------------------------------------------------
# Función que asigna un ruido del porcentaje dado a las etiquetas pasadas como argumento, de forma
# que algunas queden desclasificadas con respecto a la distribución que seguían inicialmente.
# Primero obtiene las posiciones de valores positivos y negativos. Sobre ellas, obtiene el porcentaje
# de valores de cada tipo a modificar y los extrae con un sample. Por último, esos valores positivos
# se cambian a -1 y viceversa.
asignarRuido = function(etiquetas, porcentaje=10){
  positivos = which(etiquetas==1)
  negativos = which(etiquetas==-1)
  cambiarPositivos = sample(positivos, round(length(positivos)*0.01*porcentaje))
  cambiarNegativos = sample(negativos, round(length(negativos)*0.01*porcentaje))
  etiquetas[cambiarPositivos]=-1
  etiquetas[cambiarNegativos]=1
  etiquetas
}

etiquetasRuido = asignarRuido(etiquetas2, 10)
par(mfrow=c(1,2))
plot(datos2, col=etiquetas2+3)
abline(recta[2], recta[1])
plot(datos2, col=etiquetasRuido+3)
abline(recta[2], recta[1])

## ----Ejercicio 1.3 1-----------------------------------------------------
f1 = function(x,y){etiquetas = sign((x-10)^2 + (y-20)^2 - 400)}
f2 = function(x,y){etiquetas = sign(0.5*(x+10)^2 + (y-20)^2 - 400)}
f3 = function(x,y){etiquetas = sign(0.5*(x-10)^2 - (y+20)^2 - 400)}
f4 = function(x,y){etiquetas = sign(y - 20*x^2 - 5*x + 3)}

etiquetasf1 = f1(datos2[,1], datos2[,2])
etiquetasf2 = f2(datos2[,1], datos2[,2])
etiquetasf3 = f3(datos2[,1], datos2[,2])
etiquetasf4 = f4(datos2[,1], datos2[,2])

## ----Ejercicio 1.3 2-----------------------------------------------------
par(mfrow=c(1,2))
pintar_frontera(f1)
points(datos2[,1], datos2[,2], col=etiquetasf1+3)
pintar_frontera(f2)
points(datos2[,1], datos2[,2], col=etiquetasf2+3)
pintar_frontera(f3)
points(datos2[,1], datos2[,2], col=etiquetasf3+3)
pintar_frontera(f4)
points(datos2[,1], datos2[,2], col=etiquetasf4+3)

## ----Ejercicio 1.3 3-----------------------------------------------------
par(mfrow=c(1,2))
pintar_frontera(f1)
points(datos2[,1], datos2[,2], col=etiquetasRuido+3)
plot(datos2, col=etiquetasRuido+3)
abline(recta[2], recta[1])
pintar_frontera(f2)
points(datos2[,1], datos2[,2], col=etiquetasRuido+3)
plot(datos2, col=etiquetasRuido+3)
abline(recta[2], recta[1])
pintar_frontera(f3)
points(datos2[,1], datos2[,2], col=etiquetasRuido+3)
plot(datos2, col=etiquetasRuido+3)
abline(recta[2], recta[1])
pintar_frontera(f4)
points(datos2[,1], datos2[,2], col=etiquetasRuido+3)
plot(datos2, col=etiquetasRuido+3)
abline(recta[2], recta[1])

## ----Ejercicio 2.1-------------------------------------------------------
# Función que ajusta el algoritmo Perceptron para unos datos y unas etiquetas pasadas
# como argumento, hasta un máximo de iteraciones también dado, y con un vector de pesos
# inicial como último argumento.
# Su funcionamiento es el siguiente:
# En primer lugar, sabiendo que tendremos que hacer un producto vectorial entre el vector
# de pesos y el vector de características, como el vector de pesos tiene 3 valores
# (un peso para cada característica, y el umbral), hay que añadir una columna de 1 a los
# datos para que pueda hacerse este producto. El algoritmo itera hasta que no se haya
# llegado al máximo de iteraciones, o no se haya cambiado el vector de pesos en una
# iteración. Cada iteración comprueba los vectores de características en un orden diferente,
# esto lo he conseguido con un sample de su longitud. Cambia los pesos si alguna etiqueta
# está situada incorrectamente, según los cálculos del conocido algoritmo.
# En mi implementación del algoritmo, si encuentra un punto en mala posición no sale del
# bucle for en el que está iterando, llega hasta el final evaluando los valores que le quedan.
# Por último, devuelve una lista con el vector de pesos final y el número de iteraciones
# que ha necesitado para converger.
ajusta_PLA = function(datos, label, max_iter, vini){
  w = vini
  cambio = T
  datos = cbind(datos,1)
  iteraciones = 0

  while(iteraciones < max_iter & cambio){
    cambio = F
    for(j in sample(1:dim(datos)[1])){
      if (sign(crossprod(datos[j,],w)) != label[j]){
        w = w + datos[j,]*label[j]
        cambio = T
      }
    }
    
    iteraciones = iteraciones+1
  }
  
  list(w=w, iter=iteraciones)
}

# Función que obtiene la pendiente y el punto de corte del hiperplano de un vector
# de pesos. Esta ecuación se puede despejar de w1x1 + w2x2 + umbral = 0,
# siendo xn la característica n de nuestro vector de características, y siendo
# w1, w2 y umbral las componentes 1, 2 y 3 del vector de pesos, respectivamente.
obtenerRectaPesos = function(pesos){
  c(-pesos[1]/pesos[2], -pesos[3]/pesos[2])
}


## ----Ejercicio 2.2a------------------------------------------------------
# Función que hace lo pedido en el ejercicio 2.2 a), a fin de poder llamarla con replicate
ejercicio22a = function(datos, etiquetas, max_iter){
  modelo = ajusta_PLA(datos, etiquetas, max_iter, c(0,0,0))
  modelo$iter
}

set.seed(100)
resultado = replicate(10, ejercicio22a(datos2, etiquetas2, 200))
mean(resultado)

## ----Ejercicio 2.2 b-----------------------------------------------------
# Función que hace lo pedido en el ejercicio 2.2 a), a fin de poder llamarla con replicate.
# Genera los valores del vector inicial con la función runif entre 0 y 1.
ejercicio22b = function(datos, etiquetas, max_iter){
  vectorIni = runif(3, 0, 1)
  modelo = ajusta_PLA(datos, etiquetas, max_iter, vectorIni)
  modelo$iter
}

set.seed(100)
resultado = replicate(10, ejercicio22b(datos2, etiquetas2, 200))
mean(resultado)

## ----Ejercicio 2.3-------------------------------------------------------
resultado2a = replicate(10, ejercicio22a(datos2, etiquetasRuido, 200))
mean(resultado2a)

resultado2b = replicate(10, ejercicio22b(datos2, etiquetasRuido, 200))
mean(resultado2b)

## ----Ejercicio 3---------------------------------------------------------
# ------------------------------------------------------------------------
digit.train <- read.table("datos/zip.train",
                          quote="\"", comment.char="", stringsAsFactors=FALSE)

digitos15.train = digit.train[digit.train$V1==1 | digit.train$V1==5,]
digitos = digitos15.train[,1]  # etiquetas
ndigitos = nrow(digitos15.train)

# se retira la clase y se monta una matriz 3D: 599*16*16
grises = array(unlist(subset(digitos15.train,select=-V1)),c(ndigitos,16,16))
rm(digit.train) 
rm(digitos15.train)

# Para visualizar los 4 primeros
## ------------------------------------------------------------------------

par(mfrow=c(2,2)) 
for(i in 1:4){
  imagen = grises[i,,16:1] # se rota para verlo bien
  image(z=imagen)
}

digitos[1:4] # etiquetas correspondientes a las 4 imágenes

## ----Ejercicio 3.2-------------------------------------------------------
# Función simetría aportada por la práctica, pero cambiando sum por mean para que calcule los valores medios.
fsimetria <- function(A){
  A = abs(A-A[,ncol(A):1])
  -mean(A)
}

simetria = apply(grises, 1, fsimetria)
intensidadPromedio = apply(grises, 1, mean)
plot(x=intensidadPromedio, y=simetria, col=digitos+8)

## ----Ejercicio 3.3 Leer test---------------------------------------------
digit.test <- read.table("datos/zip.test",
                          quote="\"", comment.char="", stringsAsFactors=FALSE)

digitos15.test = digit.test[digit.test$V1==1 | digit.test$V1==5,]
digitos.test = digitos15.test[,1]  # etiquetas
ndigitos.test = nrow(digitos15.test)

# se retira la clase y se monta una matriz 3D: 599*16*16
grises.test = array(unlist(subset(digitos15.test,select=-V1)),c(ndigitos.test,16,16))
rm(digit.test) 
rm(digitos15.test)

simetria.test = apply(grises.test, 1, fsimetria)
intensidadPromedio.test = apply(grises.test, 1, mean)
plot(x=intensidadPromedio.test, y=simetria.test, col=digitos.test+8)

## ----Ejercicio 3.3-------------------------------------------------------
# Función que calcula los pesos por regresión lineal de unos datos y unas etiquetas
# aportadas. Se hace uso de la fórmula para calcular la matriz pseudoinversa de
# los datos que aparece en las diapositivas de teoría. Según esto, la matriz
# pseudoinversa de X se puede obtener como la multiplicación de la inversa de
# la multiplicación de X traspuesta por X, por la traspuesta de X. En fórmula
# matemática, pseudoX = (XT*X)-1 * XT.
# El resto de operaciones son las mismas que aparecen en las transparencias,
# empleando la función svd para extraer las matrices U, D y V de X, y sabiendo
# que (XT*X)-1 = V * (pseudoD)^2 * VT (en las transparencias no aparece el cuadrado
# por error, pero lo corregimos en clase) y que la pseudoD es la diagonal de 1/D.
# Finalmente, los pesos se obtienen de multiplicar la pseudoinversa de X por las
# etiquetas.
Regress_Lin = function(datos,label){
  
  datos=cbind(datos,1)

  descomposicion = svd(datos)
  pseudoinversaD = diag(1/descomposicion$d)
  inversadatosTdatos = (descomposicion$v)%*%(pseudoinversaD^2) %*%t(descomposicion$v)
  pseudoinversa = inversadatosTdatos%*%(t(datos))
  
  pesos = (pseudoinversa%*%label)
}

datos3 = matrix(c(intensidadPromedio,simetria),nrow=length(intensidadPromedio),ncol=2)
digitos[digitos==5]=-1

pesos3 = Regress_Lin(datos3,digitos)
plot(datos3, col=digitos+3)
coeficientespesos3 = obtenerRectaPesos(pesos3)
abline(coeficientespesos3[2], coeficientespesos3[1])

## ----Ejercicio 3.3 errores-----------------------------------------------
# Función que calcula el error de unas etiquetas de unos datos para un vector de pesos
# en base a cuántas etiquetas están mal situadas con respecto a la recta que formaría
# dicho vector.
errores = function(datos,label,pesos){
  datos = cbind(datos,1)
  (sum(sign(datos%*%pesos) != label))/length(label)
}

datos3.test = matrix(c(intensidadPromedio.test,simetria.test),nrow=length(intensidadPromedio.test),ncol=2)
digitos.test[digitos.test==5]=-1

plot(datos3.test, col=digitos.test+3)
abline(coeficientespesos3[2], coeficientespesos3[1])

Ein = errores(datos3, digitos, pesos3)
Eout = errores(datos3.test, digitos.test, pesos3)

print(Ein)
print(Eout)

## ----Ejercicio 3 Experimento1a-------------------------------------------
set.seed(100)
datosExp1 = simula_unif(N=1000, dim=2, rango=c(-1,1))
plot(datosExp1)

## ----Ejercicio 3 Experimento1b-------------------------------------------
fexp1 = function(x,y){
  sign((x+0.2)^2 + y^2-0.6)
}

etiquetasexp1 = fexp1(datosExp1[,1], datosExp1[,2])
etiquetasexp1ruido = asignarRuido(etiquetasexp1, 10)
par(mfrow=c(1,2))
plot(datosExp1, col=etiquetasexp1+3)
plot(datosExp1, col=etiquetasexp1ruido+3)

## ----Ejercicio 3 Experimento1c-------------------------------------------
pesosExp1 = Regress_Lin(datosExp1,etiquetasexp1ruido)
EinExp1 = errores(datosExp1,etiquetasexp1ruido,pesosExp1)
print(pesosExp1)
print(EinExp1)


## ----Ejercicio 3 Experimento1d-------------------------------------------
# Función que realiza la funcionalidad pedida por el apartado d) del experimento 1.
# Realiza 1000 iteraciones de los apartados a) al c) de este mismo experimento
# y calcula el Ein en base a ello. Para calcular el Eout creo una muestra aleatoria
# de 1000 datos, y asigno ruido a sus etiquetas, y las utilizo como test.
# La función devuelve una lista que contiene la media de Ein y de Eout que se han
# calculado en las 1000 iteraciones.
experimento1d = function(){
  EinTotal = 0
  EoutTotal = 0
  for (i in 1:1000){
    datosExp1 = simula_unif(N=1000, dim=2, rango=c(-1,1))
    etiquetasexp1 = fexp1(datosExp1[,1], datosExp1[,2])
    etiquetasexp1ruido = asignarRuido(etiquetasexp1, 10)
    pesosExp1 = Regress_Lin(datosExp1,etiquetasexp1ruido)
    EinExp1 = errores(datosExp1,etiquetasexp1ruido,pesosExp1)
    
    datostest = simula_unif(N=1000, dim=2, rango=c(-1,1))
    etiquetastest = fexp1(datostest[,1], datostest[,2])
    etiquetastestruido = asignarRuido(etiquetastest, 10)
    EoutExp1 = errores(datostest, etiquetastestruido, pesosExp1)
    
    EinTotal = EinTotal + EinExp1
    EoutTotal = EoutTotal + EoutExp1
  }
  EinMedia = EinTotal / 1000
  EoutMedia = EoutTotal / 1000
  list(Ein=EinMedia, Eout=EoutMedia)
}

set.seed(100)
erroresExp1 = experimento1d()
print(erroresExp1$Ein)
print(erroresExp1$Eout)

## ----Experimento2a-------------------------------------------------------
set.seed(100)
datosExp2 = cbind(datosExp1,datosExp1[,1]*datosExp1[,2], (datosExp1[,1])^2, (datosExp1[,2])^2)
etiquetasexp2 = fexp1(datosExp2[,1], datosExp2[,2])
etiquetasexp2ruido = asignarRuido(etiquetasexp2, 10)
pesosExp2 = Regress_Lin(datosExp2, etiquetasexp2ruido)
EinExp2 = errores(datosExp2, etiquetasexp2ruido, pesosExp2)

print(pesosExp2)
print(EinExp2)

## ----Experimento2b-------------------------------------------------------
# Función que realiza la funcionalidad pedida por el apartado b) del experimento 2.
# Realiza 1000 iteraciones en las que genera los datos de train de los que genera
# las etiquetas con ruido y los pesos. Los datos de train tienen las características
# solicitadas en el enunciado. A continuación se calcula el Ein.
# Para calcular el Eout creo una muestra aleatoria de 1000 datos, y asigno ruido a
# sus etiquetas, y las utilizo como test. Los datos test también tienen las
# características especificadas. Obtengo el Eout en base a estos datos test.
# La función devuelve una lista que contiene la media de Ein y de Eout que se han
# calculado en las 1000 iteraciones.
experimento2b = function(){
  EinTotal = 0
  EoutTotal = 0
  for (i in 1:1000){
    datos= simula_unif(N=1000, dim=2, rango=c(-1,1))
    etiquetasdatos = fexp1(datos[,1], datos[,2])
    etiquetasdatosruido = asignarRuido(etiquetasdatos, 10)
    datosExp2 = cbind(datos,datos[,1]*datos[,2], (datos[,1])^2, (datos[,2])^2)
    pesosExp2 = Regress_Lin(datosExp2,etiquetasdatosruido)
    EinExp2 = errores(datosExp2,etiquetasdatosruido,pesosExp2)
    
    datosaux = simula_unif(N=1000, dim=2, rango=c(-1,1))
    etiquetastest = fexp1(datosaux[,1], datosaux[,2])
    etiquetastestruido = asignarRuido(etiquetastest, 10)
    datostest = cbind(datosaux,datosaux[,1]*datosaux[,2], (datosaux[,1])^2, (datosaux[,2])^2)
    EoutExp2 = errores(datostest, etiquetastestruido, pesosExp2)
    
    EinTotal = EinTotal + EinExp2
    EoutTotal = EoutTotal + EoutExp2
  }
  EinMedia = EinTotal / 1000
  EoutMedia = EoutTotal / 1000
  list(Ein=EinMedia, Eout=EoutMedia)
}

set.seed(100)
erroresExp2 = experimento2b()
print(erroresExp2$Ein)
print(erroresExp2$Eout)

## ----Bonus a-------------------------------------------------------------
# Función que realiza las operaciones requeridas para el apartado A del bonus.
# Crea en cada iteración una muestra de 100 datos en rango [-10,10] y a cada una
# le asigna una recta que delimita sus etiquetas. Saco su ruido y los pesos
# con la regresión lineal y extraigo el Ein.
bonusA = function(){
  datosBonus = simula_unif(N=100, dims=2, rango=c(-10,10))
  rectaBonus = simula_recta(intervalo=c(-10,10))
  etiquetasBonus = asignarEtiquetasSegunRecta(rectaBonus,datosBonus)
  etiquetasBonusRuido = asignarRuido(etiquetasBonus, 10)
  pesosBonus = Regress_Lin(datosBonus,etiquetasBonusRuido)
  EinBonus = errores(datosBonus, etiquetasBonusRuido, pesosBonus)
}

set.seed(100)
datosBonus = simula_unif(N=100, dims=2, rango=c(-10,10))
rectaBonus = simula_recta(intervalo=c(-10,10))
etiquetasBonus = asignarEtiquetasSegunRecta(rectaBonus,datosBonus)
etiquetasBonusRuido = asignarRuido(etiquetasBonus, 10)
pesosBonus = Regress_Lin(datosBonus,etiquetasBonusRuido)
EinBonus = errores(datosBonus, etiquetasBonusRuido, pesosBonus)
print(EinBonus)

EinBonusA = replicate(1000, bonusA())
mean(EinBonusA)

## ----Bonus b-------------------------------------------------------------
# Función que realiza las operaciones requeridas para el apartado B del bonus.
# Crea en cada iteración una muestra de 100 datos en rango [-10,10] y a cada una
# le asigna una recta que delimita sus etiquetas. Saco su ruido y los pesos
# con la regresión lineal. A continuación, con ese vector de pesos calculo
# el error Eout con un conjunto de datos test creado en cada iteración, del mismo
# modo que la muestra.
bonusB = function(){
  datosBonus = simula_unif(N=100, dims=2, rango=c(-10,10))
  rectaBonus = simula_recta(intervalo=c(-10,10))
  etiquetasBonus = asignarEtiquetasSegunRecta(rectaBonus,datosBonus)
  etiquetasBonusRuido = asignarRuido(etiquetasBonus, 10)
  pesosBonus = Regress_Lin(datosBonus,etiquetasBonusRuido)

  datosTest = simula_unif(N=100, dims=2, rango=c(-10,10))
  rectaTest = simula_recta(intervalo=c(-10,10))
  etiquetasTest = asignarEtiquetasSegunRecta(rectaTest,datosTest)
  etiquetasTestRuido = asignarRuido(etiquetasTest, 10)
  
  EoutBonus = errores(datosTest, etiquetasTestRuido, pesosBonus)
}

set.seed(100)
EoutBonusB = replicate(1000,bonusB())
mean(EoutBonusB)

## ----Bonus c-------------------------------------------------------------
# Función que realiza las operaciones requeridas para el apartado C del bonus.
# Crea en cada iteración una muestra de 10 datos en rango [-10,10] y a cada una
# le asigna una recta que delimita sus etiquetas. Saco su ruido y los pesos
# con la regresión lineal. A continuación, utilizo ese vector de pesos como el
# vector inicial del PLA, poniendo un máximo de 200 iteraciones y pasando como
# datos la muestra y como etiquetas las de ruido. De la lista que devuelve la
# función, saco las iteraciones necesarias para converger.
bonusC = function(){
  datosBonus = simula_unif(N=10, dims=2, rango=c(-10,10))
  rectaBonus = simula_recta(intervalo=c(-10,10))
  etiquetasBonus = asignarEtiquetasSegunRecta(rectaBonus,datosBonus)
  etiquetasBonusRuido = asignarRuido(etiquetasBonus, 10)
  pesosBonus = Regress_Lin(datosBonus,etiquetasBonusRuido)

  resultadoPLA = ajusta_PLA(datosBonus,etiquetasBonusRuido,200,pesosBonus)
  resultadoPLA$iter
}

set.seed(100)
iteracionesBonusC = replicate(1000,bonusC())
mean(iteracionesBonusC)

## ----iteraciones---------------------------------------------------------
print(iteracionesBonusC)

