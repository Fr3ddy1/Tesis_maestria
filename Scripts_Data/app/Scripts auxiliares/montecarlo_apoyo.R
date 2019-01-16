#exponencial
curve(exp,0.5,3.5,lwd=2,axes=FALSE,xlab = "",ylab = "")
axis(1)
t <- seq(1,3,by = 0.01)
x <- c(1,t,3)
y <- c(0,exp(t),0)
polygon(x,y,col="grey")
abline(h=0,lwd=2)
rect(1,0,3,exp(3),border = "red",lwd=2)
n <- 2000
x1 <- runif(n,1,3)
y1 <- runif(n,0,exp(3))
ab <- as.factor((y1<=exp(x1))*1)
points(x1,y1,col=c(1,2)[ab],pch=c(19,19)[ab])

#valor a aproximar
exp(3)-exp(1)
#resultado de simulaciones
(table(ab)/n)*2*exp(3)
text(2,exp(3.2),"y=f(x)=exp(x)")


#distribucion normal
curve(dnorm,-3,3,axes=FALSE,xlab = "",ylab = "",lwd=2.5)
axis(1)
abline(h=0,lwd=2.5)

#programacion
rect(0,0,1,dnorm(0),border = "blue",lwd = 2.5)
contador <- 0
n <- 2000
for(i in 1:n){
  x <- runif(n,0,1)
  y <- runif(n,0,dnorm(0))
  if(y[i]<=dnorm(x[i]))
    contador <- contador+1 
}
prob <- (contador/n)*1*dnorm(0)
#area aproximada
prob

#area real
pnorm(1)-pnorm(0)

#funcion montecarlo
f <- function(x)  exp(-0.5*x^2)/sqrt(2*pi)
 
  x2 <- seq(-3,3,by = 0.01)
  plot(x2,f(x2),type = "l",lwd=2,axes=F,xlab = "",ylab = "")
  axis(1)
  abline(h=0,lwd=2)
  polygon(c(0,seq(0,1,by=0.01),1),c(0,dnorm(seq(0,1,by=0.01)),0),col="grey90")
  rect(0,0,1,dnorm(0),border = "blue")
  n2 <- 2000
  x3 <- runif(n2,0,1)
  y3 <- runif(n2,0,dnorm(0))
  colour <- ifelse(y3<=dnorm(x3),"red","black")
  points(x3,y3,col=colour,pch=c(19,19))

  prob3 <- (table(colour)/n2)*1*dnorm(0)
  prob3
  
  pnorm(1)-pnorm(0)
  
####################################
####################################
#calculo VaR por simulacion de Montecarlo 1 activo

#leo data
  #Prueba VaR, usando paquete VaRES
  library(corpcor)
  library(tseries)
  library(fitdistrplus)
  library(rriskDistributions)
  #quito notacion cientifica
  options(scipen=999) 
  
  #cargo la data
  tif <- read.delim2("~/Downloads/tif.txt")
  veb <- read.delim2("~/Downloads/vebono.txt")
  data_montecarlo <- read.delim2("~/Downloads/Data-Montecarlo-Diario.txt")
  
  #ordeno la data de fecha mas reciente a fecha mas antigua
  tif$Fecha <- as.Date(tif$Fecha)
  veb$Fecha <- as.Date(veb$Fecha)
  data_montecarlo$Fecha <- as.Date(data_montecarlo$Fecha)
  
  #quito tif vencidos
  tif <- tif[order(tif$Fecha,decreasing = TRUE),-c(2,3)]
  veb <- veb[order(veb$Fecha,decreasing = TRUE),]
  data_montecarlo <- data_montecarlo[order(data_montecarlo$Fecha,decreasing = TRUE),]
  
  #pruebo con TIF
  #seleciono 252 obs
  tif_252 <- tif[1:252,]

  #valor nominal TIF
  #tif
  a <- as.data.frame(matrix(0,nrow = (ncol(tif)-1),ncol=2))
  names(a) <- c("titulo","valor_nominal")
  a[,1] <- names(tif)[-1]
  
  vt <- c(2000000,10000000,5000000,7000000,1000000,
          2000000,4000000,3000000,20000000,6000000,8000000,2000000,
          10000000,30000000,6000000,10000000,5000000,9000000,5000000,
          5000000,9000000,4000000,70000000,5000000,7000000)
  
  a[,2] <- vt

  #1) leer datos de precio (ver arriba) 
  #la variable con la que se trabajara es tif_252
  head(tif_252)
  
  #2)calcular rend a los precios 
  rend_tif <- as.data.frame(matrix(0,nrow = (nrow(tif_252)-1),ncol = (ncol(tif_252)-1)))
  names(rend_tif) <- names(tif_252)[-1]
  
  
  for(i in 1:(ncol(tif_252)-1)){
    rend_tif[,i] <- diff(log(tif_252[,(i+1)]))
  }

  
  #trabajo con el primer instrumeto, la columna 1!
  d <- useFitdist(rend_tif[,1]) 
  
  #asumo normalidad
  #genero numero aleatorios segun dist
  n_norm <- rnorm(n = 100000,mean = as.numeric(fitdistr(rend_tif[,1],"normal")$estimate)[1],sd = as.numeric(fitdistr(rend_tif[,1],"normal")$estimate)[2])
  
  #calculo incrementos 
  pre_inc_n <- vt[1]*n_norm
  
  #precio
  pre_n <- vt[1]-pre_inc_n
  
  #calculo ubicacion
  length(n_norm)*5/100
  
  #valor corte
  #ordeno precios
  pre1_n <- pre_n[order(pre_n)]
  hist(pre1_n)
  vc_n <- pre1_n[length(n_norm)*5/100]
  
  #VaR
  var_n <- vt[1]-vc_n
  
  #asumo dist logistica
  #genero numero aleatorios segun dist
  n_log <- rlogis(n = 100000,location =  as.numeric(d$fit.list$Logistic$estimate)[1],scale =  as.numeric(d$fit.list$Logistic$estimate)[2])
  
  #calculo incrementos 
  pre_inc_log <- vt[1]*n_log
  
  #precio
  pre_log <- vt[1]-pre_inc_log
  
  #calculo ubicacion
  length(n_log)*5/100
  
  #valor corte
  #ordeno precios
  pre1_log <- pre_log[order(pre_log)]
  hist(pre1_log)
  vc_log <- pre1_log[length(n_log)*5/100]
  
  #VaR
  var_log <- vt[1]-vc_log
  
  
  #asumo dist uniforme
  #genero numero aleatorios segun dist
  n_u <- runif(n = 100000,min  =  as.numeric(d$fit.list$Uniform$estimate)[1],max =  as.numeric(d$fit.list$Uniform$estimate)[2])
  
  #calculo incrementos 
  pre_inc_u <- vt[1]*n_u
  
  #precio
  pre_u <- vt[1]-pre_inc_u
  
  #calculo ubicacion
  length(n_u)*5/100
  
  #valor corte
  #ordeno precios
  pre1_u <- pre_u[order(pre_u)]
  hist(pre1_u)
  vc_u <- pre1_u[length(n_u)*5/100]
  
  #VaR
  var_u <- vt[1]-vc_u
  
  #comparacion normal vs logistica vs uniforme
  c(var_n,var_log,var_u)
    
  #calculo VaR por simulacion PORTAFOLIO
  #UNA VEZ OBTENIDOS LOS RENDIMIENTOS (VARIABLE REND_TIF)
  
  #CALCULO PESOS
  a$pesos <- a[,2]/sum(a[,2])
  
  #creo matriz donde guardare simulaciones de cada instrumento
  mat <- as.data.frame(matrix(0,nrow = 100000,ncol = (ncol(rend_tif)+2)))
  names(mat) <- c(names(rend_tif),"incremento","escenario")
  
  #relleno matriz de simulaciones
  for(i in 1:ncol(rend_tif)){
    mat[,i] <-  rnorm(n = 100000,mean = as.numeric(fitdistr(rend_tif[,i],"normal")$estimate)[1],sd = as.numeric(fitdistr(rend_tif[,i],"normal")$estimate)[2])
  }
  
  #calculo columna "incremento precio"
  for(i in 1:nrow(mat)){
    mat$incremento[i] <- sum(a[,2])*sum(as.numeric(a[,3])*as.numeric(mat[i,1:ncol(rend_tif)]))
  }
  
  #calculo columna "escenario"
  for(i in 1:nrow(mat)){
    mat$escenario[i] <- sum(a[,2])+mat$incremento[i]
  }
  
  #ordeno columna "escenarios"
  mat1 <- mat$escenario
  mat1 <- mat1[order(mat1)]
  
  #calculo valor de corte y VaR Monte Carlo
  vc <- (mat1[length(mat1)*5/100])
  var_sm <- sum(a[,2])-vc
  (var_sm)
  
  ##############
  ##############
  ##############
  ##############
  ##############
  ##############
  #ALTERNATIVA OTRO PAQUETE
  #EXISTEN PROBLEMAS AL GENERAR NUMEROS ALEATORIOS CON PAQUETE BASE
  #PARA SOLVENTAR ESTO USARE PAQUETE LMOMCO
  #ejemplo error
  fitdistr(rend_tif[,1],"logistic")
  
  #uso paquete nuevo
  #calculo momentos
  a1=lmom.ub(rend_tif[,1])

  #convierto valor anterior en parametros
  (b1=lmom2par(a1,type="glo")) #logistica generalizada  

  #realizo simulacion
  c1=rlmomco(10000,b1)  
  
  #OJO
  #USANDO ESTE PAQUETE Y UNA PRUEBA DE BONDAD DE AJUSTE ES POSIBLE 
  #VERIFICAR SI ALGUNAS DE ESTAS DISTRIBUCIONES SE AJUSTA A LOS DATOS
  #ESTO SE PUEDE USAR PARA LA PARTE DE SELECCION DE DISTRIBUCION AUTOMATICA
  
  
  ###########
  ###########
  ###########
  ###########
  ###########
  ###########
  ###########
  ###########
  #VAR MONTECARLO COMO EN SCRIPT
  #USO MATRIZ DE CORRELACION
  #1) CALCULO RENDIMIENTOS
  library(corpcor)
  library(tseries)
  library(fitdistrplus)
  library(rriskDistributions)
  #quito notacion cientifica
  options(scipen=999) 
  
  #cargo la data
  tif <- read.delim2("~/Downloads/tif.txt")
  veb <- read.delim2("~/Downloads/vebono.txt")
  data_montecarlo <- read.delim2("~/Downloads/Data-Montecarlo-Diario.txt")
  
  #ordeno la data de fecha mas reciente a fecha mas antigua
  tif$Fecha <- as.Date(tif$Fecha)
  veb$Fecha <- as.Date(veb$Fecha)
  data_montecarlo$Fecha <- as.Date(data_montecarlo$Fecha)
  
  #quito tif vencidos
  tif <- tif[order(tif$Fecha,decreasing = TRUE),-c(2,3)]
  veb <- veb[order(veb$Fecha,decreasing = TRUE),]
  data_montecarlo <- data_montecarlo[order(data_montecarlo$Fecha,decreasing = TRUE),]
  
  #pruebo con TIF
  #seleciono 252 obs
  tif_252 <- tif[1:252,]
  
  #valor nominal TIF
  #tif
  a <- as.data.frame(matrix(0,nrow = (ncol(tif)-1),ncol=2))
  names(a) <- c("titulo","valor_nominal")
  a[,1] <- names(tif)[-1]
  
  vt <- c(2000000,10000000,5000000,7000000,1000000,
          2000000,4000000,3000000,20000000,6000000,8000000,2000000,
          10000000,30000000,6000000,10000000,5000000,9000000,5000000,
          5000000,9000000,4000000,70000000,5000000,7000000)
  
  a[,2] <- vt
  
  #1) leer datos de precio (ver arriba) 
  #la variable con la que se trabajara es tif_252
  head(tif_252)
  
  #2)calcular rend a los precios 
  rend_tif <- as.data.frame(matrix(0,nrow = (nrow(tif_252)-1),ncol = (ncol(tif_252)-1)))
  names(rend_tif) <- names(tif_252)[-1]
  
  
  for(i in 1:(ncol(tif_252)-1)){
    rend_tif[,i] <- diff(log(tif_252[,(i+1)]))
  }
  
  
  #2) CALCULO MATRIZ DE CHOLESKY
  cholesky <- t(chol(cor(rend_tif,use="complete.obs")))
  
  #3) CALCULO PARAMETROS DE CADA INSTRUMENTO
  #creo matriz donde guardare simulaciones de cada instrumento
  mat <- (matrix(0,nrow = 100000,ncol = (ncol(rend_tif))))
  names(mat) <- c(names(rend_tif))
  
  #4) GENERO NUMEROS ALEATORIOS SEGUN DISTRIBUCION (ESC)
  #relleno matriz de simulaciones
  for(i in 1:ncol(rend_tif)){
    mat[,i] <-  rnorm(n = 100000,mean = as.numeric(fitdistr(rend_tif[,i],"normal")$estimate)[1],sd = as.numeric(fitdistr(rend_tif[,i],"normal")$estimate)[2])
  }
  
  #5) REALIZO PRODUCTO DE ESC*MAT CHOLESKY
  mrs <- mat%*%t(cholesky)
  
  #6) CALCULO PRECIO SIMULADO DE CADA DIA
  psim <- (matrix(0,nrow = 100000,ncol = (ncol(rend_tif))))
  names(psim) <- c(names(rend_tif))
  
  #relleno matriz de precios simulados
  for(i in 1:ncol(rend_tif)){
    psim[,i] <-  tif_252[nrow(tif_252),i+1]*exp(mrs[,i])
  }
  
  #7) CALCULO PRECIO REAL
  preal <- (matrix(0,nrow = 100000,ncol = (ncol(rend_tif))))
  names(preal) <- c(names(rend_tif))
  
  #relleno matriz de precios simulados
  for(i in 1:ncol(rend_tif)){
    preal[,i] <-  vt[i]*psim[,i]/100
  }
  
  #8) CALCULO GANANCIA O PERDIDA
  gop <- (matrix(0,nrow = 100000,ncol = (ncol(rend_tif))))
  names(gop) <- c(names(rend_tif))
  
  #relleno matriz de precios simulados
  for(i in 1:ncol(rend_tif)){
    gop[,i] <-  preal[,i]-(tif_252[nrow(tif_252),i+1]/100*vt[i])
  }
  
  #9) CALCULO GOP DE CADA DIA
  gop2 <- rowSums(gop)
  
  #10) CALCULO CUANTIL, QUE ES EL VAR
  quantile(gop2,  probs = c(0.05),type = 1)
  
  
  ########
  #PRUEBA PARA UNIR LOS DOS ENFOQUES 
  #PRIMER ENFOQUE, CALCULO DE ESCENARIOS, CALCULANDO VC
  #SEGUNDO ENFOQUE PROCESO ANTERIOR CALCULANDO GOP
  
  #4) calculo "incremento de precio"
  mrs1 <- mrs%*%vt
  
  #5) calculo "precio" (siempre positivo)
  mrs2 <- sum(vt)+mrs1
  
  #6) calculo vc
  vc <- quantile(mrs2,  probs = c(0.05),type = 1)
  
  #7) VaR
  sum(vt)-vc
  
  #grafica escenarios
  hist(mrs2)
  
  #########Z
  #APOYO FECHAS DISPONIBLES PARA CALCULO VAR
  obs <- nrow(tif)-252
  
  #de la celda 1 a la celda obs es posible calcular VaR con 252 obs
  #fechas disponibles para calcular VaR
  fechas <- tif$Fecha[1:obs]
  
  #elijo fecha
  fechas[1]
  
  #extraigo data
  tif_sub <- tif[which(fechas[10]==tif$Fecha):(251+which(fechas[10]==tif$Fecha)),]
  
  
  
  