#Prueba VaR, usando paquete VaRES
library(corpcor)
library(tseries)
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

#calculo rendimientos
rend1 <- diff(tif_252$TIF082018)

#quito ceros
rend1 <- rend1[-which(rend1==0)]

#veo distribucion
a <- useFitdist(rend1)

#es posible q sea cauchy
#obtengo parametros cauchy
a$fit.list$Cauchy$estimate

#calculo VaR (sin posicion no horizonte temporal)
b <- varCauchy(0.95,mu =a$fit.list$Cauchy$estimate[1] ,sigma =a$fit.list$Cauchy$estimate[2])

#calculo VaR en terminos de la posicion y horizonte temporal
#posicion
valor1 <- 200000000

#horizonte, 7,15,23,30, 60 ,90, 180, 360, 720

valor1*b*sqrt(7)
valor1*b*sqrt(15)-valor1*b*sqrt(7)
valor1*b*sqrt(23)
valor1*b*sqrt(30)
valor1*b*sqrt(60)
valor1*b*sqrt(90)
valor1*b*sqrt(180)
valor1*b*sqrt(360)
valor1*b*sqrt(720)-valor1*b*sqrt(360)



#prueba con todos los titulos
dim(tif)

tif1 <- as.data.frame(matrix(0,nrow = 341,ncol = 27))
names(tif1) <- names(tif)[-1]


for(i in 1:(ncol(tif)-1)){
tif1[,i] <- diff(log(tif[,i+1]))
}


############
#PRUEBAS CALCULO DEL VAR- PROBLEMAS

#n <- which(input$inst==names(data()))
n <- 4
dat <- tif[,n]
dat1 <- diff(log(dat))
dat2 <- useFitdist(dat1)
#print(dat2)
#print(uFitdifflog(data1)$fit.list$Normal$estimate[1])
p <- 0.95
dist <- "Normal"

v<-VarR(p,dat2,dist)

#
dat3 <- uFitdifflog(dat)
v<-VarR(p,dat3,dist)


#creo txt de valores nominales
#tif
a <- as.data.frame(matrix(0,nrow = (ncol(tif)-1),ncol=2))
names(a) <- c("titulo","valor_nominal")
a[,1] <- names(tif)[-1]

vt <- c(1000000,3000000,2000000,10000000,5000000,7000000,1000000,
        2000000,4000000,3000000,20000000,6000000,8000000,2000000,
        10000000,30000000,6000000,10000000,5000000,9000000,5000000,
        5000000,9000000,4000000,70000000,5000000,7000000)

a[,2] <- vt

write.table(a,"posicion_tif.txt",row.names = FALSE,quote = FALSE,sep = ",")

#data prueba
a[,2] <- vt[-c(1,2)]
write.table(a,"posicion_tif_1.txt",row.names = FALSE,quote = FALSE,sep = ",")


#veb
a <- as.data.frame(matrix(0,nrow = (ncol(veb)-1),ncol=2))
names(a) <- c("titulo","valor nominal")
a[,1] <- names(veb)[-1]

vb <- c(1000000,3000000,2000000,10000000,5000000,7000000,1000000,
        2000000,4000000,3000000,20000000,6000000,8000000,2000000,
        10000000,30000000,6000000,10000000,5000000,9000000,5000000,
        5000000,9000000,4000000,70000000,5000000,7000000,7000000,
        4000000,8000000,9000000)

a[,2] <- vb

write.table(a,"posicion_veb.txt",row.names = FALSE,quote = FALSE,sep = ",")

#tif-veb
a <- as.data.frame(matrix(0,nrow = (ncol(data_montecarlo)-1),ncol=2))
names(a) <- c("titulo","valor nominal")
a[,1] <- names(data_montecarlo)[-1]

vb <- c(1000000,3000000,2000000,10000000,5000000,7000000,1000000,
        2000000,4000000,3000000,20000000,6000000,8000000,2000000,
        10000000,30000000,6000000,10000000,5000000,9000000,5000000,
        5000000,9000000,4000000,70000000,5000000,7000000,7000000,
        4000000,8000000,9000000,5000000,3000000)

a[,2] <- c(vt,vb)

write.table(a,"posicion_montec.txt",row.names = FALSE,quote = FALSE,sep = ",")


#calculo VaR portafolio por los pasos anotados en el cuaderno
#1) leer datos de precio (ver arriba) 
#la variable con la que se trabajara es tif_252
head(tif_252)

#2)calcular rend a los precios 
rend_tif <- as.data.frame(matrix(0,nrow = (nrow(tif_252)-1),ncol = (ncol(tif_252)-1)))
names(rend_tif) <- names(tif_252)[-1]


for(i in 1:(ncol(tif_252)-1)){
  rend_tif[,i] <- diff(log(tif_252[,(i+1)]))
}

#3)calcular parametros de la dist normal por instrumento
dist_tif <- as.data.frame(matrix(0,nrow = 2,ncol = (ncol(tif_252)-1)))
names(dist_tif) <- names(tif_252)[-1]
rownames(dist_tif) <- c("Media","Desviación estandar")

for(i in 1:ncol(dist_tif)){
  if(anyNA(rend_tif[,i])){
    a <- which(is.na(rend_tif[,i])|is.infinite(rend_tif[,i]))
    dist_tif[1,i] <- as.numeric(fitdistr(rend_tif[-a,i],"normal")$estimate)[1]
    dist_tif[2,i] <- as.numeric(fitdistr(rend_tif[-a,i],"normal")$estimate)[2]
  }else{
    dist_tif[1,i] <- as.numeric(fitdistr(rend_tif[,i],"normal")$estimate)[1]
    dist_tif[2,i] <- as.numeric(fitdistr(rend_tif[,i],"normal")$estimate)[2]
  }
}

#4) ingreso valores nominales para calcular el rend esperado del portafolio
#que se define como sigue q1*u1+q2*u2+...+qn*un
#donde ui es la media de titulo i, qi es la participacion del titulo i

vt <- vt[-c(1,2)]
q <- vt
V_inicial <- sum(vt)

q <- vt/V_inicial


rend_esp_p <-  sum(q*as.numeric(dist_tif[1,]))

#5)calculo ahora la varianza del portafolio que se define como
#[q1 q2 ... qn][S][q1 q2 ... qn]T , donde S es la matriz de varianzas y cov

S <- cov(rend_tif)
#Shrinkage estimate of covariance
cov.shrink(S)

#Transform covariance to correlation matrix
S_tif <- cov2cor(S)

S_tif_1 <- cor(rend_tif)

#ojo en este caso la transpuesta funciono con as.matrix
#esto debido a la forma de vt que es un vector de valores
var_p <- q%*%S_tif%*%as.matrix(q)

sd_p <- sqrt(var_p)


#6)una vez tengo la media y varianza dek portafolio, procedo a calcular el VaR
#como si fuera para una activo
V_inicial <- sum(vt)
V_inicial <- 100

#calculo rend final esperado
e_vf <- V_inicial*(1+(rend_esp_p))

#calculo var final del portafolio
var_vf <- V_inicial*sqrt(var_p)

#calculo valor de corte
V_corte <- qnorm(0.05,e_vf,var_vf)

VaR_tif <- V_inicial-V_corte
VaR_tif

##
qnorm(0.95,rend_esp_p,sd_p)

#metodo video
#1)calculo rend usar rend_tif

#2)calculo matriz de correlaciones (diagonal de 1's)
S_cor_tif <- cor(rend_tif)

#3)calculo var individual, para ello creo tabla
#1era col: sd de cada instrumento
#2da col: valor nominal de cada instrumento
#3era col: VaR = sd*nominal*factor dist normal (1,64 si es 95%)
#4ta col: VaR en %
#usar variable dist_tif

tabla <- as.data.frame(matrix(0,nrow = ncol(rend_tif),ncol = 4))
names(tabla) <- c("SD","Nominal","VaR_individual","VaR_porcentaje")
rownames(tabla) <- names(rend_tif)

tabla[,1] <- as.numeric(dist_tif[2,])
tabla[,2] <- vt

tabla[,3] <-  tabla[,1]*tabla[,2]*qnorm(0.95,0,1)
tabla[,4] <- tabla[,3]*100/sum(tabla[,3])


#4) calculo VaR 

VaR <-  sqrt(tabla[,3]%*%S_cor_tif%*%as.matrix(tabla[,3]))
VaR

#var individuales suma
sum(tabla[,3])


#GRAFICOS
#GRAFICO VALOR NOMINAL
library(plotly)

USPersonalExpenditure <- data.frame("Categorie"=rownames(USPersonalExpenditure), USPersonalExpenditure)
data <- USPersonalExpenditure[,c('Categorie', 'X1960')]

p <- plot_ly(data, labels = ~Categorie, values = ~X1960, type = 'pie') %>%
  layout(title = 'United States Personal Expenditures by Categories in 1960',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))


p

#creo data
pie <- cbind.data.frame(rownames(tabla),tabla$Nominal)
names(pie) <- c("Titulo","nominal")

p <- plot_ly(pie, labels = ~Titulo, values = ~nominal, type = 'pie') %>%
  layout(title = 'Valor nominal TIF',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))


p

#GRAFICO VAR IND
pie1 <- cbind.data.frame(rownames(tabla),tabla$VaR_individual)
names(pie1) <- c("Titulo","var")

p1 <- plot_ly(pie1, labels = ~Titulo, values = ~var, type = 'pie') %>%
  layout(title = 'VaR individual TIF',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))


p1

#GRAFICO COMPARATIVO VAR IND VS PORTAFOLIO


# p2 <- plot_ly(
#   x = c("giraffes", "orangutans", "monkeys"),
#   y = c(20, 14, 23),
#   #name = "SF Zoo",
#   type = "bar"
# )
# 
# p2

#
# x_b = c("Suma VaRes individuales", "VaR Portafolio")
# y_b = c(sum(tabla[,3]), VaR)
# width = c(0.3, 0.3)
# data <- data.frame(x_b, y_b, width)
# 
# 
# p <- plot_ly(data,marker = list(color = 'royalblue1',
#                                 line = list(color = 'rgb(8,48,107)',
#                                             width = 3))) %>%
#   add_bars(
#     x= ~x_b,
#     y= ~y_b,
#     width = ~width
#   )
# 
# p

#otra opcion
p <- plot_ly(marker = list(color = 'royalblue1',
                           line = list(color = 'rgb(8,48,107)',
                                       width = 3))) %>%
  add_bars(
    x = c(" "),
    y = c(sum(tabla[,3])),
    width = 0.3,
    marker = list(
      color = 'royalblue1'
    ),
    name = 'Suma VaRes individuales'
  ) %>%
  add_bars(
    x = c(" "),
    y = c(VaR),
    width = 0.3,
    marker = list(
      color = 'mediumseagreen'
    ),
    name = 'VaR Portafolio'
  )

p


###PRUEBAS VAR POR SIMULACION HISTORICA
#uso toda la data tif, deberia se 252 o 504 
#mientras mas info mejor 
#1) calculo rend
#hecho usar variable rend_tif
#2) considerar valor nominal para calcular columna de escenarios
#si es un instrumento usar Vo(1+R)
#si es portafolio usar Vo[q1*(1+R1)+...+qn*(1+Rn)]
#qi son los pesos de cada instrumento segun su valor nominal
#la suma de los qi es 1
head(rend_tif)
dim(rend_tif)

#uso variable a, y calculo pesos qi
a1 <- rbind.data.frame(a,c("Totales",sum(a$valor_nominal)))
a1$valor_nominal <- as.numeric(a1$valor_nominal)
a1$pesos <- a1$valor_nominal/a1$valor_nominal[nrow(a1)]

#validacion
sum(a1$pesos[1:(nrow(a1)-1)])


esc <- rep(0,nrow(rend_tif))
#calculo escenarios
for(i in 1:nrow(rend_tif)){
esc[i] <- sum((1+as.numeric(rend_tif[i,]))*a1$pesos[1:(nrow(a1)-1)])*a1$valor_nominal[nrow(a1)]
}

#ordeno data
esc_orden <- esc[order(esc)]

#calculo ubicacion
round(length(esc_orden)*5/100)

#valor de corte
vc <- esc_orden[round(length(esc_orden)*5/100)]

#VaR
a1$valor_nominal[nrow(a1)]-vc

#grafico escenarios
hist(esc,breaks = 20)

#ejemplo
p1 <- plot_ly(diamonds, x = ~cut) %>% add_histogram()
p1

#
p2 <- plot_ly(cbind.data.frame(seq(1,length(esc)),esc), x = ~esc) %>% add_histogram()
p2


