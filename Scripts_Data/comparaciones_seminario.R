#comparaciones de precios para tesis 
#Splines vs Svensson
#script que me ayuda a montar seccion splines en app
#cargo data para elaborar curva a partir de la data usando spline
#de esta manera para esta curva fija, tendre un rendimiento para cada t
library(dplyr)
library(lubridate)
#library(xlsx)
library(readxl)
library(nloptr)
library(jrvFinance)

#source('C:/Users/Freddy Tapia/Desktop/Avances Tesis/Funciones auxiliares.R')
#setwd("C:/Users/Freddy Tapia/Desktop/Avances Tesis")

source(paste(getwd(),'Scripts_Data/funciones.R',sep = "/"))

#
################################
########## SPLINES #############
################################

#Funcion que extrae de una data dada una cantidad determinada de observaciones
#argumentos:
#fv = fecha a partir de la cual se extraera la data hacia atras
#dias = cantidad de dias a extraer
#data = data a trabajar
#ojo con el orden de la data debe estar ordenada de mas reciente a mas antigua
extrae <- function(fv,dias,data){
  f1 <- as.Date(fv)
  f2 <- f1-dias
  f3 <- data$Fecha.op-f2
  
  #con esto hallo el extremo inferior de la data
  f4 <- data[which(as.numeric(min(abs(f3)))==abs(f3)),]
  
  #hallo ahora el extremo superior
  g<- data$Fecha.op-fv
  g1 <- data[which(as.numeric(min(abs(g)))==abs(g)),]
  
  while(anyNA(g1)){
    #print("Obs con NA")
    data1 <- data[-which(as.numeric(min(abs(g)))==abs(g)),]
    
    g <- data1$Fecha.op-fv
    
    g1 <- data1[which(as.numeric(min(abs(g)))==abs(g)),]
    
  }
  
  if(dim(g1)[1]==1){
    #print("Hay una sola obs")
    #print("Fecha selecionada")
    #print(g1$Fecha.op)
    #return(f4$Fecha.op)
  }else{
    #print("Hay mas de una obs")
    g1 <- g1[which(g1$Monto==max(g1$Monto)),]
    #print("Fecha selecionada")
    #print(g1$Fecha.op)
    #return(f4$Fecha.op)
  }#final if obs
  
  while(anyNA(f4)){
    #print("Obs con NA")
    data1 <- data[-which(as.numeric(min(abs(f3)))==abs(f3)),]
    
    f3 <- data1$Fecha.op-f2
    
    f4 <- data1[which(as.numeric(min(abs(f3)))==abs(f3)),]
    
  }
  
  
  if(dim(f4)[1]==1){
    #print("Hay una sola obs")
    #print("Fecha selecionada")
    #print(f4$Fecha.op)
    #return(f4$Fecha.op)
  }else{
    #print("Hay mas de una obs")
    f4 <- f4[which(f4$Monto==max(f4$Monto)),]
    #print("Fecha selecionada")
    #print(f4$Fecha.op)
    #return(f4$Fecha.op)
  }#final if obs
  
  #extremo inferior
  q1 <- which(f4$Fecha.op==data$Fecha.op)
  
  #extremo superior
  q2 <- which(g1$Fecha.op==data$Fecha.op)[1]
  
  
  #data2 <- data[1:q1[length(q1)],]
  data2 <- data[q1[length(q1)]:q2,]
  
  # a1 <- data.frame(t(c(length(which(data2$segmento1=="C1")),
  #                      length(which(data2$segmento1=="C2")),
  #                      length(which(data2$segmento1=="M1")),
  #                      length(which(data2$segmento1=="M2")),
  #                      length(which(data2$segmento1=="L1")),
  #                      length(which(data2$segmento1=="L2")),
  #                      length(which(data2$segmento1=="L3")))))
  # 
  # a1 <- data.frame(a1,sum(a1))
  # names(a1) <- c("Obs. C1","Obs. C2","Obs. M1","Obs. M2","Obs. L1","Obs. L2","Obs. L3","Suma")
  # print(a1)
  #s <<-a1
  return(data2)
}#final funcion extrae

#
#Funcion que calcula el precio de un titiulo a partir de la curva de rend
#generado por el spline
#tit = nombre corto de tÃ?tulos a calcular su precio
#spline1 = objeto smooth spline que se obtine del ajuste de los datos
#fv = fecha de valoraciÃ³n
#C = documento de las caracterÃ?sticas a la fecha
precio=function(tit,spline1,fv,C){
  
  Pr=c()
  #print(fv)
  for(j in 1:length(tit)){
    
    #print(j)
    
    (n=which(C$Nombre==tit[j]))
    (n1=as.Date(C$`Pago cupon 1`[n],format="%d/%m/%Y"))
    
    if(as.numeric(n1-fv)<0){
      (n1=as.Date(C$`Pago cupon 2`[n],format="%d/%m/%Y"))
    }
    
    #
    
    (n2=as.Date(C$F.Vencimiento[n],format="%d/%m/%Y"))
    (n3=as.numeric(n2-n1))
    
    #creo vector del tamaÃÂ±o de 1 + division de n3/91
    #print("Le quedan")
    #print(1+(n3/91))
    #print("cupones por pagar")
    
    
    n4=n1
    #n4[1]=as.Date(n1,format="%d/%m/%Y")
    
    #condicion queda 1 cupon
    if(n3!=0){
      n5=rep(91,(n3/91))
      for(i in 1:(n3/91)){
        n4=unique(c(n4,n4+n5[i]))
      }
    }else{}
    
    #vector de fechas de los flujos
    #(n4=unique(n4))
    
    #valores a predecir mediante el Spline
    (n5=as.numeric(n4-fv))
    
    n6=predict(spline1,n5)
    (n6=n6$y/100)
    
    #calculo exponencial
    (n7=exp(-(n5/365)*n6))
    
    #cupon
    (n8=rep(C$Cupon[n]/4,(n3/91)))
    
    (n9=c(n8,C$Cupon[n]/4+100))
    
    #(n9=unique(n9))
    
    #PRECIO ESTIMADO
    (n10=sum(n7*n9))
    
    Pr[j]=n10
    
    
  }
  
  return(Pr)
  
}#final funcion precios estimados


#
#FunciÃ³n que calcula precio diario mediante met splines
#argumentos
#fe: fecha
#num: numero de dias hacia atras
#par: parametro spar
#datatif: data a usar
#tit : titulos
#C: caracteristica
#funcion que me genera tres variables
#candidatos: data frame con titulos usados
#Pr_tit: precio de los titulos de la variable tit
#Pr_coin: precios coincidencias con precio promedio

precio_diario_sp <- function(fe,num,par,datatif,tit,C,letra){
  
  D <- extrae(fe,num,datatif)
  (s <- as.character(unique(D$Nombre)))
  
  s2 <- c()
  
  for(i in 1:length(s)){
    s1 <- D[which(s[i]==D$Nombre),]
    
    if(nrow(s1)==1){
      
      s2 <- rbind(s2,s1)
    }else{
      s1 <- s1[which(s1$Monto==max(s1$Monto)),]
      s2 <- rbind(s2,s1)
    }
  }#final for elegir obs
  
  s2 <- arrange(s2,desc(Fecha.op))
  
  if(length(which(duplicated(s2$Nombre)))!=0){
    s2 <- s2[-which(duplicated(s2$Nombre)),]
  }
  s2 <- arrange(s2,(F.Vencimiento))
  
  #quito obs con monto menor a 10 MM
  #(s2 <- s2[-which(s2$Monto<10000000),])
  candidatos <<-s2
  
  #anado letra
  
  #s2 es la data con la que creare la curva
  #tomo puntos y grafico
  #par(mfrow=c(2,1))
  if(is.data.frame(letra)){
    x <- c(letra$Plazo,s2$Plazo)
    y <- c(letra$Rendimiento,s2$Rendimiento)
  }else if(length(letra)==2){
    x <- c(letra[1],s2$Plazo)
    y <- c(letra[2],s2$Rendimiento)
  } 
  #letra<<- datatif[max(which(datatif$Tipo.Instrumento=="LETRA"))]
  #plot(x,y)
  
  spline1<<-smooth.spline(x,y,spar = par)
  
  #p <- predict(spline1,seq(1,7000,50))
  
  #plot(p$x,p$y,type = "l",col=3)
  
  
  (Pr=precio(tit,spline1,fe,C))
  
  (Pr <- cbind.data.frame(tit,Pr))
  names(Pr) <- c("TÃ?tulos","Precios")
  #Pr_tit <<- Pr
  
  #coincidencias
  #(z <- inner_join(t1,Pr,by="tit"))
  #Pr_coin <<- z
  
  #Pr1 <- list(Pr,letra)
  
  return(Pr)
}#final funcion precio_diario

#Creo funcion para obtener precios Splines
#argumentos
#data = data a trabajar es el historico proveniente de 0-22
#tipo = un caracter indicando TIF o VEBONO
#fe = fecha de valoracion , ej: as.Date("2018-03-08")
#num = numero de dÃ?as hacia atras
#par = parametro de suavizamiento spar
#tit = nombre corto de los tÃ?tulos
#C = documento de las caracterÃ?sticas al dÃ?a

Tabla.splines <- function(data,tipo,fe,num,par,tit,C,pr){
  if(tipo=="TIF"){
    #extraigo solo TIF
    
    datatif <- data[which(data$Tipo.Instrumento=="TIF"),]
    datatif <- arrange(datatif,desc(Fecha.op))
    
    datatif$F.Vencimiento <- as.Date(datatif$F.Vencimiento,format="%d/%m/%Y")
    datatif$year <- year(datatif$F.Vencimiento)
    datatif$segmento <- cut(datatif$year,breaks = c(2015,2019,2030,2038),labels = c("Corto Plazo","Mediano Plazo","Largo Plazo"))
    
    datatif$segmento1 <- cut(datatif$year,breaks = c(2015,2017,2019,2025,2030,2033,2035,2038),labels = c("C1","C2","M1","M2","L1","L2","L3"))
    
    #extraigo letra
    #letra<<- data[min(which(data$Tipo.Instrumento=="LETRA")),]
    D1 <- extrae(fe,num,arrange(data,desc(Fecha.op)))
    
    if(length(which(D1$Tipo.Instrumento=="LETRA"))!=0){
      letra<<- D1[max(which(D1$Tipo.Instrumento=="LETRA")),]
    }else{
      print("no hay letra")
      letra <- c(97,1.34)
    }
    
    #datatif <- datatif[which(datatif$Tipo.Instrumento=="TIF"),]
    
    #calculo precios
    Pr_tit_tif <- precio_diario_sp(fe,num,par,datatif,tit,C,letra)
    
    #creo Tabla de resultados (similar a NS y Sv)
    Tabla=as.data.frame(matrix(0,14,length(tit)))
    colnames(Tabla)=tit
    rownames(Tabla)=c("ISIN","Fecha de LiquidaciÃ³n",
                      "Fecha de emisiÃ³n","Fecha de Vencimiento","Tasa de CupÃ³n",
                      "Precio Prom","Fecha Ãºltimo Pago","Fecha prÃ³ximo pago",
                      "RDTO al VMTO","DuraciÃ³n","Inverso de la duraciÃ³n",
                      "PonderaciÃ³n","Precio Modelo Spline",
                      "Residuos al cuadrado")
    
    #relleno ISIN
    for(i in 1:ncol(Tabla)){
      Tabla[1,i]=as.character(C$Sicet[which(names(Tabla)[i]==C$Nombre)])
    }
    
    #relleno fecha LiquidaciÃ³n
    for(i in 1:ncol(Tabla)){
      Tabla[2,i]=as.character(fe)
    }
    
    #relleno fecha Emision
    for(i in 1:ncol(Tabla)){
      Tabla[3,i]=as.character(C$F.Emision[which(names(Tabla)[i]==C$Nombre)])
    }
    
    #relleno fecha Vencimiento
    for(i in 1:ncol(Tabla)){
      Tabla[4,i]=as.character(C$F.Vencimiento[which(names(Tabla)[i]==C$Nombre)])
    }
    
    #relleno cupÃ³n
    for(i in 1:ncol(Tabla)){
      Tabla[5,i]=C$Cupon[which(names(Tabla)[i]==C$Nombre)]/100
    }
    
    #relleno fecha ultimo pago
    for(i in 1:ncol(Tabla)){
      Tabla[7,i]=as.character(C$`Pago cupon 1`[which(names(Tabla)[i]==C$Nombre)])
    }
    
    #relleno proximo pago
    for(i in 1:ncol(Tabla)){
      Tabla[8,i]=as.character(C$`Pago cupon 2`[which(names(Tabla)[i]==C$Nombre)])
    }
    
    #aÃ±ado precios promedios
    Tabla[6,]=pr
    
    #rendimiento
    for(i in 1:ncol(Tabla)){
      Tabla[9,i]=bond.yield(as.Date(fe,format="%d/%m/%Y"),as.Date(Tabla[4,i],"%d/%m/%Y"),as.numeric(gsub("[,]",".",Tabla[5,i])), 4,as.numeric(gsub("[,]",".",Tabla[6,i])),convention = c("ACT/360"),4)
    }
    
    #duracion
    for(i in 1:ncol(Tabla)){
      Tabla[10,i]=bond.duration(as.Date(fe,format="%d/%m/%Y"),as.Date(Tabla[4,i],"%d/%m/%Y"),as.numeric(gsub("[,]",".",Tabla[5,i])), 4,as.numeric(gsub("[,]",".",Tabla[9,i])),convention = c("ACT/360"),4)
    }
    
    #aÃ±ado inverso duracion
    Tabla[11,]=1/(as.numeric(gsub("[,]",".",Tabla[10,])))
    
    #aÃ±ado ponderacion
    for(i in 1:ncol(Tabla)){
      Tabla[12,i]=(as.numeric(gsub("[,]",".",Tabla[11,i])))/sum((as.numeric(gsub("[,]",".",Tabla[11,]))))
    }
    
    #relleno precios
    Tabla[13,]=as.numeric(Pr_tit_tif[,2]) 
    
    #relleno residuos al cuadrado
    
    for(i in 1:ncol(Tabla)){
      Tabla[14,i]=(((as.numeric(gsub("[,]",".",Tabla[13,i])))-(as.numeric(gsub("[,]",".",Tabla[6,i]))))*(as.numeric(gsub("[,]",".",Tabla[12,i]))))^2
    }
    
    #SRC
    print("EL SRC es")
    print(sum(as.numeric(gsub("[,]",".",Tabla[14,]))))
    
    Pr_tit_tif <- rbind.data.frame(Pr_tit_tif,sum(as.numeric(gsub("[,]",".",Tabla[14,]))))
    #rownames(Pr_tit_tif)[length(Pr_tit_tif[,1])] <- "SRC"
    
    res_tif <- list(Pr_tit_tif,candidatos[,c(2,3,6,7,12,13,15,17,18)],letra,spline1,Tabla) 
    
    return(res_tif)
    
  }else if(tipo=="VEBONO"){
    #letra<<- data[max(which(data$Tipo.Instrumento=="LETRA")),]
    
    dataveb <- data[which(data$Tipo.Instrumento=="VEBONO"),]
    dataveb <- arrange(dataveb,desc(Fecha.op))
    
    dataveb$F.Vencimiento <- as.Date(dataveb$F.Vencimiento,format="%d/%m/%Y")
    dataveb$year <- year(dataveb$F.Vencimiento)
    dataveb$segmento <- cut(dataveb$year,breaks = c(2015,2019,2030,2038),labels = c("Corto Plazo","Mediano Plazo","Largo Plazo"))
    
    dataveb$segmento1 <- cut(dataveb$year,breaks = c(2015,2017,2019,2025,2030,2033,2035,2038),labels = c("C1","C2","M1","M2","L1","L2","L3"))
    
    #extraigo letra
    #letra<<- data[min(which(data$Tipo.Instrumento=="LETRA")),]
    D1 <- extrae(fe,num,arrange(data,desc(Fecha.op)))
    
    if(length(which(D1$Tipo.Instrumento=="LETRA"))!=0){
      letra<<- D1[max(which(D1$Tipo.Instrumento=="LETRA")),]
    }else{
      print("no hay letra")
      letra <- c(97,1.34)
    }
    
    Pr_tit_veb <- precio_diario_sp(fe,num,par,dataveb,tit,C,letra)
    
    #creo Tabla de resultados (similar a NS y Sv)
    Tabla=as.data.frame(matrix(0,14,length(tit)))
    colnames(Tabla)=tit
    rownames(Tabla)=c("ISIN","Fecha de LiquidaciÃ³n",
                      "Fecha de emisiÃ³n","Fecha de Vencimiento","Tasa de CupÃ³n",
                      "Precio Prom","Fecha Ãºltimo Pago","Fecha prÃ³ximo pago",
                      "RDTO al VMTO","DuraciÃ³n","Inverso de la duraciÃ³n",
                      "PonderaciÃ³n","Precio Modelo Spline",
                      "Residuos al cuadrado")
    
    #relleno ISIN
    for(i in 1:ncol(Tabla)){
      Tabla[1,i]=as.character(C$Sicet[which(names(Tabla)[i]==C$Nombre)])
    }
    
    #relleno fecha LiquidaciÃ³n
    for(i in 1:ncol(Tabla)){
      Tabla[2,i]=as.character(fe)
    }
    
    #relleno fecha Emision
    for(i in 1:ncol(Tabla)){
      Tabla[3,i]=as.character(C$F.Emision[which(names(Tabla)[i]==C$Nombre)])
    }
    
    #relleno fecha Vencimiento
    for(i in 1:ncol(Tabla)){
      Tabla[4,i]=as.character(C$F.Vencimiento[which(names(Tabla)[i]==C$Nombre)])
    }
    
    #relleno cupÃ³n
    for(i in 1:ncol(Tabla)){
      Tabla[5,i]=C$Cupon[which(names(Tabla)[i]==C$Nombre)]/100
    }
    
    #relleno fecha ultimo pago
    for(i in 1:ncol(Tabla)){
      Tabla[7,i]=as.character(C$`Pago cupon 1`[which(names(Tabla)[i]==C$Nombre)])
    }
    
    #relleno proximo pago
    for(i in 1:ncol(Tabla)){
      Tabla[8,i]=as.character(C$`Pago cupon 2`[which(names(Tabla)[i]==C$Nombre)])
    }
    
    #aÃ±ado precios promedios
    Tabla[6,]=pr
    
    #rendimiento
    for(i in 1:ncol(Tabla)){
      Tabla[9,i]=bond.yield(as.Date(fe,format="%d/%m/%Y"),as.Date(Tabla[4,i],"%d/%m/%Y"),as.numeric(gsub("[,]",".",Tabla[5,i])), 4,as.numeric(gsub("[,]",".",Tabla[6,i])),convention = c("ACT/360"),4)
    }
    
    #duracion
    for(i in 1:ncol(Tabla)){
      Tabla[10,i]=bond.duration(as.Date(fe,format="%d/%m/%Y"),as.Date(Tabla[4,i],"%d/%m/%Y"),as.numeric(gsub("[,]",".",Tabla[5,i])), 4,as.numeric(gsub("[,]",".",Tabla[9,i])),convention = c("ACT/360"),4)
    }
    
    #aÃ±ado inverso duracion
    Tabla[11,]=1/(as.numeric(gsub("[,]",".",Tabla[10,])))
    
    #aÃ±ado ponderacion
    for(i in 1:ncol(Tabla)){
      Tabla[12,i]=(as.numeric(gsub("[,]",".",Tabla[11,i])))/sum((as.numeric(gsub("[,]",".",Tabla[11,]))))
    }
    
    #relleno precios
    Tabla[13,]=as.numeric(Pr_tit_veb[,2]) 
    
    #relleno residuos al cuadrado
    
    for(i in 1:ncol(Tabla)){
      Tabla[14,i]=(((as.numeric(gsub("[,]",".",Tabla[13,i])))-(as.numeric(gsub("[,]",".",Tabla[6,i]))))*(as.numeric(gsub("[,]",".",Tabla[12,i]))))^2
    }
    
    #SRC
    print("EL SRC es")
    print(sum(as.numeric(gsub("[,]",".",Tabla[14,]))))
    
    Pr_tit_veb <- rbind.data.frame(Pr_tit_veb,sum(as.numeric(gsub("[,]",".",Tabla[14,]))))
    #rownames(Pr_tit_veb)[length(Pr_tit_veb[,1])] <- "SRC"
    
    res_veb <- list(Pr_tit_veb,candidatos[,c(2,3,6,7,12,13,15,17,18)],letra,spline1,Tabla) 
    
    return(res_veb)
    
    
  }#final if
  
  
}#final funcion Tabla.splines

#esta funcion me retorna una lista de 5 elementos
#1: Precios calculados
#2: data frame de candidatos
#3: letra
#4: spline
#5: Tabla de resultados


#data_completa <- read.csv("C:/Users/Freddy Tapia/Desktop/Avances Tesis/Data_splines.txt", sep="")
data_completa <- read.csv(paste(getwd(),"Scripts_Data/Data_splines.txt",sep = "/"), sep="")
data_completa$Fecha.op <- as.Date( as.character(data_completa$Fecha.op))

#
tit=c("TIF082018","TIF042019","TIF112019",
      "TIF022021","TIF032022","TIF042023","TIF012024","TIF032028","TIF052028",
      "TIF022029","TIF022030","TIF032031","TIF022032","TIF032032","TIF032033")

C=Carac("Scripts_Data/09-03-2018.xls")
#mientras tanto trabajare con esta
#C=Carac("Scripts_Data/27-02-2018.xls")
#trabajar con este mientras tanto
#para tif replique resultados
#para veb hay pequeñas diferencias, supongo q se debe a cambio de cupon
#C=Carac("Scripts_Data/27-03-2018.xls")

#Svensson
#source('C:/Users/Freddy Tapia/Desktop/Svensson/orden data frame.R')
#source('C:/Users/Freddy Tapia/Desktop/Svensson/fprecios022T.R')

#cargo librerias a usar
library(jrvFinance)
#library(splines)
#library(WriteXLS)
#library(xlsx)
library(nloptr)
library(alabama)
options(OutDec = ",")

################################
######### SVENSSON  ############
################################
#funcion rend cero cupon
#funcion que calcula el rendimiento cero cupon 
#mediante la met Svensson, para unos parametros dados
#y un tiempo t especifico que se deriva de la fecha de pago de cupon
#ARGUMENTOS
#pa: vector de 6 parametros B0, B1, B2, B3, T1 y T2
#los cuales estan sujetos a ciertas restricciones
#B0 > 0
#B0 + B1 > 0
#T1 > 0, T2 > 0
#t: tiempo 
sven=function(pa,t){
  r=pa[1]+((pa[2]+pa[3])*(1-exp((-t)/(pa[5])))/(t/pa[5]))-
    pa[3]*exp(-t/pa[5])+(pa[4]*(1-exp((-t)/(pa[6])))/(t/pa[6]))-
    pa[4]*exp(-t/pa[6])
  return(r)
}

#funcion precios estimados
#Calcula el precio de los titulos considerados mediante 
#la metodologia de Svensson
#ARGUMENTOS
#tit: t????tulo o t????tulos a considerar, debe ser el nombre corto
#ej: TIF082018 (OJO, mejor considerar el ISIN)
#fv: fecha de valoraciÃ³n, ej: "11/08/2017"
#C: documento de las caracteristicas, que previamente ya ha sido
#leido mediante la funcion "Carac"
#pa: parametros Svensson
precio.sven=function(tit,fv,C,pa){
  #creo variable vacia
  Pr=c()
  
  for(j in 1:length(tit)){
    #verifico en que posicion de la variable c se encuentra el i-esimo titulo
    (n=which(C$Nombre==tit[j]))
    #Extraigo la proxima fecha de pago de cupon (pago cupon 1, fecha inicial
    #cuando el  titulo debe pagar cupon)
    (n1=as.Date(C$`Pago cupon 1`[n],format="%d/%m/%Y"))
    
    #verifico si esta fecha es menor que la fecha de valoracion,
    #de ser asi tomo la fecha de cupon 2 (fecha final cuando el titulo debe 
    #pagar cupon)
    if(as.numeric(n1-fv)<0){
      (n1=as.Date(C$`Pago cupon 2`[n],format="%d/%m/%Y"))
    }
    
    #extraigo la fecha de vencimiento del i-esimo titulo
    (n2=as.Date(C$F.Vencimiento[n],format="%d/%m/%Y"))
    
    #creo variable para determinar diferencia entre fecha de vencimiento y
    #la fecha cupon
    (n3=as.numeric(n2-n1))
    
    #si este valor es cero, entonces en este
    #caso queda 1 solo cupon
    if(n3==0){
      #print("Al titulo")
      #print(tit[j])
      #print("Le queda un cupon por pagar")
      #reasigno variable
      (n4=n1)
      #creo vector de fechas
      (n6=c(fv,n4))
      
      #valor t aÃ±os
      #creo vector de tiempos
      (f1=c(0.000000001,(as.numeric(n6[2]-n6[1])/360)+0.000000001))
      
      #rendimiento cero cupon
      #calculo rendimientos cero cupon, para cada tiempo del vector creado
      #anteriormente
      (pa1=c(sven(pa,f1[1]),sven(pa,f1[2])))
      
      #exp del producto
      (ep=exp(-pa1*f1))
      
      #cupon
      cu=c(0,100+C$Cupon[n]/4)
      
      #precio
      #calculo precio usando una suma producto
      (Pr[j]=sum(cu*ep))
      #return(Pr)
    }else{ #final if 1 cupon
      #reasigno variable
      n4=n1
      
      #creo variable,  que me determina cuantos cupones le queda por pagar al
      #i-esimo titulo
      n5=rep(91,(n3/91))
      #creo vector de fechas
      for(i in 1:(n3/91)){
        n4=unique(c(n4,n4+n5[i]))
      }
      
      #depuro vector de fechas
      (n5=unique(c(fv,n4)))
      
      #valor t aÃ±os
      #creo vector de tiempos
      ti=0.000000001
      (f1=rep(0,length(n5)))
      f1[1]=ti
      
      for(i in 2:length(n5)){
        f1[i]=(as.numeric(n5[i]-n5[i-1])/360)+f1[i-1]
      }
      
      #calculo rend cero cupon
      (pa1=rep(0,length(n5)))
      
      for(i in 1:length(n5)){
        pa1[i]=sven(pa,f1[i])
      }
      
      #exponencial del producto
      (ep=exp(-pa1*f1))
      
      
      #cupon 
      (cu=rep(0,length(n5)))
      for(i in 2:(length(n5)-1)){
        cu[i]=C$Cupon[n]/4
      }
      cu[length(n5)]=C$Cupon[n]/4+100
      
      
      #PRECIO ESTIMADO
      (n10=sum(cu*ep))
      
      Pr[j]=n10
    } 
    
  }#final for 
  
  #retorno precios
  return(Pr)
  
}#final funcion precios estimados


#Funcion que calcula precios de los titulos considerados
#y exporta una tabla con los resultados, donde es posible 
#optimizar los precios dados inicialmente, de tal manera
#que se asemejen los mÃ¡s posible a los precios promedio ingresados
#ARGUMENTOS
#fv: fecha de valoraciÃ³n, ej: "11/08/2017"
#tit: t????tulo o t????tulos a considerar, debe ser el nombre corto
#ej: TIF082018 (OJO, mejor considerar el ISIN)
#pr: vector de precios promedios
#pa: parametros Svensson
#ind: 0 = Tif o 1 = veb
#C: documento de las caracteristicas, que previamente ya ha sido
#leido mediante la funcion "Carac"
#fe2: valor que indica si se optimizaran los precios, 1 (Si) 0 (No)
#fe3: valido solo si se optimizan precios, 1 (paquete Nloptr) 0 (alabama)
Tabla.sven=function(fv,tit,pr,pa,ind,C,fe2,fe3){
  #Creo data frame donde guardare calculo
  Tabla=as.data.frame(matrix(0,14,length(tit)))
  colnames(Tabla)=tit
  rownames(Tabla)=c("ISIN","Fecha de LiquidaciÃ³n",
                    "Fecha de emisiÃ³n","Fecha de Vencimiento","Tasa de CupÃ³n",
                    "Precio Prom","Fecha Ãºltimo Pago","Fecha prÃ³ximo pago",
                    "RDTO al VMTO","DuraciÃ³n","Inverso de la duraciÃ³n",
                    "PonderaciÃ³n","Precio Modelo Svensson Ajustado",
                    "Residuos al cuadrado")
  
  #relleno ISIN
  for(i in 1:ncol(Tabla)){
    Tabla[1,i]=as.character(C$Sicet[which(names(Tabla)[i]==C$Nombre)])
  }
  
  #relleno fecha LiquidaciÃ³n
  for(i in 1:ncol(Tabla)){
    Tabla[2,i]=fv
  }
  
  #relleno fecha Emision
  for(i in 1:ncol(Tabla)){
    Tabla[3,i]=as.character(C$F.Emision[which(names(Tabla)[i]==C$Nombre)])
  }
  
  #relleno fecha Vencimiento
  for(i in 1:ncol(Tabla)){
    Tabla[4,i]=as.character(C$F.Vencimiento[which(names(Tabla)[i]==C$Nombre)])
  }
  
  #relleno cupÃ³n
  for(i in 1:ncol(Tabla)){
    Tabla[5,i]=C$Cupon[which(names(Tabla)[i]==C$Nombre)]/100
  }
  
  #relleno fecha ultimo pago
  for(i in 1:ncol(Tabla)){
    Tabla[7,i]=as.character(C$`Pago cupon 1`[which(names(Tabla)[i]==C$Nombre)])
  }
  
  #relleno proximo pago
  for(i in 1:ncol(Tabla)){
    Tabla[8,i]=as.character(C$`Pago cupon 2`[which(names(Tabla)[i]==C$Nombre)])
  }
  
  #aÃ±ado precios promedios
  Tabla[6,]=pr
  
  if(ind==0){
    #aÃ±ado rendimiento al vencimiento y duracion
    
    #rendimiento
    for(i in 1:ncol(Tabla)){
      Tabla[9,i]=bond.yield(as.Date(fv,format="%d/%m/%Y"),as.Date(Tabla[4,i],"%d/%m/%Y"),as.numeric(gsub("[,]",".",Tabla[5,i])), 4,as.numeric(gsub("[,]",".",Tabla[6,i])),convention = c("ACT/360"),4)
    }
    
    #verifico si rendimiento es negativo
    
    while(length(which(Tabla[9,]<0))!=0){
      print("Existe rendimiento negativo")
      print("En las posiciones")
      print(which(Tabla[9,]<0))
      
      #pido ingresar nuevos valores para las posiciones indicadas
      print("Favor Ingresar los")
      print(length(which(Tabla[9,]<0)))
      print("precios promedio nuevos")
      vt=c()
      for(i in 1:length(which(Tabla[9,]<0))){
        vt[i] <- as.numeric(readline(prompt="Ej: 101.05,  "))
      }
      
      #sustituyo precios promedio
      Tabla[6,which(Tabla[9,]<0)]=vt
      
      #calculo de nuevo los rendimientos
      for(i in 1:ncol(Tabla)){
        Tabla[9,i]=bond.yield(as.Date(fv,format="%d/%m/%Y"),as.Date(Tabla[4,i],"%d/%m/%Y"),as.numeric(gsub("[,]",".",Tabla[5,i])), 4,as.numeric(gsub("[,]",".",Tabla[6,i])),convention = c("ACT/360"),4)
      }
      
    }#final if rend negativo
    
    
    
    #duracion
    for(i in 1:ncol(Tabla)){
      Tabla[10,i]=bond.duration(as.Date(fv,format="%d/%m/%Y"),as.Date(Tabla[4,i],"%d/%m/%Y"),as.numeric(gsub("[,]",".",Tabla[5,i])), 4,as.numeric(gsub("[,]",".",Tabla[9,i])),convention = c("ACT/360"),4)
    }
    
    #aÃ±ado inverso duracion
    Tabla[11,]=1/(as.numeric(gsub("[,]",".",Tabla[10,])))
    
    #aÃ±ado ponderacion
    for(i in 1:ncol(Tabla)){
      Tabla[12,i]=(as.numeric(gsub("[,]",".",Tabla[11,i])))/sum((as.numeric(gsub("[,]",".",Tabla[11,]))))
    }
    
    #print("Muestro tabla preliminar")
    
    #CALCULO PRECIOS ESTIMADOS
    fv=as.Date(fv,format="%d/%m/%Y")
    
    #relleno precios
    Tabla[13,]=precio.sven(tit,fv,C,pa)
    
    #relleno residuos al cuadrado
    
    for(i in 1:ncol(Tabla)){
      Tabla[14,i]=(((as.numeric(gsub("[,]",".",Tabla[13,i])))-(as.numeric(gsub("[,]",".",Tabla[6,i]))))*(as.numeric(gsub("[,]",".",Tabla[12,i]))))^2
    }
    
    #SRC
    print("EL SRC inicial es")
    print(sum(as.numeric(gsub("[,]",".",Tabla[14,]))))
    
    #guardo src inicial
    q <- sum(as.numeric(gsub("[,]",".",Tabla[14,])))
    
    #View(Tabla)
    Tablainitif<<-Tabla
    
    #SOLVER
    #precio prom obs
    pp=(as.numeric(gsub("[,]",".",Tabla[6,])))
    w=(as.numeric(gsub("[,]",".",Tabla[12,])))
    
    #prueba paquete alabama
    mifuncion<- function(x){
      pa=c(x[1],x[2],x[3],x[4],x[5],x[6])
      p=precio.sven(tit,fv,C,pa) 
      x=sum(((p-pp)*w)^2)
      return(x)
    }
    
    #restricciones
    res <- function(x) {
      h <- rep(NA, 1)
      h[1] <- x[1]
      h[2] <- x[1]+x[2]
      h[3] <- x[5]
      h[4] <- x[6]
      h[5] <- x[2]+0.01
      h
    }
    
    #modifico esta linea de tal manera q le pase un valor
    #fe2 <- readline(prompt="Desea optimizar?   (1) Si, (0) No    ")
    if(fe2==1){
      #print("Por favor, seleccionar el paquete a usar: ")
      #fe3 <- readline(prompt="Seleccionar (1) para Nloptr, (0) para Alabama   ")
      
      if(fe3==1){
        #BroydenâFletcherâGoldfarbâShanno-metodo cuasi Newton
        #funcion paquete nloptr resultados mejor q el solver da mejor ajuste
        print("Optimizando mediante paquete Nloptr...")
        ala1=nloptr::auglag(pa, fn=mifuncion, hin=res,localsolver="LBFGS") #mejor igual al solver
        
        mes="NLOPT_FAILURE: Generic failure code."
        if(ala1$message==mes){
          print("Fallo en el metodo de Newton")
          print("optimizando mediante un metodo diferente...")
          ala1=nloptr::auglag(pa, fn=mifuncion, hin=res)
        }
        
        ala<<-ala1
        Tabla[13,]=precio.sven(tit,fv,C,ala1$par)
        
        #calculo nuevo src
        for(i in 1:ncol(Tabla)){
          Tabla[14,i]=(((as.numeric(gsub("[,]",".",Tabla[13,i])))-(as.numeric(gsub("[,]",".",Tabla[6,i]))))*(as.numeric(gsub("[,]",".",Tabla[12,i]))))^2
        }
        
        #veo si son iguales, en caso de ser cierto optimizo por otro metodo
        q1 <- sum(as.numeric(gsub("[,]",".",Tabla[14,])))
        
        if(q==q1){
          print("OPtimizaci?n fallida..")
          print("Optimizando por un m?todo diferente")
          ala1=nloptr::auglag(pa, fn=mifuncion, hin=res)
          ala<<-ala1
          Tabla[13,]=precio.sven(tit,fv,C,ala1$par)
        }
        #SRC Nuevo
        print("El nuevo valor del SRC es")
        print(sum(as.numeric(gsub("[,]",".",Tabla[14,]))))
        
        
      }#final if nloptr
      
      if(fe3==0){
        print("Optimizando mediante paquete alabama...")
        ala1=alabama::auglag(pa, fn=mifuncion, hin=res) #mejor igual al solver
        
        ala<<-ala1
        Tabla[13,]=precio.sven(tit,fv,C,ala1$par)
        
        #calculo nuevo src
        for(i in 1:ncol(Tabla)){
          Tabla[14,i]=(((as.numeric(gsub("[,]",".",Tabla[13,i])))-(as.numeric(gsub("[,]",".",Tabla[6,i]))))*(as.numeric(gsub("[,]",".",Tabla[12,i]))))^2
        }
        
        #veo si son iguales, en caso de ser cierto optimizo por otro metodo
        q1 <- sum(as.numeric(gsub("[,]",".",Tabla[14,])))
        
        if(q==q1){
          print("Optimizaci?n fallida..")
          # print("Optimizando por un m?todo diferente")
          # ala1=alabama::auglag(pa, fn=mifuncion, hin=res)
          # ala<<-ala1
          # Tabla[13,]=precio.sven(tit,fv,C,ala1$par)
        }
        
        #SRC Nuevo
        print("El nuevo valor del SRC es")
        print(sum(as.numeric(gsub("[,]",".",Tabla[14,]))))
        
        
      }#final if alabama
      
      
      
    }#final if si
    if(fe2==0){
      print("No se  optimizarÃ¡, se mantendrÃ¡n los mismos precios")
      #sustituyo precios estimados mas ajustados
      Tabla[13,]=precio.sven(tit,fv,C,pa)
    }
    
    #relleno residuos al cuadrado
    
    for(i in 1:ncol(Tabla)){
      Tabla[14,i]=(((as.numeric(gsub("[,]",".",Tabla[13,i])))-(as.numeric(gsub("[,]",".",Tabla[6,i]))))*(as.numeric(gsub("[,]",".",Tabla[12,i]))))^2
    }
    
    #creo vector de precios para exportar
    precios <- cbind.data.frame(c(tit,"SRC"),c(precio.sven(tit,fv,C,pa),sum(as.numeric(gsub("[,]",".",Tabla[14,])))))
    names(precios) <- c("TÃ?tulos","Precios")
    
    #if para exportar resultados
    if(fe2==1){
      Tabla1 <- list(Tabla,ala$par,precios)
    }else if(fe2==0){
      Tabla1 <- list(Tabla,pa,precios)
    }
    
    return(Tabla1)
  } #final if ind -tif
  
  #CASO VEBONOS
  if(ind==1){
    #rendimiento
    for(i in 1:ncol(Tabla)){
      Tabla[9,i]=bond.yield(as.Date(fv,format="%d/%m/%Y"),as.Date(Tabla[4,i],"%d/%m/%Y"),as.numeric(gsub("[,]",".",Tabla[5,i])), 4,as.numeric(gsub("[,]",".",Tabla[6,i])),convention = c("ACT/360"),4)
    }
    
    #verifico si rendimiento es negativo
    
    while(length(which(Tabla[9,]<0))!=0){
      print("Existe rendimiento negativo")
      print("En las posiciones")
      print(which(Tabla[9,]<0))
      
      #pido ingresar nuevos valores para las posiciones indicadas
      print("Favor Ingresar los")
      print(length(which(Tabla[9,]<0)))
      print("precios promedio nuevos")
      vt=c()
      for(i in 1:length(which(Tabla[9,]<0))){
        vt[i] <- as.numeric(readline(prompt="Ej: 101.05,  "))
      }
      
      #sustituyo precios promedio
      Tabla[6,which(Tabla[9,]<0)]=vt
      
      #calculo nuevos rendimientos
      #rendimiento
      for(i in 1:ncol(Tabla)){
        Tabla[9,i]=bond.yield(as.Date(fv,format="%d/%m/%Y"),as.Date(Tabla[4,i],"%d/%m/%Y"),as.numeric(gsub("[,]",".",Tabla[5,i])), 4,as.numeric(gsub("[,]",".",Tabla[6,i])),convention = c("ACT/360"),4)
      }
      #muestro tabla
      #View(Tabla)
      
    }#final if rend negativo
    
    
    #duracion
    for(i in 1:ncol(Tabla)){
      Tabla[10,i]=bond.duration(as.Date(fv,format="%d/%m/%Y"),as.Date(Tabla[4,i],"%d/%m/%Y"),as.numeric(gsub("[,]",".",Tabla[5,i])), 4,as.numeric(gsub("[,]",".",Tabla[9,i])),convention = c("ACT/360"),4)
    }
    
    #aÃ±ado inverso duracion
    Tabla[11,]=1/(as.numeric(gsub("[,]",".",Tabla[10,])))
    
    #aÃ±ado ponderacion
    for(i in 1:ncol(Tabla)){
      Tabla[12,i]=(as.numeric(gsub("[,]",".",Tabla[11,i])))/sum((as.numeric(gsub("[,]",".",Tabla[11,]))))
    }
    
    #print("Muestro tabla preliminar")
    
    #CALCULO PRECIOS ESTIMADOS
    fv=as.Date(fv,format="%d/%m/%Y")
    
    #relleno precios
    Tabla[13,]=precio.sven(tit,fv,C,pa)
    
    #relleno residuos al cuadrado
    
    for(i in 1:ncol(Tabla)){
      Tabla[14,i]=(((as.numeric(gsub("[,]",".",Tabla[13,i])))-(as.numeric(gsub("[,]",".",Tabla[6,i]))))*(as.numeric(gsub("[,]",".",Tabla[12,i]))))^2
    }
    
    #SRC
    print("EL SRC inicial es")
    print(sum(as.numeric(gsub("[,]",".",Tabla[14,]))))
    
    #guardo src inicial
    q <- sum(as.numeric(gsub("[,]",".",Tabla[14,])))
    
    #View(Tabla)
    
    TablainiVeb<<-Tabla
    
    #SOLVER
    #precio prom obs
    pp=(as.numeric(gsub("[,]",".",Tabla[6,])))
    w=(as.numeric(gsub("[,]",".",Tabla[12,])))
    
    #prueba paquete alabama
    mifuncion<- function(x){
      pa=c(x[1],x[2],x[3],x[4],x[5],x[6])
      p=precio.sven(tit,fv,C,pa) 
      x=sum(((p-pp)*w)^2)
      return(x)
    }
    
    #restricciones
    res <- function(x) {
      h <- rep(NA, 1)
      h[1] <- x[1]
      h[2] <- x[1]+x[2]
      h[3] <- x[5]
      h[4] <- x[6]
      h
    }
    
    #fe2 <- readline(prompt="Desea optimizar?   (1) Si, (0) No    ")
    if(fe2==1){
      
      #print("Por favor seleccionar un paquete para optimizar")
      #fe3 <- readline(prompt="Seleccionar (1) para el paquete Nloptr, (0) para alabama    ")
      
      if(fe3==1){
        #BroydenâFletcherâGoldfarbâShanno-metodo cuasi Newton
        #funcion paquete nloptr resultados mejor q el solver da mejor ajuste
        print("Optimizando mediante paquete Nloptr...")
        ala1=nloptr::auglag(pa, fn=mifuncion, hin=res,localsolver="LBFGS") #mejor igual al solver
        
        mes="NLOPT_FAILURE: Generic failure code."
        if(ala1$message==mes){
          print("Fallo en el metodo de Newton")
          print("optimizando mediante un metodo diferente...")
          ala1=nloptr::auglag(pa, fn=mifuncion, hin=res)
        }
        
        ala<<-ala1
        Tabla[13,]=precio.sven(tit,fv,C,ala1$par)
        
        #calculo nuevo src
        for(i in 1:ncol(Tabla)){
          Tabla[14,i]=(((as.numeric(gsub("[,]",".",Tabla[13,i])))-(as.numeric(gsub("[,]",".",Tabla[6,i]))))*(as.numeric(gsub("[,]",".",Tabla[12,i]))))^2
        }
        
        #veo si son iguales, en caso de ser cierto optimizo por otro metodo
        q1 <- sum(as.numeric(gsub("[,]",".",Tabla[14,])))
        
        if(q==q1){
          print("OPtimizaci?n fallida..")
          print("Optimizando por un m?todo diferente")
          ala1=nloptr::auglag(pa, fn=mifuncion, hin=res)
          ala<<-ala1
          Tabla[13,]=precio.sven(tit,fv,C,ala1$par)
        }
        #SRC Nuevo
        print("El nuevo valor del SRC es")
        print(sum(as.numeric(gsub("[,]",".",Tabla[14,]))))
        
      }#final if nloptr
      
      if(fe3==0){
        print("Optimizando mediante paquete alabama...")
        ala1=alabama::auglag(pa, fn=mifuncion, hin=res) #mejor igual al solver
        
        ala<<-ala1
        Tabla[13,]=precio.sven(tit,fv,C,ala1$par)
        
        #calculo nuevo src
        for(i in 1:ncol(Tabla)){
          Tabla[14,i]=(((as.numeric(gsub("[,]",".",Tabla[13,i])))-(as.numeric(gsub("[,]",".",Tabla[6,i]))))*(as.numeric(gsub("[,]",".",Tabla[12,i]))))^2
        }
        
        #veo si son iguales, en caso de ser cierto optimizo por otro metodo
        q1 <- sum(as.numeric(gsub("[,]",".",Tabla[14,])))
        
        if(q==q1){
          print("Optimizaci?n fallida..")
          # print("Optimizando por un m?todo diferente")
          # ala1=alabama::auglag(pa, fn=mifuncion, hin=res)
          # ala<<-ala1
          # Tabla[13,]=precio.sven(tit,fv,C,ala1$par)
        }
        #SRC Nuevo
        print("El nuevo valor del SRC es")
        print(sum(as.numeric(gsub("[,]",".",Tabla[14,]))))
        
        
      }#final if alabama
      
      
      ala<<-ala1
    }#final if pregunta optimizar si
    
    if(fe2==0){
      print("No se optimizara")
      Tabla[13,]=precio.sven(tit,fv,C,pa)
    }
    
    #relleno residuos al cuadrado
    
    for(i in 1:ncol(Tabla)){
      Tabla[14,i]=(((as.numeric(gsub("[,]",".",Tabla[13,i])))-(as.numeric(gsub("[,]",".",Tabla[6,i]))))*(as.numeric(gsub("[,]",".",Tabla[12,i]))))^2
    }
    
    #creo vector de precios para exportar
    precios <- cbind.data.frame(c(tit,"SRC"),c(precio.sven(tit,fv,C,pa),sum(as.numeric(gsub("[,]",".",Tabla[14,])))))
    names(precios) <- c("TÃ?tulos","Precios")
    
    
    #if para exportar resultados
    if(fe2==1){
      Tabla1 <- list(Tabla,ala$par,precios)
    }else if(fe2==0){
      Tabla1 <- list(Tabla,pa,precios)
    }
    
    return(Tabla1)
    
  }#final if ind vebono
  
  
  
}#Final funcion excel-sven

#Exporta una lista de tres elementos
#1: Tabla de resultados
#2: ParÃ¡metros optimizados
#3: precios calculados, la ultima fila me trae el valor del SRC

###############################



tit=c("TIF082018","TIF042019","TIF082019",
      "TIF112019","TIF102020","TIF112020","TIF022021","TIF032022","TIF042023",
      "TIF012024","TIF062025","TIF012026","TIF112027","TIF032028","TIF052028",
      "TIF022029","TIF032029","TIF022030","TIF102030","TIF022031","TIF032031",
      "TIF022032","TIF032032","TIF032033","TIF052034")

#VEBONOS iniciales
tit1=c("VEBONO072018","VEBONO022019","VEBONO032019","VEBONO042019","VEBONO102019","VEBONO012020",
       "VEBONO062020","VEBONO092020","VEBONO112020","VEBONO012021","VEBONO052021",
       "VEBONO122021","VEBONO022022","VEBONO012023","VEBONO022024","VEBONO042024",
       "VEBONO012025","VEBONO022025","VEBONO062026","VEBONO032027","VEBONO042028",
       "VEBONO102028","VEBONO052029","VEBONO102029","VEBONO072030","VEBONO032031",
       "VEBONO062032","VEBONO072033","VEBONO022034")

pr=c(101,112,110,121.0234,116.5251,130.0234,
     129.0156,125.0626,128.1000,120,124,122,126.5234,128.5235,128.1913,
     129,132.0391,128.5235,129.8875,130.1,128.5313,127,128.5235,127.0156,
     127.0156)


#precios 11-08 - veb
pr1=c(100.4,106,110,111,118,121,
      127.8376,102.2,117,130.3269,127,129.45,129,129.9627,128,129,129.1875,
      128.5,102,129.9469,129.6807,130.0156,125.0313,125.75,130.5,129.5235,
      128.5313,130,128.0235)



#Pruebas
#hay q realizar las nuevas pruebas con una fecha mas reciente
#Comparacion Tif
t <- Tabla.splines(data = data_completa,tipo = "TIF",fe=as.Date("2018-03-08"),num = 40,par = 0.2,tit,C,pr=pr)

#precios
t[[1]]

#veo grafica
cand <- t[[2]]
letra <- t[[3]]
spline <- t[[4]]

plot(c(letra$Plazo,cand$Plazo),c(letra$Rendimiento,cand$Rendimiento))

plot(seq(1,20*365,1),predict(spline,seq(1,20*365,1))$y)

#calculo precios por metodología Svensson

t1 <- Tabla.sven(fv=as.Date("2018-03-08"),tit = tit,pr = pr,pa = c(1,1,1,1,1,1),ind = 0,C=C,fe2 = 1,fe3 = 0)

#tabla
t2 <- t1[[1]]
as.numeric(t2[13,])

#parametros optimizados
t1[[2]]

#precios y SRC, hay algo raro
t1[[3]]

#
t3 <- Tabla.sven(fv=as.Date("2018-03-08"),tit = tit,pr = pr,pa =t1[[2]] ,ind = 0,C=C,fe2 = 0,fe3 = 0)
t3 <- t3[[3]]

#svensson no optimizado
pa_sven=c(0.133799434790145,-0.01,-0.307885339616438,-0.134075672659356,
          0.545398124008073,0.350692201663154)
t5 <- Tabla.sven(fv=as.Date("2018-03-08"),tit = tit,pr = pr,pa = pa_sven,ind = 0,C=C,fe2 = 0,fe3 = 0)
t5 <- t5[[3]]

#tabla comparativa para fecha 2018-03-08
t4 <- cbind.data.frame(t[[1]],t5[,2],t3[,2],c(pr,0))

names(t4) <- c("Titulos","Splines","Svensson no opt","Svensson opt","Promedio")
t4$Titulos <- as.character(t4$Titulos)
t4[26,1] <- "SRC"

#tabla comparativa para tif para la fecha 2018-03-08 para los titulos tit
#write.table(t4,"Tabla_tif_comparacion.txt")

#comparacion Vebonos
v <- Tabla.splines(data = data_completa,tipo = "VEBONO",fe=as.Date("2018-03-08"),num = 40,par = 0.2,tit1,C,pr=pr1)

#precios
v[[1]]

#veo grafica
cand_v <- v[[2]]
letra_v <- v[[3]]
spline_v <- v[[4]]

plot(c(letra_v$Plazo,cand_v$Plazo),c(letra_v$Rendimiento,cand_v$Rendimiento))

plot(seq(1,20*365,1),predict(spline_v,seq(1,20*365,1))$y)

#calculo precios por metodología Svensson

v1 <- Tabla.sven(fv=as.Date("2018-03-08"),tit = tit1,pr = pr1,pa = c(1,1,1,1,1,1),ind = 0,C=C,fe2 = 1,fe3 = 0)

#tabla
v2 <- v1[[1]]
as.numeric(v2[13,])

#parametros optimizados
v1[[2]]

#precios y SRC, hay algo raro
v1[[3]]

#
v3 <- Tabla.sven(fv=as.Date("2018-03-08"),tit = tit1,pr = pr1,pa =v1[[2]] ,ind = 0,C=C,fe2 = 0,fe3 = 0)
v3 <- v3[[3]]



#parametro sven
pa1_sven=c(0.135872169451391,0.1,-0.503768911829894,-0.288755056029301,
           0.11951691203874,0.501729233062216)

v5 <- Tabla.sven(fv=as.Date("2018-03-08"),tit = tit1,pr = pr1,pa = pa1_sven,ind = 0,C=C,fe2 = 0,fe3 = 0)
v5 <- v5[[3]]

#tabla comparativa para fecha 2018-03-08
v4 <- cbind.data.frame(v[[1]],v5[,2],v3[,2],c(pr1,0))

names(v4) <- c("Titulos","Splines","Svensson no opt","Svensson opt","Promedio")
v4$Titulos <- as.character(v4$Titulos)
v4[30,1] <- "SRC"

#tabla comparativa para tif para la fecha 2018-03-08 para los titulos tit
#write.table(v4,"Tabla_veb_comparacion.txt")

#genero graficas a partir de la informacion anterior

#SPLINES
#TIF
x_t <- seq(1,20*365,1)
y_t <- predict(spline,seq(1,20*365,1))$y

ggplot(data.frame(x=c(letra$Plazo,cand[,4]),y=c(letra$Rendimiento,cand[,7])), aes(x=x, y=y)) +
  geom_point()+
  geom_line(data =data.frame(x=x_t,y=y_t),col="blue")+
  ylab("Rendimiento (%)")+
  xlab("Maduración (días)")+
  ggtitle("Curva de rendimiento Splines TIF")+
  theme(
    plot.title = element_text(color="black", size=14, face="bold.italic",hjust = 0.5)
  )


#VEBONO

#COMPARATIVO
#TIF
library(plotly)

x <- seq(1,20,0.1)
y_sven <- sven(x,pa=pa_sven)*100
y_sven_opt <- sven(x,pa=t1[[2]])*100
y_sp <- predict(spline,x*365)$y

#dataframe
data <- data.frame(x,y_sven,y_sven_opt,y_sp)
d_sven <- data.frame(x,y_sven)
d_sven$met <- "Svensson"
d_sven_opt <- data.frame(x,y_sven_opt)
d_sven_opt$met <- "Svensson Optimizado"
d_sp <-  data.frame(x,y_sp)
d_sp$met <- "Splines"

names(d_sven) <-  c("x","y","met")
names(d_sven_opt) <-  c("x","y","met")
names(d_sp) <- c("x","y","met")
data1 <- rbind.data.frame(d_sven,d_sven_opt,d_sp)

#curva 
#defino nombres de ejes
f <- list(
  family = "Courier New, monospace",
  size = 18,
  color = "#7f7f7f"
)
x1 <- list(
  title = "Maduración (días)",
  titlefont = f
)
y1 <- list(
  title = "Rendimientos (%)",
  titlefont = f
)

#

ggplot(data1,aes(x=x,y=y,colour=met,group=met))+
  geom_line() +  xlab("Maduración (días)")+
  ylab("Rendimiento (%)")+
  ggtitle("Comparativo TIF Splines vs Svensson")+
  theme(
    plot.title = element_text(color="black", size=14, face="bold.italic",hjust = 0.5)
  )




#VEBONOS

x <- seq(1,20,0.1)
y_sven_v <- sven(x,pa=pa1_sven)*100
y_sven_opt_v <- sven(x,pa=v1[[2]])*100
y_sp_v <- predict(spline_v,x*365)$y

#dataframe
data_v <- data.frame(x,y_sven_v,y_sven_opt_v,y_sp_v)
d_sven_v <- data.frame(x,y_sven_v)
d_sven_v$met <- "Svensson"
d_sven_opt_v <- data.frame(x,y_sven_opt_v)
d_sven_opt_v$met <- "Svensson Optimizado"
d_sp_v <-  data.frame(x,y_sp_v)
d_sp_v$met <- "Splines"

names(d_sven_v) <-  c("x","y","met")
names(d_sven_opt_v) <-  c("x","y","met")
names(d_sp_v) <- c("x","y","met")
data1_v <- rbind.data.frame(d_sven_v,d_sven_opt_v,d_sp_v)

#curva 
#defino nombres de ejes
f <- list(
  family = "Courier New, monospace",
  size = 18,
  color = "#7f7f7f"
)
x1 <- list(
  title = "Maduración (días)",
  titlefont = f
)
y1 <- list(
  title = "Rendimientos (%)",
  titlefont = f
)

#

ggplot(data1_v,aes(x=x,y=y,colour=met,group=met))+
  geom_line() +  xlab("Maduración (días)")+
  ylab("Rendimiento (%)")+
  ggtitle("Comparativo VEBONO Splines vs Svensson")+
  theme(
    plot.title = element_text(color="black", size=14, face="bold.italic",hjust = 0.5)
  )


