#prueba calculo de rendimiento usando diferentes bases
#en la funcion formato
#funciones necesarias

source('~/.Trash/Riesgo-de-Mercado/app/funciones.R')

#primero debo leer el doc de la 0-22 y las caracteristicas
#para luego aplicar la funcion formato q calcula el rendimiento

ca <- try(Preciosbcv(paste(getwd(),"app","data","0-22.xls",sep = "/")))
ca1 <- try(Carac(paste(getwd(),"app","data","Caracteristicas.xls",sep = "/")))

#funcion que usa convencion ACT/360
ca2 <- formatop(ca1,ca)
#convierto fecha de op y venc en fechas
ca2$`Fecha op` <- as.Date(as.character(ca2$`Fecha op`),format="%d/%m/%Y")
ca2$F.Vencimiento <- as.Date(as.character(ca2$F.Vencimiento),format="%d/%m/%Y")

#este data frame es el que utiliza la metodologia Spline para los calculos
ca3 <- dplyr::arrange(ca2,(`Fecha op`))


#................................
#funcion formato nueva
#Funcion Formato
#funcion que toma la el documento de las caracteristicas informacion 
#necesaria para darle formato al data frame b6
formatop1=function(C3,b3){
  
  #en caso de Haber letras pongo cupon 0
  if(length(which(substr(b3$Instrumento,1,3)=="LTB"))!=0){
    b3$'Cupón....'[which(substr(b3$Instrumento,1,3)=="LTB")]=0
  }
  
  #traigo la fecha de vencimiento
  b3$Vencimiento=as.character(b3$Vencimiento)
  
  for(i in 1:length(b3$b3.)){
    if(substr(b3$Instrumento[i],1,3)!="LTB"){
      #which(as.character(b3$Instrumento[i])==as.character(c3$Sicet))
      b3$Vencimiento[i]=C3$F.Vencimiento[which(as.character(b3$Instrumento[i])==as.character(C3$Sicet))]
    }else{ 
      b3$Vencimiento[i]=as.character(format(as.Date("1900-01-01")+as.numeric(b3$Vencimiento[i])-2,"%d/%m/%Y"))
    }
  }
  
  #traigo si es TIF , VEBONO o TICC
  b3.=cbind(as.character(rep("TIF",1,length(b3$b3.))),b3)
  b3.$`as.character(rep("TIF", 1, length(b3$b3.)))`=as.character(b3.$`as.character(rep("TIF", 1, length(b3$b3.)))`)
  
  for(i in 1:length(b3.$b3.)){
    if(substr(b3$Instrumento[i],1,3)!="LTB"){
      b3.$`as.character(rep("TIF", 1, length(b3$b3.)))`[i]=as.character(C3$`Tipo Instrumento`[which(as.character(b3.$Instrumento[i])==as.character(C3$Sicet))])
    }else{
      b3.$`as.character(rep("TIF", 1, length(b3$b3.)))`[i]="LETRA"
      
    }
  }
  
  #fuente
  p=as.character(rep("022 BCV",1,length(b3.$b3.)))
  
  B3=cbind(b3.[,c(1,2)],p,b3.[,3:ncol(b3.)])
  
  #traigo columna Nombre de las caracteristicas
  nom=c()
  
  for(i in 1:nrow(B3)){
    if(substr(b3$Instrumento[i],1,3)!="LTB"){
      nom[i]=as.character(C3$Nombre[which(as.character(B3$Instrumento[i])==C3$Sicet)])
    }else{
      n1=substr(b3.$Vencimiento[i],4,5)
      n2=substr(b3.$Vencimiento[i],7,10)
      
      nom[i]=paste("LTB",n1,n2,sep="")
    }
  }
  
  #frecuencia y rendimiento
  fre=rep(0,1,length(b3.$b3.))
  rend=rep(0,1,length(b3.$b3.))
  
  B3=cbind(B3[,1],nom,B3[,2:ncol(B3)],fre,rend)
  names(B3)=c("Tipo Instrumento","Nombre","Fecha op","Fuente","Sicet","F.Vencimiento","Plazo","Op","Monto","Pre min","Pre max","Pre prom","Cupon","Frecuencia","Rendimiento")
  
  #añado criterio de que si en caraceristica dice 91 dias es freq=4 si es 182 freq=2
  for(i in 1:length(B3$`Tipo Instrumento`)){
    if(substr(B3$Sicet[i],1,3)!="LTB"){
      if(substr(as.character(C3$Inicio[which(as.character(B3$Sicet[i])==C3$Sicet)]),6,8)=="91 "){
        B3$Frecuencia[i]=4
      }
      if(substr(as.character(C3$Inicio[which(as.character(B3$Sicet[i])==C3$Sicet)]),6,8)=="180" |substr(as.character(C3$Inicio[which(as.character(B3$Sicet[i])==C3$Sicet)]),6,8)=="182"){
        B3$Frecuencia[i]=2
      }
    }
  }
  
  #añado rendimiento que es la formula RENDTO de excel con base 0
  for(i in 1:nrow(B3)){
    if(substr(B3$Sicet[i],1,3)!="LTB"){
      #B3$Rendimiento=bond.yields(as.Date(B3$`Fecha op`,"%d/%m/%Y"),as.Date(B3$F.Vencimiento,"%d/%m/%Y"),(B3$Cupon/100), B3$Frecuencia,B3$`Pre prom`,convention = c("30/360"),B3$Frecuencia)*100
      B3$Rendimiento[i]=bond.yield(as.Date(B3$`Fecha op`[i],"%d/%m/%Y"),as.Date(B3$F.Vencimiento[i],"%d/%m/%Y"),(B3$Cupon[i]/100), B3$Frecuencia[i],B3$`Pre prom`[i],convention = c("30/360"),B3$Frecuencia[i])*100
    }
    if(substr(B3$Sicet[i],1,3)=="LTB"){
      #B3$Rendimiento=bond.yields(as.Date(B3$`Fecha op`,"%d/%m/%Y"),as.Date(B3$F.Vencimiento,"%d/%m/%Y"),(B3$Cupon/100), B3$Frecuencia,B3$`Pre prom`,convention = c("30/360"),B3$Frecuencia)*100
      B3$Rendimiento[i]=bond.yield(as.Date(B3$`Fecha op`[i],"%d/%m/%Y"),as.Date(B3$F.Vencimiento[i],"%d/%m/%Y"),0, 0.001,B3$`Pre prom`[i],convention = c("30/360"),0)*100
    }
  }
  
  return(B3)
}#final funcion formato precios 022

#calculo rend de manera dif, uso convencion 30/360
ca2_1 <- formatop1(ca1,ca)
#convierto fecha de op y venc en fechas
ca2_1$`Fecha op` <- as.Date(as.character(ca2_1$`Fecha op`),format="%d/%m/%Y")
ca2_1$F.Vencimiento <- as.Date(as.character(ca2_1$F.Vencimiento),format="%d/%m/%Y")

#este data frame es el que utiliza la metodologia Spline para los calculos
ca3_1 <- dplyr::arrange(ca2_1,(`Fecha op`))

#otra funcion
formatop2=function(C3,b3){
  
  #en caso de Haber letras pongo cupon 0
  if(length(which(substr(b3$Instrumento,1,3)=="LTB"))!=0){
    b3$'Cupón....'[which(substr(b3$Instrumento,1,3)=="LTB")]=0
  }
  
  #traigo la fecha de vencimiento
  b3$Vencimiento=as.character(b3$Vencimiento)
  
  for(i in 1:length(b3$b3.)){
    if(substr(b3$Instrumento[i],1,3)!="LTB"){
      #which(as.character(b3$Instrumento[i])==as.character(c3$Sicet))
      b3$Vencimiento[i]=C3$F.Vencimiento[which(as.character(b3$Instrumento[i])==as.character(C3$Sicet))]
    }else{ 
      b3$Vencimiento[i]=as.character(format(as.Date("1900-01-01")+as.numeric(b3$Vencimiento[i])-2,"%d/%m/%Y"))
    }
  }
  
  #traigo si es TIF , VEBONO o TICC
  b3.=cbind(as.character(rep("TIF",1,length(b3$b3.))),b3)
  b3.$`as.character(rep("TIF", 1, length(b3$b3.)))`=as.character(b3.$`as.character(rep("TIF", 1, length(b3$b3.)))`)
  
  for(i in 1:length(b3.$b3.)){
    if(substr(b3$Instrumento[i],1,3)!="LTB"){
      b3.$`as.character(rep("TIF", 1, length(b3$b3.)))`[i]=as.character(C3$`Tipo Instrumento`[which(as.character(b3.$Instrumento[i])==as.character(C3$Sicet))])
    }else{
      b3.$`as.character(rep("TIF", 1, length(b3$b3.)))`[i]="LETRA"
      
    }
  }
  
  #fuente
  p=as.character(rep("022 BCV",1,length(b3.$b3.)))
  
  B3=cbind(b3.[,c(1,2)],p,b3.[,3:ncol(b3.)])
  
  #traigo columna Nombre de las caracteristicas
  nom=c()
  
  for(i in 1:nrow(B3)){
    if(substr(b3$Instrumento[i],1,3)!="LTB"){
      nom[i]=as.character(C3$Nombre[which(as.character(B3$Instrumento[i])==C3$Sicet)])
    }else{
      n1=substr(b3.$Vencimiento[i],4,5)
      n2=substr(b3.$Vencimiento[i],7,10)
      
      nom[i]=paste("LTB",n1,n2,sep="")
    }
  }
  
  #frecuencia y rendimiento
  fre=rep(0,1,length(b3.$b3.))
  rend=rep(0,1,length(b3.$b3.))
  
  B3=cbind(B3[,1],nom,B3[,2:ncol(B3)],fre,rend)
  names(B3)=c("Tipo Instrumento","Nombre","Fecha op","Fuente","Sicet","F.Vencimiento","Plazo","Op","Monto","Pre min","Pre max","Pre prom","Cupon","Frecuencia","Rendimiento")
  
  #añado criterio de que si en caraceristica dice 91 dias es freq=4 si es 182 freq=2
  for(i in 1:length(B3$`Tipo Instrumento`)){
    if(substr(B3$Sicet[i],1,3)!="LTB"){
      if(substr(as.character(C3$Inicio[which(as.character(B3$Sicet[i])==C3$Sicet)]),6,8)=="91 "){
        B3$Frecuencia[i]=4
      }
      if(substr(as.character(C3$Inicio[which(as.character(B3$Sicet[i])==C3$Sicet)]),6,8)=="180" |substr(as.character(C3$Inicio[which(as.character(B3$Sicet[i])==C3$Sicet)]),6,8)=="182"){
        B3$Frecuencia[i]=2
      }
    }
  }
  
  #añado rendimiento que es la formula RENDTO de excel con base 0
  for(i in 1:nrow(B3)){
    if(substr(B3$Sicet[i],1,3)!="LTB"){
      #B3$Rendimiento=bond.yields(as.Date(B3$`Fecha op`,"%d/%m/%Y"),as.Date(B3$F.Vencimiento,"%d/%m/%Y"),(B3$Cupon/100), B3$Frecuencia,B3$`Pre prom`,convention = c("30/360"),B3$Frecuencia)*100
      B3$Rendimiento[i]=bond.yield(as.Date(B3$`Fecha op`[i],"%d/%m/%Y"),as.Date(B3$F.Vencimiento[i],"%d/%m/%Y"),(B3$Cupon[i]/100), B3$Frecuencia[i],B3$`Pre prom`[i],convention = c("ACT/ACT"),B3$Frecuencia[i])*100
    }
    if(substr(B3$Sicet[i],1,3)=="LTB"){
      #B3$Rendimiento=bond.yields(as.Date(B3$`Fecha op`,"%d/%m/%Y"),as.Date(B3$F.Vencimiento,"%d/%m/%Y"),(B3$Cupon/100), B3$Frecuencia,B3$`Pre prom`,convention = c("30/360"),B3$Frecuencia)*100
      B3$Rendimiento[i]=bond.yield(as.Date(B3$`Fecha op`[i],"%d/%m/%Y"),as.Date(B3$F.Vencimiento[i],"%d/%m/%Y"),0, 0.001,B3$`Pre prom`[i],convention = c("ACT/ACT"),0)*100
    }
  }
  
  return(B3)
}#final funcion formato precios 022

#calculo rend de manera dif, uso convencion ACT/ACT
ca2_2 <- formatop2(ca1,ca)
#convierto fecha de op y venc en fechas
ca2_2$`Fecha op` <- as.Date(as.character(ca2_2$`Fecha op`),format="%d/%m/%Y")
ca2_2$F.Vencimiento <- as.Date(as.character(ca2_2$F.Vencimiento),format="%d/%m/%Y")

#este data frame es el que utiliza la metodologia Spline para los calculos
ca3_2 <- dplyr::arrange(ca2_2,(`Fecha op`))

#otra funcion
formatop3=function(C3,b3){
  
  #en caso de Haber letras pongo cupon 0
  if(length(which(substr(b3$Instrumento,1,3)=="LTB"))!=0){
    b3$'Cupón....'[which(substr(b3$Instrumento,1,3)=="LTB")]=0
  }
  
  #traigo la fecha de vencimiento
  b3$Vencimiento=as.character(b3$Vencimiento)
  
  for(i in 1:length(b3$b3.)){
    if(substr(b3$Instrumento[i],1,3)!="LTB"){
      #which(as.character(b3$Instrumento[i])==as.character(c3$Sicet))
      b3$Vencimiento[i]=C3$F.Vencimiento[which(as.character(b3$Instrumento[i])==as.character(C3$Sicet))]
    }else{ 
      b3$Vencimiento[i]=as.character(format(as.Date("1900-01-01")+as.numeric(b3$Vencimiento[i])-2,"%d/%m/%Y"))
    }
  }
  
  #traigo si es TIF , VEBONO o TICC
  b3.=cbind(as.character(rep("TIF",1,length(b3$b3.))),b3)
  b3.$`as.character(rep("TIF", 1, length(b3$b3.)))`=as.character(b3.$`as.character(rep("TIF", 1, length(b3$b3.)))`)
  
  for(i in 1:length(b3.$b3.)){
    if(substr(b3$Instrumento[i],1,3)!="LTB"){
      b3.$`as.character(rep("TIF", 1, length(b3$b3.)))`[i]=as.character(C3$`Tipo Instrumento`[which(as.character(b3.$Instrumento[i])==as.character(C3$Sicet))])
    }else{
      b3.$`as.character(rep("TIF", 1, length(b3$b3.)))`[i]="LETRA"
      
    }
  }
  
  #fuente
  p=as.character(rep("022 BCV",1,length(b3.$b3.)))
  
  B3=cbind(b3.[,c(1,2)],p,b3.[,3:ncol(b3.)])
  
  #traigo columna Nombre de las caracteristicas
  nom=c()
  
  for(i in 1:nrow(B3)){
    if(substr(b3$Instrumento[i],1,3)!="LTB"){
      nom[i]=as.character(C3$Nombre[which(as.character(B3$Instrumento[i])==C3$Sicet)])
    }else{
      n1=substr(b3.$Vencimiento[i],4,5)
      n2=substr(b3.$Vencimiento[i],7,10)
      
      nom[i]=paste("LTB",n1,n2,sep="")
    }
  }
  
  #frecuencia y rendimiento
  fre=rep(0,1,length(b3.$b3.))
  rend=rep(0,1,length(b3.$b3.))
  
  B3=cbind(B3[,1],nom,B3[,2:ncol(B3)],fre,rend)
  names(B3)=c("Tipo Instrumento","Nombre","Fecha op","Fuente","Sicet","F.Vencimiento","Plazo","Op","Monto","Pre min","Pre max","Pre prom","Cupon","Frecuencia","Rendimiento")
  
  #añado criterio de que si en caraceristica dice 91 dias es freq=4 si es 182 freq=2
  for(i in 1:length(B3$`Tipo Instrumento`)){
    if(substr(B3$Sicet[i],1,3)!="LTB"){
      if(substr(as.character(C3$Inicio[which(as.character(B3$Sicet[i])==C3$Sicet)]),6,8)=="91 "){
        B3$Frecuencia[i]=4
      }
      if(substr(as.character(C3$Inicio[which(as.character(B3$Sicet[i])==C3$Sicet)]),6,8)=="180" |substr(as.character(C3$Inicio[which(as.character(B3$Sicet[i])==C3$Sicet)]),6,8)=="182"){
        B3$Frecuencia[i]=2
      }
    }
  }
  
  #añado rendimiento que es la formula RENDTO de excel con base 0
  for(i in 1:nrow(B3)){
    if(substr(B3$Sicet[i],1,3)!="LTB"){
      #B3$Rendimiento=bond.yields(as.Date(B3$`Fecha op`,"%d/%m/%Y"),as.Date(B3$F.Vencimiento,"%d/%m/%Y"),(B3$Cupon/100), B3$Frecuencia,B3$`Pre prom`,convention = c("30/360"),B3$Frecuencia)*100
      B3$Rendimiento[i]=bond.yield(as.Date(B3$`Fecha op`[i],"%d/%m/%Y"),as.Date(B3$F.Vencimiento[i],"%d/%m/%Y"),(B3$Cupon[i]/100), B3$Frecuencia[i],B3$`Pre prom`[i],convention = c("30/360E"),B3$Frecuencia[i])*100
    }
    if(substr(B3$Sicet[i],1,3)=="LTB"){
      #B3$Rendimiento=bond.yields(as.Date(B3$`Fecha op`,"%d/%m/%Y"),as.Date(B3$F.Vencimiento,"%d/%m/%Y"),(B3$Cupon/100), B3$Frecuencia,B3$`Pre prom`,convention = c("30/360"),B3$Frecuencia)*100
      B3$Rendimiento[i]=bond.yield(as.Date(B3$`Fecha op`[i],"%d/%m/%Y"),as.Date(B3$F.Vencimiento[i],"%d/%m/%Y"),0, 0.001,B3$`Pre prom`[i],convention = c("30/360E"),0)*100
    }
  }
  
  return(B3)
}#final funcion formato precios 022

#calculo rend de manera dif, uso convencion ACT/ACT
ca2_3 <- formatop3(ca1,ca)
#convierto fecha de op y venc en fechas
ca2_3$`Fecha op` <- as.Date(as.character(ca2_3$`Fecha op`),format="%d/%m/%Y")
ca2_3$F.Vencimiento <- as.Date(as.character(ca2_3$F.Vencimiento),format="%d/%m/%Y")

#este data frame es el que utiliza la metodologia Spline para los calculos
ca3_3 <- dplyr::arrange(ca2_3,(`Fecha op`))
#................................

#pruebo data vieja y caracteristicas vieja en fecha prueba 08/03/18
#cargo caracteristica y data vieja
#carateristica Splines
C_splines <- read.csv(paste(getwd(),"app","data","C_splines.txt",sep = "/"), sep="")
names(C_splines) <- c("Tipo Instrumento","Nombre","Sicet","F.Emision",
                      "F.Vencimiento","Tipo tasa","Inicio","Pago cupon 1" ,
                      "Pago cupon 2","Cupon")

#cargo data de prueba 0-22
data_splines <- read.csv(paste(getwd(),"app","data","Data_splines.txt",sep = "/"), sep="")
data_splines$Fecha.op <- as.Date(as.character(data_splines$Fecha.op))
data_splines$F.Vencimiento <- as.Date(as.character(data_splines$F.Vencimiento),format = "%d/%m/%Y")


#cargar titulos de script precios_diarios.R
#usando data y carac viejas
tif <- Tabla.splines(data = data_splines,tipo = "TIF",fe=as.Date("08/03/2018",format = "%d/%m/%Y"),num =40,par = 0.3,tit=tit[-c(1,3,5,11,19,20)],C_splines,pr=pos1(tit[-c(1,3,5,11,19,20)],0,Precio_prom_tif))

#usando data vieja y carac nueva (no tiene mucho sentido)
tif1 <- Tabla.splines(data = data_splines,tipo = "TIF",fe=as.Date("08/03/2018",format = "%d/%m/%Y"),num =40,par = 0.3,tit=tit[-c(1,3,5,11,19,20)],ca,pr=pos1(tit[-c(1,3,5,11,19,20)],0,Precio_prom_tif))

#usando data y carac nuevas
tif2 <- Tabla.splines(data = dat,tipo = "TIF",fe=as.Date("08/03/2018",format = "%d/%m/%Y"),num =40,par = 0.5,tit=tit[-c(1,3,5,11,19,20)],ca,pr=pos1(tit[-c(1,3,5,11,19,20)],0,Precio_prom_tif))

#comparacion precios
t <- cbind.data.frame(tif[[1]],tif1[[1]]$Precios,tif2[[1]]$Precios)

#realizo comparacion de titulos candidatos
rend <- list(tif[[2]][c(1,7)],tif1[[2]][c(1,7)],tif2[[2]][c(1,7)])

#comparo splines generados
sp <- list(tif[[4]],tif1[[4]],tif2[[4]])

#realizo graficas
#1era
plot(x=seq(0.1,20,0.1)*365,predict(tif[[4]],seq(0.1,20,0.1)*365)$y,type="l")

#2da
lines(x=seq(0.1,20,0.1)*365,predict(tif1[[4]],seq(0.1,20,0.1)*365)$y)

#3era
lines(x=seq(0.1,20,0.1)*365,predict(tif2[[4]],seq(0.1,20,0.1)*365)$y)

#candidatos
#1era y 2da
uno <- as.data.frame(tif[[2]])
plot(uno$Plazo,uno$Rendimiento)
lines(x=seq(0.1,20,0.1)*365,predict(tif[[4]],seq(0.1,20,0.1)*365)$y)


#3era
tres <- as.data.frame(tif2[[2]])
plot(tres$Plazo,tres$Rendimiento)
lines(x=seq(0.1,20,0.1)*365,predict(tif2[[4]],seq(0.1,20,0.1)*365)$y)

#pruebo ahora elimninando obs (1000,4.5) 
tres_1 <- tres[-2,]
plot(tres_1$Plazo,tres_1$Rendimiento)

spline3 <- smooth.spline(tres_1$Plazo,tres_1$Rendimiento,spar = 0.5)
lines(x=seq(0.1,20,0.1)*365,predict(spline3,seq(0.1,20,0.1)*365)$y)

#calculo nuevos precios 
pre <- precio(tit = tit[-c(1,3,5,11,19,20)],spline1 =spline3 ,fv =as.Date("08/03/2018",format = "%d/%m/%Y") ,C = ca)

#nueva comparacion
freddy <- cbind.data.frame(t,c(pre,0))
freddy

#####
#####
#####

#veo grafico
y <-predict(Tabla.splines(data = data_splines,tipo = "TIF",fe=as.Date("08/03/2018",format = "%d/%m/%Y"),num =40,par = 0.3,tit=tit[-c(1,3,5,11,19,20)],C_splines,pr=pos1(tit[-c(1,3,5,11,19,20)],0,Precio_prom_tif))[[4]],seq(0.1,20,0.1)*365)$y
# f <- ggplot(cbind.data.frame(x=seq(1,20,0.1)*365,y),aes(x=x,y=y))+
#   geom_line(color="black")+
#   geom_point(data = pto_sp_tif(),aes(x=pto_sp_tif()[,1],y=pto_sp_tif()[,2]),color="blue",size=3)+
#   xlab("Maduración (días)")+
#   ylab("Rendimiento (%)")+theme_gray()+
#   ggtitle("Curva de redimientos Splines TIF ")+
#   theme(plot.title = element_text(hjust = 0.5))
#   
# 
# ggplotly(f) 

letra <- tif[[3]]
letra1 <- data.frame(letra[,c(2,3,6,7,12,13,15)],"Corto Plazo","C1")

cand <- tif[[2]]
names(letra1)=names(cand)

figure(width = 1000,height = 400) %>%
  ly_points(c(letra[,7],cand[,4]),c(letra[,15],cand[,7]),rbind.data.frame(letra1,cand),hover=list("Nombre"=c(as.character(letra[,2]),as.character(cand[,1])),"Fecha de operación"=c(letra[,3],cand[,2]))) %>%
  ly_points(x=cbind.data.frame(x=seq(0.1,20,0.1)*365,y)[,1],y=cbind.data.frame(x=seq(0.1,20,0.1)*365,y)[,2],color="blue",hover=list("Plazo"=cbind.data.frame(x=seq(0.1,20,0.1)*365,y)[,1],"Rendimiento"=cbind.data.frame(x=seq(0.1,20,0.1)*365,y)[,2]),size=4) %>%
  # theme_title(text_color="green",text_align="center",text_font_style="italic")%>%
  x_axis("Plazo (días)") %>% y_axis("Rendimiento (%)") 




