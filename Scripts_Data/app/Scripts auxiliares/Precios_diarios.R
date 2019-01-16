#elaboro Base de Datos usando diferentes metodologías
#Nelson y Siegel, Svensson, Diebold-Li y Splines

#cargo funciones necesarias
source('~/.Trash/Riesgo-de-Mercado/app/funciones.R')

#Defino función para extraer precios promedio del archivo en la carpeta data
pos1 <- function(t,ind,tif){
  #caso tif
  if(ind==0){
    #tif <- read.csv(paste(getwd(),"data","Precio_prom_tif.txt",sep = "/"),sep="")
    tif$Títulos <- as.character(tif$Títulos)
    p <- c()
    
    #condicional de existencia
    for(i in 1:length(t)){
      if(length(which(t[i]==tif$Títulos))!=0){
        p[i] <- tif$Precio.Promedio[which(t[i]==tif$Títulos)]
      }else{
        #print("Titulo no encontrado")
        p[i] <- 0
      }
    }
    
    names(p) <- t
    return(p)
    
    
  } #final if tif
  
  
  #caso veb
  if(ind==1){
    #veb <- read.csv(paste(getwd(),"data","Precio_prom_veb.txt",sep = "/"),sep="")
    veb <- tif
    veb$Títulos <- as.character(veb$Títulos)
    
    p <- c()
    #condicional de existencia
    for(i in 1:length(t)){
      if(length(which(t[i]==veb$Títulos))!=0){
        p[i] <- veb$Precio.Promedio[which(t[i]==veb$Títulos)]
      }else{
        #print("Titulo no encontrado")
        p[i] <- 0
      }
    } 
    
    names(p) <- t
    return(p)
    
  }
  
  
  
}#final funcion pos1

#titulos
#TIF iniciales
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

#funcion que calcula precio, para no tener un rend negativo
#es una manera de evitar el inconveniente de ingresar el precio promedio 
#de forma manual
#argumentos:
#pre: precio para el cual existe rend negativo
#Tabla: tabla que genera la funcion Tabla.sven
#output:
#precio: precio para el cual no existe rendimiento negativo
find_pre <- function(pre,Tabla,fv,i){
  
  #vt1=seq(100,Tabla[6,which(Tabla[9,]<0)],by=0.01)
  vt1=seq(100,pre,by=0.01)
  vt1 <- vt1[length(vt1):1]
  
  j <- 1
  vt2 <- as.numeric(gsub("[,]",".",Tabla[9,j]))
  while(vt2<0){
    vt2 <- bond.yield(as.Date(fv,format="%d/%m/%Y"),as.Date(Tabla[4,i],"%d/%m/%Y"),as.numeric(gsub("[,]",".",Tabla[5,i])), 4,vt1[j],convention = c("ACT/360"),4)
    j <- j+1
  }
  
  #consigo precio de manera que el rend no es negativo
  return(vt1[j])
  
} #final funcion find_pre


#creo funcion auxiliar para garantizar que los rendimientos
#en la curva sean positivos
fre <- c(-1,4,6,7,8)

check_rn <- function(fre){
  if (length(which(fre<0))==0){
    return(1)
  }else{
    return(-1)
  }
}

check_rn(fre)

#leo caracteristicas
ca <- (Carac(paste(getwd(),"app","data","Caracteristicas.xls",sep = "/")))
names(ca) <- c("Tipo Instrumento","Nombre","Sicet","F.Emision",
               "F.Vencimiento","Tipo tasa","Inicio","Pago cupon 1" ,
               "Pago cupon 2","Cupon")

#parametros
#NS
#tif
pa_ns=c(0.133799434790145,-0.01,-0.307885339616438,0.545398124008073)

#veb
pa1_ns <- c(0.135872169451391,0.1,-0.503768911829894,0.11951691203874)


#SV
#tif
pa_sven=c(0.133799434790145,-0.01,-0.307885339616438,-0.134075672659356,
          0.545398124008073,0.350692201663154)

#vebono
pa1_sven=c(0.135872169451391,0.1,-0.503768911829894,-0.288755056029301,
           0.11951691203874,0.501729233062216)

#precios promedio nuevos
Precio_prom_tif <- read.csv("~/.Trash/Riesgo-de-Mercado/app/data/Precio_prom_tif.txt", sep="")
Precio_prom_veb <- read.csv("~/.Trash/Riesgo-de-Mercado/app/data/Precio_prom_veb.txt", sep="")


#busco precio promedio
pos1(tit[12],0,Precio_prom_tif)

#funciona bien
#no logra optimizar pero devuelve mensaje de error
#FECHAS
oct <- c("01/10/2018","02/10/2018","03/10/2018","04/10/2018","05/10/2018",
                "08/10/2018","09/10/2018","10/10/2018","11/10/2018","15/10/2018",
                "16/10/2018","17/10/2018","18/10/2018","19/10/2018","22/10/2018",
                "23/10/2018","24/10/2018","25/10/2018")


#PRECIOS NELSON Y SIEGEL
#TIF
p <- Tabla.ns(fv=oct[1],tit[-c(1,3,5,11,19,20)],pos1(tit[-c(1,3,5,11,19,20)],0,Precio_prom_tif),pa=c(1,1,1,1),ind=0,C=ca,fe2=1,fe3=0)[[3]]

p2 <- rep(0,20)
for(i in 2:length(oct)){
p1 <- Tabla.ns(fv=oct[i],tit[-c(1,3,5,11,19,20)],pos1(tit[-c(1,3,5,11,19,20)],0,Precio_prom_tif),pa=c(1,1,1,1),ind=0,C=ca,fe2=1,fe3=0)[[3]]
p2 <- cbind.data.frame(p2,p1$Precios)
}

q <- cbind.data.frame(p,p2[,-1])
names(q) <- c("Titulos/Precios",oct)

write.table(q,"precios_ns_tif_oct.txt")

#VEBONO
p_veb <- Tabla.ns(fv=oct[1],tit1[-c(1,5,8,12,19,24,25)],pos1(tit1[-c(1,5,8,12,19,24,25)],1,Precio_prom_veb),pa=c(1,1,1,1),ind=1,C=ca,fe2=1,fe3=0)[[3]]

p2_veb <- rep(0,23)
for(i in 2:length(oct)){
  p1_veb <- Tabla.ns(fv=oct[i],tit1[-c(1,5,8,12,19,24,25)],pos1(tit1[-c(1,5,8,12,19,24,25)],1,Precio_prom_veb),pa=c(1,1,1,1),ind=1,C=ca,fe2=1,fe3=0)[[3]]
  p2_veb <- cbind.data.frame(p2_veb,p1_veb$Precios)
}

q_veb <- cbind.data.frame(p_veb,p2_veb[,-1])
names(q_veb) <- c("Titulos/Precios",oct)

write.table(q_veb,"precios_ns_veb_oct.txt")

#PRECIOS SVENSSON
#TIF
s_tif <- Tabla.sven(fv=oct[1],tit[-c(1,3,5,11,19,20)],pos1(tit[-c(1,3,5,11,19,20)],0,Precio_prom_tif),pa=c(1,1,1,1,1,1),ind=0,C=ca,fe2=1,fe3=0)[[3]]

s2_tif <- rep(0,20)
for(i in 2:length(oct)){
  s1_tif <- Tabla.sven(fv=oct[i],tit[-c(1,3,5,11,19,20)],pos1(tit[-c(1,3,5,11,19,20)],0,Precio_prom_tif),pa=c(1,1,1,1,1,1),ind=0,C=ca,fe2=1,fe3=0)[[3]]
  s2_tif <- cbind.data.frame(s2_tif,s1_tif$Precios)
}

Stif <- cbind.data.frame(s_tif,s2_tif[,-1])
names(Stif) <- c("Titulos/Precios",oct)

write.table(Stif,"precios_sv_tif_oct.txt")


#VEBONO
s_veb <- Tabla.sven(fv=oct[1],tit1[-c(1,5,8,12,19,24,25)],pos1(tit1[-c(1,5,8,12,19,24,25)],1,Precio_prom_veb),pa=c(1,1,1,1,1,1),ind=1,C=ca,fe2=1,fe3=0)[[3]]

s2_veb <- rep(0,23)
for(i in 2:length(oct)){
  s1_veb <- Tabla.sven(fv=oct[i],tit1[-c(1,5,8,12,19,24,25)],pos1(tit1[-c(1,5,8,12,19,24,25)],1,Precio_prom_veb),pa=c(1,1,1,1,1,1),ind=1,C=ca,fe2=1,fe3=0)[[3]]
  s2_veb <- cbind.data.frame(s2_veb,s1_veb$Precios)
}

Sveb <- cbind.data.frame(s_veb,s2_veb[,-1])
names(Sveb) <- c("Titulos/Precios",oct)

write.table(Sveb,"precios_sv_veb_oct.txt")

#DIEBOLD-LI
dat <- read.csv(paste(getwd(),"app","data","Historico_act.txt",sep = "/"),sep="")
dat[,3] <- as.Date(as.character(dat[,3]))
car <- Carac(paste(getwd(),"app","data","Caracteristicas.xls",sep = "/"))



#TIF
sp_tif <- Tabla.splines(data = dat,tipo = "TIF",fe=as.Date(oct[1],format = "%d/%m/%Y"),num =40,par = 0.3,tit=tit[-c(1,3,5,11,19,20)],ca,pr=pos1(tit[-c(1,3,5,11,19,20)],0,Precio_prom_tif))[[4]] 


d_tif <- precio.dl(tit = tit[-c(1,3,5,11,19,20)],fv = as.Date(oct[1],format = "%d/%m/%Y") ,C = ca,pa = c(1,1,1,1),spline1 = sp_tif,pr=pos1(tit[-c(1,3,5,11,19,20)],0,Precio_prom_tif))[[2]]

  
d2_tif <- rep(0,20)
for(i in 2:length(oct)){
  sp_tif <- Tabla.splines(data = dat,tipo = "TIF",fe=as.Date(oct[i],format = "%d/%m/%Y"),num =40,par = 0.2,tit=tit[-c(1,3,5,11,19,20)],ca,pr=pos1(tit[-c(1,3,5,11,19,20)],0,Precio_prom_tif))[[4]] 
  d1_tif <-  precio.dl(tit = tit[-c(1,3,5,11,19,20)],fv = as.Date(oct[i],format = "%d/%m/%Y") ,C = ca,pa = c(1,1,1,1),spline1 = sp_tif,pr=pos1(tit[-c(1,3,5,11,19,20)],0,Precio_prom_tif))[[2]]
  d2_tif <- cbind.data.frame(d2_tif,d1_tif$Precio)
}

Dtif <- cbind.data.frame(d_tif,d2_tif[,-1])
names(Dtif) <- c("Titulos/Precios",oct)

write.table(Dtif,"precios_DL_tif_oct.txt")

#pruebo funcion extrae
#funcion vieja
# extrae <- function(fv,dias,data){
#   f1 <- as.Date(fv)
#   f2 <- f1-dias
#   f3 <- data$Fecha.op-f2
#   
#   #con esto hallo el extremo inferior de la data
#   f4 <- data[which(as.numeric(min(abs(f3)))==abs(f3)),]
#   
#   #hallo ahora el extremo superior
#   g<- data$Fecha.op-fv
#   g1 <- data[which(as.numeric(min(abs(g)))==abs(g)),]
#   
#   while(anyNA(g1)){
#     #print("Obs con NA")
#     data1 <- data[-which(as.numeric(min(abs(g)))==abs(g)),]
#     
#     g <- data1$Fecha.op-fv
#     
#     g1 <- data1[which(as.numeric(min(abs(g)))==abs(g)),]
#     
#   }
#   
#   if(dim(g1)[1]==1){
#     #print("Hay una sola obs")
#     #print("Fecha selecionada")
#     #print(g1$Fecha.op)
#     #return(f4$Fecha.op)
#   }else{
#     #print("Hay mas de una obs")
#     g1 <- g1[which(g1$Monto==max(g1$Monto)),]
#     #print("Fecha selecionada")
#     #print(g1$Fecha.op)
#     #return(f4$Fecha.op)
#   }#final if obs
#   
#   while(anyNA(f4)){
#     #print("Obs con NA")
#     data1 <- data[-which(as.numeric(min(abs(f3)))==abs(f3)),]
#     
#     f3 <- data1$Fecha.op-f2
#     
#     f4 <- data1[which(as.numeric(min(abs(f3)))==abs(f3)),]
#     
#   }
#   
#   
#   if(dim(f4)[1]==1){
#     #print("Hay una sola obs")
#     #print("Fecha selecionada")
#     #print(f4$Fecha.op)
#     #return(f4$Fecha.op)
#   }else{
#     #print("Hay mas de una obs")
#     f4 <- f4[which(f4$Monto==max(f4$Monto)),]
#     #print("Fecha selecionada")
#     #print(f4$Fecha.op)
#     #return(f4$Fecha.op)
#   }#final if obs
#   
#   #extremo inferior
#   q1 <- which(f4$Fecha.op==data$Fecha.op)
#   
#   #extremo superior
#   q2 <- which(g1$Fecha.op==data$Fecha.op)[1]
#   
#   
#   #data2 <- data[1:q1[length(q1)],]
#   data2 <- data[q1[length(q1)]:q2,]
#   
#   # a1 <- data.frame(t(c(length(which(data2$segmento1=="C1")),
#   #                      length(which(data2$segmento1=="C2")),
#   #                      length(which(data2$segmento1=="M1")),
#   #                      length(which(data2$segmento1=="M2")),
#   #                      length(which(data2$segmento1=="L1")),
#   #                      length(which(data2$segmento1=="L2")),
#   #                      length(which(data2$segmento1=="L3")))))
#   # 
#   # a1 <- data.frame(a1,sum(a1))
#   # names(a1) <- c("Obs. C1","Obs. C2","Obs. M1","Obs. M2","Obs. L1","Obs. L2","Obs. L3","Suma")
#   # print(a1)
#   #s <<-a1
#   return(data2)
# }#final funcion extrae


#ojo con el orden de la data debe estar ordenada de mas reciente a mas antigua
dat1 <- dat[order(dat$Fecha.op,decreasing = TRUE),]

extrae <- function(fv,dias,data){
  f1 <- as.Date(fv)
  f2 <- f1-dias
  
  #hallo extremo superior (mas reciente) de la data a extraer
  f3 <- fv
  while(length(which(f3==data$Fecha.op))==0){
    f3 <- f3-1
  }
  
  #con esto hallo el extremo inferior (mas antiguo) de la data
  f4 <- f2
  while(length(which(f4==data$Fecha.op))==0){
    f4 <- f4-1
  }
  
  data1 <- data[which(f3==data$Fecha.op)[1]:which(f4==data$Fecha.op)[1],]
  
  
  return(data1)
}#final funcion extrae


ext <- extrae(fv = as.Date(oct[1],format = "%d/%m/%Y"),dias = 40,data = dat1)


#VEBONOS
sp_veb <- Tabla.splines(data = dat,tipo = "VEBONO",fe=as.Date(oct[1],format = "%d/%m/%Y"),num =40,par = 0.3,tit=tit1[-c(1,5,8,12,19,24,25)],ca,pr=pos1(tit1[-c(1,5,8,12,19,24,25)],1,Precio_prom_veb))[[4]] 


d_veb <- precio.dl(tit = tit1[-c(1,5,8,12,19,24,25)],fv = as.Date(oct[1],format = "%d/%m/%Y") ,C = ca,pa = c(1,1,1,1),spline1 = sp_veb,pr=pos1(tit1[-c(1,5,8,12,19,24,25)],1,Precio_prom_veb))[[2]]


d2_veb <- rep(0,23)
for(i in 2:length(oct)){
  sp_veb <- Tabla.splines(data = dat,tipo = "TIF",fe=as.Date(oct[i],format = "%d/%m/%Y"),num =40,par = 0.2,tit=tit1[-c(1,5,8,12,19,24,25)],ca,pr=pos1(tit1[-c(1,5,8,12,19,24,25)],1,Precio_prom_veb))[[4]] 
  d1_veb <-  precio.dl(tit = tit1[-c(1,5,8,12,19,24,25)],fv = as.Date(oct[i],format = "%d/%m/%Y") ,C = ca,pa = c(1,1,1,1),spline1 = sp_veb,pr=pos1(tit1[-c(1,5,8,12,19,24,25)],1,Precio_prom_veb))[[2]]
  d2_veb <- cbind.data.frame(d2_veb,d1_veb$Precio)
}

Dveb <- cbind.data.frame(d_veb,d2_veb[,-1])
names(Dveb) <- c("Titulos/Precios",oct)

write.table(Dveb,"precios_DL_veb_oct.txt")

#SPLINES
#TIF
spline_tif <- Tabla.splines(data = dat,tipo = "TIF",fe=as.Date(oct[1],format = "%d/%m/%Y"),num =40,par = 0.3,tit=tit[-c(1,3,5,11,19,20)],ca,pr=pos1(tit[-c(1,3,5,11,19,20)],0,Precio_prom_tif))[[1]]


spline2_tif <- rep(0,20)
for(i in 2:length(oct)){
  spl_tif <-  Tabla.splines(data = dat,tipo = "TIF",fe=as.Date(oct[i],format = "%d/%m/%Y"),num =40,par = 0.3,tit=tit[-c(1,3,5,11,19,20)],ca,pr=pos1(tit[-c(1,3,5,11,19,20)],0,Precio_prom_tif))[[1]]
  spline2_tif <- cbind.data.frame(spline2_tif,spl_tif$Precios)
}

SPtif <- cbind.data.frame(spline_tif,spline2_tif[,-1])
names(SPtif) <- c("Titulos/Precios",oct)

write.table(Dtif,"precios_DL_tif_oct.txt")




#VEBONOS








