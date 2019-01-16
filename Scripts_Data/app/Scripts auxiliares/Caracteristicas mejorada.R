#reescribo funcion caracteristicas
library(readxl)
library(jrvFinance)

ruta <- "/Users/freddytapia/Riesgo-de-Mercado/app/data/Caracteristicas.xls"
ruta1 <- "/Users/freddytapia/Riesgo-de-Mercado/app/data/29-06-18.xls"


#carac anterior
#funcion Carateristicas
#funcion que extrae informacion del documento de las
#caracteristicas y lee todas sus pesta?as
Carac=function(ruta){
  #c=read.xlsx("C:/Users/ftapia/Documents/DESCARGAS R/Caracteristicas.xls", sheetName = "DPN",startRow = 7,colIndex = 1:15,header = TRUE)
  #c=read.xlsx("C:/Users/Nancy/Desktop/Modelos Aleatorios/prueba 190717/Caracteristicas.xls", sheetName = "DPN",startRow = 7,colIndex = 1:15,header = TRUE)
  c=read.xlsx(ruta, sheetName = "DPN",startRow = 7,colIndex = 1:15,header = TRUE)
  c1=c[,c(3,5,6,8,10,11,13,15)]
  c2=c1[-which(is.na(c1$X...)),]
  c3=cbind(as.character(rep("TIF",1,length(c2$X...))),c2)
  c3$`as.character(rep("TIF", 1, length(c2$X...)))`=as.character(c3$`as.character(rep("TIF", 1, length(c2$X...)))`)
  c3$`as.character(rep("TIF", 1, length(c2$X...)))`[which(c3$Referencia...b_..!="Tasa Fija")]="VEBONO"
  names(c3)=c("Tipo Instrumento","Sicet","F.Emision","F.Vencimiento","Tipo tasa","Inicio","Pago cupon 1","Pago cupon 2","Cupon")
  
  #tranformo formato fecha
  c3$F.Emision=format(c3$F.Emision,"%d/%m/%Y")
  c3$F.Vencimiento=format(c3$F.Vencimiento,"%d/%m/%Y")
  c3$`Pago cupon 1`=format(c3$`Pago cupon 1`,"%d/%m/%Y")
  c3$`Pago cupon 2`=format(c3$`Pago cupon 2`,"%d/%m/%Y")
  
  #agrego columna Nombre TIf o Vebono + fecha venc
  c4=c3$`Tipo Instrumento`
  
  Nombre=c()
  for(i in 1:length(c4)){
    Nombre[i]=paste(c4[i],paste(substr(c3$F.Vencimiento[i],4,5),substr(c3$F.Vencimiento[i],7,10),sep = ""),sep = "")
  }
  
  c3=cbind(c3$`Tipo Instrumento`,Nombre,c3[,2:ncol(c3)])
  names(c3)[1]="Tipo Instrumento"
  
  #pestaña DPN-TICC
  #d=read.xlsx("C:/Users/Nancy/Desktop/Modelos Aleatorios/prueba 190717/Caracteristicas.xls", sheetName = "DPN - TICC",startRow = 7,colIndex = 1:14,header = TRUE)
  d=read.xlsx(ruta, sheetName = "DPN - TICC",startRow = 7,colIndex = 1:14,header = TRUE)
  
  #d=read.xlsx("C:/Users/ftapia/Documents/DESCARGAS R/Caracteristicas.xls", sheetName = "DPN - TICC",startRow = 7,colIndex = 1:14,header = TRUE)
  
  d=d[-which(is.na(d[,2])),c(3,4,5,7,9,10,12,14)]
  #cambio formato fechas
  d[,2]=format(d[,2],"%d/%m/%Y")
  d[,3]=format(d[,3],"%d/%m/%Y")
  d[,6]=format(d[,6],"%d/%m/%Y")
  d[,7]=format(d[,7],"%d/%m/%Y")
  
  d1=rep("TICC",1,nrow(d))
  
  d2=c()
  for(i in 1:length(d1)){
    d2[i]=paste(d1[i],paste(substr(d[i,3],4,5),substr(d[i,3],7,10),sep = ""),sep = "")
  }
  
  d3=cbind(d1,d2,d)
  names(d3)=names(c3) 
  
  #Añado nuevo titulo VF
  #v=read.xlsx("C:/Users/ftapia/Documents/DESCARGAS R/Caracteristicas.xls", sheetName = "VF",startRow = 7,colIndex = 1:14,header = TRUE)
  v=try(read.xlsx(ruta, sheetName = "VF",startRow = 7,colIndex = 1:14,header = TRUE),silent = TRUE)
  #b=try(read.xlsx("C:/Users/ftapia/Documents/DESCARGAS R/0-22.xls", sheetName = m3,startRow = 10,colIndex = 1:9,header = TRUE),silent = T)
  vb=try(read.xlsx(ruta, sheetName = "VB",startRow = 7,colIndex = 1:15,header = TRUE),silent = TRUE)
  #
  cf=try(read.xlsx(ruta, sheetName = "CF",startRow = 7,colIndex = 1:14,header = TRUE),silent = TRUE)
  #
  cb=try(read.xlsx(ruta, sheetName = "CB",startRow = 7,colIndex = 1:14,header = TRUE),silent = TRUE)
  
  
  #if peta?a VF
  if(class(v)=="try-error"){
    print("No hay pesta?a VF")
    #CARACTERISTICAS DEFINITIVA
    #C3=rbind(c3,d3) 
    
    #En caso que no lea bien un numero
    #C3$Cupon=as.numeric(sub(",",".",C3$Cupon))
    w3=1
    #return(C3)  
  }else{
    #leo VF
    v=v[-which(is.na(v[,2])),c(3,4,5,7,9,10,12,14)]
    #cambio formato fechas
    v[,2]=format(v[,2],"%d/%m/%Y")
    v[,3]=format(v[,3],"%d/%m/%Y")
    v[,6]=format(v[,6],"%d/%m/%Y")
    v[,7]=format(v[,7],"%d/%m/%Y")
    
    w1=rep("Valores Fin.",1,nrow(v))
    
    w2=c()
    for(i in 1:length(w1)){
      w2[i]=paste(w1[i],paste(substr(v[i,3],4,5),substr(v[i,3],7,10),sep = ""),sep = "")
    }
    
    w3=cbind(w1,w2,v)
    names(w3)=names(c3)
    
  }#final if VF
  
  #if Pesta?a VB
  if(class(vb)=="try-error"){
    print("No hay pesta?a VB")
    #CARACTERISTICAS DEFINITIVA
    #C3=rbind(c3,d3) 
    
    #En caso que no lea bien un numero
    #C3$Cupon=as.numeric(sub(",",".",C3$Cupon))
    
    #return(C3)  
    v3=1
  }else{
    #leo Vb   
    vb=vb[,-4]
    vb=vb[-which(is.na(vb[,2])),c(3,4,5,7,9,10,12,14)]
    #cambio formato fechas
    vb[,2]=format(vb[,2],"%d/%m/%Y")
    vb[,3]=format(vb[,3],"%d/%m/%Y")
    vb[,6]=format(vb[,6],"%d/%m/%Y")
    vb[,7]=format(vb[,7],"%d/%m/%Y")
    
    v1=rep("Valores Bol.",1,nrow(vb))
    
    v2=c()
    for(i in 1:length(v1)){
      v2[i]=paste(v1[i],paste(substr(vb[i,3],4,5),substr(vb[i,3],7,10),sep = ""),sep = "")
    }
    
    v3=cbind(v1,v2,vb)
    names(v3)=names(c3) 
    
  }#final if VB
  
  
  if(class(cf)=="try-error"){
    print("No hay pesta?a CF")
    #CARACTERISTICAS DEFINITIVA
    #C3=rbind(c3,d3) 
    
    #En caso que no lea bien un numero
    #C3$Cupon=as.numeric(sub(",",".",C3$Cupon))
    
    #return(C3)  
    x3=1
  }else{
    #leo cf
    cf=cf[-which(is.na(cf[,2])),c(3,4,5,7,9,10,12,14)]
    #cambio formato fechas
    cf[,2]=format(cf[,2],"%d/%m/%Y")
    cf[,3]=format(cf[,3],"%d/%m/%Y")
    cf[,6]=format(cf[,6],"%d/%m/%Y")
    cf[,7]=format(cf[,7],"%d/%m/%Y")
    
    x1=rep("Certificado Part. Simon Bolivar",1,nrow(cf))
    
    x2=c()
    for(i in 1:length(x1)){
      x2[i]=paste(x1[i],paste(substr(cf[i,3],4,5),substr(cf[i,3],7,10),sep = ""),sep = "")
    }
    
    x3=cbind(x1,x2,cf)
    names(x3)=names(c3) 
    
  }#final if CF
  
  
  if(class(cb)=="try-error"){
    print("No hay pesta?a CB")
    #CARACTERISTICAS DEFINITIVA
    #C3=rbind(c3,d3) 
    
    #En caso que no lea bien un numero
    #C3$Cupon=as.numeric(sub(",",".",C3$Cupon))
    
    #return(C3)  
    y3=1
  }else{
    #leo pesta?a VB
    #b=try(read.xlsx("C:/Users/ftapia/Documents/DESCARGAS R/0-22.xls", sheetName = m3,startRow = 10,colIndex = 1:9,header = TRUE),silent = T)
    
    
    #v=read.xlsx(ruta, sheetName = "VF",startRow = 7,colIndex = 1:14,header = TRUE)
    #leo Vb   
    #vb=vb[,-4]
    #vb=vb[-which(is.na(vb[,2])),c(3,4,5,7,9,10,12,14)]
    #cambio formato fechas
    #vb[,2]=format(vb[,2],"%d/%m/%Y")
    #vb[,3]=format(vb[,3],"%d/%m/%Y")
    #vb[,6]=format(vb[,6],"%d/%m/%Y")
    #vb[,7]=format(vb[,7],"%d/%m/%Y")
    
    #v1=rep("Valores Bol.",1,nrow(vb))
    
    #v2=c()
    #for(i in 1:length(v1)){
    #  v2[i]=paste(v1[i],paste(substr(vb[i,3],4,5),substr(vb[i,3],7,10),sep = ""),sep = "")
    #}
    
    #v3=cbind(v1,v2,vb)
    #names(v3)=names(c3) 
    
    #leo VF
    #v=v[-which(is.na(v[,2])),c(3,4,5,7,9,10,12,14)]
    #cambio formato fechas
    #v[,2]=format(v[,2],"%d/%m/%Y")
    #v[,3]=format(v[,3],"%d/%m/%Y")
    #v[,6]=format(v[,6],"%d/%m/%Y")
    #v[,7]=format(v[,7],"%d/%m/%Y")
    
    #w1=rep("Valores Fin.",1,nrow(v))
    
    #w2=c()
    #for(i in 1:length(w1)){
    # w2[i]=paste(w1[i],paste(substr(v[i,3],4,5),substr(v[i,3],7,10),sep = ""),sep = "")
    #}
    
    #w3=cbind(w1,w2,v)
    #names(w3)=names(c3) 
    
    #leo cf
    #cf=cf[-which(is.na(cf[,2])),c(3,4,5,7,9,10,12,14)]
    #cambio formato fechas
    #cf[,2]=format(cf[,2],"%d/%m/%Y")
    #cf[,3]=format(cf[,3],"%d/%m/%Y")
    #cf[,6]=format(cf[,6],"%d/%m/%Y")
    #cf[,7]=format(cf[,7],"%d/%m/%Y")
    
    #x1=rep("Certificado Part. Simon Bolivar",1,nrow(cf))
    
    #x2=c()
    #for(i in 1:length(x1)){
    # x2[i]=paste(x1[i],paste(substr(cf[i,3],4,5),substr(cf[i,3],7,10),sep = ""),sep = "")
    #}
    
    #x3=cbind(x1,x2,cf)
    #names(x3)=names(c3) 
    
    
    #leo cb
    cb=cb[-which(is.na(cb[,2])),c(3,4,5,7,9,10,12,14)]
    #cambio formato fechas
    cb[,2]=format(cb[,2],"%d/%m/%Y")
    cb[,3]=format(cb[,3],"%d/%m/%Y")
    cb[,6]=format(cb[,6],"%d/%m/%Y")
    cb[,7]=format(cb[,7],"%d/%m/%Y")
    
    y1=rep("Certificado Part. Bandes",1,nrow(cb))
    
    y2=c()
    for(i in 1:length(y1)){
      y2[i]=paste(y1[i],paste(substr(cb[i,3],4,5),substr(cb[i,3],7,10),sep = ""),sep = "")
    }
    
    y3=cbind(y1,y2,cb)
    names(y3)=names(c3) 
  }#final if CB
  #CARACTERISTICAS DEFINITIVA
  #if de pesta?as
  if(length(c3)==1|length(d3)==1|length(v3)==1|length(w3)==1|length(x3)==1|length(y3)==1){
    print("No hay una pesta?a!") 
    ve=c(length(c3),length(d3),length(v3),length(w3),length(x3),length(y3))
    h1=which(ve==1)
    
    if(h1==3){
      print("Falta pesta?a VB")
      C3=rbind(c3,d3,w3,x3,y3)
      return(C3)
    }
    if(h1==4){
      print("Falta pesta?a VF")
      C3=rbind(c3,d3,v3,x3,y3)
      return(C3)
    }
    if(h1==5){
      print("Falta pesta?a CF")
      C3=rbind(c3,d3,v3,w3,y3)
      return(C3)
    }
    if(h1==6){
      print("Falta pesta?a CB")
      C3=rbind(c3,d3,v3,w3,x3)
      return(C3)
    }
    
  }else{
    print("Existen todas las pesta?as!")   
    C3=rbind(c3,d3,v3,w3,x3,y3) 
    #En caso que no lea bien un numero
    C3$Cupon=as.numeric(sub(",",".",C3$Cupon))
    
    return(C3)}#final if pesta?as
  
}#final funcion Caracteristicas

#carac mejorada
Carac=function(ruta){
  c=read_excel(ruta, sheet = "DPN",range = "A7:O90",col_names = TRUE)
  c1=c[,c(3,5,6,8,10,11,13,15)]
  c2=c1[-which(is.na(c1[,1])),]
  c3=cbind(as.character(rep("TIF",1,length(c2[,1]))),c2)
  c3[,1]=as.character(c3[,1])
  c3[which(c3[,5]!="Tasa Fija"),1]="VEBONO"
  names(c3)=c("Tipo Instrumento","Sicet","F.Emision","F.Vencimiento","Tipo tasa","Inicio","Pago cupon 1","Pago cupon 2","Cupon")
  
  #tranformo formato fecha
  c3[,3]=format(c3[,3],"%d/%m/%Y")
  c3[,4]=format(c3[,4],"%d/%m/%Y")
  c3[,7]=format(c3[,7],"%d/%m/%Y")
  c3[,8]=format(c3[,8],"%d/%m/%Y")
  
  #agrego columna Nombre TIf o Vebono + fecha venc
  c4=c3[,1]
  
  Nombre=c()
  for(i in 1:length(c4)){
    Nombre[i]=paste(c4[i],paste(substr(c3[i,4],4,5),substr(c3[i,4],7,10),sep = ""),sep = "")
  }
  
  c3=cbind(c3[,1],Nombre,c3[,2:ncol(c3)])
  names(c3)[1]="Tipo Instrumento"


  #pestaña DPN-TICC
  #d=read.xlsx(ruta, sheetName = "DPN - TICC",startRow = 7,colIndex = 1:14,header = TRUE)
  d <- try(read_excel(ruta,sheet = "DPN - TICC",range = "A7:N13",col_names = TRUE),silent = TRUE)
  #Añado nuevo titulo VF
  #v=try(read.xlsx(ruta, sheetName = "VF",startRow = 7,colIndex = 1:14,header = TRUE),silent = TRUE)
  v=try(read_excel(ruta, sheet = "VF",range = "A7:N30" ,col_names = TRUE),silent = TRUE)
  #
  #vb=try(read.xlsx(ruta, sheetName = "VB",startRow = 7,colIndex = 1:15,header = TRUE),silent = TRUE)
  vb=try(read_excel(ruta, sheet = "VB",range = "A7:O50",col_names = TRUE),silent = TRUE)
  #
  #cf=try(read.xlsx(ruta, sheetName = "CF",startRow = 7,colIndex = 1:14,header = TRUE),silent = TRUE)
  cf=try(read_excel(ruta, sheet = "CF",range = "A7:N30",col_names = TRUE),silent = TRUE)
  #
  #cb=try(read.xlsx(ruta, sheetName = "CB",startRow = 7,colIndex = 1:14,header = TRUE),silent = TRUE)
  cb=try(read_excel(ruta, sheet = "CB",range = "A7:N20",col_names = TRUE),silent = TRUE)
  
  
  #if pestaña dpn - ticc
  if(class(d)[1]=="try-error"){
    d3=1
  }else{
    d=d[-which(is.na(d[,2])),c(3,4,5,7,9,10,12,14)]
    d <- as.data.frame(d)
    #cambio formato fechas
    d[,2]=format(d[,2],"%d/%m/%Y")
    d[,3]=format(d[,3],"%d/%m/%Y")
    d[,6]=format(d[,6],"%d/%m/%Y")
    d[,7]=format(d[,7],"%d/%m/%Y")
    
    d1=rep("TICC",1,nrow(d))
    
    d2=c()
    for(i in 1:length(d1)){
      d2[i]=paste(d1[i],paste(substr(d[i,3],4,5),substr(d[i,3],7,10),sep = ""),sep = "")
    }
    
    d3=cbind(d1,d2,d)
    names(d3)=names(c3) 
    
  }
  
  
  #if peta?a VF
  if(class(v)[1]=="try-error"){
    print("No hay pesta?a VF")
    w3=1
  }else{
    #leo VF
    v=v[-which(is.na(v[,2])),c(3,4,5,7,9,10,12,14)]
    v<-as.data.frame(v)
    #cambio formato fechas
    v[,2]=format(v[,2],"%d/%m/%Y")
    v[,3]=format(v[,3],"%d/%m/%Y")
    v[,6]=format(v[,6],"%d/%m/%Y")
    v[,7]=format(v[,7],"%d/%m/%Y")
    
    w1=rep("Valores Fin.",1,nrow(v))
    
    w2=c()
    for(i in 1:length(w1)){
      w2[i]=paste(w1[i],paste(substr(v[i,3],4,5),substr(v[i,3],7,10),sep = ""),sep = "")
    }
    
    w3=cbind(w1,w2,v)
    names(w3)=names(c3)
    
  }#final if VF
  
  #if Pesta?a VB
  if(class(vb)[1]=="try-error"){
    print("No hay pesta?a VB")
    #CARACTERISTICAS DEFINITIVA
    #C3=rbind(c3,d3) 
    
    #En caso que no lea bien un numero
    #C3$Cupon=as.numeric(sub(",",".",C3$Cupon))
    
    #return(C3)  
    v3=1
  }else{
    #leo Vb   
    vb=vb[,-4]
    vb=vb[-which(is.na(vb[,2])),c(3,4,5,7,9,10,12,14)]
    vb <- as.data.frame(vb)
    #cambio formato fechas
    vb[,2]=format(vb[,2],"%d/%m/%Y")
    vb[,3]=format(vb[,3],"%d/%m/%Y")
    vb[,6]=format(vb[,6],"%d/%m/%Y")
    vb[,7]=format(vb[,7],"%d/%m/%Y")
    
    v1=rep("Valores Bol.",1,nrow(vb))
    
    v2=c()
    for(i in 1:length(v1)){
      v2[i]=paste(v1[i],paste(substr(vb[i,3],4,5),substr(vb[i,3],7,10),sep = ""),sep = "")
    }
    
    v3=cbind(v1,v2,vb)
    names(v3)=names(c3) 
    
  }#final if VB
  
  
  if(class(cf)[1]=="try-error"){
    print("No hay pesta?a CF")
    #CARACTERISTICAS DEFINITIVA
    #C3=rbind(c3,d3) 
    
    #En caso que no lea bien un numero
    #C3$Cupon=as.numeric(sub(",",".",C3$Cupon))
    
    #return(C3)  
    x3=1
  }else{
    #leo cf
    cf=cf[-which(is.na(cf[,2])),c(3,4,5,7,9,10,12,14)]
    cf <- as.data.frame(cf)
    #cambio formato fechas
    cf[,2]=format(cf[,2],"%d/%m/%Y")
    cf[,3]=format(cf[,3],"%d/%m/%Y")
    cf[,6]=format(cf[,6],"%d/%m/%Y")
    cf[,7]=format(cf[,7],"%d/%m/%Y")
    
    x1=rep("Certificado Part. Simon Bolivar",1,nrow(cf))
    
    x2=c()
    for(i in 1:length(x1)){
      x2[i]=paste(x1[i],paste(substr(cf[i,3],4,5),substr(cf[i,3],7,10),sep = ""),sep = "")
    }
    
    x3=cbind(x1,x2,cf)
    names(x3)=names(c3) 
    
  }#final if CF
  
  
  if(class(cb)[1]=="try-error"){
    print("No hay pesta?a CB")
    #CARACTERISTICAS DEFINITIVA
    #C3=rbind(c3,d3) 
    
    #En caso que no lea bien un numero
    #C3$Cupon=as.numeric(sub(",",".",C3$Cupon))
    
    #return(C3)  
    y3=1
  }else{

    #leo cb
    cb=cb[-which(is.na(cb[,2])),c(3,4,5,7,9,10,12,14)]
    cb <- as.data.frame(cb)
    #cambio formato fechas
    cb[,2]=format(cb[,2],"%d/%m/%Y")
    cb[,3]=format(cb[,3],"%d/%m/%Y")
    cb[,6]=format(cb[,6],"%d/%m/%Y")
    cb[,7]=format(cb[,7],"%d/%m/%Y")
    
    y1=rep("Certificado Part. Bandes",1,nrow(cb))
    
    y2=c()
    for(i in 1:length(y1)){
      y2[i]=paste(y1[i],paste(substr(cb[i,3],4,5),substr(cb[i,3],7,10),sep = ""),sep = "")
    }
    
    y3=cbind(y1,y2,cb)
    names(y3)=names(c3) 
  }#final if CB
  
  #CARACTERISTICAS DEFINITIVA
  #if de pesta?as
  if(length(c3)==1|length(d3)==1|length(v3)==1|length(w3)==1|length(x3)==1|length(y3)==1){
    print("No hay una pesta?a!") 
    ve=c(length(c3),length(d3),length(v3),length(w3),length(x3),length(y3))
    h1=which(ve==1)
    
    if(h1==2){
      print("Falta pesta?a DPN-TICC")
      C3=rbind(c3,v3,w3,x3,y3)
      return(C3)
    }
    if(h1==3){
      print("Falta pesta?a VB")
      C3=rbind(c3,d3,w3,x3,y3)
      return(C3)
    }
    if(h1==4){
      print("Falta pesta?a VF")
      C3=rbind(c3,d3,v3,x3,y3)
      return(C3)
    }
    if(h1==5){
      print("Falta pesta?a CF")
      C3=rbind(c3,d3,v3,w3,y3)
      return(C3)
    }
    if(h1==6){
      print("Falta pesta?a CB")
      C3=rbind(c3,d3,v3,w3,x3)
      return(C3)
    }
    
  }else{
    print("Existen todas las pesta?as!")   
    C3=rbind(c3,d3,v3,w3,x3,y3) 
    #En caso que no lea bien un numero
    C3$Cupon=as.numeric(sub(",",".",C3$Cupon))
    
    return(C3)}#final if pesta?as
  
}#final funcion Caracteristicas

ca <- Carac(ruta)
ca1 <- Carac(ruta1)

###########
#precios 022
#funcion inicial
#ca:mes de evaluacion, ej: "01" para Enero
#dh:dÌa habil o vector de dias habiles  
#ar:nombre documento 0-22, ej: "0-22.xls"  
#ruta: direccion del documento 0-22 
Preciosbcv_original=function(ca,dh,ar,ruta){
  b5=array()
  print(ca)
  print(ar)
  
  for(i in dh){
    nn=0
    if(i<10){ #IF MENOR A 10   
      m1=paste("022 0",i,sep="")
      m2=paste(m1,ca,sep = "-")
      fe.=paste(ca,"2017",sep="/")
      fe=paste(i,fe.,sep = "/")
      fe1=paste(0,fe,sep = "")
      ar1=paste(ruta,ar,sep = "/")
      b=try(read.xlsx(ar1, sheetName = m2,startRow = 10,colIndex = 1:9,header = TRUE),silent = T)
      
      if(class(b)=="try-error"){ #IF ESPACIO
        print("hay un espacio" )
        m3=paste(m2,"",sep = " ")
        ar1=paste(ruta,ar,sep = "/")
        b=try(read.xlsx(ar1, sheetName = m3,startRow = 10,colIndex = 1:9,header = TRUE),silent = T)
        if(ncol(b)==8){#if solo letras
          print("Existe un dia donde hubo solo op de Letras!")
          b=data.frame(b,nueva=rep(0,dim(b)[1]))
        }#final if solo letras
        
        names(b)[9]="Cupón...."
        
        #if 
        if(class(b)=="try-error"){
          print("el doc 0-22 no corresponde con el mes ingresado o existe un problema con el nombre de la pestaña" )
          print("Favor Revisar el dia")
          print(i)
        }#final if error lectura
        
        
        if(is.na(b[1,5])!=T){ 
          if(ncol(b)==8){#if solo letras
            print("Existe un dia donde hubo solo op de Letras!")
            b=data.frame(b,nueva=rep(0,dim(b)[1]))
          }#final if solo letras
          
          names(b)[9]="Cupón...."
          b1=b[-which(is.na(b[,1])),]
          
          #filtro para cuando no haya TICC
          if(length(which(is.na(b1[,8])))!=0){
            b2=b1[-which(is.na(b1[,8])),]
            b3=b2[-which(b2[,1]=="Instrumento "),]
            b3.=rep(fe1,length(b3[,1]))
            b4=cbind(b3.,b3)
            b5=rbind(b5,b4)
            b5[which(substr(b5[,1],1,3)=="LTB"),10]=0
            b5=faux(b5)
          }#final if na en promedio
          
          if(length(which(is.na(b1[,8])))==0){
            b3.=rep(fe1,length(b1[,1]))
            b4=cbind(b3.,b1)
            b5=rbind(b5,b4)
            b5[which(substr(b5[,2],1,3)=="LTB"),10]=0
            b5=faux(b5)
          }
          nn=nn+1
        }
        
        if(is.na(b[1,5])==T){ 
          #caso en que no hay operaciones
          print("No hubo operacion el dia ")
          print(i)
          
        }
        
      }#final if espacio
      #caso normal
      
      if(is.na(b[1,5])!=T & nn==0){  
        if(ncol(b)==8){#if solo letras
          print("Existe un dia donde hubo solo op de Letras!")
          b=data.frame(b,nueva=rep(0,dim(b)[1]))
        }#final if solo letras
        
        names(b)[9]="Cupón...."
        b1=b[-which(is.na(b[,1])),]
        
        #filtro para cuando no haya TICC
        if(length(which(is.na(b1[,8])))!=0){
          b2=b1[-which(is.na(b1[,8])),]
          b3=b2[-which(b2[,1]=="Instrumento "),]
          b3.=rep(fe1,length(b3[,1]))
          b4=cbind(b3.,b3)
          b5=rbind(b5,b4)
          b5[which(substr(b5[,2],1,3)=="LTB"),10]=0
          b5=faux(b5)
        }
        
        if(length(which(is.na(b1[,8])))==0){
          b3.=rep(fe1,length(b1[,1]))
          b4=cbind(b3.,b1)
          b5=rbind(b5,b4)
          b5[which(substr(b5[,2],1,3)=="LTB"),10]=0
          b5=faux(b5)
        }
      }
      
      if(is.na(b[1,5])==T){ 
        #caso en que no hay operaciones
        print("No hubo operacion el dia ")
        print(i)
      }
      
    }#final if < 10
    
    if(i>=10){ #IF MAYOR A 10    
      m1=paste("022 ",i,sep="")
      m2=paste(m1,ca,sep = "-")
      fe.=paste(ca,"2017",sep="/")
      fe=paste(i,fe.,sep = "/")
      ar1=paste(ruta,ar,sep = "/")
      b=try(read.xlsx(ar1, sheetName = m2,startRow = 10,colIndex = 1:9,header = TRUE),silent = T)
      
      if(class(b)=="try-error"){ #IF ESPACIO
        print("hay un espacio" )
        m3=paste(m2,"",sep = " ")
        ar1=paste(ruta,ar,sep = "/")
        b=try(read.xlsx(ar1, sheetName = m3,startRow = 10,colIndex = 1:9,header = TRUE),silent = T)
        names(b)[9]="Cupón...."
        
        if(class(b)=="try-error"){
          print("el doc 0-22 no corresponde con el mes ingresado o existe un problema con el nombre de la pestaña" )
          print("Favor Revisar el dia")
          print(i)
        }
        
        if(is.na(b[1,5])!=T){ 
          if(ncol(b)==8){#if solo letras
            print("Existe un dia donde hubo solo op de Letras!")
            b=data.frame(b,nueva=rep(0,dim(b)[1]))
          }#final if solo letras
          
          names(b)[9]="Cupón...."
          b1=b[-which(is.na(b[,1])),]
          
          #filtro para cuando no haya TICC
          if(length(which(is.na(b1[,8])))!=0){
            b2=b1[-which(is.na(b1[,8])),]
            b3=b2[-which(b2[,1]=="Instrumento "),]
            b3.=rep(fe,length(b3[,1]))
            b4=cbind(b3.,b3)
            b5=rbind(b5,b4)
            b5[which(substr(b5[,2],1,3)=="LTB"),10]=0
            b5=faux(b5)
          }
          
          if(length(which(is.na(b1[,8])))==0){
            b3.=rep(fe,length(b1[,1]))
            b4=cbind(b3.,b1)
            b5=rbind(b5,b4)
            b5[which(substr(b5[,2],1,3)=="LTB"),10]=0
            b5=faux(b5)
          }
          nn=nn+1
        }
        
        if(is.na(b[1,5])==T){ 
          #caso en que no hay operaciones
          print("No hubo operacion el dia ")
          print(i)
        }
        
      }#final if espacio
      
      #CASO NORMAL
      if(is.na(b[1,5])!=T & nn==0){ 
        if(ncol(b)==8){#if solo letras
          print("Existe un dia donde hubo solo op de Letras!")
          b=data.frame(b,nueva=rep(0,dim(b)[1]))
        }#final if solo letras
        
        names(b)[9]="Cupón...."
        b1=b[-which(is.na(b[,1])),]
        
        #filtro para cuando no haya TICC
        if(length(which(is.na(b1[,8])))!=0){
          b2=b1[-which(is.na(b1[,8])),]
          b3=b2[-which(b2[,1]=="Instrumento "),]
          b3.=rep(fe,length(b3[,1]))
          b4=cbind(b3.,b3)
          b5=rbind(b5,b4)
          b5[which(substr(b5[,2],1,3)=="LTB"),10]=0
          b5=faux(b5)
        }
        
        if(length(which(is.na(b1[,8])))==0){
          b3.=rep(fe,length(b1[,1]))
          b4=cbind(b3.,b1)
          b5=rbind(b5,b4)
          b5[which(substr(b5[,2],1,3)=="LTB"),10]=0
          b5=faux(b5)
        }
        
      }
      
      if(is.na(b[1,5])==T){
        #caso en que no hay operaciones
        print("No hubo operacion el dia ")
        print(i)
      }
      
    }#final if > 10
    
  }#final for
  
  if(nrow(b5)!=1){
    b6=b5[-1,]
    
    b6=faux(b6)
    
    return(b6)}else{
      print("No hay documento disponible")
    } 
}#final funcion

#funcion auxiliar
#funcion que le da formato al data frame b6
faux=function(b6){
  b6[,5] =as.numeric(sub(",",".",b6[,5]))
  b6[,3]=as.numeric(sub(",",".",b6[,3]))
  b6[,4]=as.numeric(sub(",",".",b6[,4]))
  b6[,6]=as.numeric(sub(",",".",b6[,6]))
  b6[,7]=as.numeric(sub(",",".",b6[,7]))
  b6[,8]=as.numeric(sub(",",".",b6[,8]))
  b6[,9]=as.numeric(sub(",",".",b6[,9]))
  b6[,10]=as.numeric(sub(",",".",b6[,10]))
  return(b6)
}

#funcion mejorada
ca <- "09"
dh <- c(3,4,5,6,7,11,12,13,14,17,18,19,20,21,24,25,26,27,28)
     #   ,17,18,19,20,21,24,)
ar <- "0-22.xlsx"
ruta <- "/Users/freddytapia/Riesgo-de-Mercado/app/data"

#funcion preciosbcv mejorada
#lee el doc de la 0-22 y usa la funcion read_excel
#se arreglo el error del nombre de la pestaña
#no es la mejor version
Preciosbcv_mejorada=function(ca,dh,ar,ruta){
  b5=array()
  print(ca)
  print(ar)
  
  for(i in dh){
    nn=0
    if(i<10){ #IF MENOR A 10   
      m1=paste("022 0",i,sep="")
      m2=paste(m1,ca,sep = "-")
      fe.=paste(ca,"2018",sep="/")
      fe=paste(i,fe.,sep = "/")
      fe1=paste(0,fe,sep = "")
      ar1=paste(ruta,ar,sep = "/")
      #b=try(read.xlsx(ar1, sheetName = m2,startRow = 10,colIndex = 1:9,header = TRUE),silent = T)
      b=try(read_excel(ar1, sheet = m2,range = "A10:I34",col_names = TRUE),silent = TRUE)
      
      
      
      if(class(b)[1]=="try-error"){ #IF ESPACIO
        print("hay un espacio" )
        m3=paste(m2,"",sep = " ")
        ar1=paste(ruta,ar,sep = "/")
        #b=try(read.xlsx(ar1, sheetName = m3,startRow = 10,colIndex = 1:9,header = TRUE),silent = T)
        b=try(read_excel(ar1, sheet = m3,range = "A10:I34",col_names = TRUE),silent = TRUE)
        
        if(class(b)[1]=="try-error"){
          #print("hay un espacio al inicio" )
          m3=paste("",m2,sep = " ")
          ar1=paste(ruta,ar,sep = "/")
          #b=try(read.xlsx(ar1, sheetName = m3,startRow = 10,colIndex = 1:9,header = TRUE),silent = T)
          b=try(read_excel(ar1, sheet = m3,range = "A10:I34",col_names = TRUE),silent = TRUE)
          nn=nn+1
          
        }
        
        
        if(ncol(b)==8){#if solo letras
          print("Existe un dia donde hubo solo op de Letras!")
          b=data.frame(b,nueva=rep(0,dim(b)[1]))
        }#final if solo letras
        
        names(b)[9]="Cupón...."
        
        #if 
        if(class(b)[1]=="try-error"){
          print("el doc 0-22 no corresponde con el mes ingresado o existe un problema con el nombre de la pestaña" )
          print("Favor Revisar el dia")
          print(i)
        }#final if error lectura
        
        
        if(is.na(b[1,5])!=T){ 
          if(ncol(b)==8){#if solo letras
            print("Existe un dia donde hubo solo op de Letras!")
            b=data.frame(b,nueva=rep(0,dim(b)[1]))
          }#final if solo letras
          
          names(b)[9]="Cupón...."
          b1=b[-which(is.na(b[,1])),]
          
          #filtro para cuando no haya TICC
          if(length(which(is.na(b1[,8])))!=0){
            b2=b1[-which(is.na(b1[,8])),]
            b3=b2[-which(b2[,1]=="Instrumento"),]
            b3.=rep(fe1,length(b3[,1]))
            b4=cbind(b3.,b3)
            b5=rbind(b5,b4)
            b5[which(substr(b5[,1],1,3)=="LTB"),10]=0
            b5=faux(b5)
          }#final if na en promedio
          
          if(length(which(is.na(b1[,8])))==0){
            b3.=rep(fe1,length(b1[,1]))
            b4=cbind(b3.,b1)
            b5=rbind(b5,b4)
            b5[which(substr(b5[,2],1,3)=="LTB"),10]=0
            b5=faux(b5)
          }
          nn=nn+1
        }
        
        if(is.na(b[1,5])==T  & nn==0){ 
          #caso en que no hay operaciones
          print("No hubo operacion el dia ")
          print(i)
          
        }
        
      }#final if espacio
      #caso normal
      
      if(is.na(b[1,5])!=T & nn==0){  
        if(ncol(b)==8){#if solo letras
          print("Existe un dia donde hubo solo op de Letras!")
          b=data.frame(b,nueva=rep(0,dim(b)[1]))
        }#final if solo letras
        
        names(b)[9]="Cupón...."
        b1=b[-which(is.na(b[,1])),]
        
        #filtro para cuando no haya TICC
        if(length(which(is.na(b1[,8])))!=0){
          b2=b1[-which(is.na(b1[,8])),]
          b3=b2[-which(b2[,1]=="Instrumento"),]
          b3.=rep(fe1,length(b3[,1]))
          b4=cbind(b3.,b3)
          b5=rbind(b5,b4)
          b5[which(substr(b5[,2],1,3)=="LTB"),10]=0
          b5=faux(b5)
        }
        
        if(length(which(is.na(b1[,8])))==0){
          b3.=rep(fe1,length(b1[,1]))
          b4=cbind(b3.,b1)
          b5=rbind(b5,b4)
          b5[which(substr(b5[,2],1,3)=="LTB"),10]=0
          b5=faux(b5)
        }
      }
      
      if(is.na(b[1,5])==T){ 
        #caso en que no hay operaciones
        print("No hubo operacion el dia ")
        print(i)
      }
      
    }#final if < 10
    
    if(i>=10){ #IF MAYOR A 10    
      m1=paste("022 ",i,sep="")
      m2=paste(m1,ca,sep = "-")
      fe.=paste(ca,"2018",sep="/")
      fe=paste(i,fe.,sep = "/")
      ar1=paste(ruta,ar,sep = "/")
      #b=try(read.xlsx(ar1, sheetName = m2,startRow = 10,colIndex = 1:9,header = TRUE),silent = T)
      b=try(read_excel(ar1, sheet = m2,range = "A10:I34",col_names = TRUE),silent = TRUE)
      
      
      if(class(b)[1]=="try-error"){ #IF ESPACIO
        print("hay un espacio" )
        print("en el día")
        print(i)
        m3=paste(m2,"",sep = " ")
        ar1=paste(ruta,ar,sep = "/")
        #b=try(read.xlsx(ar1, sheetName = m3,startRow = 10,colIndex = 1:9,header = TRUE),silent = T)
        b=try(read_excel(ar1, sheet = m3,range = "A10:I34",col_names = TRUE),silent = TRUE)
      
        
        #names(b)[9]="Cupón...."
        if(class(b)[1]=="try-error"){
          #print("hay un espacio al inicio" )
          m3=paste("",m2,sep = " ")
          ar1=paste(ruta,ar,sep = "/")
          #b=try(read.xlsx(ar1, sheetName = m3,startRow = 10,colIndex = 1:9,header = TRUE),silent = T)
          b=try(read_excel(ar1, sheet = m3,range = "A10:I34",col_names = TRUE),silent = TRUE)
          nn=nn+1
     
        }
        
        
        if(class(b)[1]=="try-error"){
          print("el doc 0-22 no corresponde con el mes ingresado o existe un problema con el nombre de la pestaña" )
          print("Favor Revisar el dia")
          print(i)
          break
        }
        
        if(is.na(b[1,5])!=T){ 
          if(ncol(b)==8){#if solo letras
            print("Existe un dia donde hubo solo op de Letras!")
            b=data.frame(b,nueva=rep(0,dim(b)[1]))
          }#final if solo letras
          
          names(b)[9]="Cupón...."
          b1=b[-which(is.na(b[,1])),]
          
          #filtro para cuando no haya TICC
          if(length(which(is.na(b1[,8])))!=0){
            b2=b1[-which(is.na(b1[,8])),]
            b3=b2[-which(b2[,1]=="Instrumento"),]
            b3.=rep(fe,length(b3[,1]))
            b4=cbind(b3.,b3)
            b5=rbind(b5,b4)
            b5[which(substr(b5[,2],1,3)=="LTB"),10]=0
            b5=faux(b5)
          }
          
          if(length(which(is.na(b1[,8])))==0){
            b3.=rep(fe,length(b1[,1]))
            b4=cbind(b3.,b1)
            b5=rbind(b5,b4)
            b5[which(substr(b5[,2],1,3)=="LTB"),10]=0
            b5=faux(b5)
          }
          nn=nn+1
        }
        
        if(is.na(b[1,5])==T & nn==0){ 
          #caso en que no hay operaciones
          print("No hubo operacion el dia ")
          print(i)
        }
        
      }#final if espacio
      
      #CASO NORMAL
      if(is.na(b[1,5])!=T & nn==0){ 
        if(ncol(b)==8){#if solo letras
          print("Existe un dia donde hubo solo op de Letras!")
          b=data.frame(b,nueva=rep(0,dim(b)[1]))
        }#final if solo letras
        
        names(b)[9]="Cupón...."
        b1=b[-which(is.na(b[,1])),]
        
        #filtro para cuando no haya TICC
        if(length(which(is.na(b1[,8])))!=0){
          b2=b1[-which(is.na(b1[,8])),]
          b3=b2[-which(b2[,1]=="Instrumento"),]
          b3.=rep(fe,length(b3[,1]))
          b4=cbind(b3.,b3)
          b5=rbind(b5,b4)
          b5[which(substr(b5[,2],1,3)=="LTB"),10]=0
          b5=faux(b5)
        }
        
        if(length(which(is.na(b1[,8])))==0){
          b3.=rep(fe,length(b1[,1]))
          b4=cbind(b3.,b1)
          b5=rbind(b5,b4)
          b5[which(substr(b5[,2],1,3)=="LTB"),10]=0
          b5=faux(b5)
        }
        
      }
      
      if(is.na(b[1,5])==T){
        #caso en que no hay operaciones
        print("No hubo operacion el dia ")
        print(i)
      }
      
    }#final if > 10
    
  }#final for
  
  if(nrow(b5)!=1){
    b6=b5[-1,]
    
    b6=faux(b6)
    
    return(b6)}else{
      print("No hay documento disponible")
    } 
}#final funcion

#prueba
a <- Preciosbcv(ca,dh,ar,ruta)

#0-22 de Agosto
ca <- "08"
dh <- c(1,2,3,6,7,8,9,10,13,14,15,16,17,21,22,23,24,27,28,29,30,31)
ar <- "resumersec0818.xls"
ruta <- "/Users/freddytapia/Downloads"

a1 <- Preciosbcv(ca,dh,ar,ruta)

#0-22 de Julio
ca <- "07"
dh <- c(3,4,6,9,10,11,12,13,16,17,18,19,20,23,25,26,27,30,31)
ar <- "resumersec0718.xlsx"
ruta <- "/Users/freddytapia/Downloads"

a2 <- Preciosbcv(ca,dh,ar,ruta)

ruta <- paste(ruta,ar,sep = "/")
ruta

#funcion preciosbcv definitiva
#funcion mas optima que lee el numero de la pestaña
#tiene validador para saber si lee pestaña 0-22

Preciosbcv=function(ruta){
  b5=array()
  pes <- excel_sheets(ruta)
  
  
  for(i in 1:(length(pes))){
    nn=0
    
      #Validador de lectura pestaña 0-22
      if(length(grep(pattern ='022',pes[i],fixed = TRUE))==1){
        b=try(read_excel(ruta, sheet = i,range = "A10:I34",col_names = TRUE),silent = TRUE)
        
        #verificardor posible error
        if(class(b)[1]=="try-error"){ #IF ESPACIO
          print("Error de lectura")
          print("Favor revisar la pestaña")
          print(pes[i])
        }
        
        #leo nombre del dia de operacion
        #ojo es una manera, la otra forma seria usar el vector de dh
        name <- try(read_excel(ruta, sheet = i,range = "A2",col_names = FALSE),silent = TRUE)
        fecha <- substr(name,68,77)
        
        
        if(ncol(b)==8){#if solo letras
          print("Existe un dia donde hubo solo op de Letras!")
          b=data.frame(b,nueva=rep(0,dim(b)[1]))
        }#final if solo letras
        
        
        
        if(is.na(b[1,5])!=T){ 
          
          if(ncol(b)==8){#if solo letras
            print("Existe un dia donde hubo solo op de Letras!")
            b=data.frame(b,nueva=rep(0,dim(b)[1]))
          }#final if solo letras
          
          names(b)[9]="Cupón...."
          b1=b[-which(is.na(b[,1])),]
          
          #filtro para cuando no haya TICC
          if(length(which(is.na(b1[,8])))!=0){
            b2=b1[-which(is.na(b1[,8])),]
            b3=b2[-which(b2[,1]=="Instrumento"),]
            b3.=rep(fecha,length(b3[,1]))
            b4=cbind(b3.,b3)
            b5=rbind(b5,b4)
            b5[which(substr(b5[,1],1,3)=="LTB"),10]=0
            b5=faux(b5)
          }else{
            b3.=rep(fecha,length(b1[,1]))
            b4=cbind(b3.,b1)
            b5=rbind(b5,b4)
            b5[which(substr(b5[,2],1,3)=="LTB"),10]=0
            b5=faux(b5)
          }
          nn=nn+1
        }
        
        # if(is.na(b[1,5])==T  & nn==0){
        #   #caso en que no hay operaciones
        #   print("No hubo operacion el dia ")
        #   print(pes[i])
        # }
        
        #caso normal
        
        if(is.na(b[1,5])!=T & nn==0){  
          if(ncol(b)==8){#if solo letras
            print("Existe un dia donde hubo solo op de Letras!")
            b=data.frame(b,nueva=rep(0,dim(b)[1]))
          }#final if solo letras
          
          names(b)[9]="Cupón...."
          b1=b[-which(is.na(b[,1])),]
          
          #filtro para cuando no haya TICC
          if(length(which(is.na(b1[,8])))!=0){
            b2=b1[-which(is.na(b1[,8])),]
            b3=b2[-which(b2[,1]=="Instrumento"),]
            b3.=rep(fecha,length(b3[,1]))
            b4=cbind(b3.,b3)
            b5=rbind(b5,b4)
            b5[which(substr(b5[,2],1,3)=="LTB"),10]=0
            b5=faux(b5)
          }else{
            b3.=rep(fecha,length(b1[,1]))
            b4=cbind(b3.,b1)
            b5=rbind(b5,b4)
            b5[which(substr(b5[,2],1,3)=="LTB"),10]=0
            b5=faux(b5)
          }
        }
        
        if(is.na(b[1,5])==T){
          #caso en que no hay operaciones
          print("No hubo operacion el dia ")
          print(pes[i])
        }
        
        
      }else{
        # print("La pestaña")
        # print(pes[i])
        # print("No es pestaña 0-22")
      }
      
    
  }#final for
  
  if(nrow(b5)!=1){
    b6=b5[-1,]
    
    b6=faux(b6)
    
    return(b6)}else{
      print("No hay documento disponible")
    } 
}#final funcion

#Funcion Formato

formatop=function(C3,b3){
  
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
      B3$Rendimiento[i]=bond.yield(as.Date(B3$`Fecha op`[i],"%d/%m/%Y"),as.Date(B3$F.Vencimiento[i],"%d/%m/%Y"),(B3$Cupon[i]/100), B3$Frecuencia[i],B3$`Pre prom`[i],convention = c("ACT/360"),B3$Frecuencia[i])*100
    }
    if(substr(B3$Sicet[i],1,3)=="LTB"){
      #B3$Rendimiento=bond.yields(as.Date(B3$`Fecha op`,"%d/%m/%Y"),as.Date(B3$F.Vencimiento,"%d/%m/%Y"),(B3$Cupon/100), B3$Frecuencia,B3$`Pre prom`,convention = c("30/360"),B3$Frecuencia)*100
      B3$Rendimiento[i]=bond.yield(as.Date(B3$`Fecha op`[i],"%d/%m/%Y"),as.Date(B3$F.Vencimiento[i],"%d/%m/%Y"),0, 0.001,B3$`Pre prom`[i],convention = c("ACT/360"),0)*100
    }
  }
  
  return(B3)
}#final funcion formato precios 022

#prueba Enero
ruta <- "/Users/freddytapia/Downloads/resumersec0118.xls"
a_ene <- Preciosbcv(ruta)

C_ene <- Carac("/Users/freddytapia/Downloads/Caracteristicas 31-01-2018.xls")

hist_ene <- formatop(C_ene,a_ene)

#prueba Febrero
ruta <- "/Users/freddytapia/Downloads/resumersec0218.xls"
a_feb <- Preciosbcv(ruta)

C_feb <- Carac("/Users/freddytapia/Downloads/Caracteristicas 27-02-2018.xls")

hist_feb <- formatop(C_feb,a_feb)

#prueba Marzo
ruta <- "/Users/freddytapia/Downloads/resumersec03.xls"
a_mar <- Preciosbcv(ruta)

C_mar <- Carac("/Users/freddytapia/Downloads/Caracteristicas 27-03-2018.xls")

hist_mar <- formatop(C_mar,a_mar)



#prueba Abril
ruta <- "/Users/freddytapia/Downloads/resumersec0418.xls"
a_abr <- Preciosbcv(ruta)

hist_abr <- formatop(C_jun,a_abr)


#prueba Mayo
ruta <- "/Users/freddytapia/Downloads/resumersec0518.xls"
a_may <- Preciosbcv(ruta)

hist_may <- formatop(C_jun,a_may)


#prueba Junio
ruta <- "/Users/freddytapia/Downloads/resumersec0618.xls"
a_jun <- Preciosbcv(ruta)

C_jun <- Carac("/Users/freddytapia/Documents/29-06-18.xls")

hist_jun <- formatop(C_jun,a_jun)

#prueba Julio
ruta <- "/Users/freddytapia/Downloads/resumersec0718.xls"
a_jul <- Preciosbcv(ruta)

C_jul <- Carac("/Users/freddytapia/Downloads/Caracteristicas 30-07-2018.xls")

hist_jul <- formatop(C_jul,a_jul)

#prueba Agosto
ruta <- "/Users/freddytapia/Downloads/resumersec0818.xls"
a_ago <- Preciosbcv(ruta)

hist_ago <- formatop(C_oct,a_ago)

#Septiembre
ruta <- "/Users/freddytapia/Downloads/resumersec0918.xls"
a_sep <- Preciosbcv(ruta)

hist_sept <- formatop(C_oct,a_sep)

#Octubre
ruta <- "/Users/freddytapia/Downloads/resumersec1018.xls"
a_oct <- Preciosbcv(ruta)

#carac octubre
ruta <- "/Users/freddytapia/.Trash/Riesgo-de-Mercado/app/data/Caracteristicas.xls"

C_oct <- Carac(ruta)

hist_oct <- formatop(C_oct,a_oct)

hist<- rbind.data.frame(hist_ene,hist_feb,hist_mar,hist_abr,hist_may,hist_jun,hist_jul,hist_ago,hist_sept,hist_oct)

#convierto fecha de op y venc en fechas
hist$`Fecha op` <- as.Date(as.character(hist$`Fecha op`),format="%d/%m/%Y")
hist$F.Vencimiento <- as.Date(as.character(hist$F.Vencimiento),format="%d/%m/%Y")

#este data frame es el que utiliza la metodologia Spline para los calculos
hist <- dplyr::arrange(hist,(`Fecha op`))

#tit diferentes en 2018
a <- levels(hist$Nombre)

#posiciones en el historico donde se encuentra un tit
b <- list()

for(i in 1:length(a)){
b[[i]] <-  which(a[i]==hist$Nombre)
}

#precio promedio 2018
c <- c()

for(i in 1:length(a)){
  c[i] <-  mean(hist$`Pre prom`[b[[i]]])
}

#tit con su precio prom
d <- cbind.data.frame("titulos"=a,"precio prom"=c)

#precios prom tif 2018
d_tif <- d[which(substr(d$titulos,1,3)=="TIF"),]

#precios prom veb 2018
d_veb <- d[which(substr(d$titulos,1,3)=="VEB"),]

#titulos

tif=c("TIF082018","TIF042019","TIF082019",
      "TIF112019","TIF102020","TIF112020","TIF022021","TIF032022","TIF042023",
      "TIF012024","TIF062025","TIF012026","TIF112027","TIF032028","TIF052028",
      "TIF022029","TIF032029","TIF022030","TIF102030","TIF022031","TIF032031",
      "TIF022032","TIF032032","TIF032033","TIF052034")

#VEBONOS iniciales
veb=c("VEBONO072018","VEBONO022019","VEBONO032019","VEBONO042019","VEBONO102019","VEBONO012020",
       "VEBONO062020","VEBONO092020","VEBONO112020","VEBONO012021","VEBONO052021",
       "VEBONO122021","VEBONO022022","VEBONO012023","VEBONO022024","VEBONO042024",
       "VEBONO012025","VEBONO022025","VEBONO062026","VEBONO032027","VEBONO042028",
       "VEBONO102028","VEBONO052029","VEBONO102029","VEBONO072030","VEBONO032031",
       "VEBONO062032","VEBONO072033","VEBONO022034")


#titulos de la app que salen en 2018
e <- c()

for(i in 1:length(tif)){
e[i] <- length(which(tif[i]==as.character(d_tif$titulos)))
}

#los ceros son que no salen
e1 <- cbind.data.frame("tit"=tif,"aparece"=e)

#Titulos en app que no salen en 2018
e2 <- as.character(e1[which(e1$aparece==0),1])
e2

#busco dichos titulos en 2017
#cargo data 2016-2017
library(lubridate)
Data_splines <- read.csv("/Users/freddytapia/.Trash/Riesgo-de-Mercado/app/data/Data_splines.txt", sep="")

Data_splines$Fecha.op <- as.Date(as.character(Data_splines$Fecha.op))

Data_splines$year <- year(Data_splines$Fecha.op)

data_2017 <- Data_splines[which(Data_splines$year==2017),]

le <- levels(as.factor(as.character(unique(data_2017$Nombre))))

#posiciones en el historico donde se encuentra un tit
p <- list()

for(i in 1:length(le)){
  p[[i]] <-  which(le[i]==data_2017$Nombre)
}

#precio promedio 2017
p1 <- c()

for(i in 1:length(le)){
  p1[i] <-  mean(data_2017$Pre.prom[p[[i]]])
}

#tit con su precio prom
p2 <- cbind.data.frame("titulos"=le,"Pre prom"=p1)

#precios prom tif 2017
tif17 <- p2[which(substr(p2$titulos,1,3)=="TIF"),]

#precios prom veb 2017
veb17 <- p2[which(substr(p2$titulos,1,3)=="VEB"),]

#los titulos a buscar son e2 pues no salen en 2018
e2

#titulos de la app que no salen en 2018
g <- c()

for(i in 1:length(e2)){
  g[i] <- length(which(e2[i]==as.character(tif17$titulos)))
}

#los ceros son que no salen
g1 <- cbind.data.frame("tit"=e2,"aparece"=g)

#Titulos en app que no salen en 2017
g2 <- as.character(g1[which(g1$aparece==0),1])
g2

#consolidado 2017-2018 precio promedio TIF

#Titulos en app que salen en 2018
t18 <- as.character(e1[which(e1$aparece==1),1])

h <- c()
for(i in 1:length(t18)){
  h[i] <- which(t18[i]==as.character(d_tif$titulos))
}

T18 <- d_tif[h,]
T18$year <- "2018"

#titulos que salen en 2017
t17 <- as.character(g1[which(g1$aparece==1),1])

h1 <- c()
for(i in 1:length(t17)){
  h1[i] <- which(t17[i]==as.character(tif17$titulos))
}

T17 <- tif17[h1,]
T17$year <- "2017"


names(T18)=names(T17)

TIF <- rbind.data.frame(T18,T17)

#verifico si estan en 2016
data_2016 <- Data_splines[which(Data_splines$year==2016),]

le16 <- levels(as.factor(as.character(unique(data_2016$Nombre))))

which(g2[1]==le16)

#No salen los titulos de g2 en 2016 la opcion es asignar precio prom
#de instrumentos similares

#titulos que agregare a precios q faltan
titulos <- c("TIF112019","TIF112020","TIF012026","TIF022030","TIF032031")

k <- c()

for(i in 1:length(titulos)){
k[i] <- which(titulos[i]==TIF)
}

k1 <- cbind.data.frame("tit"=g2,"precio"=TIF$`Pre prom`[k])
k1$year <- "0"  
names(k1)=names(TIF)

#TIF definitivo
TIF_PROM <- rbind.data.frame(TIF,k1)
  
#Caso VEBONOS

#titulos de la app que salen en 2018
ev <- c()

for(i in 1:length(veb)){
  ev[i] <- length(which(veb[i]==as.character(d_veb$titulos)))
}

#los ceros son que no salen
e1v <- cbind.data.frame("tit"=veb,"aparece"=ev)

#Titulos en app que no salen en 2018
e2v <- as.character(e1v[which(e1v$aparece==0),1])
e2v

#busco estos instrumentos en la data 2017
veb17

#titulos de la app que no salen en 2018
gv <- c()

for(i in 1:length(e2v)){
  gv[i] <- length(which(e2v[i]==as.character(veb17$titulos)))
}

#los ceros son que no salen
g1v <- cbind.data.frame("tit"=e2v,"aparece"=gv)

#Titulos en app que no salen en 2017
g2v <- as.character(g1v[which(g1v$aparece==0),1])
g2v

l <- list()

for(i in 1:length(g2v)){
l[[i]] <- which(g2v[i]==data_2016$Nombre)
}

#
l1 <- c()

for(i in 1:length(l)){
  l1[i] <- mean(data_2016$Pre.prom[l[[i]]])
  
}

#veb q salen en 2016

l2 <- cbind.data.frame("tit"=g2v,"pre"=l1)

#veb 2016
l3 <- l2[c(1,2,5),]
l3$year <- "2016"


x <- as.character(e1v[which(e1v$aparece==1),1])

x1 <- c()
for(i in 1:length(x)){
x1[i] <- which(x[i]==d_veb$titulos)
}

x2 <- d_veb[x1,]
x2$year <- "2018"

#titulos veb q no salen
l4 <- as.character(l2[c(3,4,6,7,8,9),1])

#funcion que me calcula el precio promedio a partir de la historia de la 022
head(hist)

#imputs: 
#hist: historico 022 con el que se va a trabajar
#ye: año donde se quiere buscar el precio promedio
#el mismo debe estar en el historico
#output: 
#f: lista de 3 elementos
#f1: total de titulos en el historico con su precio promedio
#f2: precio promedio TIF
#f3: precio promedio VEBONO
pre_prom <- function(hist,ye){
  
  hist$year <- year(hist[,3])
  
  hist <- hist[which(hist$year==ye),]
  
  #tit diferentes en 2018
  a <- levels(as.factor(as.character(unique(hist[,2]))))
  
  #posiciones en el historico donde se encuentra un tit
  b <- list()
  
  for(i in 1:length(a)){
    b[[i]] <-  which(a[i]==as.character(hist[,2]))
  }
  
  #precio promedio 2018
  c <- c()
  
  for(i in 1:length(a)){
    c[i] <-  mean(hist[b[[i]],12])
  }
  
  f <- list()
  
  #tit con su precio prom
  f[[1]] <- cbind.data.frame("titulos"=a,"precio prom"=c)
  f[[1]]$year <- ye
  
  #precios prom tif 2018
  f[[2]] <- f[[1]][which(substr(f[[1]]$titulos,1,3)=="TIF"),]
  
  #precios prom veb 2018
  f[[3]] <- f[[1]][which(substr(f[[1]]$titulos,1,3)=="VEB"),]
  
  
  return(f)
  
}#final funcion pre_prom


#hist es el historico de 2018
#caso VEBONO
v <- pre_prom(hist,"2018")

v1 <- v[[3]]

#Data_splines contiene historico de 2016, 2017 y parte de 2018
#busco en 2017
v2 <- pre_prom(Data_splines,"2017")

v3 <- v2[[3]]

#busco en 2016
v4 <- pre_prom(Data_splines,"2016")

v5 <- v4[[3]]

#busco los titulos veb en v1 , v3 y v5
#creo funcion que me dicd que titulos de mi cartera esta en un
#determinado año
#imputs
#veb: titulos en mi cartera 
#v1: precios prom de tit en determinado año
#outpus: lista de 2 elementos
#1er: precio promedio de los que salen
#2do: nombre de los q no salen
comp <- function(veb,v1){
  v1$titulos <- as.character(v1$titulos)
  
  x <- c()
  for(i in 1:length(veb)){
    x[i] <- length(which(veb[i]==v1$titulos))
    
  }
  
  #titulos que no salen
  veb1 <- veb[c(which(x==0))]
  
  #titulos que salen
  veb2 <- veb[c(which(x==1))]
  
  #los busco en v1
  y <- c()
  
  for(i in 1:length(veb2)){
    y[i] <- which(veb2[i]==v1$titulos)
  }
  
  #
  veb3 <- v1[y,]
  
  #exporto resultados
  f <- list()
  
  f[[1]] <-  veb3
  
  f[[2]] <- veb1
  
  return(f)
  
}

#veb q salen en 2018 y estan en veb
v6 <- comp(veb,v1)
#no salen en 2018
v7 <- v6[[2]]

#busco v7 en 2017
v8 <- comp(v7,v3)

#veb q no salen en 2017
v9 <- v8[[2]]

#busco en 2016
v10 <- comp(v9,v5)

#vebono que no salen en 2018 , 2017 y 2016
v11 <- v10[[2]]

#historico VEB
VEB <- rbind.data.frame(v6[[1]],v8[[1]],v10[[1]])
  
  
  

