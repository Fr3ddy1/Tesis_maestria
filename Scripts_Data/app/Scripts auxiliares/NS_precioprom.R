#creo funcion que hace variar el precio promedio
#de tal manera que el rendimiento no es negativo
source('~/.Trash/Riesgo-de-Mercado/app/funciones.R')

#Funcion que calcula precios de los titulos considerados
#y exporta una tabla con los resultados, donde es posible 
#optimizar los precios dados inicialmente, de tal manera
#que se asemejen los m?s posible a los precios promedio ingresados
#ARGUMENTOS
#fv: fecha de valoraci?n, ej: "11/08/2017"
#tit: titulo o titulos a considerar, debe ser el nombre corto
#ej: TIF082018 (OJO, mejor considerar el ISIN)
#pr: vector de precios promedios
#pa: parametros Nelson y siegel
#ind: 0 = Tif o 1 = veb
#C: documento de las caracteristicas, que previamente ya ha sido
#leido mediante la funcion "Carac"
#fe2: valor que indica si se optimizaran los precios, 1 (Si) 0 (No)
#fe3: valido solo si se optimizan precios, 1 (paquete Nloptr) 0 (alabama)
Tabla.ns=function(fv,tit,pr,pa,ind,C,fe2,fe3){
  #Creo data frame donde guardare calculo
  Tabla=as.data.frame(matrix(0,14,length(tit)))
  colnames(Tabla)=tit
  rownames(Tabla)=c("ISIN","Fecha de Liquidación",
                    "Fecha de emisión","Fecha de Vencimiento","Tasa de Cupón",
                    "Precio Prom","Fecha último Pago","Fecha próximo pago",
                    "RDTO al VMTO","Duración","Inverso de la duración",
                    "Ponderación","Precio Modelo Nelson y Siegel Ajustado",
                    "Residuos al cuadrado")
  
  #relleno ISIN
  for(i in 1:ncol(Tabla)){
    Tabla[1,i]=as.character(C$Sicet[which(names(Tabla)[i]==C$Nombre)])
  }
  
  #relleno fecha Liquidación
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
  
  #relleno cupón
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
  
  #añado precios promedios
  Tabla[6,]=pr
  
  if(ind==0){
    #añado rendimiento al vencimiento y duracion
    
    #rendimiento
    for(i in 1:ncol(Tabla)){
      Tabla[9,i]=bond.yield(as.Date(fv,format="%d/%m/%Y"),as.Date(Tabla[4,i],"%d/%m/%Y"),as.numeric(gsub("[,]",".",Tabla[5,i])), 4,as.numeric(gsub("[,]",".",Tabla[6,i])),convention = c("ACT/360"),4)
    }
    
    #verifico si rendimiento es negativo
    
    while(length(which(Tabla[9,]<0))!=0){
      #print("Existe rendimiento negativo")
      #print("En las posiciones")
      #print(which(Tabla[9,]<0))
      
      #pido ingresar nuevos valores para las posiciones indicadas
      #print("Favor Ingresar los")
      #print(length(which(Tabla[9,]<0)))
      #print("precios promedio nuevos")
      
      
      vt <- c()
      for(i in 1:length(which(Tabla[9,]<0))){
        #vt[i] <- as.numeric(readline(prompt="Ej: 101.05,  "))
        vt[i] <- find_pre(as.numeric(gsub("[,]",".",Tabla[6,i])),Tabla)
      }
      
      #sustituyo precios promedio
      Tabla[6,which(Tabla[9,]<0)]=vt
      
      #calculo nuevos rendimientos
      #rendimiento
      for(i in 1:ncol(Tabla)){
        Tabla[9,i]=bond.yield(as.Date(fv,format="%d/%m/%Y"),as.Date(Tabla[4,i],"%d/%m/%Y"),as.numeric(gsub("[,]",".",Tabla[5,i])), 4,as.numeric(gsub("[,]",".",Tabla[6,i])),convention = c("ACT/360"),4)
      }
      #muestro tabla
      # View(Tabla)
      
    }#final if rend negativo
    
    
    
    #duracion
    for(i in 1:ncol(Tabla)){
      Tabla[10,i]=bond.duration(as.Date(fv,format="%d/%m/%Y"),as.Date(Tabla[4,i],"%d/%m/%Y"),as.numeric(gsub("[,]",".",Tabla[5,i])), 4,as.numeric(gsub("[,]",".",Tabla[9,i])),convention = c("ACT/360"),4)
    }
    
    #añado inverso duracion
    Tabla[11,]=1/(as.numeric(gsub("[,]",".",Tabla[10,])))
    
    #añado ponderacion
    for(i in 1:ncol(Tabla)){
      Tabla[12,i]=(as.numeric(gsub("[,]",".",Tabla[11,i])))/sum((as.numeric(gsub("[,]",".",Tabla[11,]))))
    }
    
    print("Muestro tabla preliminar")
    
    #CALCULO PRECIOS ESTIMADOS
    fv=as.Date(fv,format="%d/%m/%Y")
    
    #relleno precios
    Tabla[13,]=precio.ns(tit,fv,C,pa)
    
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
      pa=c(x[1],x[2],x[3],x[4])
      p=precio.ns(tit,fv,C,pa) 
      x=sum(((p-pp)*w)^2)
      return(x)
    }
    
    #restricciones
    res <- function(x) {
      h <- rep(NA, 1)
      h[1] <- x[1] # B0 > 0
      h[2] <- x[1]+x[2] # B0 + B1 > 0
      h[3] <- x[4] # lambda1 > 0
      h[4] <- x[2]+0.01
      #h[4] <- x[2] #ojo
      h
    }
    
    #fe2 <- readline(prompt="Desea optimizar?   (1) Si, (0) No    ")
    if(fe2==1){
      # print("Por favor, seleccionar el paquete a usar: ")
      #fe3 <- readline(prompt="Seleccionar (1) para Nloptr, (0) para Alabama   ")
      
      if(fe3==1){
        #Broyden–Fletcher–Goldfarb–Shanno-metodo cuasi Newton
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
        Tabla[13,]=precio.ns(tit,fv,C,ala1$par)
        
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
          Tabla[13,]=precio.ns(tit,fv,C,ala1$par)
        }
        #SRC Nuevo
        print("El nuevo valor del SRC es")
        print(sum(as.numeric(gsub("[,]",".",Tabla[14,]))))
        
        
      }#final if nloptr
      
      if(fe3==0){
        print("Optimizando mediante paquete alabama...")
        ala1=try(alabama::auglag(pa, fn=mifuncion, hin=res)) #mejor igual al solver
        
        if(class(ala1)[1]=="try-error"){
          print("Existe un problema al optimizar")
          break
        }
        
        ala<<-ala1
        Tabla[13,]=precio.ns(tit,fv,C,ala1$par)
        
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
      print("No se  optimizará, se mantendrán los mismos precios")
      #sustituyo precios estimados mas ajustados
      Tabla[13,]=precio.ns(tit,fv,C,pa)
    }
    
    #relleno residuos al cuadrado
    
    for(i in 1:ncol(Tabla)){
      Tabla[14,i]=(((as.numeric(gsub("[,]",".",Tabla[13,i])))-(as.numeric(gsub("[,]",".",Tabla[6,i]))))*(as.numeric(gsub("[,]",".",Tabla[12,i]))))^2
    }
    
    #creo vector de precios para exportar
    precios <- cbind.data.frame(c(tit,"SRC"),c(precio.ns(tit,fv,C,pa),sum(as.numeric(gsub("[,]",".",Tabla[14,])))))
    names(precios) <- c("Títulos","Precios")
    
    
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
      #print("Existe rendimiento negativo")
      #print("En las posiciones")
      #print(which(Tabla[9,]<0))
      
      #pido ingresar nuevos valores para las posiciones indicadas
      #print("Favor Ingresar los")
      #print(length(which(Tabla[9,]<0)))
      #print("precios promedio nuevos")
      
      
      vt <- c()
      for(i in 1:length(which(Tabla[9,]<0))){
        #vt[i] <- as.numeric(readline(prompt="Ej: 101.05,  "))
        vt[i] <- find_pre(as.numeric(gsub("[,]",".",Tabla[6,i])),Tabla)
      }
      
      #sustituyo precios promedio
      Tabla[6,which(Tabla[9,]<0)]=vt
      
      #calculo nuevos rendimientos
      #rendimiento
      for(i in 1:ncol(Tabla)){
        Tabla[9,i]=bond.yield(as.Date(fv,format="%d/%m/%Y"),as.Date(Tabla[4,i],"%d/%m/%Y"),as.numeric(gsub("[,]",".",Tabla[5,i])), 4,as.numeric(gsub("[,]",".",Tabla[6,i])),convention = c("ACT/360"),4)
      }
      #muestro tabla
      # View(Tabla)
      
    }#final if rend negativo
    
    
    
    #duracion
    for(i in 1:ncol(Tabla)){
      Tabla[10,i]=bond.duration(as.Date(fv,format="%d/%m/%Y"),as.Date(Tabla[4,i],"%d/%m/%Y"),as.numeric(gsub("[,]",".",Tabla[5,i])), 4,as.numeric(gsub("[,]",".",Tabla[9,i])),convention = c("ACT/360"),4)
    }
    
    #añado inverso duracion
    Tabla[11,]=1/(as.numeric(gsub("[,]",".",Tabla[10,])))
    
    #añado ponderacion
    for(i in 1:ncol(Tabla)){
      Tabla[12,i]=(as.numeric(gsub("[,]",".",Tabla[11,i])))/sum((as.numeric(gsub("[,]",".",Tabla[11,]))))
    }
    
    print("Muestro tabla preliminar")
    
    #CALCULO PRECIOS ESTIMADOS
    fv=as.Date(fv,format="%d/%m/%Y")
    
    #relleno precios
    Tabla[13,]=precio.ns(tit,fv,C,pa)
    
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
      pa=c(x[1],x[2],x[3],x[4])
      p=precio.ns(tit,fv,C,pa) 
      x=sum(((p-pp)*w)^2)
      return(x)
    }
    
    #restricciones
    res <- function(x) {
      h <- rep(NA, 1)
      h[1] <- x[1]
      h[2] <- x[1]+x[2]
      h[3] <- x[4]
      h[4] <- x[2]+0.01
      #h[4] <- x[2]
      h
    }
    
    #fe2 <- readline(prompt="Desea optimizar?   (1) Si, (0) No    ")
    if(fe2==1){
      
      # print("Por favor seleccionar un paquete para optimizar")
      #fe3 <- readline(prompt="Seleccionar (1) para el paquete Nloptr, (0) para alabama    ")
      
      if(fe3==1){
        #Broyden–Fletcher–Goldfarb–Shanno-metodo cuasi Newton
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
        Tabla[13,]=precio.ns(tit,fv,C,ala1$par)
        
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
          Tabla[13,]=precio.ns(tit,fv,C,ala1$par)
        }
        #SRC Nuevo
        print("El nuevo valor del SRC es")
        print(sum(as.numeric(gsub("[,]",".",Tabla[14,]))))
        
      }#final if nloptr
      
      if(fe3==0){
        print("Optimizando mediante paquete alabama...")
        ala1=try(alabama::auglag(pa, fn=mifuncion, hin=res)) #mejor igual al solver
        
        if(class(ala1)[1]=="try-error"){
          print("Existe un problema al optimizar")
          break
        }
        
        ala<<-ala1
        Tabla[13,]=precio.ns(tit,fv,C,ala1$par)
        
        #calculo nuevo src
        for(i in 1:ncol(Tabla)){
          Tabla[14,i]=(((as.numeric(gsub("[,]",".",Tabla[13,i])))-(as.numeric(gsub("[,]",".",Tabla[6,i]))))*(as.numeric(gsub("[,]",".",Tabla[12,i]))))^2
        }
        
        #veo si son iguales, en caso de ser cierto optimizo por otro metodo
        q1 <- sum(as.numeric(gsub("[,]",".",Tabla[14,])))
        
        if(q==q1){
          print("Optimización fallida..")
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
      Tabla[13,]=precio.ns(tit,fv,C,pa)
    }
    
    #relleno residuos al cuadrado
    
    for(i in 1:ncol(Tabla)){
      Tabla[14,i]=(((as.numeric(gsub("[,]",".",Tabla[13,i])))-(as.numeric(gsub("[,]",".",Tabla[6,i]))))*(as.numeric(gsub("[,]",".",Tabla[12,i]))))^2
    }
    
    #creo vector de precios para exportar
    precios <- cbind.data.frame(c(tit,"SRC"),c(precio.ns(tit,fv,C,pa),sum(as.numeric(gsub("[,]",".",Tabla[14,])))))
    names(precios) <- c("Títulos","Precios")
    
    #if para exportar resultados
    if(fe2==1){
      Tabla1 <- list(Tabla,ala$par,precios)
    }else if(fe2==0){
      Tabla1 <- list(Tabla,pa,precios)
    }
    
    return(Tabla1)
    
  }#final if ind vebono
  
  
  
}#Final funcion 

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

#-----------------------
#funcion que calcula precio, para no tener un rend negativo
#es una manera de evitar el inconveniente de ingresar el precio promedio 
#de forma manual
#argumentos:
#pre: precio para el cual existe rend negativo
#Tabla: tabla que genera la funcion Tabla.sven
#output:
#precio: precio para el cual no existe rendimiento negativo
find_pre <- function(pre,Tabla){
  
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

#-----------------------

#leo caracteristicas
ca <- (Carac(paste(getwd(),"app","data","Caracteristicas.xls",sep = "/")))

#caracteristicas vieja
ca1 <- read.csv("~/.Trash/Riesgo-de-Mercado/app/data/C.txt", sep="")
names(ca1) <- c("Tipo Instrumento","Nombre","Sicet","F.Emision",
              "F.Vencimiento","Tipo tasa","Inicio","Pago cupon 1" ,
              "Pago cupon 2","Cupon")

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

#parametroa
pa_ns=c(0.133799434790145,-0.01,-0.307885339616438,0.545398124008073)

names(pa_ns) <- c("B0","B1","B2","T1")

#precios 11-08 - tif
pr=c(101,112,110,121.0234,116.5251,130.0234,
     129.0156,125.0626,128.1000,120,124,122,126.5234,128.5235,128.1913,
     129,132.0391,128.5235,129.8875,130.1,128.5313,127,128.5235,127.0156,
     127.0156)

#precios promedio nuevos
Precio_prom_tif <- read.csv("~/.Trash/Riesgo-de-Mercado/app/data/Precio_prom_tif.txt", sep="")

#busco precio promedio
pos1(tit[12],0,Precio_prom_tif)

#funciona bien
tit[c(5,6)]
pr[c(5,6)]
p <- Tabla.ns(fv="08/03/2018",tit[c(5,6,7)],pr[c(5,6,7)],pa=pa_ns,ind=0,C=ca1,fe2=0,fe3=0)

#aqui se da un rendimiento negativo
p1 <- Tabla.ns(fv="08/03/2018",tit[c(5,6,7)],c(130,150,150),pa=pa_ns,ind=0,C=ca1,fe2=0,fe3=0)

fv="08/03/2018"
tit <- tit[c(5,6)]
pr <- c(130,150)
pa=pa_ns
ind=0
C=ca1
fe2=0
fe3=0

#Creo data frame donde guardare calculo
Tabla=as.data.frame(matrix(0,14,length(tit)))
colnames(Tabla)=tit
rownames(Tabla)=c("ISIN","Fecha de Liquidación",
                  "Fecha de emisión","Fecha de Vencimiento","Tasa de Cupón",
                  "Precio Prom","Fecha último Pago","Fecha próximo pago",
                  "RDTO al VMTO","Duración","Inverso de la duración",
                  "Ponderación","Precio Modelo Nelson y Siegel Ajustado",
                  "Residuos al cuadrado")

#relleno ISIN
for(i in 1:ncol(Tabla)){
  Tabla[1,i]=as.character(C$Sicet[which(names(Tabla)[i]==C$Nombre)])
}

#relleno fecha Liquidación
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

#relleno cupón
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

#añado precios promedios
Tabla[6,]=pr

  #rendimiento
  for(i in 1:ncol(Tabla)){
    Tabla[9,i]=bond.yield(as.Date(fv,format="%d/%m/%Y"),as.Date(Tabla[4,i],"%d/%m/%Y"),as.numeric(gsub("[,]",".",Tabla[5,i])), 4,as.numeric(gsub("[,]",".",Tabla[6,i])),convention = c("ACT/360"),4)
  }
  
#verifico si rendimiento es negativo
length(which(Tabla[9,]<0))!=0

#arreglo rend negativo
while(length(which(Tabla[9,]<0))!=0){
  print("Existe rendimiento negativo")
  print("En las posiciones")
  print(which(Tabla[9,]<0))
  
  #pido ingresar nuevos valores para las posiciones indicadas
  #print("Favor Ingresar los")
  #print(length(which(Tabla[9,]<0)))
  #print("precios promedio nuevos")
  
  
  vt <- c()
  for(i in 1:length(which(Tabla[9,]<0))){
    #vt[i] <- as.numeric(readline(prompt="Ej: 101.05,  "))
    vt[i] <- find_pre(Tabla[6,i],Tabla)
  }
  
  #sustituyo precios promedio
  Tabla[6,which(Tabla[9,]<0)]=vt
  
  #calculo nuevos rendimientos
  #rendimiento
  for(i in 1:ncol(Tabla)){
    Tabla[9,i]=bond.yield(as.Date(fv,format="%d/%m/%Y"),as.Date(Tabla[4,i],"%d/%m/%Y"),as.numeric(gsub("[,]",".",Tabla[5,i])), 4,as.numeric(gsub("[,]",".",Tabla[6,i])),convention = c("ACT/360"),4)
  }
  #muestro tabla
  # View(Tabla)
  
}#final if rend negativo


  

#la funcion donde existe el problema es bond.yield


p <- Tabla.ns(fv="08/03/2018",tit[c(1,13,14,25)],pr=as.numeric(pos1(tit[c(1,13,14,25)],0,Precio_prom_tif)),pa=pa_ns,ind=0,C=ca1,fe2=0,fe3=0)



