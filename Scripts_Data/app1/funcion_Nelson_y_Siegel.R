#funcion Nelson y Siegel DEFINITIVA
#ARGUMENTOS
#fv: fecha de valoración ej: "11/08/2017"
#tit: titulos a considerar (por ahora solo TIF)
#pr: precios promedio de los titulos 
#pa: parametros iniciales (poseen ciertas restricciones)
#ind: 0 = Tif o 1 = veb
#cargo scripts con funciones que utiliza la funcion Tabla.sven
source('C:/Users/Freddy Tapia/Desktop/Svensson/orden data frame.R')
source('C:/Users/Freddy Tapia/Desktop/Svensson/fprecios022T.R')

#cargo librerias a usar
library(jrvFinance)
library(xlsx)
library(nloptr)
library(alabama)
options(OutDec = ",")

#DEFINO OTRAS FUNCIONES
#funcion rend cero cupon
#funcion que calcula el rendimiento cero cupon 
#mediante la met Nelson y Siegel, para unos parametros dados
#y un tiempo t especifico que se deriva de la fecha de pago de cupon
#ARGUMENTOS
#pa: vector de 4 parametros B0, B1, B2, y T1
#los cuales estan sujetos a ciertas restricciones
#B0 > 0
#B0 + B1 > 0
#T1 > 0
#t: tiempo 
nelson_siegel <- function(pa,t){
  r=pa[1]+((pa[2]+pa[3])*(1-exp((-t)/(pa[4])))/(t/pa[4]))-
    pa[3]*exp(-t/pa[4])
  return(r)
}

#funcion precios estimados
#Calcula el precio de los titulos considerados mediante 
#la metodologia de Nelson y siegel
#ARGUMENTOS
#tit: titulo o titulos a considerar, debe ser el nombre corto
#ej: TIF082018 (OJO, mejor considerar el ISIN)
#fv: fecha de valoraci?n, ej: "11/08/2017"
#C: documento de las caracteristicas, que previamente ya ha sido
#leido mediante la funcion "Carac"
#pa: parametros Nelson y siegel
precio.ns=function(tit,fv,C,pa){
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
      
      #valor t años
      #creo vector de tiempos
      (f1=c(0.000000001,(as.numeric(n6[2]-n6[1])/360)+0.000000001))
      
      #rendimiento cero cupon
      #calculo rendimientos cero cupon, para cada tiempo del vector creado
      #anteriormente
      (pa1=c(nelson_siegel(pa,f1[1]),nelson_siegel(pa,f1[2])))
      
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
      
      #valor t años
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
        pa1[i]=nelson_siegel(pa,f1[i])
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
Tabla.ns=function(fv,tit,pr,pa,ind,C){
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
  
  View(Tabla)
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
  
  fe2 <- readline(prompt="Desea optimizar?   (1) Si, (0) No    ")
  if(fe2==1){
    print("Por favor, seleccionar el paquete a usar: ")
    fe3 <- readline(prompt="Seleccionar (1) para Nloptr, (0) para Alabama   ")
    
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
      ala1=alabama::auglag(pa, fn=mifuncion, hin=res) #mejor igual al solver
      
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
  

  
  return(Tabla)
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
      View(Tabla)
      
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
    
    View(Tabla)
    
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
    
    fe2 <- readline(prompt="Desea optimizar?   (1) Si, (0) No    ")
    if(fe2==1){
    
      print("Por favor seleccionar un paquete para optimizar")
      fe3 <- readline(prompt="Seleccionar (1) para el paquete Nloptr, (0) para alabama    ")
      
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
        ala1=alabama::auglag(pa, fn=mifuncion, hin=res) #mejor igual al solver
        
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
    
    return(Tabla)
    
  }#final if ind vebono
  
  
  
}#Final funcion 

##################
#Prueba Julio 2018
#ejemplo
#tif
tit=c("TIF082018","TIF042019","TIF082019",
      "TIF112019","TIF102020","TIF112020","TIF022021","TIF032022","TIF042023",
      "TIF012024","TIF062025","TIF012026","TIF112027","TIF032028","TIF052028",
      "TIF022029","TIF032029","TIF022030","TIF102030","TIF022031","TIF032031",
      "TIF022032","TIF032032","TIF032033","TIF052034")

#veb
tit1=c("VEBONO072018","VEBONO022019","VEBONO032019","VEBONO042019","VEBONO102019","VEBONO012020",
       "VEBONO062020","VEBONO092020","VEBONO112020","VEBONO012021","VEBONO052021",
       "VEBONO122021","VEBONO022022","VEBONO012023","VEBONO022024","VEBONO042024",
       "VEBONO012025","VEBONO022025","VEBONO062026","VEBONO032027","VEBONO042028",
       "VEBONO102028","VEBONO052029","VEBONO102029","VEBONO072030","VEBONO032031",
       "VEBONO062032","VEBONO072033","VEBONO022034")


#####

#precios 11-08 - tif
pr=c(101,112,110,121.0234,116.5251,130.0234,
     129.0156,125.0626,128.1000,120,124,122,126.5234,128.5235,128.1913,
     129,132.0391,128.5235,129.8875,130.1,128.5313,127,128.5235,127.0156,
     127.0156)


#precios 11-08 - veb
pr1=c(100.4,106,110,111,118,121,
      127.8376,102.2,117,130.3269,127,129.45,129,129.9627,128,129,129.1875,
      128.5,102,129.9469,129.6807,130.0156,125.0313,125.75,130.5,129.5235,
      128.5313,130,128.0235)



#parametros 11-08 - tif
#pa=c(0.133799434790145,-0.0379321105061609,-0.307885339616438,0.350692201663154)

pa=c(0.133799434790145,-0.01,-0.307885339616438,0.545398124008073)


#parametros 11-08 - veb
#existe un error al momento de hacer la curva en excel 
#la manera de calcular el rend no es la misma de la que existe en los flujos
#y genera rend neg, al cambiar el 2do parametro esto se solventa
#pa1=c(0.135872169451391,-0.329312552285111,-0.503768911829894,-0.288755056029301,
#    0.11951691203874,0.501729233062216)

pa1=c(0.135872169451391,0.1,-0.503768911829894,0.11951691203874)


#####

#tif
C <- Carac("C:/Users/Freddy Tapia/Desktop/29-06-18.xls")
Tabla11t=Tabla.ns(fv = "06/07/2018",tit = tit,pr = pr,pa = pa,ind = 0,C = C) 

#vebono
Tabla11v=Tabla.ns(fv = "06/07/2018",tit = tit1,pr = pr1,pa = pa1,ind = 1,C = C) 

#elaboro curva
(x=seq(1,16,0.1))

(y_pa=nelson_siegel(pa ,x))

(y_pam <- nelson_siegel(pam ,x))

(y_ala <- nelson_siegel(ala$par ,x))

par(mfrow=c(3,1))
plot(x,y_pa,type = "l")
plot(x,y_pam,type = "l")
plot(x,y_ala,type = "l")

