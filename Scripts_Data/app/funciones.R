#DEFINO OTRAS FUNCIONES
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
#fv: fecha de valoración, ej: "11/08/2017"
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
      
      #valor t años
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
#que se asemejen los más posible a los precios promedio ingresados
#ARGUMENTOS
#fv: fecha de valoración, ej: "11/08/2017"
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
  #variable que me permite señalar error
  cond <- 0
  #Creo data frame donde guardare calculo
  Tabla=as.data.frame(matrix(0,14,length(tit)))
  colnames(Tabla)=tit
  rownames(Tabla)=c("ISIN","Fecha de Liquidación",
                    "Fecha de emisión","Fecha de Vencimiento","Tasa de Cupón",
                    "Precio Prom","Fecha último Pago","Fecha próximo pago",
                    "RDTO al VMTO","Duración","Inverso de la duración",
                    "Ponderación","Precio Modelo Svensson Ajustado",
                    "Residuos al cuadrado")
  
  #relleno ISIN
  for(i in 1:ncol(Tabla)){
    Tabla[1,i]=as.character(C$Sicet[which(names(Tabla)[i]==C$Nombre)])
  }
  
  #relleno fecha Liquidación
  for(i in 1:ncol(Tabla)){
    Tabla[2,i]=paste(substr(fv,9,10),substr(fv,6,7),substr(fv,1,4),sep = "/")
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
      
      rendneg <- which(Tabla[9,]<0)
      
      vt <- c()
      for(i in 1:length(which(Tabla[9,]<0))){
        #vt[i] <- as.numeric(readline(prompt="Ej: 101.05,  "))
        vt[i] <- find_pre(as.numeric(gsub("[,]",".",Tabla[6,rendneg[i]])),Tabla,fv,rendneg[i])
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
      h[5] <- check_rn(sven(pa = c(x[1],x[2],x[3],x[4],x[5],x[6]),t = seq(0.1,20,0.1) ))
      #h[5] <- x[2]+0.01
      h
    }
    
    #modifico esta linea de tal manera q le pase un valor
    #fe2 <- readline(prompt="Desea optimizar?   (1) Si, (0) No    ")
    if(fe2==1){
      #print("Por favor, seleccionar el paquete a usar: ")
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
        ala1=try(alabama::auglag(pa, fn=mifuncion, hin=res)) #mejor igual al solver
        
        if(class(ala1)[1]=="try-error"){
          #print("Existe un problema al optimizar")
          ff <- print("Existe un problema al optimizar")
          #Tabla[13,]=precio.ns(tit,fv,C,pa)
          
          #return(as.data.frame(ff))
          #break
          ala1 <- c()
          ala1$par <- pa
          cond <- 1
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
      ala1 <- c()
      ala1$par <- pa
      Tabla[13,]=precio.sven(tit,fv,C,ala1$par)
    }
    
    #relleno residuos al cuadrado
    
    for(i in 1:ncol(Tabla)){
      Tabla[14,i]=(((as.numeric(gsub("[,]",".",Tabla[13,i])))-(as.numeric(gsub("[,]",".",Tabla[6,i]))))*(as.numeric(gsub("[,]",".",Tabla[12,i]))))^2
    }
    
    #creo vector de precios para exportar
    precios <- cbind.data.frame(c(tit,"SRC"),c(precio.sven(tit,fv,C,ala1$par),sum(as.numeric(gsub("[,]",".",Tabla[14,])))))
    names(precios) <- c("Títulos","Precios")
    
    #if para exportar resultados
    if(fe2==1){
      if(cond==1){
        Tabla <- as.data.frame(print("Problemas al optimizar"))
        names(Tabla) <- "Aviso"
        Tabla1 <- list(Tabla,NULL,NULL)
      }else{
        Tabla1 <- list(Tabla,ala$par,precios)
      }
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
      
      rendneg <- which(Tabla[9,]<0)
      
      vt <- c()
      for(i in 1:length(which(Tabla[9,]<0))){
        #vt[i] <- as.numeric(readline(prompt="Ej: 101.05,  "))
        vt[i] <- find_pre(as.numeric(gsub("[,]",".",Tabla[6,rendneg[i]])),Tabla,fv,rendneg[i])
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
      h[5] <- check_rn(sven(pa = c(x[1],x[2],x[3],x[4],x[5],x[6]),t = seq(0.1,20,0.1) ))
      h
    }
    
    #fe2 <- readline(prompt="Desea optimizar?   (1) Si, (0) No    ")
    if(fe2==1){
      
      #print("Por favor seleccionar un paquete para optimizar")
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
        ala1=try(alabama::auglag(pa, fn=mifuncion, hin=res)) #mejor igual al solver
        
        if(class(ala1)[1]=="try-error"){
          #print("Existe un problema al optimizar")
          ff <- print("Existe un problema al optimizar")
          #Tabla[13,]=precio.ns(tit,fv,C,pa)
          
          #return(as.data.frame(ff))
          #break
          ala1 <- c()
          ala1$par <- pa
          cond <- 1
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
      ala1 <- c()
      ala1$par <- pa
      Tabla[13,]=precio.sven(tit,fv,C,ala1$par)
    }
    
    #relleno residuos al cuadrado
    
    for(i in 1:ncol(Tabla)){
      Tabla[14,i]=(((as.numeric(gsub("[,]",".",Tabla[13,i])))-(as.numeric(gsub("[,]",".",Tabla[6,i]))))*(as.numeric(gsub("[,]",".",Tabla[12,i]))))^2
    }
    
    #creo vector de precios para exportar
    precios <- cbind.data.frame(c(tit,"SRC"),c(precio.sven(tit,fv,C,ala1$par),sum(as.numeric(gsub("[,]",".",Tabla[14,])))))
    names(precios) <- c("Títulos","Precios")
    
    
    #if para exportar resultados
    if(fe2==1){
      if(cond==1){
        Tabla <- as.data.frame(print("Problemas al optimizar"))
        names(Tabla) <- "Aviso"
        Tabla1 <- list(Tabla,NULL,NULL)
      }else{
        Tabla1 <- list(Tabla,ala$par,precios)
      }
    }else if(fe2==0){
      Tabla1 <- list(Tabla,pa,precios)
    }
    
    return(Tabla1)
    
  }#final if ind vebono
  
  
  
}#Final funcion excel-sven

#Exporta una lista de tres elementos
#1: Tabla de resultados
#2: Parámetros optimizados
#3: precios calculados, la ultima fila me trae el valor del SRC

################################
###### NELSON Y SIEGEL #########
################################
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

#funcion que calcula precio, para no tener un rend negativo
#es una manera de evitar el inconveniente de ingresar el precio promedio 
#de forma manual
#argumentos:
#pre: precio para el cual existe rend negativo
#Tabla: tabla que genera la funcion Tabla.sven
#fv: fecha de valoracion
#i: posiciones donde hay rendimientos negativos
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
  #variable que me permite señalar error
  cond <- 0
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
    Tabla[2,i]=paste(substr(fv,9,10),substr(fv,6,7),substr(fv,1,4),sep = "/")
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
      
      rendneg <- which(Tabla[9,]<0)
      
      vt <- c()
      for(i in 1:length(which(Tabla[9,]<0))){
        #vt[i] <- as.numeric(readline(prompt="Ej: 101.05,  "))
        vt[i] <- find_pre(as.numeric(gsub("[,]",".",Tabla[6,rendneg[i]])),Tabla,fv,rendneg[i])
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
      h[4] <- check_rn(nelson_siegel(pa = c(x[1],x[2],x[3],x[4]),t = seq(0.1,20,0.1) ))
      #h[4] <- x[2]+0.01
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
          print("Optimizaci?n fallida..")
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
          #print("Existe un problema al optimizar")
          ff <- print("Existe un problema al optimizar")
          #Tabla[13,]=precio.ns(tit,fv,C,pa)
          
          #return(as.data.frame(ff))
          #break
          ala1 <- c()
          ala1$par <- pa
          cond <- 1
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
      ala1 <- c()
      ala1$par <- pa
      Tabla[13,]=precio.ns(tit,fv,C,ala1$par)
    }
    
    #relleno residuos al cuadrado
    
    for(i in 1:ncol(Tabla)){
      Tabla[14,i]=(((as.numeric(gsub("[,]",".",Tabla[13,i])))-(as.numeric(gsub("[,]",".",Tabla[6,i]))))*(as.numeric(gsub("[,]",".",Tabla[12,i]))))^2
    }
    
    #creo vector de precios para exportar
    precios <- cbind.data.frame(c(tit,"SRC"),c(precio.ns(tit,fv,C,ala1$par),sum(as.numeric(gsub("[,]",".",Tabla[14,])))))
    names(precios) <- c("Títulos","Precios")
    
    
    #if para exportar resultados
    if(fe2==1){
      if(cond==1){
        Tabla <- as.data.frame(print("Problemas al optimizar"))
        names(Tabla) <- "Aviso"
        Tabla1 <- list(Tabla,NULL,NULL)
      }else{
        Tabla1 <- list(Tabla,ala$par,precios)
      }
      
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
      
      rendneg <- which(Tabla[9,]<0)
      
      vt <- c()
      for(i in 1:length(which(Tabla[9,]<0))){
        #vt[i] <- as.numeric(readline(prompt="Ej: 101.05,  "))
        vt[i] <- find_pre(as.numeric(gsub("[,]",".",Tabla[6,rendneg[i]])),Tabla,fv,rendneg[i])
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
      h[4] <- check_rn(nelson_siegel(pa = c(x[1],x[2],x[3],x[4]),t = seq(0.1,20,0.1) ))
      #h[4] <- x[2]+0.01
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
          print("Optimizaci?n fallida..")
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
          #print("Existe un problema al optimizar")
          ff <- print("Existe un problema al optimizar")
          #Tabla[13,]=precio.ns(tit,fv,C,pa)
          
          #return(as.data.frame(ff))
          #break
          ala1 <- c()
          ala1$par <- pa
          
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
      ala1 <- c()
      ala1$par <- pa
      Tabla[13,]=precio.ns(tit,fv,C,ala1$par)
    }
    
    #relleno residuos al cuadrado
    
    for(i in 1:ncol(Tabla)){
      Tabla[14,i]=(((as.numeric(gsub("[,]",".",Tabla[13,i])))-(as.numeric(gsub("[,]",".",Tabla[6,i]))))*(as.numeric(gsub("[,]",".",Tabla[12,i]))))^2
    }
    
    #creo vector de precios para exportar
    precios <- cbind.data.frame(c(tit,"SRC"),c(precio.ns(tit,fv,C,ala1$par),sum(as.numeric(gsub("[,]",".",Tabla[14,])))))
    names(precios) <- c("Títulos","Precios")
    
    #if para exportar resultados
    if(fe2==1){
      if(cond==1){
        Tabla <- as.data.frame(print("Problemas al optimizar"))
        names(Tabla) <- "Aviso"
        Tabla1 <- list(Tabla,Tabla,Tabla)
      }else{
        Tabla1 <- list(Tabla,ala$par,precios)
      }
    }else if(fe2==0){
      Tabla1 <- list(Tabla,pa,precios)
    }
    
    return(Tabla1)
    
  }#final if ind vebono
  
  
  
}#Final funcion 

#creo funcion auxiliar para garantizar que los rendimientos
#en la curva sean positivos
check_rn <- function(fre){
  if (length(which(fre<0))==0){
    return(1)
  }else{
    return(-1)
  }
}

#funcion que me retorna una lista de 3 elementos
#1: tabla de resultados
#2: parametros
#3: precios calculados, la ultima fila me trae el valor del SRC


################################
####### DIEBOLD-LI #############
################################
#Defino funcion que calcula el rendimiento mediante la metodologia de Diebold-Li
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
diebold_li <- function(pa,t){
  r=pa[1]+((pa[2]+pa[3])*(1-exp((-t)*(pa[4])))/(t*pa[4]))-
    pa[3]*exp(-t*pa[4])
  return(r)
}


#creo funcion que calcula parametros de Diebold-Li por tiempo
#Caso 1: donde vario parametro por cada tiempo, es lo que deberia ser
#aunque lo q veo es que estos valores van a ser muy similares a los
#obtenidos por la curva de rendimiento de los splines
#caso 2: vario parametros por conjunto de t's los cuales se obtiene 
#para cada titulo
#para acceder a cada caso se debe pasar un unico valor (caso 1)
#o pasar un vector (caso 2) (OJO)

par_dl <- function(t,spline1,pa){
  #defino rendimientos observados, con los cuales se hará la comparacion
  pp <- predict(spline1,t*365)$y/100
  
  #ojo-prueba
  pa <- as.numeric(pa)
  
  #defino funcion a minimizar
  mifuncion<- function(x){
    pa=c(x[1],x[2],x[3],x[4])
    #p=precio.ns(tit,fv,C,pa) 
    p <- diebold_li(pa,t)
    #x=sum(((p-pp)*w)^2)
    x <- sum(((p-pp))^2)
    return(x)
  }
  
  #restricciones
  res <- function(x) {
    h <- rep(NA, 1)
    h[1] <- x[1] # B0 > 0
    h[2] <- x[1]+x[2] # B0 + B1 > 0
    h[3] <- x[4] # lambda1 > 0
    #h[4] <- x[2]+0.01
    #h[4] <- x[2] #ojo
    h
  }
  
  #
  ala1=alabama::auglag(pa, fn=mifuncion, hin=res,control.outer=list(trace=FALSE)) #mejor igual al solver
  
  return(ala1$par)
  
}#final funcion par_dl

#funcion precios estimados
#Calcula el precio de los titulos considerados mediante 
#la metodologia de Diebold Li
#ARGUMENTOS
#tit: titulo o titulos a considerar, debe ser el nombre corto
#ej: TIF082018 (OJO, mejor considerar el ISIN)
#fv: fecha de valoraci?n, ej: "11/08/2017"
#C: documento de las caracteristicas, que previamente ya ha sido
#leido mediante la funcion "Carac"
#pa: parametros Diebold Li, ojo se puede empezar con cualquier parametros
#la funcion internamente calcula diferentes parametros por cada tiempo
#con as.numeric
precio.dl=function(tit,fv,C,pa,spline1,pr){
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
      var_par <- as.data.frame(matrix(0,length(f1),4))
      
      #guardo parametros segun cada tiempo
      for(i in 1:length(f1)){
        var_par[i,] <- par_dl(f1[i],spline1,pa)
      }
      
      #calculo nuevos rendimientos a partir de los nuevos parametros
      pa1 <- as.data.frame(matrix(0,length(f1),1))
      
      for(i in 1:length(f1)){
        #pa1[i,1] <- diebold_li(var_par[i,],f1[i])
        pa1[i,1] <- diebold_li(as.numeric(var_par[i,]),f1[i])
      }
      
      #(pa1=c(nelson_siegel(pa,f1[1]),nelson_siegel(pa,f1[2])))
      
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
      # (pa1=rep(0,length(n5)))
      # 
      # for(i in 1:length(n5)){
      #   pa1[i]=nelson_siegel(pa,f1[i])
      # }
      
      #rendimiento cero cupon
      #calculo rendimientos cero cupon, para cada tiempo del vector creado
      #anteriormente
      var_par <- as.data.frame(matrix(0,length(f1),4))
      
      #guardo parametros segun cada tiempo
      for(i in 1:length(f1)){
        var_par[i,] <- par_dl(f1[i],spline1,pa)
      }
      
      #calculo nuevos rendimientos a partir de los nuevos parametros
      pa1 <- as.data.frame(matrix(0,length(f1),1))
      
      for(i in 1:length(f1)){
        #pa1[i,1] <- diebold_li(var_par[i,],f1[i])
        pa1[i,1] <- diebold_li(as.numeric(var_par[i,]),f1[i])
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
  
  #Creo tabla de Resultados
  Tabla=as.data.frame(matrix(0,14,length(tit)))
  colnames(Tabla)=tit
  rownames(Tabla)=c("ISIN","Fecha de Liquidación",
                    "Fecha de emisión","Fecha de Vencimiento","Tasa de Cupón",
                    "Precio Prom","Fecha último Pago","Fecha próximo pago",
                    "RDTO al VMTO","Duración","Inverso de la duración",
                    "Ponderación","Precio Modelo Diebold Li",
                    "Residuos al cuadrado")
  
  #relleno ISIN
  for(i in 1:ncol(Tabla)){
    Tabla[1,i]=as.character(C$Sicet[which(names(Tabla)[i]==C$Nombre)])
  }
  
  #relleno fecha Liquidación
  for(i in 1:ncol(Tabla)){
    Tabla[2,i]=paste(substr(fv,9,10),substr(fv,6,7),substr(fv,1,4),sep = "/")
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
  while(length(which(Tabla[9,]<0))!=0){
    #print("Existe rendimiento negativo")
    #print("En las posiciones")
    #print(which(Tabla[9,]<0))
    
    #pido ingresar nuevos valores para las posiciones indicadas
    #print("Favor Ingresar los")
    #print(length(which(Tabla[9,]<0)))
    #print("precios promedio nuevos")
    
    rendneg <- which(Tabla[9,]<0)
    
    vt <- c()
    for(i in 1:length(which(Tabla[9,]<0))){
      #vt[i] <- as.numeric(readline(prompt="Ej: 101.05,  "))
      vt[i] <- find_pre(as.numeric(gsub("[,]",".",Tabla[6,rendneg[i]])),Tabla,fv,rendneg[i])
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
  
  #relleno precios
  Tabla[13,]=Pr
  
  #relleno residuos al cuadrado
  
  for(i in 1:ncol(Tabla)){
    Tabla[14,i]=(((as.numeric(gsub("[,]",".",Tabla[13,i])))-(as.numeric(gsub("[,]",".",Tabla[6,i]))))*(as.numeric(gsub("[,]",".",Tabla[12,i]))))^2
  }
  
  #SRC
  print("EL SRC es")
  print(sum(as.numeric(gsub("[,]",".",Tabla[14,]))))
  
  
  #retorno precios
  Pre <- cbind.data.frame("Titulos"=c(tit,"SRC"),"Precio"=c(Pr,sum(as.numeric(gsub("[,]",".",Tabla[14,])))))
  
  #creo lista
  Pre1 <- list(Tabla,Pre)
  
  return(Pre1)
  
}#final funcion precios estimados

#esta funcion me retorna una lista de dos elementos
#1: Tabla de resultados
#2: precios calculados, la ultima fila trae el valor del SRC

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

#
#Funcion que calcula el precio de un titiulo a partir de la curva de rend
#generado por el spline
#tit = nombre corto de títulos a calcular su precio
#spline1 = objeto smooth spline que se obtine del ajuste de los datos
#fv = fecha de valoración
#C = documento de las características a la fecha
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
    
    #creo vector del tamaÃ±o de 1 + division de n3/91
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
#Función que calcula precio diario mediante met splines
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
  names(Pr) <- c("Títulos","Precios")
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
#num = numero de días hacia atras
#par = parametro de suavizamiento spar
#tit = nombre corto de los títulos
#C = documento de las características al día

Tabla.splines <- function(data,tipo,fe,num,par,tit,C,pr){
  if(tipo=="TIF"){
    #extraigo solo TIF
    #data[,3] <- as.Date(as.character(data[,3]))
    
    datatif <- data[which(data$Tipo.Instrumento=="TIF"),]
    datatif <- arrange(datatif,desc(Fecha.op))
    
    #datatif$F.Vencimiento <- as.Date(datatif$F.Vencimiento,format="%d/%m/%Y")
    datatif$F.Vencimiento <- as.Date(datatif$F.Vencimiento)
    
        
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
    rownames(Tabla)=c("ISIN","Fecha de Liquidación",
                      "Fecha de emisión","Fecha de Vencimiento","Tasa de Cupón",
                      "Precio Prom","Fecha último Pago","Fecha próximo pago",
                      "RDTO al VMTO","Duración","Inverso de la duración",
                      "Ponderación","Precio Modelo Spline",
                      "Residuos al cuadrado")
    
    #relleno ISIN
    for(i in 1:ncol(Tabla)){
      Tabla[1,i]=as.character(C$Sicet[which(names(Tabla)[i]==C$Nombre)])
    }
    
    #relleno fecha Liquidación
    for(i in 1:ncol(Tabla)){
      Tabla[2,i]=paste(substr(fe,9,10),substr(fe,6,7),substr(fe,1,4),sep = "/")
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
      Tabla[9,i]=bond.yield(as.Date(fe,format="%d/%m/%Y"),as.Date(Tabla[4,i],"%d/%m/%Y"),as.numeric(gsub("[,]",".",Tabla[5,i])), 4,as.numeric(gsub("[,]",".",Tabla[6,i])),convention = c("ACT/360"),4)
    }
    
    #verifico si rendimiento es negativo
    fv <- fe
    while(length(which(Tabla[9,]<0))!=0){
      #print("Existe rendimiento negativo")
      #print("En las posiciones")
      #print(which(Tabla[9,]<0))
      
      #pido ingresar nuevos valores para las posiciones indicadas
      #print("Favor Ingresar los")
      #print(length(which(Tabla[9,]<0)))
      #print("precios promedio nuevos")
      
      rendneg <- which(Tabla[9,]<0)
      
      vt <- c()
      for(i in 1:length(which(Tabla[9,]<0))){
        #vt[i] <- as.numeric(readline(prompt="Ej: 101.05,  "))
        vt[i] <- find_pre(as.numeric(gsub("[,]",".",Tabla[6,rendneg[i]])),Tabla,fv,rendneg[i])
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
      Tabla[10,i]=bond.duration(as.Date(fe,format="%d/%m/%Y"),as.Date(Tabla[4,i],"%d/%m/%Y"),as.numeric(gsub("[,]",".",Tabla[5,i])), 4,as.numeric(gsub("[,]",".",Tabla[9,i])),convention = c("ACT/360"),4)
    }
    
    #añado inverso duracion
    Tabla[11,]=1/(as.numeric(gsub("[,]",".",Tabla[10,])))
    
    #añado ponderacion
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
    
    #Pr_tit_tif <- rbind.data.frame(Pr_tit_tif,sum(as.numeric(gsub("[,]",".",Tabla[14,]))))
    Pr_tit_tif <- cbind.data.frame("Títulos"=c(Pr_tit_tif[,1],"SRC"),"Precios"=c(Pr_tit_tif[,2],sum(as.numeric(gsub("[,]",".",Tabla[14,])))))
    
    #rownames(Pr_tit_tif)[length(Pr_tit_tif[,1])] <- "SRC"
    
    res_tif <- list(Pr_tit_tif,candidatos[,c(2,3,6,7,12,13,15,17,18)],letra,spline1,Tabla) 
    
    return(res_tif)
    
  }else if(tipo=="VEBONO"){
    #letra<<- data[max(which(data$Tipo.Instrumento=="LETRA")),]
    
    dataveb <- data[which(data$Tipo.Instrumento=="VEBONO"),]
    dataveb <- arrange(dataveb,desc(Fecha.op))
    
    #dataveb$F.Vencimiento <- as.Date(dataveb$F.Vencimiento,format="%d/%m/%Y")
    dataveb$F.Vencimiento <- as.Date(dataveb$F.Vencimiento)
    
    
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
    rownames(Tabla)=c("ISIN","Fecha de Liquidación",
                      "Fecha de emisión","Fecha de Vencimiento","Tasa de Cupón",
                      "Precio Prom","Fecha último Pago","Fecha próximo pago",
                      "RDTO al VMTO","Duración","Inverso de la duración",
                      "Ponderación","Precio Modelo Spline",
                      "Residuos al cuadrado")
    
    #relleno ISIN
    for(i in 1:ncol(Tabla)){
      Tabla[1,i]=as.character(C$Sicet[which(names(Tabla)[i]==C$Nombre)])
    }
    
    #relleno fecha Liquidación
    for(i in 1:ncol(Tabla)){
      Tabla[2,i]=paste(substr(fe,9,10),substr(fe,6,7),substr(fe,1,4),sep = "/")
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
      Tabla[9,i]=bond.yield(as.Date(fe,format="%d/%m/%Y"),as.Date(Tabla[4,i],"%d/%m/%Y"),as.numeric(gsub("[,]",".",Tabla[5,i])), 4,as.numeric(gsub("[,]",".",Tabla[6,i])),convention = c("ACT/360"),4)
    }
    
    #verifico si rendimiento es negativo
    fv <- fe
    while(length(which(Tabla[9,]<0))!=0){
      #print("Existe rendimiento negativo")
      #print("En las posiciones")
      #print(which(Tabla[9,]<0))
      
      #pido ingresar nuevos valores para las posiciones indicadas
      #print("Favor Ingresar los")
      #print(length(which(Tabla[9,]<0)))
      #print("precios promedio nuevos")
      
      rendneg <- which(Tabla[9,]<0)
      
      vt <- c()
      for(i in 1:length(which(Tabla[9,]<0))){
        #vt[i] <- as.numeric(readline(prompt="Ej: 101.05,  "))
        vt[i] <- find_pre(as.numeric(gsub("[,]",".",Tabla[6,rendneg[i]])),Tabla,fv,rendneg[i])
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
      Tabla[10,i]=bond.duration(as.Date(fe,format="%d/%m/%Y"),as.Date(Tabla[4,i],"%d/%m/%Y"),as.numeric(gsub("[,]",".",Tabla[5,i])), 4,as.numeric(gsub("[,]",".",Tabla[9,i])),convention = c("ACT/360"),4)
    }
    
    #añado inverso duracion
    Tabla[11,]=1/(as.numeric(gsub("[,]",".",Tabla[10,])))
    
    #añado ponderacion
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
    
    #Pr_tit_veb <- rbind.data.frame(Pr_tit_veb,sum(as.numeric(gsub("[,]",".",Tabla[14,]))))
    Pr_tit_veb <- cbind.data.frame("Títulos"=c(Pr_tit_veb[,1],"SRC"),"Precios"=c(Pr_tit_veb[,2],sum(as.numeric(gsub("[,]",".",Tabla[14,])))))
    
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

##################

#funcion Carateristicas
# Carac=function(ruta){
#   #c=read.xlsx("C:/Users/ftapia/Documents/DESCARGAS R/Caracteristicas.xls", sheetName = "DPN",startRow = 7,colIndex = 1:15,header = TRUE)
#   #c=read.xlsx("C:/Users/Nancy/Desktop/Modelos Aleatorios/prueba 190717/Caracteristicas.xls", sheetName = "DPN",startRow = 7,colIndex = 1:15,header = TRUE)
#   c=read.xlsx(ruta, sheetName = "DPN",startRow = 7,colIndex = 1:15,header = TRUE)
#   c1=c[,c(3,5,6,8,10,11,13,15)]
#   c2=c1[-which(is.na(c1[,8])),]
#   c3=cbind(as.character(rep("TIF",1,length(c2[,8]))),c2)
#   c3[,1]=as.character(c3[,1])
#   c3[,1][which(c3[,5]!="Tasa Fija")]="VEBONO"
#   names(c3)=c("Tipo Instrumento","Sicet","F.Emision","F.Vencimiento","Tipo tasa","Inicio","Pago cupon 1","Pago cupon 2","Cupon")
#   
#   #tranformo formato fecha
#   c3$F.Emision=format(c3$F.Emision,"%d/%m/%Y")
#   c3$F.Vencimiento=format(c3$F.Vencimiento,"%d/%m/%Y")
#   c3$`Pago cupon 1`=format(c3$`Pago cupon 1`,"%d/%m/%Y")
#   c3$`Pago cupon 2`=format(c3$`Pago cupon 2`,"%d/%m/%Y")
#   
#   #agrego columna Nombre TIf o Vebono + fecha venc
#   c4=c3$`Tipo Instrumento`
#   
#   Nombre=c()
#   for(i in 1:length(c4)){
#     Nombre[i]=paste(c4[i],paste(substr(c3$F.Vencimiento[i],4,5),substr(c3$F.Vencimiento[i],7,10),sep = ""),sep = "")
#   }
#   
#   c3=cbind(c3$`Tipo Instrumento`,Nombre,c3[,2:ncol(c3)])
#   names(c3)[1]="Tipo Instrumento"
#   
#   #pestaña DPN-TICC
#   #d=read.xlsx("C:/Users/Nancy/Desktop/Modelos Aleatorios/prueba 190717/Caracteristicas.xls", sheetName = "DPN - TICC",startRow = 7,colIndex = 1:14,header = TRUE)
#   d=read.xlsx(ruta, sheetName = "DPN - TICC",startRow = 7,colIndex = 1:14,header = TRUE)
#   
#   #d=read.xlsx("C:/Users/ftapia/Documents/DESCARGAS R/Caracteristicas.xls", sheetName = "DPN - TICC",startRow = 7,colIndex = 1:14,header = TRUE)
#   
#   d=d[-which(is.na(d[,2])),c(3,4,5,7,9,10,12,14)]
#   #cambio formato fechas
#   d[,2]=format(d[,2],"%d/%m/%Y")
#   d[,3]=format(d[,3],"%d/%m/%Y")
#   d[,6]=format(d[,6],"%d/%m/%Y")
#   d[,7]=format(d[,7],"%d/%m/%Y")
#   
#   d1=rep("TICC",1,nrow(d))
#   
#   d2=c()
#   for(i in 1:length(d1)){
#     d2[i]=paste(d1[i],paste(substr(d[i,3],4,5),substr(d[i,3],7,10),sep = ""),sep = "")
#   }
#   
#   d3=cbind(d1,d2,d)
#   names(d3)=names(c3) 
#   
#   #Añado nuevo titulo VF
#   #v=read.xlsx("C:/Users/ftapia/Documents/DESCARGAS R/Caracteristicas.xls", sheetName = "VF",startRow = 7,colIndex = 1:14,header = TRUE)
#   v=read.xlsx(ruta, sheetName = "VF",startRow = 7,colIndex = 1:14,header = TRUE)
#   v=v[-which(is.na(v[,2])),c(3,4,5,7,9,10,12,14)]
#   #cambio formato fechas
#   v[,2]=format(v[,2],"%d/%m/%Y")
#   v[,3]=format(v[,3],"%d/%m/%Y")
#   v[,6]=format(v[,6],"%d/%m/%Y")
#   v[,7]=format(v[,7],"%d/%m/%Y")
#   
#   v1=rep("Valores Fin.",1,nrow(v))
#   
#   v2=c()
#   for(i in 1:length(v1)){
#     v2[i]=paste(v1[i],paste(substr(v[i,3],4,5),substr(v[i,3],7,10),sep = ""),sep = "")
#   }
#   
#   v3=cbind(v1,v2,v)
#   names(v3)=names(c3) 
#   
#   
#   
#   #CARACTERISTICAS DEFINITIVA
#   C3=rbind(c3,d3,v3) 
#   
#   #En caso que no lea bien un numero
#   C3$Cupon=as.numeric(sub(",",".",C3$Cupon))
#   
#   return(C3)
# } #final funcion Caracteristicas


##################################

#ordenar data frame
sort.data.frame <- function(form,dat){ 
  # Author: Kevin Wright 
  # Some ideas from Andy Liaw 
  # http://tolstoy.newcastle.edu.au/R/help/04/07/1076.html 
  # Use + for ascending, - for decending. 
  # Sorting is left to right in the formula 
  
  
  # Useage is either of the following: 
  # sort.data.frame(~Block-Variety,Oats) 
  # sort.data.frame(Oats,~-Variety+Block) 
  
  
  # If dat is the formula, then switch form and dat 
  if(inherits(dat,"formula")){ 
    f=dat 
    dat=form 
    form=f 
  } 
  if(form[[1]] != "~") 
    stop("Formula must be one-sided.")
  
  # Make the formula into character and remove spaces 
  formc <- as.character(form[2]) 
  formc <- gsub(" ","",formc) 
  # If the first character is not + or -, add + 
  if(!is.element(substring(formc,1,1),c("+","-")))     formc <- paste("+",formc,sep="") 
  # Extract the variables from the formula 
  vars <- unlist(strsplit(formc, "[\\+\\-]"))
  vars <- vars[vars!=""] # Remove spurious "" terms
  
  # Build a list of arguments to pass to "order" function 
  calllist <- list() 
  pos=1 # Position of + or - 
  for(i in 1:length(vars)){ 
    varsign <- substring(formc,pos,pos) 
    pos <- pos+1+nchar(vars[i]) 
    if(is.factor(dat[,vars[i]])){
      
      if(varsign=="-")
        calllist[[i]] <- -rank(dat[,vars[i]])
      else
        calllist[[i]] <- rank(dat[,vars[i]])
    } 
    else {
      if(varsign=="-")
        calllist[[i]] <- -dat[,vars[i]]
      else
        calllist[[i]] <- dat[,vars[i]]
      
    } 
  } 
  dat[do.call("order",calllist),]
}

#d = data.frame(b=factor(c("Hi","Med","Hi","Low"),levels=c("Low","Med","Hi"),ordered=TRUE),x=c("A","D","A","C"),y=c(8,3,9,9),z=c(1,1,1,2))
#sort.data.frame(~-z-b,d)
#sort.data.frame(~x+y+z,d)
#sort.data.frame(~-x+y+z,d) 
#sort.data.frame(d,~x-y+z)

#funcion ruta bcv
ruta_bcv <- function(file){
  if(file=="0-22"){
    webpage <- read_html("http://www.bcv.org.ve/sistemas-de-pago/sicet/estadisticas")
    # Extract records info
    results <- webpage %>% html_nodes(".file")
    
    # Building the dataset
    records <- vector("list", length = length(results))
    
    for (i in seq_along(results)) {
      url <- results[i] %>% html_nodes("a") %>% html_attr("href")
      records[[i]] <- data_frame(url = url)
    }
    
    df <- bind_rows(records)
    return(as.character(df[1,1]))
  }#final if 0-22
  else if(file=="caracteristicas" | file=="tasas"){
    webpage <- read_html("http://www.bcv.org.ve/estadisticas/tasas-de-interes")
    # Extract records info
    results <- webpage %>% html_nodes(".file")
    
    # Building the dataset
    records <- vector("list", length = length(results))
    
    for (i in seq_along(results)) {
      url <- results[i] %>% html_nodes("a") %>% html_attr("href")
      records[[i]] <- data_frame(url = url)
    }
    
    df <- bind_rows(records)
    if(file=="tasas"){
      return(as.character(df[33,1]))
    }
    if(file=="caracteristicas"){
      return(as.character(df[37,1]))
    }
    
    
  }#final caracteristicas y tasas
  else if(file=="abs"){
    webpage <- read_html("http://www.bcv.org.ve/politica-monetaria/operaciones-absorcion-diaria")
    # Extract records info
    results <- webpage %>% html_nodes(".file")
    
    # Building the dataset
    records <- vector("list", length = length(results))
    
    for (i in seq_along(results)) {
      url <- results[i] %>% html_nodes("a") %>% html_attr("href")
      records[[i]] <- data_frame(url = url)
    }
    
    df <- bind_rows(records)
    return(as.character(df[1,1]))
  }#final abs
  else if(file=="iny"){
    webpage <- read_html("http://www.bcv.org.ve/politica-monetaria/operaciones-inyeccion-diaria")
    # Extract records info
    results <- webpage %>% html_nodes(".file")
    
    # Building the dataset
    records <- vector("list", length = length(results))
    
    for (i in seq_along(results)) {
      url <- results[i] %>% html_nodes("a") %>% html_attr("href")
      records[[i]] <- data_frame(url = url)
    }
    
    df <- bind_rows(records)
    return(as.character(df[1,1]))
  }#final iny
  else if(file=="letras"){
    webpage <- read_html("http://www.bcv.org.ve/politica-monetaria/letras-del-tesoro")
    # Extract records info
    results <- webpage %>% html_nodes(".file")
    
    # Building the dataset
    records <- vector("list", length = length(results))
    
    for (i in seq_along(results)) {
      url <- results[i] %>% html_nodes("a") %>% html_attr("href")
      records[[i]] <- data_frame(url = url)
    }
    
    df <- bind_rows(records)
    return(as.character(df[1,1]))
  }#final letras
  else if(file=="tif" | file=="veb" ){
    webpage <- read_html("http://www.bcv.org.ve/politica-monetaria/bonos-dpn")
    # Extract records info
    results <- webpage %>% html_nodes(".file")
    
    # Building the dataset
    records <- vector("list", length = length(results))
    
    for (i in seq_along(results)) {
      url <- results[i] %>% html_nodes("a") %>% html_attr("href")
      records[[i]] <- data_frame(url = url)
    }
    
    df <- bind_rows(records)
    if(file=="tif"){return(as.character(df[10,1]))}
    if(file=="veb"){return(as.character(df[6,1]))}
  }#final tif o veb
  else{
    print("Nombre inválido, revise eintente nuevamente")
  }
  
}#final funcion ruta_bcv


#############
#dh:d?a habil o vector de dias habiles  
#ar:nombre documento 0-22, ej: "0-22.xls"  
#ruta: direccion del documento 0-22  

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

#funcion Carateristicas
#funcion que extrae informacion del documento de las
#caracteristicas y lee todas sus pesta?as
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
    d3=c()
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
    w3=c()
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
    v3=c()
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
    x3=c()
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
    y3=c()
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
    #h1=which(ve==1)
    
    if(ve[2]==1){
      print("Falta pesta?a DPN-TICC")
      C3=rbind(c3,v3,w3,x3,y3)
      return(C3)
    }
    if(ve[3]==1){
      print("Falta pesta?a VB")
      C3=rbind(c3,d3,w3,x3,y3)
      return(C3)
    }
    if(ve[4]==1){
      print("Falta pesta?a VF")
      C3=rbind(c3,d3,v3,x3,y3)
      return(C3)
    }
    if(ve[5]==1){
      print("Falta pesta?a CF")
      C3=rbind(c3,d3,v3,w3,y3)
      return(C3)
    }
    if(ve[6]==1){
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


#Funcion Formato
#funcion que toma la el documento de las caracteristicas informacion 
#necesaria para darle formato al data frame b6
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
#########################################


#Calculo moda de un vector
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}