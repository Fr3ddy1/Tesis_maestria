#Test de Kupiec en R, solo considerando excepciones negativas
#data a ingresar K= 92 o 252 obs 6 columnas- 1era: num obs, 2da:Fecha, 3era:VaR
#4ta: VaR-, 5ta: Posicion/precio, 6ta: VaR+
#p= nivel de confianza, 0.05, 0.01
#install.packages('WriteXLS')
#library(WriteXLS)
#install.packages("xlsx")
#library(xlsx)
#setwd("C:/Users/ftapia/Desktop/Pdf backtesting y C. Svenson y otros/data Kupiec")
#setwd("T:/Riesgos/Intercambio/Coordinacion de Riesgo de Mercado, Liquidez y Tasas de Interes/RIESGO DE MERCADO/Valoracion Riesgo/Backtesting/Kupiec")
      
#K=read.delim("ku.txt", header = FALSE)

kup1=function(K,p){
  a=K$V4
  b=K$V5
  c=K$V6
  cp=0
  cn=0
  cr=0
  print("Numero de Observaciones")
  print(length(a))
  f=rep(0,length(a))
  for (i in 1:length(a)){
    if(b[i]<a[i]){
      #excepcion negativa
      cn=cn+1
      f[i]=i
    }
  }
 # for (i in 1:length(a)){
  #  if(c[i]<b[i]){
   #   #excepcion positiva
    #  cp=cp+1
     # f[i]=i
    #}
  #}
  cr=length(a)-(cn)
  #numero de excepciones
  d=c(cn,cr,cn+cr)
  print("Excepxiones Negativas, Rango VaR, Total Excepciones")
  print(d)
  #calculo del estadistico de Kupiec
  estK=(-2)*log((((1-p)^(length(a)-d[1]))*(p^d[1]))/(((1-(d[1]/length(a)))^(length(a)-d[1]))*(d[1]/length(a))^(d[1])))
  #calculo de la eficiencia del modelo
  efi=estK/length(a)*100
  
  #calculo del estadistico de Haas
  #calculo del tiempo entre excepciones
  #f1=rep(0,cn+cp)
  #cont=1
  qq=which(b<a)
  #for (j in 1:length(f)){
   # if (f[j] != 0){
    #  f1[cont]=f[j] 
     # cont=cont+1
    #}
  #}
  f2=rep(0,cn)
  f2[1]=qq[1]
  for(i in 2:length(qq)){
    f2[i]=(qq[i]-qq[i-1])+1
  }
  estH=rep(0,length(f2))
  #para asignar a la primera entrada del vector un 2 en vez de uno
  if(f2[1]==1){
    f2[1]=2 
  }
  for (i in 1:length(f2)){
    if(f2[i]==1){
      estH[i]=0
    }else{
      estH[i]=(-2)*log((((1-p)^(f2[i]-1))*(p))/(((1-(1/f2[i]))^(f2[i]-1))*(1/f2[i])))
    }}
  estH=sum(estH)
  #muestro tiempo entre excepciones
  print("tiempo entre excepciones")
  print(f2)
  #calculo del estadistico del test mixto (Kupiec + Haas)
  est=estK+estH  
  #resultados, estadistico Kupiec, estadistico Haas y estadistico prueba mixta
  e=c(estK, estH,est)
  
  print("Resultados, Estadistico Kupiec, Estadistico Haas, Estadistico prueba mixta")
  print(e)
  
  #return(f2)
  
  #valores criticos para cada test
  #test Kupiec
  n1=qchisq(0.95,1)
  #test Haas
  n2=qchisq(0.95,cn)
  #test Mixto
  n3=qchisq(0.95,(cn)+1)
  
  print("valores criticos, Estadistico Kupiec, Estadistico Haas y Estadistico prueba mixta")
  n=c(n1,n2,n3)
  print(n)
  
  W2=rep(0,3)
  #Conclusiones Tests
  #test de Kupiec
  if(n1>estK){
    print("Segun el Test de Kupiec, se esta en zona de aceptacion")
    W2[1]=c("Segun el Test de Kupiec, se esta en zona de aceptacion")
  } else {
    print("Segun el Test de Kupiec, se esta en zona de rechazo")
    W2[1]=c("Segun el Test de Kupiec, se esta en zona de rechazo")
  }
  
  #test de Haas
  if(n2>estH){
    print("Segun el Test de Haas, las excepciones son independientes")
    W2[2]=c("Segun el Test de Haas, las excepciones son independientes")
  } else {
    print("Segun el Test de Haas, las excepciones no son independientes")
    W2[2]=c("Segun el Test de Haas, las excepciones no son independientes")
  }
  
  #test Mixto
  if(n3>est){
    print("Segun el Test de Mixto de Kupiec, se esta en zona de aceptacion")
    W2[3]=c("Segun el Test de Mixto de Kupiec, se esta en zona de aceptacion")
  } else {
    print("Segun el Test Mixto de Kupiec, se esta en zona de rechazo")
    W2[3]=c("Segun el Test Mixto de Kupiec, se esta en zona de rechazo")
  }
  
  d1=c("  Negativas  ","  Total  ","  Observaciones  ")
  e1=c("  Kupiec  ","  Haas  ","  Mixto  ")
  n1=c("  Valor critico Kupiec  ","  Valor Critico Haas  ","  Valor Critico Mixto  ")
  Excepciones=c(d[-2],length(a))
  Est.=e
  VCritico=n
  
  W=Excepciones
  W=data.frame(t(W))
  names(W)=d1
  
  W1=rbind(Est.,VCritico)
  W1=data.frame(W1)
  names(W1)=e1
  
  #W2=data.frame(W2)
  
  #write.xlsx(W,"Test.xls",row.names=FALSE,sheetName = "Excepciones")
  #options(OutDec = ",")
  #write.xlsx(W1,"Test.xls",sheetName = "Estadisticos", append = TRUE)
  #write.xlsx(W2,"Test.xls",row.names=FALSE, col.names= FALSE,sheetName = "Conclusiones", append = TRUE)
  W3 <- list(W,W1,W2)
  return(W3)
  
}


#EJEMPLO
#K <- read.delim2("T:/Riesgos/Intercambio/Coordinacion de Riesgo de Mercado, Liquidez y Tasas de Interes/RIESGO DE MERCADO/Valoracion Riesgo/Backtesting/Kupiec/k252Mar18.txt", header=FALSE)
#leo data
k252Mar18 <- read.delim2("~/Documents/Backtesting/k252Mar18.txt", header=FALSE)
K <- kup1(k252Mar18,0.05)

head(k252Mar18)

