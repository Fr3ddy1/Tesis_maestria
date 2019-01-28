rm(list = ls())

#cargo librerias a usar
library(shiny)
library(readr)
library(rriskDistributions)
library(fitdistrplus)
library(shinydashboard)
library(dplyr)
library(lubridate)
library(ggplot2)
library(reshape2)
library(jrvFinance)
library(plotly)
library(rbokeh)
#library(reshape2)
#library(xlsx)
library(nloptr)
library(alabama)
library(DT)
library(xtable)
library(webshot)
library(readxl)
library(xml2)
library(rvest)
#library(VaRES)
#library(lmomco)


options(OutDec = ",")


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

#Precios promedio
#precios 11-08 - tif
pr=c(101,112,110,121.0234,116.5251,130.0234,
     129.0156,125.0626,128.1000,120,124,122,126.5234,128.5235,128.1913,
     129,132.0391,128.5235,129.8875,130.1,128.5313,127,128.5235,127.0156,
     127.0156)

#names(pr) <- tit

#defino vector de precios donde tengo nombre de titulo
p_t <- cbind.data.frame(tit,pr)
#p_t <- t(pr)

#precios 11-08 - veb
pr1=c(100.4,106,110,111,118,121,
      127.8376,102.2,117,130.3269,127,129.45,129,129.9627,128,129,129.1875,
      128.5,102,129.9469,129.6807,130.0156,125.0313,125.75,130.5,129.5235,
      128.5313,130,128.0235)

#defino vector de precios donde tengo nombre de titulo
p_v <- cbind.data.frame(tit1,pr1)

#Defino funcion para extraer precios promedios segun tit considerados
pos <- function(t,ind){
p1 <- c()
#caso tif
if(ind==0){
for(i in 1:length(t)){
  p1[i] <- pr[which(t[i]==p_t$tit)]
}
}#final caso tif
#caso veb
if(ind==1){
  for(i in 1:length(t)){
    p1[i] <- pr1[which(t[i]==p_v$tit1)]
  }
}#final caso veb
names(p1) <- t
return(p1)
}#final funcion pos


#Defino función para extraer precios promedio del archivo en la carpeta data
pos1 <- function(t,ind){
  #caso tif
  if(ind==0){
    tif <- read.csv(paste(getwd(),"data","Precio_prom_tif.txt",sep = "/"),sep="")
    tif$'Títulos' <- as.character(tif$'Títulos')
    p <- c()
    
    #condicional de existencia
    for(i in 1:length(t)){
      if(length(which(t[i]==tif$'Títulos'))!=0){
           p[i] <- tif$Precio.Promedio[which(t[i]==tif$'Títulos')]
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
    veb <- read.csv(paste(getwd(),"data","Precio_prom_veb.txt",sep = "/"),sep="")
    veb$'Títulos' <- as.character(veb$'Títulos')
    
    p <- c()
    #condicional de existencia
    for(i in 1:length(t)){
      if(length(which(t[i]==veb$'Títulos'))!=0){
        p[i] <- veb$Precio.Promedio[which(t[i]==veb$'Títulos')]
      }else{
        #print("Titulo no encontrado")
        p[i] <- 0
      }
    } 
    
    names(p) <- t
    return(p)
    
  }
  
  
  
}#final funcion pos1


#Parametros iniciales
#tif
pa_sven=c(0.133799434790145,-0.01,-0.307885339616438,-0.134075672659356,
    0.545398124008073,0.350692201663154)

names(pa_sven) <- c("B0","B1","B2","B3","T1","T2")
#names(pa) <- c("$$\beta_{0}$$","$$\\beta_{1}$$","$$\\beta_{2}$$","$$\\beta_{3}$$","$$\\tau_{1}$$","$$\\tau_{2}$$")

#pa <- xtable(as.data.frame(pa))

pa_ns=c(0.133799434790145,-0.01,-0.307885339616438,0.545398124008073)

names(pa_ns) <- c("B0","B1","B2","T1")

#vebono
pa1_sven=c(0.135872169451391,0.1,-0.503768911829894,-0.288755056029301,
      0.11951691203874,0.501729233062216)

names(pa1_sven) <- c("B0","B1","B2","B3","T1","T2")

pa1_ns <- c(0.135872169451391,0.1,-0.503768911829894,0.11951691203874)

names(pa1_ns) <- c("B0","B1","B2","T1")

#leo documento caracteristicas
#source('C:/Users/Freddy Tapia/Riesgo-de-Mercado/app/funciones.R')
source(paste(getwd(),"funciones.R",sep = "/"))
#C <- Carac("C:/Users/Freddy Tapia/Desktop/29-06-18.xls")
#C <- Carac(paste(getwd(),"data","29-06-18.xls",sep = "/"))
C <- read.csv(paste(getwd(),"data","C.txt",sep = "/"), sep="")
names(C) <- c("Tipo Instrumento","Nombre","Sicet","F.Emision",
              "F.Vencimiento","Tipo tasa","Inicio","Pago cupon 1" ,
              "Pago cupon 2","Cupon")

#carateristica Splines
C_splines <- read.csv(paste(getwd(),"data","C_splines.txt",sep = "/"), sep="")
names(C_splines) <- c("Tipo Instrumento","Nombre","Sicet","F.Emision",
              "F.Vencimiento","Tipo tasa","Inicio","Pago cupon 1" ,
              "Pago cupon 2","Cupon")

#cargo data de prueba 0-22
data_splines <- read.csv(paste(getwd(),"data","Data_splines.txt",sep = "/"), sep="")
data_splines$Fecha.op <- as.Date(as.character(data_splines$Fecha.op))

datatif <- data_splines[which(data_splines$Tipo.Instrumento=="TIF"),]
datatif <- arrange(datatif,desc(Fecha.op))
datatif$F.Vencimiento <- as.Date(datatif$F.Vencimiento,format="%d/%m/%Y")
datatif$year <- year(datatif$F.Vencimiento)
datatif$segmento <- cut(datatif$year,breaks = c(2015,2019,2030,2038),labels = c("Corto Plazo","Mediano Plazo","Largo Plazo"))
datatif$segmento1 <- cut(datatif$year,breaks = c(2015,2017,2019,2025,2030,2033,2035,2038),labels = c("C1","C2","M1","M2","L1","L2","L3"))

#TEXTOS
############################################# DATA ###############################################

UPLOADDATA_TEXT<-"Cargar el archivo con los datos"
SELECTFILE_TEXT<-'Seleccione el archivo'
FILESELEC_TEXT<-'Aun no seleccionas el archivo...'
BUTTSELEC_TEXT<-'Buscar'
WITHHEADER_TEXT<-"Con encabezado"
SEPARATOR_TEXT<-"Separador"
COMILLAS_TEXT<-"Comillas"
ENCABEZADO_TEXT<-"Encabezado de los datos"

UPLOADFILETYPE_CONF<-c('text/csv',
                       'text/comma-separated-values',
                       'text/tab-separated-values',
                       'text/plain',
                       '.csv',
                       '.tsv')

UPLOADFILESEP_CONF<-c('Coma'=',',
                      'Punto y coma'=';',
                      'Tab'='\t')

UPLOADCOMILLAS_CONF<-c('Ninguna'='',
                       'Comilla doble'='"',
                       'Comilla simple'="'")

############################################ ESTADÍSTICA ##############################################

RENDTITLE_TEXT<-'Rendimientos'
FRETITLE_TEXT<-'Frecuencias'
STATTITLE_TEXT<-'Estadísticos Básicos'

############################################# ANÁLISIS ##############################################

PRUEBDISTTITLE_TEXT<-"Resultados de Pruebas de Distribución"
DISTWITHPARAMTITLE_TEXT<-"Función de Distribución con los Parámetros correspondientes"
GRAFTITLE_TEXT<-"Gráficos"
AHORROTABTITLE_TEXT<-"Análisis Cuentas de Ahorro"
CORRITABTITLE_TEXT<-"Análisis Cuentas Corrientes"
CORRIRTABTITLE_TEXT<-"Análisis Cuentas Corrientes Remuneradas"
SELECFUNCTION_TEXT<-"Seleccione una Distribución"
CORRTABTITLE_TEXT<-"Análisis Cuentas Corrientes"
CORRRTABTITLE_TEXT<-"Análisis Cuentas Corrientes Remuneradas"

#SELECCIÓN DE DISTRIBUCIÓN
#NORMAL
NLABEL1<-"Media"
NLABEL2<-"Desviación Típica"
#EXPONENCIAL
ELABEL1<-"Lambda"
#CAUCHY
CLABEL1<-"Mu"
CLABEL2<-"Theta"
#LOGISTICA
LLABEL1<-"S"
LLABEL2<-"L"
#BETA
BLABEL1<-"S1"
BLABEL2<-"S2"
#CHICUADRADO
CCLABEL<-"Grados de Libertad"
#UNIFORME
ULABEL1<-"Valor mínimo"
ULABEL2<-"Valor máximo"
#GAMMA
GLABEL1<-"M"
GLABEL2<-"Lambda"
#LOGNORMAL
LNLABEL1<-"Media"
LNLABEL2<-"Desviación Típica"
#WEIBULL
WLABEL1<-"S1"
WLABEL2<-"S2"
#FISHER
FLABEL1<-"Grados de Libertad 1"
FLABEL2<-"Grados de Libertad 2"
#T-STUDENT
TLABEL1<-"Grados de Libertad"
#GOMPERTZ
GOLABEL1<-"S1"
GOLABEL2<-"S2"


#Selección análisis cuentas de ahorro
DISTANALAH_CONF<-c("Normal"="Normal", "Exponential"="Exponential",
                   "Cauchy"="Cauchy", "Logistic"="Logistic",
                   "Beta"="Beta", "Chi-square"="Chi-square",
                   "Uniform"="Uniform","Gamma"="Gamma",
                   "Lognormal"="Lognormal", "Weibull"="Weibull",
                   "F"="F", "Student"="Student", "Gompertz"="Gompertz")

############################################# VAR ###############################################
#TÍTULO DE LA CAJA
BOXSELECVARTITLE_TEXT<-"Seleccione Porcentaje del VaR"

#SUBTITULOS DEL ITEM
VARAHTITLE_TEXT<-"Valor en Riesgo (VaR) para las Cuentas de Ahorro"
VARCOTITLE_TEXT<-"Valor en Riesgo (VaR) para las Cuentas Corrientes"
VARCRTITLE_TEXT<-"Valor en Riesgo (VaR) para las Cuentas Corrientes Remuneradas"

#DISTRIBUCIÓN
VARINNOR_TEXT<-"Formulación del VaR para la Distribución Normal $$VaR_p(X) = \\mu + \\sigma \\Phi^{-1}(p)$$"
VARINEXP_TEXT<-"Formulación del VaR para la Distribución Exponencial $$VaR_p(X) = -\\frac{1}{\\lambda}log(1-p)$$"
VARINCAU_TEXT<-"Formulación del VaR para la Distribución Cauchy $$VaR_p(X) = \\mu + \\sigma tan(\\pi(p-\\frac{1}{2}))$$"
VARINLOG_TEXT<-"Formulación del VaR para la Distribución Logistica $$VaR_p(X) = \\mu + \\sigma log[p(1-p)]$$"
VARINBET_TEXT<- "Formulación del VaR para la Distribución Beta"
VARINCHC_TEXT<- "Formulación del VaR para la Distribución Chi Cuadrado"
VARINUNF_TEXT<- "Formulación del VaR para la Distribución Uniforme $$VaR_p(X) = a + p(b-a)$$"
VARINGAM_TEXT<- "Formulación del VaR para la Distribución Gamma $$VaR_p(X) = \\frac{1}{b} Q^{-1}(a, 1-p)$$"
VARINLGN_TEXT<- "Formulación del VaR para la Distribución Lognormal $$VaR_p(X) = e^{[\\mu + \\sigma \\Phi^{-1}(p)]}$$"
VARINWEI_TEXT<- "Formulación del VaR para la Distribución Weibull $$VaR_p(X) = \\sigma[-log(1-p)]^{\\frac{1}{\alpha}}$$"
VARINF_TEXT<- "Formulación del VaR para la Distribución F $$VaR_p(X) = \\mu + F^{-1}(p)\\sigma$$"
VARINTST_TEXT<- "Formulación del VaR para la Distribución T student $$VaR_p(X) = \\mu + T^{-1}(p)\\sigma$$"
VARINGOM_TEXT<- "Formulación del VaR para la Distribución Gompertz"

VARTINNOR_TEXT<- "Formulación del TVaR para la Distribución Normal $$TVaR_p(X) = \\mu + \\frac{σ}{p} \\int_{0}^{p} \\Phi^{-1}(v) dv$$"
VARTINEXP_TEXT<- "Formulación del TVaR para la Distribución Exponencial $$VaR_p(X) = -\\frac{1}{\\lambda}log(1-p)$$"
VARTINCAU_TEXT<- "Formulación del TVaR para la Distribución Cauchy $$TVaR_p(X) = \\mu + \\frac{\\sigma}{p} \\int_{0}^{p} tan(\\pi(v-\\frac{1}{2}))dv$$"
VARTINLOG_TEXT<- "Formulación del TVaR para la Distribución Lognormal $$TVaR_p(X) = \\frac{e^{\\mu}}{p} \\int_{0}^{p} e^{\\sigma \\Phi^{-1}(v)}dv$$"
VARTINBET_TEXT<- "Formulación del TVaR para la Distribución Beta"
VARTINCHC_TEXT<- "Formulación del TVaR para la Distribución Chi Cuadrado"
VARTINUNF_TEXT<- "Formulación del TVaR para la Distribución Uniforme $$TVaR_p(X) = a + \\frac{p}{2} (b-a)$$"
VARTINGAM_TEXT<- "Formulación del TVaR para la Distribución Gamma $$TVaR_p(X) = \\frac{1}{bp} \\int_{0}^{p}Q^{-1}(a, 1-v)dv$$"
VARTINLGN_TEXT<- "Formulación del TVaR para la Distribución Lognormal $$TVaR_p(X) = \\frac{e^{\\mu}}{p} \\int_{0}^{p} e^{\\sigma \\Phi^{-1}(v)}dv$$"
VARTINWEI_TEXT<- "Formulación del TVaR para la Distribución Weibull $$TVaR_p(X) = \\frac{\\sigma}{p}\\gamma[1+\\frac{1}{\\alpha}, -log(1-p)]$$"
VARTINF_TEXT<- "Formulación del TVaR para la Distribución F $$TVaR_p(X) = \\mu + \\int_{0}^{p}F^{-1}(v)\\sigma dv$$"
VARTINTST_TEXT<- "Formulación del TVaR para la Distribución T student $$TVaR_p(X) = \\mu + \\int_{0}^{p}T^{-1}(v)\\sigma dv$$"
VARTINGOM_TEXT<- "Formulación del TVaR para la Distribución Gompertz"



# Encabezado Vision
VisionHeader <- function(){tags$head(
  tags$link(rel = "stylesheet", type = "text/css", href = "app.css"),
  tags$img(src="img/vision1.png" , id = "VisionLogo", width = 130 ),
  singleton(includeScript("www/js/d3.js")),
  singleton(includeScript("www/js/underscore.js")),
  singleton(includeScript("www/js/jquery-ui.js")),
  singleton(includeCSS("www/css/app.css"))
)}

ACERTITLE_TEXT<-"Acerca de VisionRisk™"
ACERSUBSV_TEXT<-"Tecnología para Especulación, Inversión, Economía, Finanzas y Riesgo"
ACERVER_TEXT<-"Versión: 0.0.0"
ACERRIF_TEXT<-"Rif: "
ACERRS_TEXT<-"Copyright © 2014-2018 Synergy Vision"
ACERRS_TEXT2 <- "All Rights Reserved"
ACERDIR_TEXT<-"Centro Gerencial Mohedano, La Castellana"
ACERTLF_TEXT<-"0212-2630808 / 0414-2769752"
ACERCORR_TEXT<-"contacto@synergy.vision"