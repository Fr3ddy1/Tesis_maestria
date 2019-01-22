#pruebas usar txt historico_act con la funcion Splines



dat <- read.csv(paste(getwd(),"app","data","Historico_act.txt",sep = "/"),sep="")
dat[,3] <- as.Date(as.character(dat[,3]))
car <- Carac(paste(getwd(),"app","data","Caracteristicas.xls",sep = "/"))

print(str(data_splines))
print(str(dat))

#funciona
a <- Tabla.splines(data = data_splines,tipo = "TIF",fe=as.Date("2018-03-08"),num =40,par = 0.2,tit=tit[c(1,6,13)],C_splines,pr=c(101,102,103))

#no corre
Tabla.splines(data = dat,tipo = "TIF",fe=as.Date("2018-03-08"),num =40,par = 0.2,tit=tit[c(1,6,13)],C_splines,pr=c(101,102,103))

#el problema era el formato de la fecha de vencimiento