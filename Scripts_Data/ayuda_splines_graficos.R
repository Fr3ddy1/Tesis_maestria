library(lspline)
library(ggplot2)
library(splines)

x <- seq(1,10,0.5)
y <- runif(length(x),1,3)
plot(x,y)

#spline lineal para nodos especificados
m1 <- lm(y ~ lspline(x, c(2.5,5,7.5)))
#calcula nodos a usar
m1 <- lm(y ~ qlspline(x,q = 3))


fig <- ggplot(data.frame(x=x,y=y), aes(x=x, y=y)) +
  geom_point()
  
 fig

 fig +
   geom_smooth(method="lm", formula=formula(m1), se=FALSE) +
   geom_vline(xintercept = c(2.5,5,7.5), linetype=2)+
   ggtitle("Ajuste mediante Spline Lineal")+
   theme(
     plot.title = element_text(color="black", size=14, face="bold.italic",hjust = 0.5)
    )
 
 #spline cuadratico
 m2 <- lm(y ~ bs(x, degree = 2,df=4)) 
 y2 <- predict(m2, data.frame(x = x))
 
 
 fig +
   geom_line(aes(x=x, y=y2,col=1))+
   geom_vline(xintercept = c(2.5,5,7.5), linetype=2)+
   ggtitle("Ajuste mediante Spline Cuadrático")+
   theme(
     legend.position="none",
     plot.title = element_text(color="black", size=14, face="bold.italic",hjust = 0.5)
   )
 
 #spline cubico
 #m3 <- ns(x,knots=c(2.5,5,7.5)) 
 m3 <- lm(y ~ ns(x,knots=c(2.5,5,7.5)))
 
 
 fig +
   geom_smooth(method="lm", formula=formula(m3), se=FALSE) +
   geom_vline(xintercept = c(2.5,5,7.5), linetype=2)+
   ggtitle("Ajuste mediante Spline Cúbico")+
   theme(
     plot.title = element_text(color="black", size=14, face="bold.italic",hjust = 0.5)
   )
 
 
 #comparativo
 
 fig+
   geom_smooth(method="lm", formula=formula(m1), se=FALSE,colour="blue") +
   geom_line(aes(x=x, y=y2,col="red"))+
   geom_smooth(method="lm", formula=formula(m3), se=FALSE,colour="green") +
   geom_vline(xintercept = c(2.5,5,7.5), linetype=2)+
   ggtitle("Comparativo ajustes mediante Splines")+
   theme(
     legend.position="none",
     plot.title = element_text(color="black", size=14, face="bold.italic",hjust = 0.5)
   )
   
 