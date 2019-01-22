#cargo paquetes
library(magrittr)
library(dplyr)
library(ggplot2)
library(scales)
library(waffle)

mydata <- data.frame(group=c("A", "B", "0", "AB"), FR=c(20, 32, 32, 16)) 

s <- ggplot(mydata, aes(x ="", y=FR,fill=factor(group))) + geom_bar(width = 1,stat="identity")+coord_polar(theta = "y") 


pie <- cbind.data.frame(posicion_tif[,1],posicion_tif[,2])
names(pie) <- c("Titulo","nominal")

p <- plot_ly(pie, labels = ~Titulo, values = ~nominal, type = 'pie') %>%
  layout(title = 'Valor nominal',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))


#ejemplo
df <- data.frame(
  group = c("Male", "Female", "Child"),
  value = c(25, 25, 50)
)
head(df)


# Barplot
bp<- ggplot(df, aes(x="", y=value, fill=group))+
  geom_bar(width = 1, stat = "identity")
bp

pie <- bp + coord_polar("y", start=0)
pie


pie + scale_fill_grey()  +
  theme(axis.text.x=element_blank()) +
  geom_text(aes(y = value/3 + c(0, cumsum(value)[-length(value)]), 
                label = percent(value/100)), size=5)


#mi caso

# Barplot
bp<- ggplot(posicion_tif, aes(x="", y=valor_nominal, fill=titulo))+
  geom_bar(width = 1, stat = "identity")
bp

pie <- bp + coord_polar("y", start=0)
pie

pie + scale_fill_grey()  +
  theme(axis.text.x=element_blank()) +
  geom_text(aes(label = percent(valor_nominal/100)), size=3)



#alternativa
# Load dataset from github
data <- read.table("https://raw.githubusercontent.com/holtzy/data_to_viz/master/Example_dataset/7_OneCatOneNum.csv", header=TRUE, sep=",")

# plot
data %>%
  filter(!is.na(Value)) %>%
  arrange(Value) %>%
  mutate(Country=factor(Country, Country)) %>%
  ggplot( aes(x=Country, y=Value) ) +
  geom_segment( aes(x=Country ,xend=Country, y=0, yend=Value), color="grey") +
  geom_point(size=3, color="#69b3a2") +
  coord_flip() +
  theme(
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position="none"
  ) +
  xlab("")


#mi caso
posicion_tif <- read.csv("~/.Trash/Riesgo-de-Mercado/app/data/posicion_tif.txt")

#grafico 1
posicion_tif  %>%
  arrange(valor_nominal) %>%
  mutate(Titulo=factor(titulo, titulo)) %>%
  ggplot( aes(x=Titulo, y=valor_nominal) ) +
  geom_segment( aes(x=Titulo ,xend=Titulo, y=0, yend=valor_nominal), color="black") +
  geom_point(size=1, color="blue") +
  coord_flip() +
  theme(
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position="none"
  ) +
  xlab("")

#grafico 2
pie(posicion_tif$valor_nominal,labels = posicion_tif$titulo,radius=0.9,clockwise = FALSE)

#variante
pieval<-c(2,1,3,94)
plot(0,xlim=c(1.5,5),ylim=c(1,5),type="n",axes=FALSE,xlab="",ylab="")
box()
bisect.angles<-floating.pie(3,3,pieval,explode=c(0.1,0.2,0.3,0))
pie.labels(3,3,bisect.angles,c("two","one","three","ninety\nfour"),
           minangle=0.2,,explode=c(0.1,0.2,0.3,0))


#mi caso
pos <- posicion_tif[order(posicion_tif$valor_nominal),]
pos$porc <- pos$valor_nominal/sum(pos$valor_nominal)
  
plot(0,xlim=c(1.5,5),ylim=c(1,5),type="n",axes=FALSE,xlab="",ylab="")
#box()
bisect.angles<-floating.pie(3,3,pos$valor_nominal[1:20],explode=pos$porc[20:1]*14)
pie.labels(3,3,bisect.angles,pos$titulo[1:20],
           minangle=0.2,explode = rep(0.6,20))

#NO LO VEO VIABLE PERO BUEN INTENTO
#GRAFICO DE BARRAS
library(reshape2) # Defines melt function
iris_mean <- aggregate(iris[,1:4], by=list(Species=iris$Species), FUN=mean) 
df_mean <- melt(iris_mean, id.vars=c("Species"), variable.name = "Samples", value.name="Values")


p <- ggplot(df_mean, aes(Samples, Values, fill = Species)) + 
  geom_bar(position="dodge", stat="identity") + coord_flip() + 
  theme(axis.text.y=element_text(angle=0, hjust=1))
print(p) 

#
p <- ggplot(pos, aes(reorder(titulo,valor_nominal), valor_nominal, fill = titulo)) + 
  geom_bar(position="stack", stat="identity",show.legend = FALSE) + coord_flip() + 
  labs(x = "Títulos",y = "Valor Nominal")+
  theme(axis.text.y=element_text(angle=0, hjust=1))
print(p) 

#waffle
tax_count <- c(`Archaea and Viruses (4,271)`= 4271, `Bacteria (21,345)`= 21345, `Eukaryota (470,122)`= 470122, `Fungi (41,952)`= 41952, `Metazoa (255,771)`= 255771, `Viridiplantae (156,967)`= 156967)

waffle(tax_count/1000, rows=20, size=0.1, colors=c("#cc0000", "#ff9900", "#ff6699", "#6699ff", "#006666", "#33cc33"), title="All NCBI Taxonomy Nodes by Kingdom", xlab="1 square is 1,000 nodes.")

#caso mio
n <- pos[,2]/500000
names(n) <- pos[,1]
waffle(n, rows=20, size=0.1, colors=rainbow(nrow(pos)),xlab="Un cuadrado es 500.000 VES.")
