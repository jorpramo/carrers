# install.packages("ggmap")
# install.packages("RODBC")
# install.packages("rworldmap")
# install.packages("igraph")
#install.packages("reshape")

library(reshape)
library(ggmap)
library("RODBC")
#prueba
library(rworldmap)
library(RODBC)
library(stringr)
library(igraph)
library(plyr)
library(doBy)


#setwd('E:/PYPROYECTOS/CALLES2/R/DATOS')
setwd('C:/PYPROYECTOS/CALLES/R/DATOS')

myconn <-odbcConnect("CARRERS", uid="loginR", pwd="loginR")

municipios <- sqlQuery(myconn, "SELECT CPRO,CMUN, NOMBRE FROM [CARRERS].[dbo].[MUNICIPIOS] where nombre in ('Valencia','Madrid','Barcelona','Sevilla','Bilbao')")
municipios$CMUN<-str_pad(municipios$CMUN,3,pad = "0")
municipios$CPRO<-str_pad(municipios$CPRO,2,pad = "0")

pobles_com <- sqlQuery(myconn, paste("SELECT count(*), com.nombre from CARRERS.dbo.MUNICIPIOS m 
                                join CARRERS.dbo.COMUNIDAD_PROVINCIA com on com.CPRO=m.cpro
                                     group by com.nombre", sep=""))
colnames(pobles_com)<-c("total","comunidad")

#par(mfrow=c(2,2))

spain.limits <- geocode(c("Vivero, Spain","Tenerife, Spain","Menorca, Spain"))

total_fuera<-data.frame(tipo=character(), nombre=character(),latitud=numeric(),longitud=numeric(), comunidad=character())

#hacia fuera, calles en la capital con nombre de poblaci?n
for (i in 1:nrow(municipios)) {
  nombre=as.character(municipios[i,]$NOMBRE)
  datos <- sqlQuery(myconn, paste("SELECT '",nombre,"' as ciudad,[TVIA],[NVIA]  , Latitud, longitud, com.NOMBRE    FROM [CARRERS].[dbo].[VIAS] v join CARRERS.dbo.MUNICIPIOS m on NVIA=NOMBRE
                   join carrers.dbo.COMUNIDAD_PROVINCIA com on com.CPRO=m.cpro  where v.cmum='",municipios[i,]$CMUN,"' and v.CPRO='",municipios[i,]$CPRO,"'", sep=""))
  total_fuera<-rbind(total_fuera, datos)
}  

head(total_fuera)
com_haciafuera<-ddply(total_fuera,c("ciudad","NOMBRE"), summarise,N=length(NOMBRE))
colnames(com_haciafuera)<-c("ciudad","comunidad","total")
com_pueblos_fuera<-merge(com_haciafuera, pobles_com, by="comunidad",all.y=TRUE)
com_pueblos_fuera$relativo=com_pueblos_fuera$total.x/com_pueblos_fuera$total.y

for (i in 1:nrow(municipios)) {
  temp<-merge(com_pueblos_fuera[com_pueblos_fuera$ciudad==municipios[i,]$NOMBRE,], pobles_com, by ="comunidad",all.y=TRUE)
  temp<-temp[!complete.cases(temp), ]
  temp$total.x<-temp$total.y
  temp$ciudad<-municipios[i,]$NOMBRE
  temp[is.na(temp)]<-0
  temp<-temp[,-c(6)]
  colnames(temp)<-colnames(com_pueblos_fuera)
  com_pueblos_fuera<-rbind(com_pueblos_fuera,temp)
}


write.csv(unique(total_fuera[,c(1,3,4,5)]),"C:/PYPROYECTOS/CALLES/R/DATOS/total_fuera.csv")


write.csv(com_pueblos_fuera,"com_fuera_com.csv")
write.csv(total_fuera,"total_fuera.csv")


total_dentro<-data.frame(tipo=character(), nombre=character(),latitud=numeric(),longitud=numeric(), comunidad=character())

#hacia dentro . calles en diferentes municipios con nombre de la ciudad
for (i in 1:nrow(municipios)) {
  nombre=municipios[i,]$NOMBRE
  datos <- sqlQuery(myconn, paste("SELECT '",nombre,"' as ciudad,[TVIA],m.nombre as origen  , Latitud, longitud  , com.NOMBRE as comunidad
FROM [CARRERS].[dbo].[VIAS] v join CARRERS.dbo.MUNICIPIOS m on v.cpro =m.CPRO and m.CMUN=v.cmum 
                                  join CARRERS.dbo.COMUNIDAD_PROVINCIA com on com.CPRO=m.cpro
                                  where nvia='",nombre,"'", sep=""))
  total_dentro<-rbind(total_dentro, datos)
}  


resumen_haciadentro<-summary(total_dentro[,1])
com_haciadentro<-ddply(total_dentro,c("ciudad","comunidad"), summarise,N=length(comunidad))
com_pueblos<-merge(com_haciadentro, pobles_com, by="comunidad",all.y=TRUE)
com_pueblos$relativo=com_pueblos$N/com_pueblos$total

for (i in 1:nrow(municipios)) {
  temp<-merge(com_pueblos[com_pueblos$ciudad==municipios[i,]$NOMBRE,], pobles_com, by ="comunidad",all.y=TRUE)
  temp<-temp[!complete.cases(temp), ]
  temp$total.x<-temp$total.y
  temp$ciudad<-municipios[i,]$NOMBRE
  temp[is.na(temp)]<-0
  temp<-temp[,-c(6)]
  colnames(temp)<-colnames(com_pueblos)
  com_pueblos<-rbind(com_pueblos,temp)
}


write.csv(com_pueblos,"com_dentro_com.csv")
write.csv(total_dentro,"total_dentro.csv")


final<-merge(com_pueblos_fuera, com_pueblos, by=c("comunidad","ciudad"))
colnames(final)<-c("comunidad","ciudad","calles_fuera","total_pueblos","relativo_fuera","calles_dentro","t","relativo_dentro")
final$relativo_dentro<-final$relativo_dentro*100
final$relativo_fuera<-final$relativo_fuera*100

final$dif<-final$relativo_fuera - final$relativo_dentro


resumen <- cast(final[,c("comunidad","ciudad","dif")], comunidad~ciudad, mean)
resumen<-resumen[!(resumen$comunidad %in% c("Ceuta","Melilla")),]

write.csv(final,"final.csv")

for (i in 1:nrow(final)) {
 print(paste("['",final[i,]$comunidad,"','",final[i,]$ciudad,"',",final[i,]$calles_dentro,"]",sep=""), row.names = FALSE)
}
for (i in 1:nrow(final)) {
  print(paste("['",final[i,]$ciudad,"','",final[i,]$comunidad," ',",final[i,]$calles_fuera,"]",sep=""), row.names = FALSE)
}
for (i in 1:nrow(final)) {
  print(paste("['",final[i,]$comunidad,"','",final[i,]$ciudad,"',",final[i,]$relativo_dentro,"]",sep=""), row.names = FALSE)
}
for (i in 1:nrow(final)) {
  print(paste("['",final[i,]$ciudad,"','",final[i,]$comunidad," ',",final[i,]$relativo_fuera,"]",sep=""), row.names = FALSE)
}


for (i in 1:nrow(municipios)) {
datos <- sqlQuery(myconn, paste("SELECT [TVIA],[NVIA]  , Latitud, longitud   FROM [CARRERS].[dbo].[VIAS] v join dbo.MUNICIPIOS on NVIA=NOMBRE
                   where v.cmum='",municipios[i,]$CMUN,"' and v.CPRO='",municipios[i,]$CPRO,"'", sep=""))
newmap <- getMap(resolution = "low")
plot(newmap,
     xlim = range(spain.limits$lon),
     ylim = range(spain.limits$lat),
     asp = 1
)
points(gsub(",", ".", datos$longitud), gsub(",", ".", datos$Latitud), col = "red", cex = .6)
}

dev.new()
municipios <- sqlQuery(myconn, "SELECT distinct NOMBRE, latitud, longitud, habitantes  FROM [CARRERS].[dbo].[MUNICIPIOS] where CPRO=46")

enlaces <- sqlQuery(myconn, "SELECT distinct  orig.NOMBRE as [from], dest.NOMBRE as [to] FROM [CARRERS].[dbo].[VIAS] v join 
                    dbo.MUNICIPIOS dest on NVIA=NOMBRE join dbo.MUNICIPIOS orig on v.CMUM=orig.CMUN  where v.CPRO=46 and dest.CPRO=46 and orig.CPRO=46")

g <- graph_from_data_frame(enlaces, directed=TRUE, vertices=municipios)

V(g)$name
plot(g)
# dibujar con pesos
#plot(g, edge.width=E(g)$weight, vertex.size=V(g)$weight*10)

length(E(g))
graph.density(g)

#cercania a otros nodos
head(sort(closeness(g), decreasing=F),5)

# importancia, nodos que llegan a el
sort(betweenness(g), decreasing=F)[1:5]

#numero de enlances
sort(degree(g,mode="in"), decreasing=T)[1:10]
sort(degree(g,mode="out"), decreasing=T)[1:10]
g<-upgrade_graph(g)
valencia<-graph.neighborhood(g, 1, "Valencia",mode="in")
Gandia<-graph.neighborhood(g, 1, "Gandia",mode="in")
Alzira<-graph.neighborhood(g, 1, "Alzira",mode="in")

g1<-valencia%u%Gandia%u%Alzira
c1<-cluster_edge_betweenness(g1)
plot(c1, g1)

#comunidades 
comunidades<-edge.betweenness.community(g)
max(comunidades$membership)
plot(comunidades, g, vertex.size=4, vertex.label.cex=0.5)

# close(myconn)


distribucion_mad <- sqlQuery(myconn, "SELECT habitantes  FROM CARRERS.[dbo].[VIAS] v join CARRERS.[dbo].[MUNICIPIOS] m on v.cpro=m.CPRO and cmum=m.CMUN  where nvia='Valencia'")
hist(distribucion_mad$habitantes)
summary(distribucion_mad)
hist(distribucion_mad[distribucion_mad$habitantes<21587,])