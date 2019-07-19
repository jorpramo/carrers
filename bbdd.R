library(RODBC)
library(stringr)
myconn <-odbcConnect("CARRERS", uid="loginR", pwd="loginR")


dame_municipios<-function(){
  municipios <- sqlQuery(myconn, "SELECT CPRO,CMUN, NOMBRE FROM [CARRERS].[dbo].[MUNICIPIOS] where nombre in ('Valencia','Madrid','Barcelona','Sevilla','Bilbao')")
  municipios$CMUN<-str_pad(municipios$CMUN,3,pad = "0")
  municipios$CPRO<-str_pad(municipios$CPRO,2,pad = "0")
  return(municipios)
}

mun_hacia_fuera<-function(pro, mun, nombre){
  datos <- sqlQuery(myconn, paste("SELECT '",nombre,"' as ciudad,nvia,m.latitud as latitude, m.longitud as longitude, com.NOMBRE as nombre     FROM [CARRERS].[dbo].[VIAS] v join CARRERS.dbo.MUNICIPIOS m on NVIA=NOMBRE
                   join carrers.dbo.COMUNIDAD_PROVINCIA com on com.CPRO=m.cpro  where v.cmum='",mun,"' and v.CPRO='",pro,"'", sep=""))
  return(datos)
}

