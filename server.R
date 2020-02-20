
library(httpuv)
library(leaflet)

#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

source("./bbdd.R")

library(shiny)

function(input, output, session) {
  capas<-st_read("municipios.shp")
  capas$NAMEUNIT<-toupper(capas$NAMEUNIT)
  
  datos_municipio<-reactive({
  municipios<-dame_municipios()
    municipios[municipios$NOMBRE==input$municipio,]
  })
  
  output$municipio <- renderText({
    paste("Ciudad:",input$municipio,sep="")
    })
  
  output$mapa <- renderLeaflet({
    mun<-datos_municipio()
    
    hacia_fuera<-mun_hacia_fuera(mun$CPRO,mun$CMUN,mun$NOMBRE)
    #hacia_fuera<-mun_hacia_fuera('08','019',"Barcelona")
    #print(nrow(hacia_fuera))
    puntos<-hacia_fuera
    
    
    puntos$longitude<-as.numeric(as.character(str_replace(puntos$longitude,',','.')))
    puntos$latitude<-as.numeric(as.character(str_replace(puntos$latitude,',','.')))
    
    pal <- colorFactor(palette = c("red", "blue", "#9b4a11"), 
                       levels = levels(puntos$nombre))
    if (input$tipo=="Normal"){
    leaflet() %>%
      addTiles() %>%
      addCircleMarkers(data =puntos, radius = 3
                       , color = ~pal(nombre)
                       ,popup= ~paste0("Ciudad:",nvia))
    }
    else
    {
      
      #andilla<-capas[capas$NAMEUNIT %in% puntos$nvia,]
      puntos$nvia<-trimws(puntos$nvia)
      andilla<-subset(capas,NAMEUNIT %in% puntos$nvia)
      
      leaflet() %>% 
        addTiles() %>% 
        addPolygons(data = andilla,stroke = TRUE, fillOpacity = 0.8,
                    highlight = highlightOptions(weight = 3,
                                                 color = "red",
                                                 bringToFront = TRUE)
                    ,popup= ~paste0("Ciudad:",NAMEUNIT))
    }
  })
  
}


