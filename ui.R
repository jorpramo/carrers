#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#


library(markdown)
library(leaflet)


source("./bbdd.R")
municipios<-dame_municipios()


pageWithSidebar(
  headerPanel('Carrers'),
  sidebarPanel(
    selectInput("municipio", "Selecciona el municipio",
                choices=municipios$NOMBRE),
    radioButtons("tipo", "Representación:",
                 c("Normal","Término Municipal"))
  ),
  mainPanel(
    h1("Las ciudades hacia fuera"),
    h3("Como las ciudades se han proyectado hacia fuera. Nombres de calles que coinciden con municipios"),
    #h3(paste("Ciudad:",textOutput("municipio"),sep="")),
    h3(textOutput("municipio")),
    leafletOutput('mapa',height = "600px")
  )
)
