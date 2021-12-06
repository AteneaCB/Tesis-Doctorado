#### PROYECTO: Tesis Doctorado Ciencias Económicas
### AUTORA: Mtra. Atenea De La Cruz Brito
### FECHA: 3 diciembre 2021 | Modificado: 
### DATOS: VivaAnuncios, Century21, Century21 Global, Realtor.CA
### PROYECTO: a)MX-US: Heroica Nogales-Nogales, b)US-CA: Sault St. Marie-SSM
### ARCHIVOS: a)NOG_C21_20211119.csv b)CHI_C21_20211102_000-More-1b.csv
### DESCRIPCION: App interactiva de Fase 1, Analisis Exploratorio de Datos

#### Bibliotecas ####
library(shiny)
library(shinydashboard)
library(shinythemes)
library(ggplot2)
library(leaflet)
library(leaflet.extras)
library(sf)
library(rworldxtra)
library(RColorBrewer)

#### Directorio ####
setwd("https://github.com/AteneaCB/Tesis-Doctorado/tree/main/app")
usssm <- read.csv("https://raw.githubusercontent.com/AteneaCB/Tesis-Doctorado/main/app/2111usssm.csv", header = TRUE) #Para tabla
nog <- read.csv("https://raw.githubusercontent.com/AteneaCB/Tesis-Doctorado/main/app/2111nog.csv", header = TRUE) #Para mapa

############################ I. UI  ############################
ui <-   
    fluidPage(
        dashboardPage(
            #Título
            dashboardHeader(title = "Atenea | Avance Tesis"),
            #Sidebar
            dashboardSidebar(
                #Menu
                sidebarMenu(
                    
                    menuItem("Presentación", tabName = "intro", icon = icon("file-text")),
                    menuItem("Área de estudio", tabName = "area", icon = icon("sun-o")),
                    menuItem("Análisis Exploratorio de Datos", tabName = "aed", icon = icon("area-chart")),
                    menuItem("Ejemplo de datos", tabName = "table_p", icon = icon("table")),
                    menuItem("Mapa Interactivo", tabName = "map", icon = icon("map"))
                )
            ),
            #Cuerpo
            dashboardBody(
                tabItems(
                    # Presentación
                    tabItem(tabName = "intro",
                            fluidRow(
                                titlePanel(h2("Tesis. Mercados de vivienda transfronterizos entre México, Estados Unidos y Canadá: submercados e información asimétrica")),
                                includeHTML("index.html")
                                )
                    ),
                    #Área de estudio
                    tabItem(tabName = "area", 
                            fluidRow(
                                titlePanel(h3("Área de Estudio")),
                                img(src= "BorderCrossing_MX-US-CA_1.jpg", width = 800),
                            )
                    ),
                    #Análisis Exploratorio de Datos
                    tabItem(tabName = "aed", 
                            fluidRow(
                                titlePanel(h3("Diagrama de dispersión MX Nogales")),
                                img(src= "2111mxnog_splot.png", width = 500),
                                titlePanel(h3("Gráfica de burbuja MX Nogales")),
                                img(src= "2111mxnog_bubblechart.png",width =602),
                                titlePanel(h3("Diagrama de dispersión US Nogales")),
                                img(src= "2111usnog_splot.png", width = 500),
                                titlePanel(h3("Gráfica de burbuja US Nogales")),
                                img(src= "2111usnog_bubblechart.png",width =602),
                                titlePanel(h3("Diagrama de dispersión US Sault St. Marie")),
                                img(src= "2111usssm_splot.png", width = 500),
                                titlePanel(h3("Gráfica de burbuja US Sault St. Marie")),
                                img(src= "2111usssm_bubblechart.png",width =602),
                                titlePanel(h3("Diagrama de dispersión CA Sault Sainte Marie")),
                                img(src= "2111cassm_splot.png", width = 500),
                                titlePanel(h3("Gráfica de burbuja CA Sault Sainte Marie")),
                                img(src= "2111cassm_bubblechart.png",width =602)
                            )
                    ),
                    #Ejemplo de datos
                    tabItem(tabName = "table_p",
                            fluidRow(        
                                titlePanel(h3("Ejemplo de base de datos de oferta de vivienda, US Sault St. Marie")),
                                dataTableOutput ("data_table")
                            )
                    ),
                    #Mapa
                    tabItem(tabName = "map",
                            fluidRow(
                                titlePanel(h3("Mapa interactivo de oferta de vivienda en la conurbación Nogales-Nogales")),
                                leafletOutput("data_map")
                            )
                    )
                )
            )
        )
    )
########################## II. Server ##########################
#Server
server <- function(input, output) {
    
    #Presentación 
    
    
    #Análisis Exploratorio de Datos

    
    #Ejemplo de datos
    output$data_table <- renderDataTable( {usssm}, 
                                            options = list(lengthMenu = c(5,20,50),
                                                           pageLength = 5)
    )
    

    #Mapa Interactivo
    #Datos
    
    #Generar mapa
    output$data_map <- renderLeaflet({
        leaflet() %>% 
            addTiles() %>% 
            addCircles(data = nog, lng = ~long, lat = ~lat) %>% 
            addMarkers(data = nog, lng = ~long, lat = ~lat, clusterOptions = markerClusterOptions())
        
        #Cambiar colores
        pal <- colorNumeric(palette = "viridis", domain = nog$price_usd)
        #Mapa con colores
        m <- leaflet() %>% addTiles() %>% addCircles(data = nog, lng = ~long, lat = ~lat,
                                                     color = pal(nog$price_usd), fillOpacity = 1, popup = nog$price_usd, label = nog$price_usd,
                                                     group = "Viviendas") %>%
            addMarkers(data = nog, lng = ~long, lat = ~lat, clusterOptions = markerClusterOptions())
        
        #Leyenda
        m <- m %>% addLegend(data = nog, "topright", pal = pal, 
                             values = ~price_usd, title = "Precio USD", opacity = 0.8, group = "Leyenda")
        #Capas
        (m <- m %>% addLayersControl(overlayGroups = c("Viviendas", "Leyenda"),
                                     options = layersControlOptions(collapsed = F)))
    })
}#fin server



###################### III. Run App ########################
shinyApp(ui = ui, server = server)
