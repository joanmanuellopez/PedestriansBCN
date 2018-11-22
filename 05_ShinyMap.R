#### 5. SCRIPT FOR THE SHINY APP SHOWING A LEAFLET MAP ABOUT PEDESTRIAN ACCIDENTABILITY ####

library("shiny")
library("leaflet")
library("RColorBrewer")

library("readr")
library("dplyr")

AccPersVian_DF <- read_csv("./WorkingData/accidents_vianants.csv")

# Dismiss all those registers with coordinates NA... NAs in X and Y are in the same row, so there is only need for dismiss by one of them
AccPersVian_DF <- AccPersVian_DF[which(!is.na(AccPersVian_DF$CoordenadaUTM_X)),]

# Create a date column with dd-mm-yyyy format, just to allow filtering by date
AccPersVian_DF <- AccPersVian_DF %>% mutate(Data = as.Date(paste(Any, MesAny, DiaMes, sep = "-" )))

## Transform Coordinates
library("rgdal")
# prepare UTM coordinates matrix
utmcoor <- SpatialPoints(cbind(lon = AccPersVian_DF$CoordenadaUTM_X,lat = AccPersVian_DF$CoordenadaUTM_Y), proj4string=CRS("+proj=utm +zone=31"))

#utmdata$X and utmdata$Y are corresponding to UTM Easting and Northing, respectively.
#zone= UTM zone
# converting
longlatcoor <- spTransform(utmcoor,CRS("+proj=longlat"))

AccPersVian_DF$LAT <- longlatcoor@coords[,2]
AccPersVian_DF$LON <- longlatcoor@coords[,1]

# Defining a color palette
victpal <- colorFactor(c("#6ab04c","#30336b","#050799"), AccPersVian_DF$DescripcioVictimitzacio)


#### Shiny - User Interface ####
ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10, style="background-color:#fcfcdc;padding: 0 20px 20px 20 px; opacity:0.85",
                dateRangeInput("dataId","Date (from 01-01-2014 to 31-12-2017)",
                               start = as.Date("2014-01-01"), end = as.Date("2017-12-31"),
                               min = as.Date("2014-01-01"), max = as.Date("2017-12-31"),
                               separator = " - ", format = "dd/mm/yyyy",
                               startview = 'month', language = 'en', weekstart = 1),
                sliderInput("hourId", "Period (Hours of day)", min(AccPersVian_DF$HoraDia), max(AccPersVian_DF$HoraDia),
                            value = range(AccPersVian_DF$HoraDia), step = 1),
                radioButtons("causeId", "Pedestrian fault",
                             choices = list("All" = "All", 
                                            "Crossing out of pass" = "CreuaForaPas", 
                                            "Disobeying traffic light" = "DsbSemafor"), 
                             selected = "All")
  )
)

#### Shiny - Server ####
server <- function(input, output, session) {
  
  filteredData <- reactive({
    
    if(input$causeId=="All"){
      AccPersVian_DF[AccPersVian_DF$HoraDia >= input$hourId[1]
                     & AccPersVian_DF$HoraDia <= input$hourId[2]
                     & !AccPersVian_DF$DescripcioVictimitzacio=="Lleu"
                     & AccPersVian_DF$Data >= input$dataId[1]
                     & AccPersVian_DF$Data <= input$dataId[2],]
    } else {
      AccPersVian_DF[AccPersVian_DF$HoraDia >= input$hourId[1]
                     & AccPersVian_DF$HoraDia <= input$hourId[2]
                     & !AccPersVian_DF$DescripcioVictimitzacio=="Lleu"
                     & AccPersVian_DF$DescripcioCausaVianant==input$causeId
                     & AccPersVian_DF$Data >= input$dataId[1]
                     & AccPersVian_DF$Data <= input$dataId[2],]
    }
  })
  
  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet(AccPersVian_DF) %>% addTiles() %>%
      fitBounds(~min(LON), ~min(LAT), ~max(LON), ~max(LAT)) 
    
  })
  
  observe({
    
    leafletProxy("map", data = filteredData()) %>%
      clearShapes() %>%
      addCircles(radius = 10, weight = 30, color = ~victpal(DescripcioVictimitzacio), 
                 fillColor="#556565", fillOpacity = 0.75)
    
  })
  
}

## Run the ShinyApp & Have Fun!!
shinyApp(ui, server)
