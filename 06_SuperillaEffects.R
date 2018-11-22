#### 6. SuperIlla effects on traffic accidents ####

## Create Dataframes for the Superilla [neighbourhoods] district (Codi = 10, Sant Marti)
## Consider only accidents in the district #10, where the Superilla is placed - Not considered neighbourhoods
library("readr")
Accidents.DF <- read_csv("./WorkingData/accidents.csv")

Accidents.DF$CodiDistricte <- as.factor(Accidents.DF$CodiDistricte)
Accidents.DF$DiaSetmana <- as.factor(Accidents.DF$DiaSetmana)
Accidents.DF$DiaSetmana <- ordered(Accidents.DF$DiaSetmana, levels = c("Dl","Dm", "Dc", "Dj", "Dv", "Ds", "Dg"))
Accidents.DF$HoraDia <- as.integer(Accidents.DF$HoraDia)

## Filter 1 -- take only those accidents which happened in the district #10
Accidents.Sup.DF <- Accidents.DF %>% filter(CodiDistricte == "10")

## Filter 2 -- take only registers in the range of 5 sept 2015 to 4 sept 2017
# Create a new column with the date (dd/mm/yyyy) and filter :: -1 year <-- Superilla (05/sept/2016) --> +1 year
Accidents.Sup.DF <- Accidents.Sup.DF %>% mutate(DateAcc = as.Date(paste(DiaMes, MesAny, Any),format='%d %m %Y')) %>% 
  filter(DateAcc >= "2015-09-05" & DateAcc <= "2017-09-04") %>% filter(!is.na(CoordenadaUTM_X))

# Determine if it happened before or after the Superilla implementation
Accidents.Sup.DF <- Accidents.Sup.DF %>% mutate(RelativeTime = ifelse(DateAcc >= "2016-09-05","After","Before"))
Accidents.Sup.DF$RelativeTime <- as.factor(Accidents.Sup.DF$RelativeTime)


#### Shiny App :: Create a compelling, interactive map that covers the area under study ####
## Filter by "after", "before" or "all" the Accidents.Sup.DF dataframe
library("shiny")
library("leaflet")

## Transform Coordinates
library("rgdal")

# prepare UTM coordinates matrix
utmcoor <- SpatialPoints(cbind(LON = Accidents.Sup.DF$CoordenadaUTM_X,LAT = Accidents.Sup.DF$CoordenadaUTM_Y), 
                         proj4string = CRS("+proj=utm +zone=31"))

#utmdata$X and utmdata$Y are corresponding to UTM Easting and Northing, respectively.
#zone= UTM zone
# converting
longlatcoor <- spTransform(utmcoor,CRS("+proj=longlat"))

Accidents.Sup.DF$LAT <- longlatcoor@coords[,2]
Accidents.Sup.DF$LON <- longlatcoor@coords[,1]

## Defining a color palette to display accidents in the map according the relative time
relative.date.palette <- colorFactor(c("#7daf4b","#644baf"), Accidents.Sup.DF$RelativeTime)


#### Shiny - User Interface ####
ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10, style="background-color:#fcfcdc;padding: 0 20px 20px 20 px; opacity:0.85",
                radioButtons("relativeDateId", "When did the accident happen",
                             choices = list("All" = "All", 
                                            "Before the Superilla" = "Before", 
                                            "After the Superilla" = "After"), 
                             selected = "All")
  )
)

#### Shiny - Server ####
server <- function(input, output, session) {
  
  # Reactive expression for the data subsetted to what the user selected
  filteredData <- reactive({
    if(input$relativeDateId=="Before"){
      Accidents.Sup.DF[Accidents.Sup.DF$RelativeTime=="Before",]
    } else if(input$relativeDateId=="After"){
      Accidents.Sup.DF[Accidents.Sup.DF$RelativeTime=="After",]
    } else {
      Accidents.Sup.DF
    }
  })
  
  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    
    leaflet(Accidents.Sup.DF) %>% addTiles() %>%
      fitBounds(~min(LON), ~min(LAT), ~max(LON), ~max(LAT))
    
  })
  
  observe({
    
    leafletProxy("map", data = filteredData()) %>%
      clearShapes() %>%
      addCircles(radius = 5, weight = 10, color = ~relative.date.palette(RelativeTime), 
                 fillColor="#556565", fillOpacity = 0.5)
    
  })
  
}

## Run the ShinyApp & Have Fun!!
shinyApp(ui, server)
