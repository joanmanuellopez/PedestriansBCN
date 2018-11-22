#### 2. FIRST INSIGHTS ON DATA ####
library("readr")
library("ggplot2", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.0")

## Load the dataframes from stored information
AccPersVian_DF <- read_csv("./WorkingData/accidents_vianants.csv")

# Assign the suitable dtatypes at each attribute
AccPersVian_DF$CodiDistricte <- as.factor(AccPersVian_DF$CodiDistricte)
AccPersVian_DF$DiaSetmana <- as.factor(AccPersVian_DF$DiaSetmana)
AccPersVian_DF$DiaSetmana <- ordered(AccPersVian_DF$DiaSetmana, levels = c("Dl","Dm", "Dc", "Dj", "Dv", "Ds", "Dg"))
AccPersVian_DF$DescripcioCausaVianant <- as.factor(AccPersVian_DF$DescripcioCausaVianant)
AccPersVian_DF$TipusVehicleImplicat <- as.factor(AccPersVian_DF$TipusVehicleImplicat)
AccPersVian_DF$DescripcioSexe <- as.factor(AccPersVian_DF$DescripcioSexe)
AccPersVian_DF$DescripcioTipusPersona <- as.factor(AccPersVian_DF$DescripcioTipusPersona)
AccPersVian_DF$DescripcioVictimitzacio <- as.factor(AccPersVian_DF$DescripcioVictimitzacio)


#### Visualization of plots regarding each parameter ####
## Create an alternative dataframe to work with
APVPlots_DF <- AccPersVian_DF
## Establish a customized palette for the three levels of severity
damagePalette <- c("#acafb9","#718093","#2f3640")

# Arrange data to create better descriptive plots
APVPlots_DF$TipusVehicleImplicat <- as.character(APVPlots_DF$TipusVehicleImplicat)
APVPlots_DF$TipusVehicleImplicat[which(!(APVPlots_DF$TipusVehicleImplicat %in% c("Turisme","Motocicleta","Bicicleta","Taxi","Furgoneta","Ciclomotor","Bus")))] <- "Altres"
APVPlots_DF$TipusVehicleImplicat <- as.factor(APVPlots_DF$TipusVehicleImplicat)
APVPlots_DF$TipusVehicleImplicat <- ordered(APVPlots_DF$TipusVehicleImplicat, levels = c("Turisme","Motocicleta","Bicicleta","Taxi","Furgoneta","Ciclomotor","Bus","Altres"))

APVPlots_DF$DescripcioCausaVianant <- ordered(APVPlots_DF$DescripcioCausaVianant, levels = c("No","CreuaForaPas","DsbSemafor","TrnPeuCalcada","DsbSenyals","Altres"))
APVPlots_DF$DescripcioVictimitzacio <- ordered(APVPlots_DF$DescripcioVictimitzacio, levels = c("Lleu","Greu","Mort"))

APVPlots_DF$Edat <- cut(APVPlots_DF$Edat,c(0,3,12,25,50,70,Inf),include.lowest = TRUE,labels = c("0-3","4-12","13-25","26-50","51-70",">70"))

## WHAT?
ggplot(APVPlots_DF, aes(DescripcioVictimitzacio, fill=DescripcioVictimitzacio)) + geom_bar() + 
  scale_fill_manual(values = damagePalette) + 
  theme(legend.position="none", axis.title = element_blank())

## WHERE? -- Maps (below...)
ggplot(APVPlots_DF, aes(CodiDistricte, fill = APVPlots_DF$DescripcioVictimitzacio)) + geom_bar() +
  scale_fill_manual(values = damagePalette) + 
  theme(legend.title = element_blank(), axis.title = element_blank())

ggplot(APVPlots_DF, aes(CodiDistricte, fill = APVPlots_DF$DescripcioVictimitzacio)) + geom_bar(position="fill") +
  scale_fill_manual(values = damagePalette) + 
  theme(legend.title = element_blank(), axis.title = element_blank())

## WHY?
ggplot(APVPlots_DF[which(!(APVPlots_DF$DescripcioCausaVianant=="No")),], aes(DescripcioCausaVianant, fill=DescripcioVictimitzacio)) + geom_bar() +
  scale_fill_manual(values = damagePalette) + 
  scale_x_discrete(labels = c('OutPs','TrfLt','FootRd','Sig','Others')) +
  theme(legend.title = element_blank(), axis.title = element_blank())

ggplot(APVPlots_DF[which(!(APVPlots_DF$DescripcioCausaVianant=="No")),], aes(DescripcioCausaVianant, fill=DescripcioVictimitzacio)) + geom_bar(position="fill") +
  scale_fill_manual(values = damagePalette) + 
  scale_x_discrete(labels = c('OutPs','TrfLt','FootRd','Sig','Others')) +
  theme(legend.title = element_blank(), axis.title = element_blank())

summary(APVPlots_DF[which(APVPlots_DF$DescripcioCausaVianant=="CreuaForaPas"),]$DescripcioVictimitzacio)
summary(APVPlots_DF$DescripcioCausaVianant)

## WHEN -- Hour
ggplot(APVPlots_DF, aes(as.factor(HoraDia), fill = DescripcioVictimitzacio)) + geom_bar() +
  scale_fill_manual(values = damagePalette) + 
  theme(legend.title = element_blank(), axis.title = element_blank())

ggplot(APVPlots_DF, aes(as.factor(HoraDia), fill = DescripcioVictimitzacio)) + geom_bar(position="fill") +
  scale_fill_manual(values = damagePalette) + 
  theme(legend.title = element_blank(), axis.title = element_blank())

## WHEN -- Day Week
ggplot(APVPlots_DF, aes(DiaSetmana, fill = DescripcioVictimitzacio)) + geom_bar() +
  scale_fill_manual(values = damagePalette) + 
  theme(legend.title = element_blank(), axis.title = element_blank())

ggplot(APVPlots_DF, aes(DiaSetmana, fill = DescripcioVictimitzacio)) + geom_bar(position="fill") +
  scale_fill_manual(values = damagePalette) + 
  theme(legend.title = element_blank(), axis.title = element_blank())

## WHEN -- Compare distribution of daytime every dayweek
require(ggjoy)

weekdaysPalette <- c("#b1e7ce","#8adbb5","#63cf9d","#309c6a","#24754f","#c3493c","#752c24")

ggplot(APVPlots_DF, aes(x = HoraDia, y = DiaSetmana, fill=DiaSetmana)) +
  geom_joy(rel_min_height = 0.05 #quito outliers de la distrbucion
           , scale = 2 #controla la separacion de las categorias (por debajo de 1 no se tocan)
           , color = "#2f3542"
  ) +
  #theme_joy(grid = "#949591") + #quita el fondo y la grid
  scale_x_continuous(expand = c(0.01, 0)) +
  scale_y_discrete(expand = c(0.01, 0)) + #acorta distancia entre ejes y graphs
  scale_fill_manual(values = weekdaysPalette) + 
  theme(legend.title = element_blank(), axis.title = element_blank(), axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        panel.background = element_rect(fill = "#a4b0be",colour = "#a4b0be",size = 0.5, linetype = "solid"))

## HOW?
ggplot(APVPlots_DF, aes(TipusVehicleImplicat, fill=DescripcioVictimitzacio)) + geom_bar() + 
  scale_fill_manual(values = damagePalette) + 
  scale_x_discrete(labels = c('Car','Moto','Bike','Taxi','Van','Cyclo','Bus','Others')) +
  theme(legend.title = element_blank(), axis.title = element_blank())

ggplot(APVPlots_DF, aes(TipusVehicleImplicat, fill=DescripcioVictimitzacio)) + geom_bar(position="fill") + 
  scale_fill_manual(values = damagePalette) + 
  scale_x_discrete(labels = c('Car','Moto','Bike','Taxi','Van','Cyclo','Bus','Others')) +
  theme(legend.title = element_blank(), axis.title = element_blank())

summary(APVPlots_DF[which(APVPlots_DF$TipusVehicleImplicat=="Bicicleta"),]$DescripcioVictimitzacio)

## How: Let's see what happen with buses... Create a new dataframe only for buses
APVBuses <- APVPlots_DF[which(APVPlots_DF$TipusVehicleImplicat=="Bus"),]

## Why (buses)?
ggplot(APVBuses, aes(DescripcioCausaVianant, fill=DescripcioVictimitzacio)) + geom_bar() +
  scale_fill_manual(values = damagePalette) + 
  scale_x_discrete(labels = c('No','OutPs','TrfLt','FootRd','Others')) +
  theme(legend.title = element_blank(), axis.title = element_blank())

## When (buses)?
ggplot(APVBuses, aes(as.factor(HoraDia), fill = DescripcioVictimitzacio)) + geom_bar() +
  scale_fill_manual(values = damagePalette) + 
  theme(legend.title = element_blank(), axis.title = element_blank())

## when & why (buses)?
ggplot(APVBuses, aes(as.factor(HoraDia), fill = DescripcioCausaVianant)) + geom_bar(position = "fill") +
  #scale_fill_manual(values = damagePalette) + 
  theme(legend.title = element_blank(), axis.title = element_blank())

summary(APVBuses$DescripcioCausaVianant)

## who (buses)?
ggplot(APVBuses, aes(Edat, fill=DescripcioVictimitzacio)) + geom_bar(position = "fill") + 
  scale_fill_manual(values = damagePalette) + 
  theme(legend.title = element_blank(), axis.title = element_blank())

## WHO?
ggplot(APVPlots_DF, aes(Edat, fill=DescripcioVictimitzacio)) + geom_bar() + 
  scale_fill_manual(values = damagePalette) + 
  theme(legend.title = element_blank(), axis.title = element_blank())

ggplot(APVPlots_DF, aes(Edat, fill=DescripcioVictimitzacio)) + geom_bar(position = "fill") + 
  scale_fill_manual(values = damagePalette) + 
  theme(legend.title = element_blank(), axis.title = element_blank())

summary(APVPlots_DF[which(APVPlots_DF$Edat==">70"),]$DescripcioVictimitzacio)
summary(APVPlots_DF[which(APVPlots_DF$DescripcioSexe=="Home"),]$DescripcioVictimitzacio)

#### 2.2 WHERE? (The maps, previous step) -- As I want to analyze by District I need new data frame. ####
## As the data corresponds to a single year (2016) take only information for that year
require("dplyr")
Demo_BCN_16 <- read_csv("./Datasets/2016_superficie_dens.csv")
Accidents_DF <- read_csv("./WorkingData/accidents.csv")

AccDistrictes <- Demo_BCN_16 %>% group_by(CodiDistricte) %>% summarise(Superficie = sum(Superficie),Habitants = sum(Poblacio))
AccDistrictes$Superficie <- AccDistrictes$Superficie/100   #Conversion from ha to km2

AccDistrictes <- AccDistrictes %>% mutate(Densitat = Habitants/Superficie)

AccDistrictes_Tmp <- Accidents_DF %>% filter(Any==2016) %>% group_by(CodiDistricte) %>% summarise(Accidents=n())
AccDistrictes$Accidents <- AccDistrictes_Tmp %>% filter(!is.na(CodiDistricte)) %>% pull(Accidents)

AccDistrictes_Tmp <- AccPersVian_DF %>% filter(Any==2016) %>% group_by(CodiDistricte) %>% summarise(Vianants=n())
AccDistrictes$Vianants <- AccDistrictes_Tmp %>% filter(!is.na(CodiDistricte)) %>% pull(Vianants)

AccDistrictes_Tmp <- AccPersVian_DF %>% filter(Any==2016,DescripcioVictimitzacio=="Lleu") %>% group_by(CodiDistricte) %>% summarise(Lleu=n())
AccDistrictes$Lleus <- AccDistrictes_Tmp %>% filter(!is.na(CodiDistricte)) %>% pull(Lleu)

AccDistrictes_Tmp <- AccPersVian_DF %>% filter(Any==2016,DescripcioVictimitzacio=="Greu") %>% group_by(CodiDistricte) %>% summarise(Greu=n())
AccDistrictes$Greus <- AccDistrictes_Tmp %>% filter(!is.na(CodiDistricte)) %>% pull(Greu)

AccDistrictes_Tmp <- AccPersVian_DF %>% filter(Any==2016,DescripcioVictimitzacio=="Mort") %>% group_by(CodiDistricte) %>% summarise(Mort=n())
AccDistrictes$Morts <- AccDistrictes_Tmp %>% filter(!is.na(CodiDistricte)) %>% pull(Mort)

AccDistrictes <- AccDistrictes %>% mutate(VianSup = Vianants/Superficie, Vian10k = Vianants*10000/Habitants)

AccDistrictes$CodiDistricte <- as.factor(AccDistrictes$CodiDistricte)

AccDistrictes_Tmp <- AccPersVian_DF %>% filter(Any==2016,DescripcioCausaVianant=="No") %>% group_by(CodiDistricte) %>% summarise(NoInfr=n())
AccDistrictes$NoInfr <- AccDistrictes_Tmp %>% filter(!is.na(CodiDistricte)) %>% pull(NoInfr)

# Export to a new csv file
write.csv(AccDistrictes,"./WorkingData/accidents_districte.csv",row.names = FALSE)

#### 2.3 WHERE? Plotting the choropleth MAPS!!! ####
## Source: http://erre-que-erre-paco.blogspot.com.es/2012/03/el-mapa-de-barcelona.html

library("rgdal", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.0")
library("RColorBrewer", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.0")
library("classInt", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.0")

current.wd <- getwd()

# Change the wd to get the map
setwd("./Maps/BCN_DIVADM_ED50_SHP")
ogrInfo(dsn=".",layer="BCN_Districte_ED50_SHP")

map.bcn <- readOGR(".",layer="BCN_Districte_ED50_SHP")

## Let's create the dataframe to be plotted in the map
cuts <- 8
plot_pal <- brewer.pal(cuts, "RdPu")

plot_intvl <- classIntervals(1-(AccDistrictes$NoInfr/AccDistrictes$Vianants), cuts, style="pretty")
plot_colors <- findColours(plot_intvl, plot_pal)
plot(map.bcn, col=plot_colors, border="#2c3e50")

plot_lbls <- paste(round(100*plot_intvl$brks, 0), "%", sep="")[seq(2, length(plot_intvl$brks)-1)]
plot_lbls <- c(
  paste("less than", head(plot_lbls[1],1)),
  paste(head(plot_lbls,-1), "-", tail(plot_lbls,-1)),
  paste("more than", tail(plot_lbls,1))
)
legend("bottomleft",
       legend=plot_lbls, fill=attr(plot_colors, "palette"),
       bty="n", border=attr(plot_colors, "palette"), cex=0.5
)

# After finishing, let's make again the wd directory being the original one
setwd(current.wd)
