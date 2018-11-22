### FINAL PROJECT at UBIQUM -- PEDESTRIAN SAFETY IN BCN ####

## Importing Data into the Work Environment
library("readr")

# Incidents
Incidents17_DS <- read_csv("./Datasets/2017_incidents_gestionats_gub.csv", na = "NA")
Incidents16_DS <- read_csv("./Datasets/2016_incidents_gestionats_gub.csv", na = "NA")
Incidents15_DS <- read_csv("./Datasets/2015_incidents_gestionats_gub.csv", na = "NA")
Incidents14_DS <- read_csv("./Datasets/2014_incidents_gestionats_gub.csv", na = "NA")

# Accidents
Accidents17_DS <- read_csv("./Datasets/2017_accidents_gu_bcn.csv", na = "NA")
Accidents16_DS <- read_csv("./Datasets/2016_accidents_gu_bcn.csv", na = "NA")
Accidents15_DS <- read_delim("./Datasets/2015_accidents_gu_bcn.csv", ";", escape_double = FALSE, 
                             locale = locale(decimal_mark = ","), na = "NA", trim_ws = TRUE)
Accidents14_DS <- read_csv("./Datasets/2014_accidents_gu_bcn.csv", na = "NA")

# Accidents/Persones
AccPers17_DS <- read_csv("./Datasets/2017_accidents_persones_gu_bcn.csv", na = "NA")
AccPers16_DS <- read_csv("./Datasets/2016_accidents_persones_gu_bcn.csv", na = "NA")
AccPers15_DS <- read_csv("./Datasets/2015_accidents_persones_gu_bcn.csv", na = "NA")
AccPers14_DS <- read_csv("./Datasets/2014_accidents_persones_gu_bcn.csv", na = "NA")

#### 1. Checking Data Consistency between the datasets ####
## Incidents dataset :: 210=Accidents with no injured people -- 211=Accidents with injured people
sum(Incidents17_DS[(which(Incidents17_DS$CodiIncident=="210")),]$NumeroIncidentsGUB,na.rm = TRUE)
sum(Incidents17_DS[(which(Incidents17_DS$CodiIncident=="211")),]$NumeroIncidentsGUB,na.rm = TRUE)

sum(Incidents16_DS[(which(Incidents16_DS$CodiIncident=="210")),]$NumeroIncidentsGUB,na.rm = TRUE)
sum(Incidents16_DS[(which(Incidents16_DS$CodiIncident=="211")),]$NumeroIncidentsGUB,na.rm = TRUE)

sum(Incidents15_DS[(which(Incidents15_DS$CodiIncident=="210")),]$NumeroIncidentsGUB,na.rm = TRUE)
sum(Incidents15_DS[(which(Incidents15_DS$CodiIncident=="211")),]$NumeroIncidentsGUB,na.rm = TRUE)

sum(Incidents14_DS[(which(Incidents14_DS$CodiIncident=="210")),]$NumeroIncidentsGUB,na.rm = TRUE)
sum(Incidents14_DS[(which(Incidents14_DS$CodiIncident=="211")),]$NumeroIncidentsGUB,na.rm = TRUE)

## Accidents dataset
length(which(Accidents17_DS$NumeroVictimes==0))
length(which(Accidents17_DS$NumeroVictimes>0))
length(unique(Accidents17_DS[which(Accidents17_DS$NumeroVictimes>0),]$NumeroExpedient))

length(which(Accidents16_DS$NumeroVictimes==0))
length(which(Accidents16_DS$NumeroVictimes>0))
length(unique(Accidents16_DS[which(Accidents16_DS$NumeroVictimes>0),]$NumeroExpedient))

length(which(Accidents15_DS$NumeroVictimes==0))
length(which(Accidents15_DS$NumeroVictimes>0))
length(unique(Accidents15_DS[which(Accidents15_DS$NumeroVictimes>0),]$NumeroExpedient))

length(which(Accidents14_DS$NumeroVictimes==0))
length(which(Accidents14_DS$NumeroVictimes>0))
length(unique(Accidents14_DS[which(Accidents14_DS$NumeroVictimes>0),]$NumeroExpedient))

## AccPers dataset
length(unique(AccPers17_DS$NumeroExpedient))
length(unique(AccPers16_DS$NumeroExpedient))
length(unique(AccPers15_DS$NumeroExpedient))
length(unique(AccPers14_DS$NumeroExpedient))

## Find out duplicated CodiExpedient at Accidents Dataset
which(duplicated(Accidents17_DS[which(Accidents17_DS$NumeroVictimes>0),]$NumeroExpedient))
Accidents17_DS[c(which(duplicated(Accidents17_DS[which(Accidents17_DS$NumeroVictimes>0),]$NumeroExpedient))), ]$NumeroExpedient

## According with the obtained results, information in the incidents dataset won't be considered
## There is not consistency between the two datasets regarding accidents

#### 2. FIRST FEATURE SELECTION AND DATA TYPES DEFINITION ####
## Accidents Dataset
# 2017
Accidents17_FSDS <- Accidents17_DS

Accidents17_FSDS$NomDistricte <- NULL
Accidents17_FSDS$CodiBarri <- NULL
Accidents17_FSDS$NomBarri <- NULL
Accidents17_FSDS$CodiCarrer <- NULL
Accidents17_FSDS$NomCarrer <- NULL
Accidents17_FSDS$NumPostalCaption <- NULL
Accidents17_FSDS$DescripcioDiaSetmana <- NULL
Accidents17_FSDS$DescripcioTipusDia <- NULL
Accidents17_FSDS$DescripcioTorn <- NULL
Accidents17_FSDS$DescripcioCausaVianant <- NULL
Accidents17_FSDS$X25 <- NULL
Accidents17_FSDS$X27 <- NULL
Accidents17_FSDS$Longitud <- NULL
Accidents17_FSDS$Latitud <- NULL

Accidents17_FSDS$CodiDistricte <- as.factor(Accidents17_FSDS$CodiDistricte)
Accidents17_FSDS$DiaSetmana <- as.factor(Accidents17_FSDS$DiaSetmana)
Accidents17_FSDS$DiaSetmana <- ordered(Accidents17_FSDS$DiaSetmana, levels = c("Dl","Dm", "Dc", "Dj", "Dv", "Ds", "Dg"))
Accidents17_FSDS$HoraDia <- as.integer(Accidents17_FSDS$HoraDia)

# 2016
Accidents16_FSDS <- Accidents16_DS

Accidents16_FSDS$NomDistricte <- NULL
Accidents16_FSDS$CodiBarri <- NULL
Accidents16_FSDS$NomBarri <- NULL
Accidents16_FSDS$CodiCarrer <- NULL
Accidents16_FSDS$NomCarrer <- NULL
Accidents16_FSDS$NumPostalCaption <- NULL
Accidents16_FSDS$DescripcioDiaSetmana <- NULL
Accidents16_FSDS$DescripcioTipusDia <- NULL
Accidents16_FSDS$DescripcioTorn <- NULL
Accidents16_FSDS$DescripcioCausaVianant <- NULL
Accidents16_FSDS$Longitud <- NULL
Accidents16_FSDS$Latitud <- NULL

Accidents16_FSDS$CodiDistricte <- as.factor(Accidents16_FSDS$CodiDistricte)
Accidents16_FSDS$DiaSetmana <- as.factor(Accidents16_FSDS$DiaSetmana)
Accidents16_FSDS$DiaSetmana <- ordered(Accidents16_FSDS$DiaSetmana, levels = c("Dl","Dm", "Dc", "Dj", "Dv", "Ds", "Dg"))
Accidents16_FSDS$HoraDia <- as.integer(Accidents16_FSDS$HoraDia)
Accidents16_FSDS$CoordenadaUTM_X <- as.integer(round(Accidents16_FSDS$CoordenadaUTM_X,digits = 0))
Accidents16_FSDS$CoordenadaUTM_Y <- as.integer(round(Accidents16_FSDS$CoordenadaUTM_Y,digits = 0))

# 2015
Accidents15_FSDS <- Accidents15_DS

Accidents15_FSDS$NomDistricte <- NULL
Accidents15_FSDS$CodiBarri <- NULL
Accidents15_FSDS$NomBarri <- NULL
Accidents15_FSDS$CodiCarrer <- NULL
Accidents15_FSDS$NomCarrer <- NULL
Accidents15_FSDS$NumPostalCaption <- NULL
Accidents15_FSDS$DescripcioDiaSetmana <- NULL
Accidents15_FSDS$DescripcioTipusDia <- NULL
Accidents15_FSDS$DescripcioTorn <- NULL
Accidents15_FSDS$DescripcioCausaVianant <- NULL

Accidents15_FSDS$CodiDistricte <- as.factor(Accidents15_FSDS$CodiDistricte)
Accidents15_FSDS$DiaSetmana <- as.factor(Accidents15_FSDS$DiaSetmana)
Accidents15_FSDS$DiaSetmana <- ordered(Accidents15_FSDS$DiaSetmana, levels = c("Dl","Dm", "Dc", "Dj", "Dv", "Ds", "Dg"))
Accidents15_FSDS$HoraDia <- as.integer(Accidents15_FSDS$HoraDia)
Accidents15_FSDS$CoordenadaUTM_X <- as.integer(round(Accidents15_FSDS$CoordenadaUTM_X,digits = 0))
Accidents15_FSDS$CoordenadaUTM_Y <- as.integer(round(Accidents15_FSDS$CoordenadaUTM_Y,digits = 0))

Accidents15_FSDS <- Accidents15_FSDS[,c(1:13,15,14)]

# 2014
Accidents14_FSDS <- Accidents14_DS

Accidents14_FSDS$NomDistricte <- NULL
Accidents14_FSDS$CodiBarri <- NULL
Accidents14_FSDS$NomBarri <- NULL
Accidents14_FSDS$CodiCarrer <- NULL
Accidents14_FSDS$NomCarrer <- NULL
Accidents14_FSDS$NumPostalCaption <- NULL
Accidents14_FSDS$DescripcioDiaSetmana <- NULL
Accidents14_FSDS$DescripcioTipusDia <- NULL
Accidents14_FSDS$DescripcioTorn <- NULL
Accidents14_FSDS$DescripcioCausaVianant <- NULL
Accidents14_FSDS$X25 <- NULL
Accidents14_FSDS$X27 <- NULL

Accidents14_FSDS$CodiDistricte <- as.factor(Accidents14_FSDS$CodiDistricte)
Accidents14_FSDS$DiaSetmana <- as.factor(Accidents14_FSDS$DiaSetmana)
levels(Accidents14_FSDS$DiaSetmana) <- c("Dl","Dm","Dc","Dj","Dv","Ds","Dg")
Accidents14_FSDS$HoraDia <- as.integer(Accidents14_FSDS$HoraDia)

Accidents14_FSDS <- Accidents14_FSDS[,c(1:13,15,14)]

# Join all together & create a backup csv file
Accidents_DF <- rbind(Accidents14_FSDS,Accidents15_FSDS,Accidents16_FSDS,Accidents17_FSDS)
write.csv(Accidents_DF,"./WorkingData/accidents.csv",row.names = FALSE)

## AccPers Dataset
#2017
AccPers17_FSDS <- AccPers17_DS

AccPers17_FSDS$NomDistricte <- NULL
AccPers17_FSDS$CodiBarri <- NULL
AccPers17_FSDS$NomBarri <- NULL
AccPers17_FSDS$CodiCarrer <- NULL
AccPers17_FSDS$NomCarrer <- NULL
AccPers17_FSDS$NumPostal <- NULL
AccPers17_FSDS$DescripcioDiaSetmana <- NULL
AccPers17_FSDS$DescripcioTipusDia <- NULL
AccPers17_FSDS$DescripcioTorn <- NULL
AccPers17_FSDS$DescripcioSituacio <- NULL
AccPers17_FSDS$X26 <- NULL
AccPers17_FSDS$X28 <- NULL
AccPers17_FSDS$Long <- NULL
AccPers17_FSDS$Lat <- NULL

AccPers17_FSDS$CodiDistricte <- as.factor(AccPers17_FSDS$CodiDistricte)
AccPers17_FSDS$DiaSetmana <- as.factor(AccPers17_FSDS$DiaSetmana)
AccPers17_FSDS$DiaSetmana <- ordered(AccPers17_FSDS$DiaSetmana, levels = c("Dl","Dm", "Dc", "Dj", "Dv", "Ds", "Dg"))
AccPers17_FSDS$HoraDia <- as.integer(AccPers17_FSDS$HoraDia)
AccPers17_FSDS$DescripcioCausaVianant <- as.factor(AccPers17_FSDS$DescripcioCausaVianant)
levels(AccPers17_FSDS$DescripcioCausaVianant) <- c("Altres","CreuaForaPas","DsbSenyals","DsbSemafor","No","TrnPeuCalcada")
AccPers17_FSDS$DescripcioSexe <- as.factor(AccPers17_FSDS$DescripcioSexe)
AccPers17_FSDS$DescripcioTipusPersona <- as.factor(AccPers17_FSDS$DescripcioTipusPersona)

AccPers17_FSDS$DescripcioVictimitzacio[AccPers17_FSDS$DescripcioVictimitzacio=="Ferit greu"] <- "Greu"
AccPers17_FSDS$DescripcioVictimitzacio[AccPers17_FSDS$DescripcioVictimitzacio=="Ferit greu: hospitalitzacio superior a 24h"] <- "Greu"
AccPers17_FSDS$DescripcioVictimitzacio[AccPers17_FSDS$DescripcioVictimitzacio=="Ferit lleu"] <- "Lleu"
AccPers17_FSDS$DescripcioVictimitzacio[AccPers17_FSDS$DescripcioVictimitzacio=="Ferit lleu: Amb assistencia sanitaria en lloc de accident"] <- "Lleu"
AccPers17_FSDS$DescripcioVictimitzacio[AccPers17_FSDS$DescripcioVictimitzacio=="Ferit lleu: Hospitalitzacio fins a 24h"] <- "Lleu"
AccPers17_FSDS$DescripcioVictimitzacio[AccPers17_FSDS$DescripcioVictimitzacio=="Ferit lleu: Rebutja assistencia sanitaria"] <- "Lleu"
AccPers17_FSDS$DescripcioVictimitzacio[AccPers17_FSDS$DescripcioVictimitzacio=="Mort dins 24h posteriors accident"] <- "Mort"
AccPers17_FSDS$DescripcioVictimitzacio <- as.factor(AccPers17_FSDS$DescripcioVictimitzacio)

AccPers17_FSDS$TipusVehicleImplicat[AccPers17_FSDS$TipusVehicleImplicat=="Altres vehicles amb motor"] <- "AltresVehMotor"
AccPers17_FSDS$TipusVehicleImplicat[AccPers17_FSDS$TipusVehicleImplicat=="Otros vehic. a motor"] <- "AltresVehMotor"
AccPers17_FSDS$TipusVehicleImplicat[AccPers17_FSDS$TipusVehicleImplicat=="Altres vehicles sense motor"] <- "AltresVehSenseMotor"
AccPers17_FSDS$TipusVehicleImplicat[AccPers17_FSDS$TipusVehicleImplicat=="Autobus"] <- "Bus"
AccPers17_FSDS$TipusVehicleImplicat[AccPers17_FSDS$TipusVehicleImplicat=="Autobus articulado"] <- "Bus"
AccPers17_FSDS$TipusVehicleImplicat[AccPers17_FSDS$TipusVehicleImplicat=="Autobus articulat"] <- "Bus"
AccPers17_FSDS$TipusVehicleImplicat[AccPers17_FSDS$TipusVehicleImplicat=="Camion <= 3.5Tm"] <- "CamioLess35Tm"
AccPers17_FSDS$TipusVehicleImplicat[AccPers17_FSDS$TipusVehicleImplicat=="Camio rigid <= 3.5tones"] <- "CamioLess35Tm"
AccPers17_FSDS$TipusVehicleImplicat[AccPers17_FSDS$TipusVehicleImplicat=="Camion > 3.5Tm"] <- "CamioMore35Tm"
AccPers17_FSDS$TipusVehicleImplicat[AccPers17_FSDS$TipusVehicleImplicat=="Camio rigid > 3.5tones"] <- "CamioMore35Tm"
AccPers17_FSDS$TipusVehicleImplicat[AccPers17_FSDS$TipusVehicleImplicat=="Cuadriciclo <75cc"] <- "QuadLess75cc"
AccPers17_FSDS$TipusVehicleImplicat[AccPers17_FSDS$TipusVehicleImplicat=="Quadricicle < 75 cc"] <- "QuadLess75cc"
AccPers17_FSDS$TipusVehicleImplicat[AccPers17_FSDS$TipusVehicleImplicat=="Quadricicle > 75 cc"] <- "QuadMore75cc"
AccPers17_FSDS$TipusVehicleImplicat[AccPers17_FSDS$TipusVehicleImplicat=="Maquinaria de obres i serveis"] <- "MaqObresServ"
AccPers17_FSDS$TipusVehicleImplicat[AccPers17_FSDS$TipusVehicleImplicat=="Microbus <= 17"] <- "Microbus"
AccPers17_FSDS$TipusVehicleImplicat[AccPers17_FSDS$TipusVehicleImplicat=="Todo terreno"] <- "TotTerreny"
AccPers17_FSDS$TipusVehicleImplicat[AccPers17_FSDS$TipusVehicleImplicat=="Tot terreny"] <- "TotTerreny"
AccPers17_FSDS$TipusVehicleImplicat[AccPers17_FSDS$TipusVehicleImplicat=="Tractocamion"] <- "Tractocamio"
AccPers17_FSDS$TipusVehicleImplicat[AccPers17_FSDS$TipusVehicleImplicat=="Tractor camio"] <- "Tractocamio"
AccPers17_FSDS$TipusVehicleImplicat[AccPers17_FSDS$TipusVehicleImplicat=="Tranvia o tren"] <- "Tramvia"
AccPers17_FSDS$TipusVehicleImplicat[AccPers17_FSDS$TipusVehicleImplicat=="Tren o tramvia"] <- "Tramvia"
AccPers17_FSDS$TipusVehicleImplicat[AccPers17_FSDS$TipusVehicleImplicat=="Turismo"] <- "Turisme"
AccPers17_FSDS$TipusVehicleImplicat <- as.factor(AccPers17_FSDS$TipusVehicleImplicat)

# 2016
AccPers16_FSDS <- AccPers16_DS

AccPers16_FSDS$NomDistricte <- NULL
AccPers16_FSDS$CodiBarri <- NULL
AccPers16_FSDS$NomBarri <- NULL
AccPers16_FSDS$CodiCarrer <- NULL
AccPers16_FSDS$NomCarrer <- NULL
AccPers16_FSDS$NumPostal <- NULL
AccPers16_FSDS$DescripcioDiaSetmana <- NULL
AccPers16_FSDS$DescripcioTipusDia <- NULL
AccPers16_FSDS$DescripcioTorn <- NULL
AccPers16_FSDS$DescripcioSituacio <- NULL
AccPers16_FSDS$X26 <- NULL
AccPers16_FSDS$X28 <- NULL
AccPers16_FSDS$Long <- NULL
AccPers16_FSDS$Lat <- NULL
AccPers16_FSDS$X30 <- NULL
AccPers16_FSDS$X32 <- NULL

AccPers16_FSDS$CodiDistricte <- as.factor(AccPers16_FSDS$CodiDistricte)
AccPers16_FSDS$DiaSetmana <- as.factor(AccPers16_FSDS$DiaSetmana)
AccPers16_FSDS$DiaSetmana <- ordered(AccPers16_FSDS$DiaSetmana, levels = c("Dl","Dm", "Dc", "Dj", "Dv", "Ds", "Dg"))
AccPers16_FSDS$HoraDia <- as.integer(AccPers16_FSDS$HoraDia)
AccPers16_FSDS$DescripcioCausaVianant <- as.factor(AccPers16_FSDS$DescripcioCausaVianant)
levels(AccPers16_FSDS$DescripcioCausaVianant) <- c("Altres","CreuaForaPas","DsbSenyals","DsbSemafor","No","TrnPeuCalcada")
AccPers16_FSDS$DescripcioSexe <- as.factor(AccPers16_FSDS$DescripcioSexe)
AccPers16_FSDS$DescripcioTipusPersona <- as.factor(AccPers16_FSDS$DescripcioTipusPersona)
AccPers16_FSDS$DescripcioVictimitzacio <- as.factor(AccPers16_FSDS$DescripcioVictimitzacio)
levels(AccPers16_FSDS$DescripcioVictimitzacio) <- c("Greu","Lleu","Mort")

AccPers16_FSDS$TipusVehicleImplicat[AccPers16_FSDS$TipusVehicleImplicat=="Autobus"] <- "Bus"
AccPers16_FSDS$TipusVehicleImplicat[AccPers16_FSDS$TipusVehicleImplicat=="Autobus articulado"] <- "Bus"
AccPers16_FSDS$TipusVehicleImplicat[AccPers16_FSDS$TipusVehicleImplicat=="Camion <= 3.5Tm"] <- "CamioLess35Tm"
AccPers16_FSDS$TipusVehicleImplicat[AccPers16_FSDS$TipusVehicleImplicat=="Camion > 3.5Tm"] <- "CamioMore35Tm"
AccPers16_FSDS$TipusVehicleImplicat[AccPers16_FSDS$TipusVehicleImplicat=="Cuadriciclo <75cc"] <- "QuadLess75cc"
AccPers16_FSDS$TipusVehicleImplicat[AccPers16_FSDS$TipusVehicleImplicat=="Cuadriciclo >=75cc"] <- "QuadMore75cc"
AccPers16_FSDS$TipusVehicleImplicat[AccPers16_FSDS$TipusVehicleImplicat=="Maquinaria de obras"] <- "MaqObresServ"
AccPers16_FSDS$TipusVehicleImplicat[AccPers16_FSDS$TipusVehicleImplicat=="Otros vehic. a motor"] <- "AltresVehMotor"
AccPers16_FSDS$TipusVehicleImplicat[AccPers16_FSDS$TipusVehicleImplicat=="Todo terreno"] <- "TotTerreny"
AccPers16_FSDS$TipusVehicleImplicat[AccPers16_FSDS$TipusVehicleImplicat=="Tractocamion"] <- "Tractocamio"
AccPers16_FSDS$TipusVehicleImplicat[AccPers16_FSDS$TipusVehicleImplicat=="Tranvia o tren"] <- "Tramvia"
AccPers16_FSDS$TipusVehicleImplicat[AccPers16_FSDS$TipusVehicleImplicat=="Turismo"] <- "Turisme"
AccPers16_FSDS$TipusVehicleImplicat <- as.factor(AccPers16_FSDS$TipusVehicleImplicat)

# 2015
AccPers15_FSDS <- AccPers15_DS

AccPers15_FSDS$NomDistricte <- NULL
AccPers15_FSDS$CodiBarri <- NULL
AccPers15_FSDS$NomBarri <- NULL
AccPers15_FSDS$CodiCarrer <- NULL
AccPers15_FSDS$NomCarrer <- NULL
AccPers15_FSDS$NumPostal <- NULL
AccPers15_FSDS$DescripcioDiaSetmana <- NULL
AccPers15_FSDS$DescripcioTipusDia <- NULL
AccPers15_FSDS$X24 <- NULL
AccPers15_FSDS$X26 <- NULL

AccPers15_FSDS$CodiDistricte <- as.factor(AccPers15_FSDS$CodiDistricte)
AccPers15_FSDS$DiaSetmana <- as.factor(AccPers15_FSDS$DiaSetmana)
AccPers15_FSDS$DiaSetmana <- ordered(AccPers15_FSDS$DiaSetmana, levels = c("Dl","Dm", "Dc", "Dj", "Dv", "Ds", "Dg"))
AccPers15_FSDS$HoraDia <- as.integer(AccPers15_FSDS$HoraDia)
AccPers15_FSDS$DescripcioCausaVianant <- as.factor(AccPers15_FSDS$DescripcioCausaVianant)
levels(AccPers15_FSDS$DescripcioCausaVianant) <- c("Altres","CreuaForaPas","DsbSenyals","DsbSemafor","No","TrnPeuCalcada")
AccPers15_FSDS$DescripcioSexe <- as.factor(AccPers15_FSDS$DescripcioSexe)
AccPers15_FSDS$DescripcioTipusPersona <- as.factor(AccPers15_FSDS$DescripcioTipusPersona)
AccPers15_FSDS$DescripcioVictimitzacio <- as.factor(AccPers15_FSDS$DescripcioVictimitzacio)
levels(AccPers15_FSDS$DescripcioVictimitzacio) <- c("Greu","Lleu","Mort")

AccPers15_FSDS$TipusVehicleImplicat[AccPers15_FSDS$TipusVehicleImplicat=="Autobus"] <- "Bus"
AccPers15_FSDS$TipusVehicleImplicat[AccPers15_FSDS$TipusVehicleImplicat=="Autobus articulado"] <- "Bus"
AccPers15_FSDS$TipusVehicleImplicat[AccPers15_FSDS$TipusVehicleImplicat=="Camion <= 3.5Tm"] <- "CamioLess35Tm"
AccPers15_FSDS$TipusVehicleImplicat[AccPers15_FSDS$TipusVehicleImplicat=="Camion > 3.5Tm"] <- "CamioMore35Tm"
AccPers15_FSDS$TipusVehicleImplicat[AccPers15_FSDS$TipusVehicleImplicat=="Cuadriciclo <75cc"] <- "QuadLess75cc"
AccPers15_FSDS$TipusVehicleImplicat[AccPers15_FSDS$TipusVehicleImplicat=="Cuadriciclo >=75cc"] <- "QuadMore75cc"
AccPers15_FSDS$TipusVehicleImplicat[AccPers15_FSDS$TipusVehicleImplicat=="Maquinaria de obras"] <- "MaqObresServ"
AccPers15_FSDS$TipusVehicleImplicat[AccPers15_FSDS$TipusVehicleImplicat=="Microbus <=17 plazas"] <- "Microbus"
AccPers15_FSDS$TipusVehicleImplicat[AccPers15_FSDS$TipusVehicleImplicat=="Otros vehic. a motor"] <- "AltresVehMotor"
AccPers15_FSDS$TipusVehicleImplicat[AccPers15_FSDS$TipusVehicleImplicat=="Todo terreno"] <- "TotTerreny"
AccPers15_FSDS$TipusVehicleImplicat[AccPers15_FSDS$TipusVehicleImplicat=="Tractocamion"] <- "Tractocamio"
AccPers15_FSDS$TipusVehicleImplicat[AccPers15_FSDS$TipusVehicleImplicat=="Tranvia o tren"] <- "Tramvia"
AccPers15_FSDS$TipusVehicleImplicat[AccPers15_FSDS$TipusVehicleImplicat=="Turismo"] <- "Turisme"
AccPers15_FSDS$TipusVehicleImplicat <- as.factor(AccPers15_FSDS$TipusVehicleImplicat)

# 2014
AccPers14_FSDS <- AccPers14_DS

AccPers14_FSDS$NomDistricte <- NULL
AccPers14_FSDS$CodiBarri <- NULL
AccPers14_FSDS$NomBarri <- NULL
AccPers14_FSDS$CodiCarrer <- NULL
AccPers14_FSDS$NomCarrer <- NULL
AccPers14_FSDS$NumPostal <- NULL
AccPers14_FSDS$DescripcioDiaSetmana <- NULL
AccPers14_FSDS$DescripcioTipusDia <- NULL
AccPers14_FSDS$X24 <- NULL
AccPers14_FSDS$X26 <- NULL

AccPers14_FSDS$CodiDistricte <- as.factor(AccPers14_FSDS$CodiDistricte)
AccPers14_FSDS$DiaSetmana <- as.factor(AccPers14_FSDS$DiaSetmana)
AccPers14_FSDS$DiaSetmana <- ordered(AccPers14_FSDS$DiaSetmana, levels = c("Dl","Dm", "Dc", "Dj", "Dv", "Ds", "Dg"))
AccPers14_FSDS$HoraDia <- as.integer(AccPers14_FSDS$HoraDia)
AccPers14_FSDS$DescripcioCausaVianant <- as.factor(AccPers14_FSDS$DescripcioCausaVianant)
levels(AccPers14_FSDS$DescripcioCausaVianant) <- c("Altres","CreuaForaPas","DsbSenyals","DsbSemafor","No","TrnPeuCalcada")
AccPers14_FSDS$DescripcioSexe <- as.factor(AccPers14_FSDS$DescripcioSexe)
AccPers14_FSDS$DescripcioTipusPersona <- as.factor(AccPers14_FSDS$DescripcioTipusPersona)
AccPers14_FSDS$DescripcioVictimitzacio <- as.factor(AccPers14_FSDS$DescripcioVictimitzacio)
levels(AccPers14_FSDS$DescripcioVictimitzacio) <- c("Greu","Lleu","Mort")

AccPers14_FSDS$TipusVehicleImplicat[AccPers14_FSDS$TipusVehicleImplicat=="Autobus"] <- "Bus"
AccPers14_FSDS$TipusVehicleImplicat[AccPers14_FSDS$TipusVehicleImplicat=="Autobus articulado"] <- "Bus"
AccPers14_FSDS$TipusVehicleImplicat[AccPers14_FSDS$TipusVehicleImplicat=="Camion <= 3.5Tm"] <- "CamioLess35Tm"
AccPers14_FSDS$TipusVehicleImplicat[AccPers14_FSDS$TipusVehicleImplicat=="Camion > 3.5Tm"] <- "CamioMore35Tm"
AccPers14_FSDS$TipusVehicleImplicat[AccPers14_FSDS$TipusVehicleImplicat=="Cuadriciclo <75cc"] <- "QuadLess75cc"
AccPers14_FSDS$TipusVehicleImplicat[AccPers14_FSDS$TipusVehicleImplicat=="Cuadriciclo >=75cc"] <- "QuadMore75cc"
AccPers14_FSDS$TipusVehicleImplicat[AccPers14_FSDS$TipusVehicleImplicat=="Maquinaria de obras"] <- "MaqObresServ"
AccPers14_FSDS$TipusVehicleImplicat[AccPers14_FSDS$TipusVehicleImplicat=="Otros vehic. a motor"] <- "AltresVehMotor"
AccPers14_FSDS$TipusVehicleImplicat[AccPers14_FSDS$TipusVehicleImplicat=="Todo terreno"] <- "TotTerreny"
AccPers14_FSDS$TipusVehicleImplicat[AccPers14_FSDS$TipusVehicleImplicat=="Tractocamion"] <- "Tractocamio"
AccPers14_FSDS$TipusVehicleImplicat[AccPers14_FSDS$TipusVehicleImplicat=="Tranvia o tren"] <- "Tramvia"
AccPers14_FSDS$TipusVehicleImplicat[AccPers14_FSDS$TipusVehicleImplicat=="Turismo"] <- "Turisme"
AccPers14_FSDS$TipusVehicleImplicat <- as.factor(AccPers14_FSDS$TipusVehicleImplicat)

# Join all together & create a backup csv file
AccPers_DF <- rbind(AccPers14_FSDS,AccPers15_FSDS,AccPers16_FSDS,AccPers17_FSDS)
write.csv(AccPers_DF,"./WorkingData/accidents_persona.csv",row.names = FALSE)

# Create a subset only for pedestrians & create a csv file
AccPersVian_DF <- AccPers_DF[which(AccPers_DF$DescripcioTipusPersona=="Vianant"),]
write.csv(AccPersVian_DF,"./WorkingData/accidents_vianants.csv",row.names = FALSE)
