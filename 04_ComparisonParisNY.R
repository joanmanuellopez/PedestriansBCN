#### 4. PUT INTO CONTEXT : Compare with New York and Paris ####

## Data for New York City
NYC_Accidents_17 <- read_csv("./DataSets/2017_NYC_Collisions.csv",na = "NA")

# First feature selection
NYC_Accidents_17$BOROUGH <- NULL
NYC_Accidents_17$ZIPCODE <- NULL
NYC_Accidents_17$LATITUDE <- NULL
NYC_Accidents_17$LONGITUDE <- NULL
NYC_Accidents_17$LOCATION <- NULL
NYC_Accidents_17$ON_STREET_NAME <- NULL
NYC_Accidents_17$CROSS_STREET_NAME <- NULL
NYC_Accidents_17$OFF_STREET_NAME <- NULL

NYC_AccPedes_17 <- NYC_Accidents_17[which(NYC_Accidents_17$PEDESTRIANS_INJURED > 0 || NYC_Accidents_17$PEDESTRIANS_KILLED > 0),]

sum(NYC_AccPedes_17$PEDESTRIANS_INJURED)
sum(NYC_AccPedes_17$PEDESTRIANS_KILLED)

## Data for Paris
Accidents_Paris <- read_delim("./DataSets/Paris_accidentologie.csv", ";", 
                              escape_double = FALSE, na = "NA", trim_ws = TRUE)

# First feature selection
Accidents_Paris$Dept <- NULL
Accidents_Paris$Com <- NULL
Accidents_Paris$Adresse <- NULL
Accidents_Paris$CodePostal <- NULL
Accidents_Paris$Carr <- NULL
Accidents_Paris$Segm <- NULL
Accidents_Paris$Lieu1_NomV <- NULL
Accidents_Paris$Lieu2NomV <- NULL
Accidents_Paris$Usager_3_CatU <- NULL
Accidents_Paris$Usager_3_Grav <- NULL
Accidents_Paris$Usager_3_Lveh <- NULL
Accidents_Paris$Usager_4_CatU <- NULL
Accidents_Paris$Usager_4_Grav <- NULL
Accidents_Paris$Usager_4_Lveh <- NULL
Accidents_Paris$Coord <- NULL

summary(as.factor(AccVian_Paris[which(AccVian_Paris$Usager_1_CatU=="Piéto"),]$Usager_1_Grav))
