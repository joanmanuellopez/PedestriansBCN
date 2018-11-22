#### 3. TIME SERIES ANALYSIS ####
## Create time information for the Time Series Analysis
library("readr")
library("forecast")

AccPersVian_DF <- read_csv("./WorkingData/accidents_vianants.csv")
Accidents_DF <- read_csv("./WorkingData/accidents.csv")

dies.mes <- c(31,28,31,30,31,30,31,31,30,31,30,31,
              31,28,31,30,31,30,31,31,30,31,30,31,
              31,29,31,30,31,30,31,31,30,31,30,31,
              31,28,31,30,31,30,31,31,30,31,30,31)  # Unfortunately, there is a leap year...

## Pedestrians
APVTS_DF <- AccPersVian_DF

APVTS_DF <- APVTS_DF %>% mutate(MonthYear = paste(Any, formatC(MesAny, width = 2, flag = "0")))
PedestriansTS <- APVTS_DF %>% group_by(MonthYear) %>% summarise(Damaged = n())
PedestriansTS$DiesMes <- dies.mes

PedestriansTS <- PedestriansTS %>% mutate(DamDay = Damaged/DiesMes)
PedestriansTS %>% ggplot() + geom_line(aes(x = 1:nrow(PedestriansTS), y = DamDay),color="#2f3640") +
  theme(axis.title = element_blank(), axis.text.y = element_blank())

## Decomposing and Predicting
# Create Time Series object
monthlyPedTS <- ts(PedestriansTS$DamDay,frequency=12,start=c(2014))
plot(monthlyPedTS)

# Decomposing
monthlyPedSTL = stl(monthlyPedTS, s.window="periodic")
monthlyPedSTL = stl(monthlyPedTS, s.window=9, t.window=21)
plot(monthlyPedSTL, col = "#2f3640", main="Pedestrians damaged (2014 - 2017)")

# Generate a LM prediction
monthlyPedLM <- tslm(monthlyPedTS ~ trend + season)
monthlyPedForecast <- forecast(monthlyPedLM, h=12)
plot(monthlyPedForecast) #h=12 involves a period of 1 year (12 months)

# Generate a Holt Winters prediction
monthlyPedHW <- HoltWinters(monthlyPedTS)
plot(forecast(monthlyPedHW,h=24),col="#2f3640",main="Holt-Winters Prediction for 2 years")

## ## Accidents
AccTS_DF <- Accidents_DF

AccTS_DF <- AccTS_DF %>% mutate(MonthYear = paste(Any, formatC(MesAny, width = 2, flag = "0")))
AccidentsTS <- AccTS_DF %>% group_by(MonthYear) %>% summarise(TotalAccs = n())
AccidentsTS$DiesMes <-  dies.mes

AccidentsTS <- AccidentsTS %>% mutate(AccDay = TotalAccs/DiesMes)
AccidentsTS %>% ggplot() + geom_line(aes(x = 1:nrow(AccidentsTS), y = AccDay),color="#2f3640") +
  theme(axis.title = element_blank(), axis.text.y = element_blank())

## Decomposing and Predicting
# Create Time Series object
monthlyAccTS <- ts(AccidentsTS$AccDay,frequency=12,start=c(2014))
plot(monthlyAccTS)

# Decomposing
monthlyAccSTL = stl(monthlyAccTS, s.window="periodic")
monthlyAccSTL = stl(monthlyAccTS, s.window=9, t.window=21)
plot(monthlyAccSTL, col = "#2f3640", main="Traffic Accidents (2014 - 2017)")

# Generate a LM prediction
monthlyAccLM <- tslm(monthlyAccTS ~ trend + season)
monthlyAccForecast <- forecast(monthlyAccLM, h=12)
plot(monthlyAccForecast) #h=12 involves a period of 1 year (12 months)

# Generate a Holt Winters prediction
monthlyAccHW <- HoltWinters(monthlyAccTS)
plot(forecast(monthlyAccHW,h=24),col="#2f3640",main="Holt-Winters Prediction for 2 years")

## Observation of the trend pedestrians hitted by accidents (ratio) -- nothing seen about seasonality of pedestrians
AccidentsTS <- mutate(AccidentsTS,DamAcc = PedestriansTS$DamDay*100/AccidentsTS$AccDay)
AccidentsTS %>% ggplot() + geom_line(aes(x = 1:nrow(AccidentsTS), y = DamAcc))
