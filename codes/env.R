##
# Wind turbine enviornment analysis
#

# sample test

source('./codes/dataloader.R')
s <- scada('WTG04', '2014')

windScada <- s$data()
# Grid Production Power Average is a correct power index
power <- windScada$Grid.Production.Power.Average

# Unit change to kiloWatt 
power.kW <- power / 1000

# Show the relationship with temp and power
ambient.temp.avg <- windScada$Ambient.Temperature.Average
summary(ambient.temp.avg)
plot(ambient.temp.avg, power.kW)

# Data filtering to remove data when wind speed was above 10 m/s
dataset <- subset(windScada, windScada$Ambient.WindSpeed.Average > 10)
#dataset <- windScada

#plot time trend for ambient temp 
plot(as.Date(dataset$PCTimeStamp, "%M-%d"), dataset$Ambient.Temperature.Average,
     main='Gear Oil Temperature Trend', xlab='Date', ylab='Temperature') 

#turbine.state <- windScada$System.States.TurbineState
#summary(turbine.state)