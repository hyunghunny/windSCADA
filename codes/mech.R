##
# Wind turbine mechanic analysis
#

# sample test

source('./codes/dataloader.R')
s <- scada('WTG04', '2014')

windScada <- s$data()
# Grid Production Power Average is a correct power index
power <- windScada$Grid.Production.Power.Average

# Unit change to kiloWatt 
power.kW <- power / 1000

# Select generator's properties
generator.rpm.avg <- windScada$Generator.RPM.Average
summary(generator.rpm.avg)
plot(generator.rpm.avg, power.kW)

rotor.rpm.avg <- windScada$Rotor.RPM.Average
plot(rotor.rpm.avg, power.kW)


# Data filtering to remove data when wind speed was above 10 m/s
dataset <- subset(windScada, windScada$Ambient.WindSpeed.Average > 10)

# Plot histogram graph of bearing temp for the year  
hist(dataset$Gear.Bearing.Temperature.Average,breaks=10)

# Plot trend graph of bearing temp for the year 
plot(as.Date(dataset$PCTimeStamp, "%M-%d"), dataset$Gear.Bearing.Temperature.Average,
     main='Gear Bearing Temperature Trend', xlab='Date', ylab='Temperature')
