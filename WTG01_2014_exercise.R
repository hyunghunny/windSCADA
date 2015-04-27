# load data and check attributes
windScada <- read.csv("./dataset/WTG01_2014.csv")
windScada
names(windScada)
summary(windScada)
dim(windScada)
class(windScada)
NROW(windScada)

# sampling data 
windScada.head.10000 <- head(windScada, n=10000)
windScada.tail.10000 <- tail(windScada, n=10000)
windScada.random.10000 <- windScada[sample(NROW(windScada), 10000), ]
windScada.random.100 <- windScada[sample(NROW(windScada), 100), ]
windScada.random.50 <- windScada[sample(NROW(windScada), 50), ]

wind.speed.avg <- windScada$Ambient.WindSpeed.Average
wind.speed.max <- windScada$Ambient.WindSpeed.Maximum
summary(wind.speed.avg)
summary(wind.speed.max)
# what is the exact power?
#power <- windScada$Total.active.power
power <- windScada$Grid.Production.Power.Average

power.kW <- power / 1000
summary(power.kW)
plot(wind.speed.avg, power.kW)
plot(wind.speed.max, power.kW)

power.reactive <- windScada$Total.reactive.power
plot (power.reactive, power)

gear.bearing.temp.avg <- windScada$Gear.Bearing.Temperature.Average
gear.oil.temp.avg <- windScada$Gear.Oil.Temperature.Average

summary(gear.bearing.temp.avg)
summary(gear.oil.temp.avg)

# guess gear's bearing temperature and oil's are corelated. 
plot (gear.bearing.temp.avg, gear.oil.temp.avg)

plot(wind.speed.max, gear.bearing.temp.avg)

ambient.temp.avg <- windScada$Ambient.Temperature.Average
summary(ambient.temp.avg)

#show the relationship with temp and power
plot(ambient.temp.avg, power.kW)

generator.rpm.avg <- windScada$Generator.RPM.Average
summary(generator.rpm.avg)
plot(generator.rpm.avg, power.kW)

rotor.rpm.avg <- windScada$Rotor.RPM.Average
plot(rotor.rpm.avg, power.kW)

turbine.state <- windScada$System.States.TurbineState
summary(turbine.state)

# Select dataset
dataset <- windScada
dataset <- windScada.head.10000
dataset <- windScada.tail.10000
dataset <- windScada.random.10000 
dataset <- windScada.random.100
dataset <- windScada.random.50

# clustering by category
windScada.control <- windScada[, c(11:14, 28:31, 69:100, 123:126, 132)]
names(windScada.control)


# Show scatter plots
#pairs(Total.active.power~Ambient.WindSpeed.Average+Gear.Bearing.Temperature.Average+Generator.RPM.Average, dataset)
pairs(Grid.Production.Power.Average~Ambient.WindSpeed.Average+Gear.Bearing.Temperature.Average+Generator.RPM.Average, dataset)


subset <- dataset[, 3:132] # remove timestamp, number of wind sensor col.
subset <- dataset[, 3:10] # remove timestamp, number of wind sensor col.and only 8 cols.
names(subset)
subset <- scale(subset)
# Principal Component Analysis
pr <- princomp(subset) 
summary(pr)
biplot(pr)
summary(pr$scores)


