# load data
source('./codes/dataloader.R')
#windScada <- load_scada_data('WTG01', '2014')
s <- scada()
windScada = s$data()
s$size()
is.data.frame(windScada)

# Check attributes and statistics
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

# select wind speed properties
wind.speed.avg <- windScada$Ambient.WindSpeed.Average
wind.speed.max <- windScada$Ambient.WindSpeed.Maximum
summary(wind.speed.avg)
summary(wind.speed.max)

# what is the exact power? -> Grid Production Power Average
#power <- windScada$Total.active.power
power <- windScada$Grid.Production.Power.Average

# Unit change to kiloWatt 
power.kW <- power / 1000
summary(power.kW)

# Show wind plot with kW
plot(wind.speed.avg, power.kW)
plot(wind.speed.max, power.kW)

# Show power plot
power.reactive <- windScada$Total.reactive.power
plot (power.reactive, power)

# Select gear properties
gear.bearing.temp.avg <- windScada$Gear.Bearing.Temperature.Average
gear.oil.temp.avg <- windScada$Gear.Oil.Temperature.Average

summary(gear.bearing.temp.avg)
summary(gear.oil.temp.avg)

# guess gear's bearing temperature and oil's are corelated. 
# Show plots
plot (gear.bearing.temp.avg, gear.oil.temp.avg)
plot(wind.speed.max, gear.bearing.temp.avg)


# Show the relationship with temp and power
ambient.temp.avg <- windScada$Ambient.Temperature.Average
summary(ambient.temp.avg)
plot(ambient.temp.avg, power.kW)

# Select generator's properties
generator.rpm.avg <- windScada$Generator.RPM.Average
summary(generator.rpm.avg)
plot(generator.rpm.avg, power.kW)

rotor.rpm.avg <- windScada$Rotor.RPM.Average
plot(rotor.rpm.avg, power.kW)

turbine.state <- windScada$System.States.TurbineState
summary(turbine.state)

# Select dataset to reduce points 
dataset <- windScada
dataset <- windScada.head.10000
dataset <- windScada.tail.10000
dataset <- windScada.random.10000 
dataset <- windScada.random.100
dataset <- windScada.random.50


# Show scatter plots
#pairs(Total.active.power~Ambient.WindSpeed.Average+Gear.Bearing.Temperature.Average+Generator.RPM.Average, dataset)
pairs(Grid.Production.Power.Average~Ambient.WindSpeed.Average+Gear.Bearing.Temperature.Average+Generator.RPM.Average, dataset)

# Plot trend graph of bearing temp for the year 
plot(windScada$PCTimeStamp,windScada$Gear.Bearing.Temperature.Average)

# Plot histogram graph of bearing temp for the year  
hist(windScada$Gear.Bearing.Temperature.Average,breaks=10)

# Data filtering to remove data when wind speed was above 10 m/s  
# FilteredSCADA <- windScada[windScada$Ambient.WindSpeed.Average>10]
FilteredSCADA <- subset(windScada,windScada$Ambient.WindSpeed.Average>10)
FilteredSCADA2 <- FilteredSCADA[,2:10] #taking 9 variables only. there are 138 variables in total, and not all of them are used for the work 

# plotting
hist(FilteredSCADA$Gear.Bearing.Temperature.Average,breaks=10) #plot histogram
plot(FilteredSCADA$PCTimeStamp,FilteredSCADA$Gear.Bearing.Temperature.Average) #plot time trend for bearing temp
plot(FilteredSCADA$PCTimeStamp,FilteredSCADA$Ambient.Temperature.Average) #plot time trend for ambient temp 
plot(FilteredSCADA$PCTimeStamp,FilteredSCADA$Gear.Oil.Temperature.Average) #plot time trend for gear oil temp 
pairs(windScada$Ambient.Temperature.Average~windScada$Gear.Oil.Temperature.Average+windScada$Gear.Bearing.Temperature.Average)#plot scatter 
y<-princomp(windScada[,2:8]) #pca
biplot(y) #biplot for pca


# clustering by category
windScada.control <- s$control()
names(windScada.control)

windScada.env <- s$env()
names(windScada.env)

windScada.mech <- s$mech()
names(windScada.mech)


windScada.power <- s$power()
names(windScada.power)


# select a subset clustered by category
subset <- windScada.control
subset <- windScada.env 
subset <- windScada.mech
subset <- windScada.power

names(subset)

subset <- scale(subset) # normalize it (XXX:only env value can be mornalized)

# Principal Component Analysis
pr.control <- princomp(windScada.control) 
summary(pr.control)
biplot(pr.control)
warnings()
pca.control = pr.control$scores[, 1] # Contains 0.99 Cumulative Proposition

pr.env <- princomp(windScada.env) 
summary(pr.env)
biplot(pr.env)
warnings()
pca.env = pr.env$scores[, 1:3] # Contains 0.99 Cumulative Proposition

pr.mech <- princomp(windScada.mech) 
summary(pr.mech)
biplot(pr.mech)
warnings()
pca.mech = pr.mech$scores[, 1:9] # Contains 0.99 Cumulative Proposition

pr.power <- princomp(windScada.power) 
summary(pr.power)
biplot(pr.power)
warnings()
pca.power = pr.power$scores[, 1:2]



