# load data
source('./codes/dataloader.R')
#windScada <- load_scada_data('WTG01', '2014')
s <- scada('WTG01', '2014')
windScada = s$data()
s$size()
is.data.frame(windScada)

# Check attributes and statistics
show_basic_statistics <- function (windScada) {
  names(windScada)
  summary(windScada)
  dim(windScada)
  class(windScada)
  NROW(windScada)  
}


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


# sampling data 
windScada.head.10000 <- head(windScada, n=10000)
windScada.tail.10000 <- tail(windScada, n=10000)
windScada.random.10000 <- windScada[sample(NROW(windScada), 10000), ]
windScada.random.1000 <- windScada[sample(NROW(windScada), 100), ]
windScada.random.100 <- windScada[sample(NROW(windScada), 100), ]
windScada.random.50 <- windScada[sample(NROW(windScada), 50), ]

# Select dataset to reduce points 
dataset <- head(windScada, n=300)
#dataset <- windScada.head.10000
#dataset <- windScada.tail.10000
#dataset <- windScada.random.10000 
#dataset <- windScada.random.100
#dataset <- windScada.random.50
# Data filtering to remove data when wind speed was above 10 m/s
dataset <- subset(windScada, windScada$Ambient.WindSpeed.Average>10)
NROW(dataset)

# Show scatter plots
#pairs(Total.active.power~Ambient.WindSpeed.Average+Gear.Bearing.Temperature.Average+Generator.RPM.Average, dataset)
pairs(Grid.Production.Power.Average~Ambient.WindSpeed.Average+Gear.Bearing.Temperature.Average+Generator.RPM.Average, dataset)

# Plot trend graph of bearing temp for the year 
plot(parsed_date,dataset$Gear.Bearing.Temperature.Average)

# Plot histogram graph of bearing temp for the year  
hist(dataset$Gear.Bearing.Temperature.Average,breaks=10)

# FIXME: I couldn't convert the PCTimeStamp as Date type. Help ME!
timestamps <- as.POSIXct(levels(dataset$PCTimeStamp), tz=Sys.timezone(), format="%m-%d-%y %H:%M")
timestamps <- levels(dataset$PCTimeStamp) 
parsed <- strptime(timestamps, "%m-%d-%y %H:%M")
parsed_date = format(parsed, "%Y-%m-%d")
parsed_weekday = weekdays(as.Date(parsed_date))
parsed_time = format(parsed, "%H:%M")

# plotting
hist(dataset$Gear.Bearing.Temperature.Average, breaks=10) #plot histogram
plot(as.Date(dataset$PCTimeStamp, "%Y-%m-%d"), dataset$Gear.Bearing.Temperature.Average) # plot time trend for bearing temp

plot(parsed_date, dataset$Ambient.Temperature.Average) #plot time trend for ambient temp 


plot(parsed_date, dataset$Gear.Oil.Temperature.Average) #plot time trend for gear oil temp 
pairs(dataset$Ambient.Temperature.Average~dataset$Gear.Oil.Temperature.Average+dataset$Gear.Bearing.Temperature.Average)#plot scatter 
#y<-princomp(windScada[,2:8]) #pca
#biplot(y) #biplot for pca


summary(s$power())

# simple tric to avoid for looping by jy.han (very thx)
power_range = sapply(s$power(),range)
power_range = unlist(power_range)

# TODO:jy.han's suggestion
# calcuate eigen value for each clusters
# distinguish the most outstanding feature(s)



