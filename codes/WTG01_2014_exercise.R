

# Check attributes and statistics
show_basic_statistics <- function (s) {
  windScada <- s$data()
  names(windScada)
  summary(windScada)
  dim(windScada)
  class(windScada)
  NROW(windScada)  
}





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

#turbine.state <- windScada$System.States.TurbineState
#summary(turbine.state)

##
# Sampling data 
#
sampling <- function (windScada) {

  # Select dataset to reduce points 
  #dataset <- windScada
  #dataset <- head(windScada, n=10000)
  #dataset <- tail(windScada, n=10000)
  #dataset <- windScada[sample(NROW(windScada), 10000), ] 
  #dataset <- windScada[sample(NROW(windScada), 100), ]
  #dataset <-  windScada[sample(NROW(windScada), 50), ]
  
  # Data filtering to remove data when wind speed was above 10 m/s
  dataset <- subset(windScada, windScada$Ambient.WindSpeed.Average>10)
  
  NROW(dataset)
  return (dataset)
}


# load data
source('./codes/dataloader.R')

s <- scada('WTG01', '2014')
windScada <- s$data()
s$size()
is.data.frame(windScada) # check data type

# data sampling
dataset <- sampling(windScada)

# Show scatter plots
pairs(Grid.Production.Power.Average~Ambient.WindSpeed.Average+Gear.Bearing.Temperature.Average+Generator.RPM.Average, dataset)

# Plot trend graph of bearing temp for the year 
plot(as.Date(dataset$PCTimeStamp, "%Y-%m-%d"), dataset$Gear.Bearing.Temperature.Average)

# Plot histogram graph of bearing temp for the year  
hist(dataset$Gear.Bearing.Temperature.Average,breaks=10)



# plotting
hist(dataset$Gear.Bearing.Temperature.Average, breaks=10) #plot histogram
plot(as.Date(dataset$PCTimeStamp, "%Y-%m-%d"), dataset$Gear.Bearing.Temperature.Average) # plot time trend for bearing temp

plot(as.Date(dataset$PCTimeStamp, "%Y-%m-%d"), dataset$Ambient.Temperature.Average) #plot time trend for ambient temp 


plot(as.Date(dataset$PCTimeStamp, "%Y-%m-%d"), dataset$Gear.Oil.Temperature.Average) #plot time trend for gear oil temp 
pairs(dataset$Ambient.Temperature.Average~dataset$Gear.Oil.Temperature.Average+dataset$Gear.Bearing.Temperature.Average)#plot scatter 


summary(s$power())

# simple tric to avoid for looping by jy.han (very thx)
power_range = sapply(s$power(),range)
power_range = unlist(power_range)

# TODO:jy.han's suggestion
# calcuate eigen value for each clusters
# distinguish the most outstanding feature(s)



