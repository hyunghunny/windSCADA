##
# Gearbox properties analysis
#

showGearPlots <- function (s) {
  
  windScada <- s$data()
  
  # Select gear properties
  gear.bearing.temp.avg <- windScada$Gear.Bearing.Temperature.Average
  gear.oil.temp.avg <- windScada$Gear.Oil.Temperature.Average
  
  summary(gear.bearing.temp.avg)
  summary(gear.oil.temp.avg)
  
  # guess gear's bearing temperature and oil's are corelated. 
  # Show plots
  plot (gear.bearing.temp.avg, gear.oil.temp.avg)
  plot(wind.speed.max, gear.bearing.temp.avg)
}


# load data
source('./codes/dataloader.R')

s <- scada('WTG01', '2014')
windScada <- s$data()

# Data filtering to remove data when wind speed was above 10 m/s
dataset <- subset(windScada, windScada$Ambient.WindSpeed.Average > 10)

#plot time trend for gear oil temp 
plot(as.Date(dataset$PCTimeStamp, "%M-%d"), dataset$Gear.Oil.Temperature.Average, main='Gear Oil Temperature Trend', xlab='Date', ylab='Temperature')
