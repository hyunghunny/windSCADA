##
# Gearbox properties analysis
#

showGearPlots <- function (s) {
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
