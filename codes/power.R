##
# Wind turbine Power Analysis
#

showPowerCurve <- function (s) {
  
  windScada <- s$data()
  # select wind speed properties
  wind.speed.avg <- windScada$Ambient.WindSpeed.Average
  wind.speed.max <- windScada$Ambient.WindSpeed.Maximum
  #summary(wind.speed.avg)
  #summary(wind.speed.max)
  
  # Grid Production Power Average is a correct power index
  power <- windScada$Grid.Production.Power.Average
  
  # Unit change to kiloWatt 
  power.kW <- power / 1000
  #summary(power.kW)
  
  plot.title <- paste(c(s$title(), 'Power', 'Curve'), collapse='-')
  # Show wind/power plot with kW
  plot(wind.speed.avg, power.kW, main=plot.title, 
       xlab='wind speed (m/s)', ylab='Power (kW)')
  
  #plot(wind.speed.max, power.kW)
  
  # Show power plot
  #power <- windScada$Total.active.power
  #power.reactive <- windScada$Total.reactive.power
  #plot (power.reactive, power)
  
}

# sample test

source('./codes/dataloader.R')
for (year in seq(2008, 2014)) {
  s <- scada('WTG14', year)
  showPowerCurve(s)
}
summary(s$power())

# simple tric to avoid for looping by jy.han (very thx)
power_range = sapply(s$power(),range)
power_range = unlist(power_range)
