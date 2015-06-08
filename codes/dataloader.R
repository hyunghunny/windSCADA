#
#' Wind SCADA data loader by module pattern
#' 
#' @param turbine.id turbine ID
#' @param year year of measurement
#' 
#' @description 
#' \code{scala} returns an object which returns data
#' @details by default, WTG01/2014 will be used.
#'
scada <- function (turbine.id='WTG01', year='2014') {
  
  # concanterate strings for making csv file path
  path <- "./data"  
  fileName <- paste(c(turbine.id, '_', year, '.csv'), collapse='')
  path <- paste(c(path, fileName), collapse='/') 
  scada.title <- paste(c(turbine.id, year), collapse='-') 
  scada.data <- read.csv(path)
  scada.data.size <- NROW(scada.data)
    
  # timestamp formatting  
  converted <- strptime(scada.data$PCTimeStamp, "%m/%d/%y %H:%M")
  if (all(is.na(converted)) == TRUE) {
    converted <- strptime(scada.data$PCTimeStamp, "%Y-%m-%d %H:%M:%S")
  } 
  scada.data$PCTimeStamp <- converted  
    
  # dataset ordering
  scada.data <- scada.data[order(scada.data$PCTimeStamp), ]
  
  # split data monthly
  milestone <- paste(c(year, '2', '1'), collapse='-')
  scada.data.jan <- subset(scada.data, scada.data$PCTimeStamp < milestone)
  scada.data.remant <- subset(scada.data, scada.data$PCTimeStamp > milestone)
  
  milestone <- paste(c(year, '3', '1'), collapse='-')  
  scada.data.feb <- subset(scada.data.remant, scada.data.remant$PCTimeStamp < milestone)
  scada.data.remant <- subset(scada.data.remant, scada.data.remant$PCTimeStamp > milestone)
  
  milestone <- paste(c(year, '4', '1'), collapse='-')  
  scada.data.mar <- subset(scada.data.remant, scada.data.remant$PCTimeStamp < milestone)
  scada.data.remant <- subset(scada.data.remant, scada.data.remant$PCTimeStamp > milestone)
  
  milestone <- paste(c(year, '5', '1'), collapse='-')  
  scada.data.apr <- subset(scada.data.remant, scada.data.remant$PCTimeStamp < milestone)
  scada.data.remant <- subset(scada.data.remant, scada.data.remant$PCTimeStamp > milestone)
  
  milestone <- paste(c(year, '6', '1'), collapse='-')  
  scada.data.may <- subset(scada.data.remant, scada.data.remant$PCTimeStamp < milestone)
  scada.data.remant <- subset(scada.data.remant, scada.data.remant$PCTimeStamp > milestone)
  
  milestone <- paste(c(year, '7', '1'), collapse='-')  
  scada.data.jun <- subset(scada.data.remant, scada.data.remant$PCTimeStamp < milestone)
  scada.data.remant <- subset(scada.data.remant, scada.data.remant$PCTimeStamp > milestone)
  
  milestone <- paste(c(year, '8', '1'), collapse='-')  
  scada.data.jul <- subset(scada.data.remant, scada.data.remant$PCTimeStamp < milestone)
  scada.data.remant <- subset(scada.data.remant, scada.data.remant$PCTimeStamp > milestone)
  
  milestone <- paste(c(year, '9', '1'), collapse='-')  
  scada.data.aug <- subset(scada.data.remant, scada.data.remant$PCTimeStamp < milestone)
  scada.data.remant <- subset(scada.data.remant, scada.data.remant$PCTimeStamp > milestone)
  
  milestone <- paste(c(year, '10', '1'), collapse='-')  
  scada.data.sep <- subset(scada.data.remant, scada.data.remant$PCTimeStamp < milestone)
  scada.data.remant <- subset(scada.data.remant, scada.data.remant$PCTimeStamp > milestone)
  
  milestone <- paste(c(year, '11', '1'), collapse='-')  
  scada.data.oct <- subset(scada.data.remant, scada.data.remant$PCTimeStamp < milestone)
  scada.data.remant <- subset(scada.data.remant, scada.data.remant$PCTimeStamp > milestone)
  
  milestone <- paste(c(year, '12', '1'), collapse='-')  
  scada.data.nov <- subset(scada.data.remant, scada.data.remant$PCTimeStamp < milestone)
  scada.data.remant <- subset(scada.data.remant, scada.data.remant$PCTimeStamp > milestone)
 
  scada.data.dec <- scada.data.remant
  

  
  #' Return data title
  title <- function() {
    return (scada.title)
  }
  
  #' Returns full dataset with order by PCTimeStamp
  data <- function () {
    # set unused columns 
    scada.drops <- c("Number.of.currently.main.wind.senor..1.2.")  
    return.data <- scada.data[, !(names(scada.data) %in% scada.drops)]
    
    return (return.data)
  }
  
  #' Return monthly subset data
  #' @param Number 1~12 represents janunary to december
  #' @return dataset of the specific month 
  month <- function(m) {
    if (m == 1) {
      return (scada.data.jan)
    }
    if (m == 2) {
      return (scada.data.feb)
    }
    if (m == 3) {
      return (scada.data.mar)
    }
    if (m == 4) {
      return (scada.data.apr)
    }
    if (m == 5) {
      return (scada.data.may)
    }
    if (m == 6) {
      return (scada.data.jun)
    }
    if (m == 7) {
      return (scada.data.jul)
    }
    if (m == 8) {
      return (scada.data.aug)
    }
    if (m == 9) {
      return (scada.data.sep)
    }
    if (m == 10) {
      return (scada.data.oct)
    }
    if (m == 11) {
      return (scada.data.nov)
    }
    if (m == 12) {
      return (scada.data.dec)
    }    
  }
  
  #' Returns size of data rows 
  size <- function () {
    return (scada.data.size)
  }
  
  #' Returns time series
  date <- function () {
    return (as.Date(scada.data$PCTimeStamp, "%Y-%m-%d"))
  }
  
  #' Returns control associated features only
  controls <- function () {
    return (scada.data[, c(11:14, 28:31, 69:100, 123:126, 132)])
  }
  
  #' Returns environment associated features only
  envs <- function () {
    return (scada.data[, c(3:10, 106)])
  }
  
  #' Returns mechanic assoicated features only
  mechs <- function () {
    return (scada.data[, c(15:26, 32:34, 66:68, 101:104, 127:128)])
  }
  
  #' Returns power associated features only
  powers <- function () {
    return (scada.data[, c(35:65, 107:122)])
  }
  
  simap <- function() {
    # inputs which were selected at the paper 'Online Wind Turbine Fault Detection through Automated SCADA Data Analysis
        
    power <- scada.data$Grid.Production.Power.Average # XXX: wonder that it may be correct...
    
    active.power <- scada.data$Total.active.power
    wind.speed.avg <- scada.data$Ambient.WindSpeed.Average
    wind.speed.stdev <- scada.data$Ambient.WindSpeed.StdDev
    
    nacelle.temp <- scada.data$Nacelle.Temperature.Average
    bearing.temp <- scada.data$Gear.Bearing.Temperature.Average
    oil.temp <- scada.data$Gear.Oil.Temperature.Average
    
    ambient.temp <-scada.data$Ambient.Temperature.Average
    
    # XXX: no generator winding argument in SCADA data
    power.factor <- scada.data$Power.factor.set.point # XXX: it is an ambiguous one.
    reactive.power <- scada.data$Total.reactive.power
    
    # following are phase currents
    phase.current1 <- scada.data$Grid.Production.CurrentPhase1.Average
    phase.current2 <- scada.data$Grid.Production.CurrentPhase2.Average
    phase.current3 <- scada.data$Grid.Production.CurrentPhase3.Average
    
    return.data <- data.frame(power, active.power, wind.speed.avg, wind.speed.stdev, 
                              nacelle.temp, bearing.temp, oil.temp, ambient.temp, power.factor, reactive.power,
                              phase.current1, phase.current2, phase.current3)
    return (return.data)
  }
  
  return (list(title=title, data=data, month=month, size=size, date=date, control=controls, env=envs, mech=mechs, power=powers, simap=simap))
}