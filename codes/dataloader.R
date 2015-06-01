##
# SCADA data loader by module pattern
# @param turbine.id turbine ID
# @param year year of measurement
#
# @comment by default, WTG01/2014 will be used.
#
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
  
  # remove unused column
  drops <- c("Number.of.currently.main.wind.senor..1.2.")
  scada.data <- scada.data[, !(names(scada.data) %in% drops)]
  
  
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
  

  
  # return data title
  title <- function() {
    return (scada.title)
  }
  
  # returns full dataset with order by PCTimeStamp
  data <- function () {
    return (scada.data)
  }
  
  # return monthly subset data
  # @param Number 1~12 represents janunary to december
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
  
  # returns size of data rows 
  size <- function () {
    return (scada.data.size)
  }
  
  # returns time series
  date <- function () {
    return (as.Date(scada.data$PCTimeStamp, "%Y-%m-%d"))
  }
  
  # returns control associated features only
  controls <- function () {
    return (scada.data[, c(11:14, 28:31, 69:100, 123:126, 132)])
  }
  
  # returns environment associated features only
  envs <- function () {
    return (scada.data[, c(3:10, 106)])
  }
  
  # returns mechanic assoicated features only
  mechs <- function () {
    return (scada.data[, c(15:26, 32:34, 66:68, 101:104, 127:128)])
  }
  
  # returns power associated features only
  powers <- function () {
    return (scada.data[, c(35:65, 107:122)])
  }  
  
  return (list(title=title, data=data, month=month, size=size, date=date, control=controls, env=envs, mech=mechs, power=powers))
}