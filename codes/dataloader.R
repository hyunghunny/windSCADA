# data loader by function
load_scada_data <- function (turbine.id, year) {
  path <- "./data"
  fileName <- paste(c(turbine.id, '_', year, '.csv'), collapse='')
  path <- paste(c(path, fileName), collapse='/')  
  windScada <- read.csv(path)
  return (windScada)
}

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
  #scada.data$PCTimeStamp <- gsub("/14 ", "/2014 ", scada.data$PCTimeStamp)
  scada.data$PCTimeStamp <- strptime(scada.data$PCTimeStamp, "%m/%d/%y %H:%M")
  
  # return data title
  title <- function() {
    return (scada.title)
  }
  
  # returns full dataset
  data <- function () {
    return (scada.data)
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
  
  return (list(title=title, data=data, size=size, date=date, control=controls, env=envs, mech=mechs, power=powers))
}