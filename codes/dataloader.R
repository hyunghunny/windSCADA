# data loader by function
load_scada_data <- function (turbine.id, year) {
  path <- "./data"
  fileName <- paste(c(turbine.id, '_', year, '.csv'), collapse='')
  path <- paste(c(path, fileName), collapse='/')  
  windScada <- read.csv(path)
  return (windScada)
}

# data loader by module pattern
# by default, WTG01/2014 will be used.
scada <- function (turbine.id='WTG01', year='2014') {
  
  # concanterate strings for making csv file path
  path <- "./data"  
  fileName <- paste(c(turbine.id, '_', year, '.csv'), collapse='')
  path <- paste(c(path, fileName), collapse='/')  
  scada.data <- read.csv(path)
  scada.data.size <- NROW(scada.data)
  
  # returns full dataset
  data <- function () {
    return (scada.data)
  }  
  # returns size of data rows 
  size <- function () {
    return (scada.data.size)
  }
  
  # returns control associated features only
  control <- function () {
    return (scada.data[, c(11:14, 28:31, 69:100, 123:126, 132)])
  }
  
  # returns environment associated features only
  env <- function () {
    return (scada.data[, c(3:10, 106)])
  }
  
  # returns mechanic assoicated features only
  mech <- function () {
    return (scada.data[, c(15:26, 32:34, 66:68, 101:104, 127:128)])
  }
  
  # returns power associated power 
  power <- function () {
    return (scada.data[, c(35:65, 107:122)])
  }  
  
  return (list(data=data, size=size, control=control, env=env, mech=mech, power=power))
}