load_scada_data <- function (turbine.id, year) {
  path <- "./data"
  fileName <- paste(c(turbine.id, '_', year, '.csv'), collapse='')
  path <- paste(c(path, fileName), collapse='/')  
  windScada <- read.csv(path)
  return (windScada)
}