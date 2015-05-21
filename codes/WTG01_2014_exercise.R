##
# Exploratory Analysis for Wind turbine SCADA data
#

# Check attributes and statistics
show_basic_statistics <- function (s) {
  windScada <- s$data()
  names(windScada)
  summary(windScada)
  dim(windScada)
  class(windScada)
  NROW(windScada)  
}


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
  dataset <- subset(windScada, windScada$Ambient.WindSpeed.Average > 10)
  
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

# Show scatter plot pairs
pairs(Grid.Production.Power.Average~Ambient.WindSpeed.Average+Gear.Bearing.Temperature.Average+Generator.RPM.Average, dataset)

pairs(dataset$Ambient.Temperature.Average~dataset$Gear.Oil.Temperature.Average+dataset$Gear.Bearing.Temperature.Average)#plot scatter 


# TODO:jy.han's suggestion
# calcuate eigen value for each clusters
# distinguish the most outstanding feature(s)



