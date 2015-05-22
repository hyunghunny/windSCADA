##
# Analysis using Artificial Neural Network
#
# refer to the paper "online wind turbine fault detection through automated SCADA data analysis"

source('./codes/dataloader.R')
s <- scada('WTG01', '2014')

windScada <- s$data()
windScada.t <- tail(windScada, n=10394)
windScada.t_1_2 <- subset(windScada, condition)# TODO subset data t-1 through t-2 
windScada.t_3 <- head(windScada, n=10393)

# gear box bearing model
# TODO:time spanning required
input.bearing.temp. <- windScada$Gear.Bearing.Temperature.Average # TODO:(t-1) (t-2)
input.power <- windScada.t_3$Grid.Production.Power.Average 
input.nacelle.temp <- windScada.t$Nacelle.Temperature.Average 
output.bearing.temp <- windScada$Gear.Bearing.Temperature.Average

# cooling oil temp model
input.oil.temp <- windScada$Gear.Oil.Temperature.Average # TODO: (t-1) (t-2)
input.power <- windScada.t_3$Grid.Production.Power.Average
input.nacelle.temp <- windScada.t$Nacelle.Temperature.Average
output.oil.temp <- windScada$Gear.Oil.Temperature.Average


formula # TODO create model formular
train.data # TODO create training dataset
test.data # TODO create test dataset

weights <- 27 # TODO What will be a right weights value?
hidden.layer.size <- 3 # TODO What will be a right size?

library(nnet)
ann.model <- nnet(formula, train.data, weights, size=hidden.layer.size)
predict(ann.model, newdata=test.data)