##
# Analysis using Artificial Neural Network
#
# refer to the paper "online wind turbine fault detection through automated SCADA data analysis"

source('./codes/dataloader.R')
s <- scada('WTG04', '2014')

windScada <- s$data()

t.month <- 4 # base time span ()


# |----------|----------|----------|----------|
#      t-3       t-2        t-1         t

windScada.t <- s$month(t.month)[1:4000,]
windScada.t1 <- s$month(t.month-1)[1:4000,]
windScada.t2 <- s$month(t.month-2)[1:4000,]
windScada.t3 <- s$month(t.month-3)[1:4000,]


# won's suggested model
input.oil.temp <- windScada.t$Gear.Oil.Temperature.Average
input.bearing.temp.t1 <- windScada.t1$Gear.Bearing.Temperature.Average
input.bearing.temp.t2 <- windScada.t2$Gear.Bearing.Temperature.Average
input.power <- windScada.t$Grid.Production.Power.Average
input.ambient.temp <-windScada.t$Ambient.Temperature.Average
output.bearing.temp <- windScada.t$Gear.Bearing.Temperature.Average


#  create training dataset
train.data <- data.frame( x1=input.oil.temp,
                          x2=input.bearing.temp.t1,
                          x3=input.bearing.temp.t2,
                          x4=input.power,
                          x5=input.ambient.temp,
                          y=output.bearing.temp )

# TODO normalize training data



# Training with Neural Network
library(nnet)
seed.val <- 2
hidden.layer.size <- 3 # TODO What will be a right size?
set.seed(seed.val)
ann.model <- nnet(y ~., train.data, size=hidden.layer.size, linout=T)

# plotting with plot.nnet
library(clusterGeneration)
library(devtools)

#import the function from Github
source_url('https://gist.githubusercontent.com/Peque/41a9e20d6687f2f3108d/raw/85e14f3a292e126f1454864427e3a189c2fe33f3/nnet_plot_update.r')
plot.nnet(ann.model, alpha.val = 0.5, circle.col = list('lightgray', 'white'), bord.col = 'black')


# prediction with test data
# TODO create test dataset
test.data <- NA
predict(ann.model, newdata=test.data)