library(neuralnet)
source('./codes/dataloader.R')

s <- scada('WTG01', '2014')
s <- scada('WTG01', '2009')

windScada.2009 <- s$data()
windScada.early <- head(windScada, n=10000)
plot(windScada.early$Gear.Bearing.Temperature.Average)
windScada.2014 <- s$data()
windScada.late <- windScada[15000:40000,]
plot(windScada.late$Gear.Bearing.Temperature.Average)

# won's suggested model
input.oil.temp <- windScada.early$Gear.Oil.Temperature.Average
# input.oil.temp.t1 <- windScada.early$Gear.Oil.Temperature.Average
# input.bearing.temp.t1 <- windScada.early$Gear.Bearing.Temperature.Average
# input.bearing.temp.t2 <- windScada.t$Gear.Bearing.Temperature.Average
input.power <- windScada.early$Grid.Production.Power.Average
# input.power.t1 <- windScada.t1$Grid.Production.Power.Average
input.ambient.temp <-windScada.early$Ambient.Temperature.Average
output.bearing.temp <- windScada.early$Gear.Bearing.Temperature.Average

input.oil.temp.late <- windScada.late$Gear.Oil.Temperature.Average
# input.oil.temp.t1 <- windScada.early$Gear.Oil.Temperature.Average
# input.bearing.temp.t1 <- windScada.early$Gear.Bearing.Temperature.Average
# input.bearing.temp.t2 <- windScada.t$Gear.Bearing.Temperature.Average
input.power.late <- windScada.late$Grid.Production.Power.Average
# input.power.t1 <- windScada.t1$Grid.Production.Power.Average
input.ambient.temp.late <-windScada.late$Ambient.Temperature.Average
output.bearing.temp.late <- windScada.late$Gear.Bearing.Temperature.Average


#  create training dataset
train.data <- data.frame( x1=input.oil.temp,
                          #                          x2=input.bearing.temp.t1,
                          #                          x3=input.bearing.temp.t2,
                          x4=input.power,
                          x5=input.ambient.temp,
                          y=output.bearing.temp )

test.data <- data.frame( x1=input.oil.temp.late,
                         #                           x2=input.bearing.temp.t1,
                         #                           x3=input.bearing.temp.t2,
                         x4=input.power.late,
                         x5=input.ambient.temp.late,
                         y=output.bearing.temp.late)

# To narrow the range, filter-out the data when power is below XX 
#test.data <- subset(test.data,test.data$x4>0)
train.data.filtered <- subset(train.data,train.data$x4>0)
test.data.filtered <- subset(test.data,train.data$x4>0)
plot(x4~y,train.data.filtered)
pairs(train.data.filtered)
plot(x1~y,train.data.filtered)# TODO normalize training data

weights <- 27 # TODO What will be a right weights value?
hidden.layer.size <- 3 # TODO What will be a right size?


# Training with Neural Network
library(nnet)
#ann.model <- nnet(formula, train.data, weights, size=hidden.layer.size)
#seed.val <- 2
hidden.layer.size <- 10 # TODO What will be a right size?
#set.seed(seed.val)
ann.model <- nnet(y ~x1+x4+x5, train.data.filtered, size=hidden.layer.size,linout=T, maxit = 10000,threshold=0.01)
#ann.model <- neuralnet(y~x1+x2+x3+x4+x5,train.data.filtered,hidden=10,threshold=0.01)
print(ann.model)
# plotting with plot.nnet
#library(clusterGeneration)
#library(devtools)

#import the function from Github
#source_url('https://gist.githubusercontent.com/Peque/41a9e20d6687f2f3108d/raw/85e14f3a292e126f1454864427e3a189c2fe33f3/nnet_plot_update.r')
#plot.nnet(ann.model, alpha.val = 0.5, circle.col = list('lightgray', 'white'), bord.col = 'black')


# prediction with test data
# TODO create test dataset
#train.data.filtered<-train.data.filtered[1:1000,]

#create test data from training data itself 
datasize <- length(test.data.filtered$y)
test.data <- cbind(test.data.filtered[1:datasize,1],test.data.filtered[1:datasize,2],test.data.filtered[1:datasize,3])
colnames(test.data)<-c("x1","x4","x5")
#test.data <- cbind(train.data.filtered$x1,train.data.filtered$x2,train.data.filtered$x3,train.data.filtered$x4,train.data.filtered$x5)
#colnames(test.data)<-c("x1","x2","x3","x4","x5")

#predict 
nnet.result<-predict(ann.model, newdata=test.data,type="raw")
#nnet.result<-compute(ann.model, test.data$x1+test.data$x2+test.data$x3+test.data$x4+test.data$x5)
result <- cbind(test.data.filtered[1:datasize,4],as.data.frame(nnet.result))
colnames(result)<-c("Expected Output","Neural Net Output")
print(result)
result<-data.frame(nnet.result)
residual<-result-test.data.filtered[1:datasize,4]
x<-data.frame(seq(1,datasize,1))
resultplot<-cbind(x,residual)
par(mfrow=c(2,1))
plot(nnet.result,type="l",col="red")
lines(test.data.filtered[1:datasize,4],col="green")
plot(resultplot,type="l")
#test<-data.frame(test.data$y)
#plot(test,result)
summary(residual)

