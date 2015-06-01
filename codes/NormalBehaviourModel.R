
### ANN modeling ###

# data loading from SCADA csv file 
dataloading <- function (turbine='WTG01', yr='2014',start=0, end=10000) {

  source('./codes/dataloader.R')
  s <- scada('WTG01', '2014')
  # Remove the data when power is below zero
  windScada <- s$data()
  windScada <- subset(windScada,windScada$Grid.Production.Power.Average>0)[start:end,] 
  
  oil.temp <- windScada$Gear.Oil.Temperature.Average
  power <- windScada$Grid.Production.Power.Average
  ambient.temp <-windScada$Ambient.Temperature.Average
  bearing.temp <- windScada$Gear.Bearing.Temperature.Average
  
  data<-data.frame(oil.temp,power,ambient.temp,bearing.temp)
}

# Prepare training data
training.data<-dataloading('WTG01','2014',0,1000)

# Training with Neural Network
library(nnet)
hidden.layer.size <- 9 
ann.model <- nnet(bearing.temp ~ oil.temp+power+ambient.temp, training.data, size=hidden.layer.size,linout=T, maxit = 10000,threshold=0.01)
#ann.model <- neuralnet(y~x1+x2+x3+x4+x5,train.data.filtered,hidden=10,threshold=0.01)
print(ann.model)

## @brief plotting with plot.nnet ###
library(clusterGeneration)
library(devtools)
#import the function from Github
source_url('https://gist.githubusercontent.com/Peque/41a9e20d6687f2f3108d/raw/85e14f3a292e126f1454864427e3a189c2fe33f3/nnet_plot_update.r')
plot.nnet(ann.model, alpha.val = 0.5, circle.col = list('lightgray', 'white'), bord.col = 'black')


## Testing ## 
#
#
test.data<-dataloading('WTG01','2014',1001,2000)
test.expected.output<-test.data[,4]
test.data<-test.data[,1:3]
test.nnet.result<-data.frame(predict(ann.model, newdata=test.data,type="raw"))
test.residual<-test.expected.output-test.nnet.result
print(cbind(test.expected.output,test.nnet.result))

# plotting
x<-data.frame(seq(1,length(test.residual[,1])))
test.resultplot<-cbind(x,test.residual)
colnames(test.resultplot)<-c("x","residual")
par(mfrow=c(2,1))
plot(test.nnet.result[,1],type="l",col="red",xlab='sample point',ylab='bearing temperture',ylim=range(40,80))
lines(test.expected.output,col="green")
plot(test.resultplot,type="p",xlab='sample point',ylab='residual',pch=20,ylim=range(-10,10))
par(fig=c(0.65,1.0,0,0.5),new=TRUE)
boxplot(test.residual,axes=FALSE,ylim=range(-10,10))

library(zoo)
#Make zoo object of data
test.temp.zoo<-zoo(test.resultplot[,2])
#Calculate moving average with window 3 and make first and last value as NA (to ensure identical length of vectors)
test.m.av<-rollmean(test.temp.zoo, 50,fill = list(NA, NULL, NA))
#Add calculated moving averages to existing data frame
test.resultplot$amb.av=coredata(test.m.av)
#Add additional line for moving average in black
library(ggplot2)
library(scales)
ggplot(test.resultplot, aes(x, residual)) + geom_point(colour="green") + geom_line(aes(x,amb.av),color="black")

## Prediction ## 
#
#
predict.data<-dataloading('WTG01','2014',15001,20000)
predict.expected.output<-predict.data[,4]
predict.data<-predict.data[,1:3]
predict.nnet.result<-data.frame(predict(ann.model, newdata=predict.data,type="raw"))
predict.residual<-predict.expected.output-predict.nnet.result
print(cbind(predict.expected.output,predict.nnet.result))

# plotting
x<-data.frame(seq(1,length(predict.residual[,1])))
predict.resultplot<-cbind(x,predict.residual)
colnames(predict.resultplot)<-c("x","residual")
par(mfrow=c(2,1))
plot(predict.nnet.result[,1],type="l",col="red",xlab='sample point',ylab='bearing temperture',ylim=range(40,80))
lines(predict.expected.output,col="green")
plot(predict.resultplot,type="p",xlab='sample point',ylab='residual',pch=20,ylim=range(-10,10))
par(fig=c(0.65,1.0,0,0.5),new=TRUE)
boxplot(predict.residual,axes=FALSE,ylim=range(-10,10))


library(zoo)
#Make zoo object of data
temp.zoo<-zoo(predict.resultplot[,2])
#Calculate moving average with window 3 and make first and last value as NA (to ensure identical length of vectors)
m.av<-rollmean(temp.zoo, 100,fill = list(NA, NULL, NA))
#Add calculated moving averages to existing data frame
predict.resultplot$amb.av=coredata(m.av)
#Add additional line for moving average in red
library(ggplot2)
library(scales)
library(reshape2)
ggplot(predict.resultplot, aes(x, residual)) + geom_point(colour="green") + geom_line(aes(x,amb.av),color="black")

