
### ANN modeling ###

# data loading from SCADA csv file 
dataloading <- function (turbine='WTG01',yr='2014',start=1,end=s.length) {

  source('./codes/dataloader.R')
  s <- scada(turbine,yr)
  # Remove the data when power is below zero
  windScada <- s$data()
  s.length<-length(windScada$Gear.Oil.Temperature.Average)
  windScada <- subset(windScada,windScada$Grid.Production.Power.Average>0)[start:end,] 
  
  time <- windScada$PCTimeStamp
  oil.temp <- windScada$Gear.Oil.Temperature.Average
  power <- windScada$Grid.Production.Power.Average
  ambient.temp <-windScada$Ambient.Temperature.Average
  bearing.temp <- windScada$Gear.Bearing.Temperature.Average
  
  data<-data.frame(time,oil.temp,power,ambient.temp,bearing.temp)
  data<-na.omit(data)
}

# Training with Neural Network
library(nnet)
library(clusterGeneration)
library(devtools)
library(reshape)

# Prepare training data
bearing.training <- function(turbine,yr,start)
  {
  training.data<-dataloading(turbine,yr,start)
  hidden.layer.size <- 9 
  ann.model <- nnet(bearing.temp ~ oil.temp+power+ambient.temp, training.data, size=hidden.layer.size,linout=T, maxit = 10000,threshold=0.01)
# ann.model <- nnet(bearing.temp ~ power+ambient.temp, training.data, size=hidden.layer.size,linout=T, maxit = 10000,threshold=0.01)
#ann.model <- neuralnet(y~x1+x2+x3+x4+x5,train.data.filtered,hidden=10,threshold=0.01)
  print(ann.model)
  ## @brief plotting with plot.nnet ###
  #import the function from Github
  source_url('https://gist.githubusercontent.com/Peque/41a9e20d6687f2f3108d/raw/85e14f3a292e126f1454864427e3a189c2fe33f3/nnet_plot_update.r')
  par(mfrow=c(1,1))
  plot.nnet(ann.model, alpha.val = 0.5, circle.col = list('lightgray', 'white'), bord.col = 'black')

  return(ann.model)
}

predict.bearing <- function (turbine, yr,start,end) {
#   
#    turbine='WTG04' 
#    yr='2010'
#    start=0
#   end=10000
  library(zoo)
  library(ggplot2)
  library(scales)
  library(reshape2)

  predict.data<-dataloading(turbine,yr,start,end)
  predict.expected.output<-predict.data$bearing.temp
  predict.data<-predict.data[,1:4]
  predict.nnet.result<-data.frame(predict(ann.model, newdata=predict.data,type="raw"))
  predict.residual<-predict.expected.output-predict.nnet.result
  #Add calculated moving averages to existing data frame
  output <- cbind(predict.data[,1],predict.residual,rollmean(predict.residual, 50,fill = list(NA, NULL, NA)),predict.expected.output,predict.nnet.result,rollmean(((predict.expected.output-70)>0),3,fill = list(NA, NULL, NA)),rollmean(((predict.residual-5)>0),3,fill = list(NA, NULL, NA)))
  colnames(output) <- c('Time','Residual','amb.av','Measured','Predicted','Measured.Count','Residual.Count')
  #count <- cbind(predict.data[,1],)
  #  colnames(count) <- c('Time','Measured','Residual')
  #  plot(MTime,output)
  par(mfrow=c(2,1))
  plot(Measured.Count~Time,output)
  plot(Residual.Count~Time,output)
  #   print(output)
  
  # plotting
  par(mfrow=c(2,1))
  #todo change point to date
  plot(output$Time,output$Predicted,type="l",col="red",xlab='date',ylab='bearing temperture',ylim=range(40,80),main=paste(turbine,'  from',output$Time[1],'to',output$Time[length(output$Time)]))
  lines(output$Time,output$Measured,col="green")
  plot(output$Time,output$Residual,type="p",xlab='date',ylab='residual',pch=20,ylim=range(-15,15))
  par(fig=c(0.65,1.0,0,0.5),new=TRUE)
  boxplot(output$Residual,axes=FALSE,ylim=range(-15,15))
  
  
  #Make zoo object of data
  #Calculate moving average with window 3 and make first and last value as NA (to ensure identical length of vectors)
  #Add additional line for moving average in red
  print(ggplot(output,aes(Time,Residual)) + geom_point(colour="green") + geom_line(aes(Time,amb.av),color="black")+labs(title=paste(turbine,'Bearing Temperature Residual from',output$Time[1],'to',output$Time[length(output$Time)])) + ylim(-10,10))

  return(list(turbine=turbine,year=yr,output=output))

}

ann.model<-bearing.training('WTG09','2009',1)
WTG09.Output2009<-predict.bearing(turbine='WTG09', yr='2009',start=1)
WTG09.Output2010<-predict.bearing(turbine='WTG09', yr='2010',start=1)
WTG09.Output2011<-predict.bearing(turbine='WTG09', yr='2011',start=1)
WTG09.Output2012<-predict.bearing(turbine='WTG09', yr='2012',start=1)
WTG09.Output2013<-predict.bearing(turbine='WTG09', yr='2013',start=1)
WTG04.Output2014<-predict.bearing(turbine='WTG04', yr='2014',start=20000,11000)
  