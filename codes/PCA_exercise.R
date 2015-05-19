
##
# PCA runner for wind SCADA data set
#
pca <- function(windturbine.id, year) {  
  
  # load data
  source('./codes/dataloader.R')
  # run PCA and show bi-plots
  run <- function() {  
    # data loading by module 
    s <- scada(windturbine.id, toString(year))
    
    windScada.all = s$data()
    
    # clustering by category
    windScada.control <- s$control()
    #names(windScada.control)
    
    windScada.env <- s$env()
    #names(windScada.env)
    
    windScada.mech <- s$mech()
    #names(windScada.mech)
    
    
    windScada.power <- s$power()
    #names(windScada.power)
    
    # Principal Component Analysis
    
    pr.control <- princomp(windScada.control, scale=TRUE) 
    pr.env <- princomp(windScada.env, scale=TRUE) 
    pr.mech <- princomp(windScada.mech, scale=TRUE) 
    pr.power <- princomp(windScada.power, scale=TRUE) 
    
    
    par(mfrow = c(2, 2)) 
    #summary(pr.control)
    biplot(pr.control, cex=0.5, main=paste(windturbine.id, 'Control PCA', '(', year, ')'))
    #warnings()
    #pca.control = pr.control$scores[, 1] # Contains 0.99 Cumulative Proposition
        
    #summary(pr.env)
    biplot(pr.env, cex=0.5, main=paste(windturbine.id, 'Env. PCA', '(', year, ')'))
    #warnings()
    #pca.env = pr.env$scores[, 1:3] # Contains 0.99 Cumulative Proposition
    
    #summary(pr.mech)
    biplot(pr.mech, cex=0.5, main=paste(windturbine.id, 'Mech. PCA', '(', year, ')'))
    #warnings()
    #pca.mech = pr.mech$scores[, 1:9] # Contains 0.99 Cumulative Proposition
    
    #summary(pr.power)
    biplot(pr.power, cex=0.5, main=paste(windturbine.id, 'Power PCA', '(', year, ')'))
    #warnings()
    #pca.power = pr.power$scores[, 1:2]
  }
  
  # returns control principle components
  control <- function () {
    return(pr.control)
  }
  
  # returns env. principle components
  env <- function () {
    return (pr.env)
  }
  
  # returns mech. principle components
  mech <- function () {
    return (pr.mech)
  }

  # returns power principle components
  power <- function () {
    return (pr.power)
  }
  
  return (list(run=run, control=control, env=env, mech=mech, power=power))
}

windturbine.id <- 'WTG01'
year <- 2011

pca.runner <- pca(windturbine.id, year)
pca.runner$run()
pca.runner$summary()