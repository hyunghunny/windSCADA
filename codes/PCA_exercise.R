# load data
source('./codes/dataloader.R')

##
#' linear PCA analysis for wind SCADA data set
#' @param s wind SCADA object by data loader
#' 
pca <- function(s) {  

  # run PCA and show bi-plots
  run <- function() {  

    
    # windScada.all = s$data()
    
    # clustering by category
    windScada.control <- s$control()
    #windScada.control <- scale(windScada.control,center=F,scale=T) # normalization
    #names(windScada.control)
    
    windScada.env <- s$env()
    windScada.env <- scale(windScada.env,center=F,scale=T) # normalization
    #names(windScada.env)
    
    windScada.mech <- s$mech()
    windScada.mech <- scale(windScada.mech,center=F,scale=T) # normalization
    #names(windScada.mech)
    
    
    windScada.power <- s$power()
    #windScada.power <- scale(windScada.power,center=F,scale=T) # normalization
    #names(windScada.power)
    
    # Principal Component Analysis
    
    pr.control <- princomp(windScada.control, scale=TRUE) 
    pr.env <- princomp(windScada.env, scale=TRUE) 
    pr.mech <- princomp(windScada.mech, scale=TRUE) 
    pr.power <- princomp(windScada.power, scale=TRUE) 
  }
  
  
  run4paper <- function() {
    windScada.simap <- s$simap()
    
    # normalization
    windScada.simap <- scale(windScada.simap,center=F,scale=T)
    pr.simap <- princomp(windScada.simap, scale=TRUE)
    
    biplot(pr.simap, cex=0.5, main='PCA of SCADA Parameters', col=c("grey","red"), pch=20)
    return (pr.simap)
  }
  
  draw <- function(title='', row=1, col=1) { 
    
    par(mfrow = c(row, col)) 
    #summary(pr.control)
    biplot(pr.control, cex=0.5, main=paste(title, 'Control PCA'), col=c("grey","red"))
    #pca.control = pr.control$scores[, 1] # Contains 0.99 Cumulative Proposition
        
    #summary(pr.env)
    biplot(pr.env, cex=0.5, main=paste(title, 'Env. PCA'), col=c("grey","red"))
    #pca.env = pr.env$scores[, 1:3] # Contains 0.99 Cumulative Proposition
    
    #summary(pr.mech)
    biplot(pr.mech, cex=0.5, main=paste(title, 'Mech. PCA'), col=c("grey","red"))
    #pca.mech = pr.mech$scores[, 1:9] # Contains 0.99 Cumulative Proposition
    
    #summary(pr.power)
    biplot(pr.power, cex=0.5, main=paste(title, 'Power PCA'), col=c("grey","red"))
    #warnings()
    #pca.power = pr.power$scores[, 1:2]
    
    par(mfrow = c(1, 1)) 
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
  
  return (list(run=run, draw=draw, control=control, env=env, mech=mech, power=power, run4paper=run4paper))
}

#' Non linear PCA
#'
nlpca <- function(data) {
  library(pcaMethods)  
  
  nlpca.data <- prep(data, scale="none", center=TRUE)
  
  run <- function () {
    # TODO: add code to run NLPCA
    
    resNLPCA <- pca(nlpca.data, method="nlpca", center=FALSE, nPcs=5, maxSteps=300)
    
    
    # homogeneity analysis
    #nlpca.obj <- homals(nlpca.data) # XXX: it returns 'Error in sort.list(y) : 'x' must be atomic for 'sort.list''
  }
  
  return (list(run=run))
}

##
# draw PCA plots for all years of a specific wind turbine.
# @param windturbine.id ID of wind turbine
# @param from the start year of measurement
# @param to the end year of measurement
#
pca_years <- function (windturbine.id, from, to) {
  for (year in seq(from, to)) {
    print (toString(year))
    
    # data loading by module 
    s <- scada(windturbine.id, toString(year))    
    pca.runner <- pca(s)
    
    pca.runner$run()
    
    title <- paste(year, windturbine.id)
    
    pca.runner$draw(title, 2, 2)
    # exception handling if error occurred
    #tryCatch(pca.runner$run(), error=function(err)  print "error", finally=print "finished" )    
  }
}

# PCA plotting of a specific wind turbine for years

pca_years("WTG01", 2014, 2014)

#s <- scada("WTG01", 2014)    
#pca.runner <- pca(s)

#pr.simap <- pca.runner$run4paper()
#summary(pr.simap)
#unclass(pr.simap$loadings)

