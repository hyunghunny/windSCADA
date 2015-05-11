# load data
source('./codes/dataloader.R')
#windScada <- load_scada_data('WTG01', '2014')
s <- scada()
windScada = s$data('WTG01', '2014')

# clustering by category
windScada.control <- s$control()
names(windScada.control)

windScada.env <- s$env()
names(windScada.env)

windScada.mech <- s$mech()
names(windScada.mech)


windScada.power <- s$power()
names(windScada.power)


# select a subset clustered by category
#subset <- windScada.control
subset <- windScada.env 
#subset <- windScada.mech
#subset <- windScada.power

names(subset)

subset <- scale(subset) # normalize it (only env value can be mornalized)

# Principal Component Analysis
pr.control <- princomp(windScada.control) 
summary(pr.control)
biplot(pr.control)
warnings()
pca.control = pr.control$scores[, 1] # Contains 0.99 Cumulative Proposition

pr.env <- princomp(subset) 
summary(pr.env)
biplot(pr.env)
warnings()
pca.env = pr.env$scores[, 1:3] # Contains 0.99 Cumulative Proposition

pr.mech <- princomp(windScada.mech) 
summary(pr.mech)
biplot(pr.mech)
warnings()
pca.mech = pr.mech$scores[, 1:9] # Contains 0.99 Cumulative Proposition

pr.power <- princomp(windScada.power) 
summary(pr.power)
biplot(pr.power)
warnings()
pca.power = pr.power$scores[, 1:2]

# TODO: what CAN be done with PCA features?
