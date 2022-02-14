# Run intensity1 model

library(R2jags)
library(lattice)
library(dplyr)

datafile = '../data/1897-beachport-ssm.txt'
data = read.csv(datafile, header=TRUE, sep='\t')
print(data)
#lats = data$Latitude
# Get data without NA coordinates
data = filter(data, (data$Latitude !="-NA-") & (data$MMI !="F") & (data$MMI !="L"))

lat_ip = as.numeric(data$Latitude)
lon_ip = as.numeric(data$Longitude)
mmi = as.numeric(data$MMI)
#print(lat_ip)
#print(lon_ip)
#print(mmi)
N = length(mmi)
print(N)
sim.data.jags = list("mmi", "lon_ip", "lat_ip", "N")

# Define the parameters whose posterior distributions we want to calculate
bayes.mod.params <- c("lon_source", "lat_source", "dep", "mw")

#Define starting values
#bayes.mod.inits <- function(){
#                list("lon_source"=110, "lat_source" = -35, "dep" = 10, "mw" = 6.0))
#                             }

bayes.mod.fit <- jags(data = sim.data.jags,
              parameters.to.save = bayes.mod.params, n.chains = 3,
              n.iter = 10000, n.burnin = 1000, model.file = 'aus_ipe_allen.jags')

print(bayes.mod.fit)

# Convert to an MCMC object
bayes.mod.fit.mcmc <- as.mcmc(bayes.mod.fit)
summary(bayes.mod.fit.mcmc)

# Somore more plots
xyplot(bayes.mod.fit.mcmc, layout=c(2,2), aspect="fill")

# Density plot
densityplot(bayes.mod.fit.mcmc, layout=c(2,2), aspect="fill")

#Auto-correlation plot
autocorr.plot(bayes.mod.fit.mcmc)

dev.off()

# Dump data to file 
df_post = do.call(rbind.data.frame, bayes.mod.fit.mcmc)
filename = paste0('../outputs/df_posterior.csv')
write.csv(df_post, filename, row.names=FALSE)
