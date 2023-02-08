# Run intensity1 model

library(R2jags)
library(lattice)
library(dplyr)

# Parameter file defines range of prior distributions, i.e.,
# the bounds of the parameter space
paramfile = '../params/1897-beachport-params.txt'

#datafile = '../data/1897-beachport-ssm.txt'
#datafile = '../data/1902-warooka-ssm.txt'
#datafile = '../data/1954-adelaide-nehrp.txt'
#datafile = '../data/1954-adelaide-aees.txt'  
datafile = '../data/1918-QLD-updated.txt'
#name = '1902_warooka'
#name = '1954_Adelaide_aees'
name = '1918_QLD'
weight_data = FALSE#TRUE # Flag for whether to weight data based on quality index
data = read.csv(datafile, header=TRUE, sep='\t')
print(data)
#lats = data$Latitude
# Get data without NA coordinates
# Change EMS to MMI in column name
colnames(data)[3] = 'MMI'
data = filter(data, (data$Latitude !="") & (data$Latitude !="-NA-") & (data$Latitude !="-na-") &
     (data$MMI !="F") & (data$MMI !="L"))

lat_ip = as.numeric(data$Latitude)
lon_ip = as.numeric(data$Longitude)
mmi = as.numeric(data$MMI)
quality = data$Err
print(lat_ip)
print(lon_ip)
print(mmi)
weights = rep(1, length(mmi))
if(weight_data){
	print('Weighting data based on quality index')
	for(i in 1:length(quality)){
		if(quality[i]=="A"){
			weights[i] = 1.#4
#			lat_ip = c(lat_ip, lat_ip[i], lat_ip[i], lat_ip[i]) # Weight by four for A quality
#			lon_ip = c(lon_ip, lon_ip[i], lon_ip[i], lon_ip[i])
#			mmi = c(mmi, mmi[i], mmi[i], mmi[i])
		} else if(quality[i]=="B"){
		        weights[i] = 0.75#3
#		        lat_ip = c(lat_ip, lat_ip[i], lat_ip[i]) # Weight by three for B quality
#			lon_ip = c(lon_ip, lon_ip[i], lon_ip[i])
#			mmi = c(mmi, mmi[i], mmi[i])
		} else if(quality[i]=="C"){
		        weights[i] = 0.5#2
#		        lat_ip = c(lat_ip, lat_ip[i]) # Weight by two for C quality, by one for D quality
#			lon_ip = c(lon_ip, lon_ip[i])
#			mmi = c(mmi, mmi[i])
		} else {weights[i] = 0.25
		}
		}}
mmi
weights
N = length(mmi)
print(quality)
print(N)
print(lon_ip)
sim.data.jags = list("mmi", "lon_ip", "lat_ip", "N", "weights")


# Define the parameters whose posterior distributions we want to calculate
bayes.mod.params <- c("lon_source", "lat_source", "dep", "mw")

#Define starting values
#bayes.mod.inits <- function(){
#                list("lon_source"=110, "lat_source" = -35, "dep" = 10, "mw" = 6.0))
#                             }

bayes.mod.fit <- jags(data = sim.data.jags,
              parameters.to.save = bayes.mod.params, n.chains = 3,
              n.iter = 5000, n.burnin = 1000, model.file = 'aus_ipe_allen.jags')

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
filename = paste0('../outputs/df_posterior_', name, '.csv')
write.csv(df_post, filename, row.names=FALSE)
