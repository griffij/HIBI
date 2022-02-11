#Some separate plotting functions

library(bayesplot)
library(ggplot2)
library(coda)
library(lattice)
library(ks)
library(grid)
library(gridExtra)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggthemes)
#library(rgeos)


posterior_file = c('../outputs/df_posterior.csv')
df_post = read.csv(posterior_file)

figname = 'plots/posterior_mw.png'  
png(figname, units="in", width=6, height=6, res=300)     
print("Mean mw")
print(mean(df_post$mw))
mw_percentiles = quantile(df_post$mw, probs = c(0.025, 0.26, 0.5, 0.84, 0.975))
print("0.025, 0.26, 0.5, 0.84, 0.975")
print(mw_percentiles)
d = density(df_post$mw)
plot(d)
dev.off()


# Create 2D density grid of location
lon_source = df_post$lon_source
lat_source = df_post$lat_source
loc_data = data.frame(lon_source, lat_source)
kd <- ks::kde(loc_data, gridsize=rep(100,2) , compute.cont=TRUE)
contour_95 =with(kd, contourLines(x=eval.points[[1]], y=eval.points[[2]], 
               z=estimate, levels=cont["5%"])[[1]])
	       
contour_95 = data.frame(contour_95)
contour_25 = with(kd, contourLines(x=eval.points[[1]], y=eval.points[[2]],
	   z=estimate, levels=cont["25%"])[[1]])
contour_25 = data.frame(contour_25) 
contour_50 = with(kd, contourLines(x=eval.points[[1]], y=eval.points[[2]],
	   z=estimate, levels=cont["50%"])[[1]])
contour_50 = data.frame(contour_50)   

world <- ne_countries(scale = "medium", returnclass = "sf")

figname = 'plots/posterior_location.png'

#print(loc_data)
print(length(lon_source))
print(length(lat_source))
print(dim(loc_data))
#ggplot(df_post, aes(x=df_post$lon_source, y=df_post$lat_source)) +
ggplot(loc_data, aes(x=lon_source, y=lat_source)) + 
#  geom_tile(data = d, aes(x = lon, y = lat, fill=x)) +
#  geom_sf(data = world, fill=alpha("lightgrey", 0), color="lightgrey") +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
  geom_path(aes(x=x, y=y), data=contour_95, lwd=0.5) +
  geom_path(aes(x=x, y=y), data=contour_50, lwd=0.5) +
  geom_path(aes(x=x, y=y), data=contour_25, lwd=0.5) +
  scale_fill_distiller(palette="Greys", direction=1) +
  labs(colour = "Density") #+
#  geom_sf(data = world) #, fill=alpha("lightgrey"), color="lightgrey") #+
#  coord_sf(xlim=c(130,150), ylim=c(-40,-30)) + 
#  theme_bw()

dev.off()