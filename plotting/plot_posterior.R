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
library(dplyr)
#library(rgeos)
library(sp)
library(raster)
library(rgdal)

#event = 'Beachport_1897'
#event = 'Warooka_1902'
event = 'Adelaide_1954'
#posterior_file = c('../outputs/df_posterior1902-warooka.csv')
posterior_file = c('../outputs/df_posterior_1954_Adelaide.csv') 
df_post = read.csv(posterior_file)
#print(df_post)
#mmi_datafile = '../data/1897-beachport-ssm.txt'
#mmi_datafile = '../data/1902-warooka-ssm.txt'
mmi_datafile = '../data/1954-adelaide-nehrp.txt'
data = read.csv(mmi_datafile, header=TRUE, sep='\t')

# Get data without NA coordinates
data = filter(data, (data$Latitude !="-NA-") & (data$MMI !="F") & (data$MMI !="L"))

lat_ip = as.numeric(data$Latitude)
lon_ip = as.numeric(data$Longitude)
mmi = as.numeric(data$MMI)

mmi_data = data_frame(lat_ip, lon_ip, mmi)


figname = paste0('plots/posterior_mw_', event, '.png')
png(figname, units="in", width=6, height=6, res=300)     
print("Mean mw")
print(mean(df_post$mw))
print(min(df_post$mw))
mw_percentiles = quantile(df_post$mw, probs = c(0.025, 0.26, 0.5, 0.84, 0.975))
print("0.025, 0.26, 0.5, 0.84, 0.975")
print(mw_percentiles)
d = density(df_post$mw)
#plot(d, main='1897 Beachport', xlab='Mw', ylab='Density')
#plot(d, main='1902 Warooka', xlab='Mw', ylab='Density')
plot(d, main='1954 Adelaide', xlab='Mw', ylab='Density') 
dev.off()

figname = paste0('plots/posterior_depth_', event, '.png')
png(figname, units="in", width=6, height=6, res=300)     
print("Mean depth")
print(mean(df_post$dep))
depth_percentiles = quantile(df_post$dep, probs = c(0.025, 0.26, 0.5, 0.84, 0.975))
print("0.025, 0.26, 0.5, 0.84, 0.975")
print(depth_percentiles)
d = density(df_post$dep)
#plot(d, main='1897 Beachport', xlab='Depth (km)', ylab='Density')
#plot(d, main='1902 Warooka', xlab='Depth (km)', ylab='Density')
plot(d, main='1954 Adelaide', xlab='Depth (km)', ylab='Density')
dev.off()

# Create 2D density grid of location
lon_source = df_post$lon_source
lat_source = df_post$lat_source
loc_data = data.frame(lon_source, lat_source)
kd <- ks::kde(loc_data, gridsize=rep(100,2) , compute.cont=TRUE)

# Create raster of location density
spkde <- image2Grid(list(x = kd$eval.points[[1]], 
                         y = kd$eval.points[[2]], 
                         z = kd$estimate))
r <- raster(spkde)
writeRaster(r,'../outputs/1954_Adelaide_location.tif', overwrite=TRUE)

contour_95 =with(kd, contourLines(x=eval.points[[1]], y=eval.points[[2]], 
               z=estimate, levels=cont["5%"])[[1]])
	       
contour_95 = data.frame(contour_95)
contour_25 = with(kd, contourLines(x=eval.points[[1]], y=eval.points[[2]],
	   z=estimate, levels=cont["75%"])[[1]])
contour_25 = data.frame(contour_25) 
contour_50 = with(kd, contourLines(x=eval.points[[1]], y=eval.points[[2]],
	   z=estimate, levels=cont["50%"])[[1]])
contour_50 = data.frame(contour_50)   

# Create shapefile of percentile contours
pts_sf <- st_as_sf(contour_95, coords = c("x", "y"), crs = 4326)
l95 <- pts_sf %>% st_coordinates() %>% st_linestring()
pts_sf <- st_as_sf(contour_50, coords = c("x", "y"), crs = 4326)
l50 <- pts_sf %>% st_coordinates() %>% st_linestring()
pts_sf <- st_as_sf(contour_25, coords = c("x", "y"), crs = 4326)
l25 <- pts_sf %>% st_coordinates() %>% st_linestring() 

l = st_sfc(l25,l50,l95)
lsf = st_sf(l, crs = 4326)
lsf$percentile=c(25,50,95)
# Write to shapefile
st_write(lsf, '../outputs/shp/Adelaide1954_posterior.shp', driver = "ESRI Shapefile", append=FALSE) 

# Now do our own plotting
world <- ne_countries(country='australia', scale = "large", returnclass = "sf")

figname = paste0('plots/posterior_location_', event, '.png')
png(figname, units="in", width=6, height=6, res=300)

#print(loc_data)
print(length(lon_source))
print(length(lat_source))
print(dim(loc_data))
#ggplot(df_post, aes(x=df_post$lon_source, y=df_post$lat_source)) +
#ggplot(loc_data, aes(x=lon_source, y=lat_source)) + 
ggplot() +
#  geom_tile(data = d, aes(x = lon, y = lat, fill=x)) +
#  geom_sf(data = world, fill=alpha("lightgrey", 0), color="lightgrey") +
#  geom_sf(data = world,  fill=alpha("lightgrey", 0), color="lightgrey") + 
  stat_density_2d(data=loc_data, aes(x=lon_source, y=lat_source, fill = ..density..), geom = "raster", contour = FALSE, h=c(0.05,0.05), n=500) +
  scale_fill_distiller(palette="Greys", direction=1) +
  labs(colour = "Density") +
  geom_path(aes(x=x, y=y), data=contour_95, lwd=0.15) +
  geom_path(aes(x=x, y=y), data=contour_50, lwd=0.3) +
  geom_path(aes(x=x, y=y), data=contour_25, lwd=0.5) +
  geom_sf(data = world,  fill=alpha("lightgrey", 0), color="black", lwd=0.3) +
  geom_point(data = mmi_data, aes(x=lon_ip, y=lat_ip, color=mmi)) +
  scale_colour_distiller(palette='RdYlGn', direction=-1, limits=c(0,8)) +
  labs(colour = "Intensity") +
#  geom_sf(data = world,  fill=alpha("lightgrey", 0), color="black", lwd=0.3) +
#  coord_sf(xlim=c(139,142), ylim=c(-38.5,-35.5)) +
  coord_sf(xlim=c(135,140), ylim=c(-37.5,-33.)) +
  xlab('Longitude') +
  ylab('Latitude') +
#  ggtitle('1897 Beachport') +
#  ggtitle('1902 Warooka') +
  ggtitle('1954 Adelaide') +   
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_bw()

dev.off()