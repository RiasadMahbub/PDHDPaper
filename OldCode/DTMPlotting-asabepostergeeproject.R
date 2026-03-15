#C:\Users\rbmahbub\Documents\Data\DOPDOH-paper\SatelliteData\MTM2020DOPDOH
# source code: https://pjbartlein.github.io/REarthSysSci/rasterVis01.html
library(raster)
library(viridis)
library(rasterVis)
library(sf)
library(tidyverse)
library(ncdf4)
library(ncdf.tools)
library(raster)
library(rasterVis)
library(RColorBrewer)
library(ggplot2)
library(ggthemes)
library(zoo)
library(sf)
library(grid)
# Assuming 'withcelDOH_mean' is your data
# 'mapTheme' is a custom theme, adjust as needed

custom_settings <- list(
  axis.text = list(cex = 1.2),  # Increase axis text size
  axis.title = list(cex = 1.2),  # Increase axis title size
  par.xlab.text = list(cex = 1.2),  # Increase x-axis label size
  par.ylab.text = list(cex = 1.2),  # Increase y-axis label size
  key.text = list(cex = 1.2),  # Increase legend text size
  par.settings = list(
    axis.line = list(col = "black"),  # Customize axis lines
    layout.heights = list(top.padding = 1),  # Adjust top padding
    layout.widths = list(right.padding = 1),  # Adjust right padding
    rasterTheme = rasterTheme(region = brewer.pal(10, "RdBu"))  # Add rasterTheme
  )
)

## read the shapefile
# Read the shapefile
arkansasshape <- st_read("C:/Users/rbmahbub/Documents/Data/GeospatialData/ArkansasShapefile/ArkansasBoundaryShp/ArkansasBoundaryshp.shp")  
# Read the data
withcelDOP_mean <- raster("C:/Users/rbmahbub/Documents/Data/DOPDOH-paper/SatelliteData/MTM2020DOPDOH/withcelDOP_mean.tif")

hist(withcelDOP_mean$DOP_mean_mean)
mapTheme <- rasterTheme(region=brewer.pal(10,"RdBu"))
plt <- levelplot(withcelDOP_mean, margin=F, par.settings=custom_settings, at = seq(100, 150, by = 1), col.regions = colorRampPalette(brewer.pal(10, "RdYlBu")))
plt <- plt + latticeExtra::layer(sp.lines(world_outline, col="gray30", lwd=2))

# Assuming 'plt' is your lattice plot
library(grDevices)
# Set DPI, width, and height
dpi <- 300
width <- 8
height <- 6
png("C:/Users/rbmahbub/Documents/RProjects/DOPDOHYIELD/Figure/PlantingDTM.png", 
    width = width, height = height, units = "in", res = dpi)
print(plt)

dev.off()



withcelDOH_mean <- raster("C:/Users/rbmahbub/Documents/Data/DOPDOH-paper/SatelliteData/MTM2020DOPDOH/withcelDOH_mean.tif")
hist(withcelDOH_mean$DOH_mean_mean)
mapTheme <- rasterTheme()
plt <- levelplot(withcelDOH_mean, margin=F, par.settings=custom_settings, 
                 at = seq(200, 280, by = 1), col.regions = colorRampPalette(brewer.pal(10, "PuOr")),
     
                 ))

plt <- plt + latticeExtra::layer(sp.lines(world_outline, col="gray30", lwd=2))


ggplot2_plot <- as.ggplot(plt)
# Assuming 'plt' is your lattice plot
library(grDevices)
# Set DPI, width, and height
dpi <- 300
width <- 8
height <- 6
png("C:/Users/rbmahbub/Documents/RProjects/DOPDOHYIELD/Figure/HarvestDTM.png", 
    width = width, height = height, units = "in", res = dpi,  pointsize = 12)
print(plt)

dev.off()
# Combine spatial data with the extracted values
#combined_data <- cbind(arkansasshape, extracted_data)

plot(withcelDOP_mean, breaks = 'equal', col = function(n) hcl.colors(n, 'Spectral'), reset = FALSE)
plot(withcelDOP_mean, col = pal)
plot(st_geometry(arkansasshape), add = TRUE)
world_outline <- as(st_geometry(arkansasshape), Class="Spatial")

# plot with outlines, a better color scale, no marginal plots
hist(withcelDOP_mean$DOP_mean_mean)





levelplot(withcelDOP_mean, col.regions = viridis::viridis(20), at = seq(100, 150, length.out = 21))
plot(st_geometry(arkansasshape), add = TRUE)

levelplot(withcelDOP_mean, col.regions = viridis::viridis(20), at = seq(100, 150, length.out = 21), colorkey = FALSE)
plot(st_geometry(arkansasshape), add = TRUE)

# Plot the data
# Change the interval values
withcelDOP_mean <- cut(withcelDOP_mean, breaks = c(0, 1, 2, 3, 4, 5), labels = c("0-1", "1-2", "2-3", "3-4", "4-5"))

# Change the color scheme
pal <- colorRampPalette(c("red", "yellow", "green", "blue", "purple"))(5)
# Change the color scheme
pal <- viridis(5)
# Plot the data
plot(withcelDOP_mean, col = pal)
plot(withcelDOP_mean)

# Add a colorbar
colorbar()

# Convert the data to a vector
withcelDOP_mean_vec <- getValues(withcelDOP_mean)

# Plot the histogram
hist(withcelDOP_mean_vec)



# Assuming 'withcelDOP_mean' is your raster object
levelplot(withcelDOP_mean, col.regions = viridis::viridis(20), at = seq(100, 150, length.out = 21))


ggplot(data = withcelDOP_mean, aes(x = X, y = Y, color = pal)) +
  geom_point() +
  scale_color_manual(values = pal) +
  labs(
    x = "X Axis Label",
    y = "Y Axis Label",
    title = "Your Plot Title"
  ) +
  theme_minimal()


# Read the data
withcelDOP_mean <- raster("withcelDOP_mean.tif")

# Convert the data to a vector
withcelDOP_meandf <- as.data.frame(withcelDOP_mean)

# Convert the data to a data frame
my_df <- as.data.frame(my_raster)

# Plot the data
# Plot the data
ggplot(withcelDOP_meandf, aes(x = 1:nrow(withcelDOP_meandf), y = 1:ncol(withcelDOP_meandf), fill = DOP_mean_mean)) +
  geom_raster()

ggplot() + 
  geom_histogram(data = withcelDOP_meandf, aes(x = DOP_mean_mean)) +
  labs(x = "Day of planting", y = "Frequency", size = 30)+
  scale_x_continuous(expand = c(0, 0), limits = c(0, 250))+
  scale_x_continuous(breaks = seq(0, 250, 20), limits = c(0, 250))+
  theme(axis.text.x  = element_text(size = 18))+
  theme(axis.text.y  = element_text(size = 18))+
  theme(axis.title.x  = element_text(size = 22))+
  theme(axis.title.y  = element_text(size = 22))
  






# Read the data
withcelDOH_mean <- raster("C:/Users/rbmahbub/Documents/Data/DOPDOH-paper/SatelliteData/MTM2020DOPDOH/withcelDOH_mean.tif")

# Plot the data
# Change the interval values
withcelDOH_mean <- cut(withcelDOH_mean, breaks = c(0, 1, 2, 3, 4, 5), labels = c("0-1", "1-2", "2-3", "3-4", "4-5"))

# Change the color scheme
pal <- colorRampPalette(c("red", "yellow", "green", "blue", "purple"))(5)
# Change the color scheme
pal <- viridis(5)
# Plot the data
plot(withcelDOH_mean, col = pal)

withcelDOH_meandf<-as.data.frame(withcelDOH_mean)
ggplot()+ 
geom_histogram(data = withcelDOH_meandf, aes(x = DOH_mean_mean)) +
  labs(x = "Day of Harvest", y = "Frequency"+
  scale_x_continuous(breaks = seq(0, 1, by = 0.2))
  
  
  ggplot() + 
    geom_histogram(data = withcelDOH_meandf, aes(x = DOH_mean_mean)) +
    labs(x = "Day of Harvest", y = "Frequency", size = 30)+
    scale_x_continuous(expand = c(0, 0), limits = c(0, 250))+
    scale_x_continuous(breaks = seq(100, 400, 20), limits = c(100, 400))+
    theme(axis.text.x  = element_text(size = 18))+
    theme(axis.text.y  = element_text(size = 18))+
    theme(axis.title.x  = element_text(size = 22))+
    theme(axis.title.y  = element_text(size = 22))
  


  
