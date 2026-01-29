setwd('C:/Users/swanj/Documents/Johnsonetal2026_FuelBreaks_UWR/DEMs/')

#import DEMs
r1 <- rast("USGS_1_n34w117_20190924.tif")
r2 <- rast("USGS_1_n34w118_20190917.tif")
r3 <- rast("USGS_1_n35w118_20190924.tif")
r4 <- rast("USGS_1_n36w121_20210301.tif")

#Mosaic and reproject DEMs
DEM_mos <- mosaic(r1, r2, r3, r4)
DEM_mos <- project(x = DEM_mos, y = "epsg:32611")

#Calculate TPI
mean_neighb_9 <- focal(DEM_mos, w = 9, fun = "mean")

TPI_9 <- DEM_mos - mean_neighb_9

#Save TPI Raster
writeRaster(TPI_9, "9by9TPI.tif", overwrite = TRUE)

