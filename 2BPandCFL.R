library(terra)
library(sf)
library(tidyverse)

#Import fuel models for masking
setwd("C:\\Users\\swanj\\Documents\\Research\\fuel_break_systems\\Flammap\\")
brfm <- rast("BR_data/BR_inputs/boundaryridge.tif", lyrs = 4)
lrfm <- rast("LR_data/LR_inputs/limestoneridge/limestoneridge/limestoneridge.tif", lyrs = 4)
mlfm <- rast("ML_data/ML_inputs/mtlowe/mtlowe.tif", lyrs = 4)
nmfm <- rast("NM_data/NM_inputs/northmt/northmt.tif", lyrs = 4)
brfm_in <- rast("BR_data/BR_inputs/BoundaryRidge_Ind/BoundaryRidge_Ind_GR1.tif", lyrs = 4)
brfm_fi <- rast("BR_data/BR_inputs/BoundaryRidge_Firing/BoundaryRidge_Firing_GR1.tif", lyrs = 4)
lrfm_in <- rast("LR_data/LR_inputs/limestoneridge/LimestoneRidge_Ind_GR1/LimestoneRidge_Ind_GR1.tif"
                , lyrs = 4)
lrfm_fi <- rast("LR_data/LR_inputs/limestoneridge/LimestoneRidge_Firing_GR1/LimestoneRidge_Firing_GR1.tif",
                lyrs = 4)
mlfm_in <- rast("ML_data/ML_inputs/Mtlowe_Ind_GR1/Mtlowe_Ind_GR1.tif", lyrs = 4)
mlfm_fi <- rast("ML_data/ML_inputs/Mtlowe_Firing_GR1/Mtlowe_Firing_GR1.tif", lyrs = 4)
nmfm_in <- rast("NM_data/NM_inputs/Northmt_Ind_GR1/Northmt_Ind_GR1.tif", lyrs = 4)
nmfm_fi <- rast("NM_data/NM_inputs/Northmt_Firing_GR1/Northmt_Firing_GR1.tif", lyrs = 4)

#Create CFL Rasters
make_CFL_rasters <- function(x){
  setwd("C:/Users/swanj/Documents/Research/fuel_break_systems/Spatialdata/CFL_BP_rasters/Hazard_rasters/")
  #ReadCSV
  df <- read.csv(x)
  #Create CFL Column, rename columns to intervals
  df <- df %>%
    mutate(CFL = FIL1 * 0.25 + FIL2 * 0.75 + FIL3 * 1.25 +
             FIL4 * 1.75 + FIL5 * 2.25 + FIL6 * 2.75 +
             FIL7 * 3.25 + FIL8 * 3.75 + FIL9 * 4.25  + 
             FIL10 * 4.75 + FIL11 * 5.25 + FIL12 * 5.75 + 
             FIL13 * 6.25 + FIL14 * 6.75 + FIL15 * 7.25 + 
             FIL16 * 7.75 + FIL17 * 8.25 + FIL18 * 8.75 + 
             FIL19 * 9.25 + FIL20 * 9.75) %>%
    setNames(c("XPos", "YPos","PBurn", "< 0.5m",  "0.5-0.9meters",  "1-1.49meters",
               "1.5-1.9meters",  "2-2.49meters",  "2.5-2.9 meters",  "3-3.49meters",  
               "3.5-3.9meters", "4-4.49meters", "4.5-4.9meters", "5-5.49meters", 
               "5.5-5.9meters", "6-6.49meters", "6.5-6.9meters", "7-7.49meters",
               "7.5-7.9meters", "8-8.49meters", "8.5-8.9meters", "9-9.49meters", 
               "9.5+meters", "CFL")) %>%
    select(XPos, YPos, CFL)
  
  #Setting mask file name
  treatment_prefix <- ifelse(substr(x,4,5) == 'FB', '_fb',
                             ifelse(substr(x,4,5) == "IN", "_in",
                                    ifelse(substr(x,4,5) == 'FI', "_fi",
                                           "")))
  
  landscape <- ifelse(substr(x,1,2) == "LR", "lrfm",
                      ifelse(substr(x,1,2) == "ML", 'mlfm',
                             ifelse(substr(x,1,2) == "BR", 'brfm',
                                    "nmfm")))
  
  mask <- get(str_flatten(landscape, treatment_prefix))
  
  xy <- df %>% select(XPos, YPos) %>% as.matrix()
  
  #Turning XY coords into rasters, setting extent, crs, and masking unburnable surfaces
  b <- rast(ext(mask), res = res(mask), crs = crs(mask))
  
  # Rasterize the XYZ table onto `b`
  b <- rasterize(x = xy, y = b, df$CFL, background = NA)
  
  # Apply mask to remove unwanted values
  b <- mask(b, mask, maskvalues = c(-9999, 91, 92, 93, 98, 99))
  
  #Masking -Inf values
  b[b == -Inf,] <- NA
  
  # Get the current raster extent
  b_ext <- ext(b)
  
  # Define the inward crop distance 
  crop_x <- ifelse(str_detect(x, "LR"), 7500, 2000)
  crop_ymin <- ifelse(str_detect(x, "LR"), 8500, 2000)
  crop_ymax <- ifelse(str_detect(x, "LR"), 4500, 2000)
  
  # Compute the new extent by reducing 2 km from all sides
  new_extent <- ext(b_ext[1] + crop_x,  # New xmin
                    b_ext[2] - crop_x,  # New xmax
                    b_ext[3] + crop_ymin,  # New ymin
                    b_ext[4] - crop_ymax)
  
  # Crop the raster to the new extent
  b <- crop(b, new_extent)
  
  #update name
  names(b) <- "CFL"
  
  #Set wd for saving rasters
  setwd("C:/Users/swanj/Documents/Research/fuel_break_systems/Spatialdata/CFL_BP_rasters/JohnsEdits_Final/CFL/")
  
  #Saving rasters and assignting to the environment
  terra::writeRaster(b, filename=paste0(substr(x, 1, 6), "CFL.tif"), 
                     overwrite=TRUE)
}

#Run function on all FLP csvs
setwd("C:/Users/swanj/Documents/Research/fuel_break_systems/Spatialdata/CFL_BP_rasters/Hazard_rasters/")
FLP_csvs <- list.files(pattern = "FLPm")
lapply(FLP_csvs, make_CFL_rasters)

#Create BP Rasters
make_BP_rasters <- function(x){
  setwd("C:/Users/swanj/Documents/Research/fuel_break_systems/Spatialdata/CFL_BP_rasters/Hazard_rasters/")
  #ReadCSV
  df <- read.csv(x)
  #Create CFL Column, rename columns to intervals
  df <- df %>%
    select(XPos, YPos, PBurn)
  
  #Setting mask file name
  treatment_prefix <- ifelse(substr(x,4,5) == 'FB', '_fb',
                             ifelse(substr(x,4,5) == "IN", "_in",
                                    ifelse(substr(x,4,5) == 'FI', "_fi",
                                           "")))
  
  landscape <- ifelse(substr(x,1,2) == "LR", "lrfm",
                      ifelse(substr(x,1,2) == "ML", 'mlfm',
                             ifelse(substr(x,1,2) == "BR", 'brfm',
                                    "nmfm")))
  
  mask <- get(str_flatten(landscape, treatment_prefix))
  
  xy <- df %>% select(XPos, YPos) %>% as.matrix()
  
  #Turning XY coords into rasters, setting extent, crs, and masking unburnable surfaces
  b <- rast(ext(mask), res = res(mask), crs = crs(mask))
  
  # Rasterize the XYZ table onto `b`
  b <- rasterize(x = xy, y = b, df$PBurn, background = NA)
  
  # Apply mask to remove unwanted values
  b <- mask(b, mask, maskvalues = c(-9999, 91, 92, 93, 98, 99))
  
  #Masking -Inf values
  b[b == -Inf,] <- NA
  
  # Get the current raster extent
  b_ext <- ext(b)
  
  # Define the inward crop distance 
  crop_x <- ifelse(str_detect(x, "LR"), 7500, 2000)
  crop_ymin <- ifelse(str_detect(x, "LR"), 8500, 2000)
  crop_ymax <- ifelse(str_detect(x, "LR"), 4500, 2000)
  
  # Compute the new extent by reducing 2 km from all sides
  new_extent <- ext(b_ext[1] + crop_x,  # New xmin
                    b_ext[2] - crop_x,  # New xmax
                    b_ext[3] + crop_ymin,  # New ymin
                    b_ext[4] - crop_ymax)
  
  # Crop the raster to the new extent
  b <- crop(b, new_extent)
  
  #update name
  names(b) <- "Burn_Probability"
  
  #Set wd for saving rasters
  setwd("C:/Users/swanj/Documents/Research/fuel_break_systems/Spatialdata/CFL_BP_rasters/JohnsEdits_Final/BP/")
  
  #Saving rasters and assignting to the environment
  terra::writeRaster(b, filename=paste0(substr(x, 1, 6), "BP.tif"), 
                     overwrite=TRUE)
}

#Run function on all FLP csvs
setwd("C:/Users/swanj/Documents/Research/fuel_break_systems/Spatialdata/CFL_BP_rasters/Hazard_rasters/")
FLP_csvs <- list.files(pattern = "FLPm")
lapply(FLP_csvs, make_BP_rasters)
