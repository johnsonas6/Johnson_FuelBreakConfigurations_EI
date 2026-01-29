library(terra)
library(gtools)


##Create Uncontrollable Wildfire Risk rasters

#Import fuel model inputs to mask out unburnable fuel models

#Set directory with fuel models
setwd("C:/Users/swanj/Documents/Johnsonetal2026_FuelBreaks_UWR/Flammap/")

#Import fuel models
brfm <- rast("BR_data/BR_inputs/boundaryridge/boundaryridge.tif", lyrs = 4)
lrfm <- rast("LR_data/LR_inputs/limestoneridge/limestoneridge.tif", lyrs = 4)
mlfm <- rast("ML_data/ML_inputs/mtlowe/mtlowe.tif", lyrs = 4)
nmfm <- rast("NM_data/NM_inputs/northmt/northmt.tif", lyrs = 4)
brfm_in <- rast("BR_data/BR_inputs/BoundaryRidge_Ind_GR1/BoundaryRidge_Ind_GR1.tif", lyrs = 4)
brfm_fi <- rast("BR_data/BR_inputs/BoundaryRidge_Firing_GR1//BoundaryRidge_Firing_GR1.tif", lyrs = 4)
lrfm_in <- rast("LR_data/LR_inputs/LimestoneRidge_Ind_GR1/LimestoneRidge_Ind_GR1.tif"
                , lyrs = 4)
lrfm_fi <- rast("LR_data/LR_inputs/LimestoneRidge_Firing_GR1/LimestoneRidge_Firing_GR1.tif",
                lyrs = 4)
mlfm_in <- rast("ML_data/ML_inputs/Mtlowe_Ind_GR1/Mtlowe_Ind_GR1.tif", lyrs = 4)
mlfm_fi <- rast("ML_data/ML_inputs/Mtlowe_Firing_GR1/Mtlowe_Firing_GR1.tif", lyrs = 4)
nmfm_in <- rast("NM_data/NM_inputs/Northmt_Ind_GR1/Northmt_Ind_GR1.tif", lyrs = 4)
nmfm_fi <- rast("NM_data/NM_inputs/Northmt_Firing_GR1/Northmt_Firing_GR1.tif", lyrs = 4)

##Calculate Hazard Weights

#Plotting breakpoints of suppression effectiveness
#X is flame lengths or fireline intensity
#Byram's equation is FL in meters = 0.0775*FLI(KW/m) ^ 0.46
#1000 1.86m- handcrew, 
#2000 2.55m- Retardent drops hold fire ~1 hr (ground and air), Budd et al. and Loane and Gould
#3000 3.08m- Budd et al. hypothesize that air tanker drops and ground forces could contain this
#5000 3.9m- Aircraft and handcrews SOMETIMES succesful when working together, Budd et al.
#6700 4.49m - Upper end of aircraft suppression failing Plucinski MP. Evaluation of the effectiveness of the 10 tanker air carrier DC-10 air tanker, Victoria 2010. Technical Report. East Melbourne: Bushfire Cooperative Research Centre; 2010.from Burgan 1979 and Plucinski 2019
x1 <- c(0.25, 1.86, 2.55, 3.08, 3.9, 4.49, 9.75)
#Y is a measure of suppression difficulty 
#reflective of the number and type of resources needed, implications about their
#availability and impact 
y1 <- c(1, 5, 15, 20, 30, 40, 49.999)

#Making a population growth model to assign weights to FIL categories
data <- data.frame(x1,y1)

coef(lm(logit(y1/50)~x1))

#(Intercept)      x 
#-5.062307    1.572869  

#Making population growth model from coefficients using non-linear least squares
weights <-nls(y1~phi1/(1+exp(-(phi2+phi3*x1))),
              start=list(phi1=50, phi2=-5.062307, phi3=1.572869 ), trace=TRUE)

#
summary(weights)

phi1<-coef(weights)[1]
phi2<-coef(weights)[2]
phi3<-coef(weights)[3]
x2 <-c(min(data$x1):max(data$x1)) # construct a range of x values bounded by the data
y2 <-phi1/(1+exp(-(phi2+phi3*x2))) #predicted weights
predict<-data.frame(x2,y2) #create the prediction data frame

FIL <- c(0.25, 0.75, 1.25, 1.75, 2.25, 2.75, 3.25, 3.75, 4.25, 4.75, 5.25, 5.75, 6.25, 6.75, 7.25, 
         7.75, 8.25, 8.75, 9.25, 9.75)
weights <- phi1/(1+exp(-(phi2+phi3*FIL)))

#create data frame of flame length weights
predicted_weights <- data.frame(FIL, weights)

#FIL categories weighted by their ability to be suppressed (see Plucinski 2019 and Burgan 1979 for citations)
#Function to create weighted CFL rasters by including weight derived from a population model (above)
#and with each FIL multiplied by burn likelihood at each cell
make_UWR_rasters <- function(x){
  setwd("C:/Users/swanj/Documents/Johnsonetal2026_FuelBreaks_UWR/Flammap/FLP_files/")
  #ReadCSV
  df <- read.csv(x)
  #Create CFL Column, rename columns to intervals
  df <- df %>%
    mutate(UWR = log((FIL1 * 0.25 * predicted_weights[1,2] + FIL2 * 0.75 * predicted_weights[2,2] + 
                        
                        FIL3 * 1.25 * predicted_weights[3,2] + FIL4 * 1.75 * predicted_weights[4,2] + 
                        
                        FIL5 * 2.25 * predicted_weights[5,2] + FIL6 * 2.75 * predicted_weights[6,2] + 
                        
                        FIL7 * 3.25 * predicted_weights[7,2] + FIL8 * 3.75 * predicted_weights[8,2] + 
                        
                        FIL9 * 4.25 * predicted_weights[9,2] + FIL10 * 4.75 * predicted_weights[10,2] + 
                        
                        FIL11 * 5.25 * predicted_weights[11,2] + FIL12 * 5.75 * predicted_weights[12,2] + 
                        
                        FIL13 * 6.25 * predicted_weights[13,2] + FIL14 * 6.75 * predicted_weights[14,2] + 
                        
                        FIL15 * 7.25 * predicted_weights[15,2] + FIL16 * 7.75 * predicted_weights[16,2] + 
                        
                        FIL17 * 8.25 * predicted_weights[17,2] + FIL18 * 8.75 * predicted_weights[18,2] + 
                        
                        FIL19 * 9.25 * predicted_weights[19,2] + FIL20 * 9.75 * predicted_weights[20,2]) * PBurn)) %>%
    mutate(UWR = UWR - min(UWR[UWR > -10000], na.rm = TRUE) + 0.0001) %>% #Add a constant value so UWR starts above 0
    setNames(c("XPos", "YPos","PBurn", "< 0.5m",  "0.5-0.9meters",  "1-1.49meters",
               "1.5-1.9meters",  "2-2.49meters",  "2.5-2.9 meters",  "3-3.49meters",  
               "3.5-3.9meters", "4-4.49meters", "4.5-4.9meters", "5-5.49meters", 
               "5.5-5.9meters", "6-6.49meters", "6.5-6.9meters", "7-7.49meters",
               "7.5-7.9meters", "8-8.49meters", "8.5-8.9meters", "9-9.49meters", 
               "9.5+meters", "UWR")) %>%
    select(XPos, YPos, UWR)
  
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
  b <- rasterize(x = xy, y = b, df$UWR, background = NA)
  
  # Apply mask to remove unwanted values
  b <- mask(b, mask, maskvalues = c(-9999, 91, 92, 93, 98, 99))
  
  #Masking -Inf values
  b[b == -Inf,] <- NA
  
  # Get the current raster extent
  b_ext <- ext(b)
  
  # Define a different inward crop distance for LR landscape to decrease differences in area treated between landscapes
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
  names(b) <- "UWR"
  
  #Set wd for saving rasters
  setwd("C:/Users/swanj/Documents/Johnsonetal2026_FuelBreaks_UWR/Outputs/raster/UWR/")
  
  #Changing file name inconsistency for firing landscapes
  forpaste <- ifelse(substr(x,4,5) == "FR", paste0(substr(x,1,3), "FI_"), substr(x,1,6))
  
  #Saving rasters and assignting to the environment
  terra::writeRaster(b, filename=paste0(forpaste, "hazard.tif"), 
                     overwrite=TRUE)
  assign(paste0(forpaste, "haz"), b, envir = .GlobalEnv)
}

#Run function on all FLP csvs
UWR_csvs <- list.files(path = "C:/Users/swanj/Documents/Johnsonetal2026_FuelBreaks_UWR/Flammap/FLP_files/", pattern = ".csv")
lapply(UWR_csvs, make_UWR_rasters)

#Clear environment
rm(list = ls())