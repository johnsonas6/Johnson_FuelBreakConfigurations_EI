library(terra)
library(sf)
library(tidyverse)
library(gstat)
library(gtools)
library(caret)
library(MuMIn)

##Point sampling with crs of original rasters from UWR
#make list of firing rasters to plug into function

setwd("C:/Users/swanj/Documents/Johnsonetal2026_FuelBreaks_UWR/Outputs/raster/UWR/")

#Get list of firing landscapes for sampling
sampling_tifs <- list.files(pattern = "FI_UWR.tif")

sampling_tifs <-  sampling_tifs[!grepl(x = sampling_tifs, pattern = "xml")]

#Extract points at 120m grid spacing. 
point_samples <- lapply(sampling_tifs, function(file_path) {
  # Read the raster file
  r <- rast(file_path)
  
  # Get raster extent and resolution
  ext <- ext(r)  # Get extent
  res_x <- res(r)[1]  # X resolution (30m)
  res_y <- res(r)[2]  # Y resolution (30m)
  
  # Define 120m spacing
  spacing <- 120
  
  # Generate regular grid coordinates
  x_seq <- seq(ext[1], ext[2], by = spacing)  # X-coordinates
  y_seq <- seq(ext[3], ext[4], by = spacing)  # Y-coordinates
  
  # Create grid of points
  grid_points <- expand.grid(x = x_seq, y = y_seq)
  
  # Convert to sf points
  grid_sf <- st_as_sf(grid_points, coords = c("x", "y"), crs = crs(r))
  
  # Extract raster values at grid points
  values <- terra::extract(r, grid_sf, ID = FALSE)
  
  grid_sf <- cbind(grid_sf, values)
  
  # Remove NA values if needed
  grid_sf <- grid_sf[!is.na(grid_sf$UWR), ]
  
  # Final subsampled points
  b <- grid_sf$geometry
  
  # Create the output file path
  output_path <- paste0("C:/Users/swanj/Documents/Johnsonetal2026_FuelBreaks_UWR/Outputs/data_frames/", 
                        paste0(substr(file_path,1,2), "_point_samples.shp"))
  
  # Write the sampled points to a shapefile
  st_write(obj = b, dsn = output_path, overwrite = TRUE, append = FALSE)
})

rm(point_samples)

setwd("C:/Users/swanj/Documents/Johnsonetal2026_FuelBreaks_UWR/Outputs/data_frames/")

BR_points <- st_read("BR_point_samples.shp")
LR_points <- st_read("LR_point_samples.shp")
NM_points <- st_read("NM_point_samples.shp")
ML_points <- st_read("ML_point_samples.shp")

BR_points$landscape <- "BR"
LR_points$landscape <- "LR"
NM_points$landscape <- "NM"
ML_points$landscape <- "ML"


#Function to use points to sample UWR rasters
sample_raster <- function(x) {
  setwd("C:/Users/swanj/Documents/Johnsonetal2026_FuelBreaks_UWR/Outputs/raster/")
  
  r <- rast(x)
  
  newcolumn <- ifelse(str_detect(x, "FB"), 
                      yes = "FB_haz",
                      no = ifelse(str_detect(x, "IN"),
                                  yes = "IN_haz",
                                  no = ifelse(str_detect(x, "UT"), 
                                              yes = "UT_haz",
                                              no= "FI_haz")))
  
  oldcolumn <- 'UWR'
  
  
  #Conditional matching of file names
  if (substr(x, 1, 2) == "BR") {
    
    #create sf object from SpatVector of extracted data
    table <- st_as_sf(terra::extract(r, BR_points, xy = TRUE, ID = FALSE, weights = FALSE, 
                                     exact = FALSE, touches = FALSE), 
                      coords = c('x', 'y'), remove = TRUE)
    
    BR_points[[newcolumn]] <- table[[oldcolumn]]
    
  } else if (substr(x, 1, 2) == "LR") {
    
    #create sf object from SpatVector of extracted data
    table <- st_as_sf(terra::extract(r, LR_points, xy = TRUE, ID = FALSE, 
                                     exact = FALSE, touches = FALSE), 
                      coords = c('x', 'y'), remove = TRUE)
    
    LR_points[[newcolumn]] <- table[[oldcolumn]]
    
  } else if (substr(x, 1, 2) == "NM") {
    
    #create sf object from SpatVector of extracted data
    table <- st_as_sf(terra::extract(r, NM_points, xy = TRUE, ID = FALSE, 
                                     exact = FALSE, touches = FALSE), 
                      coords = c('x', 'y'), remove = TRUE)
    
    NM_points[[newcolumn]] <- table[[oldcolumn]]
    
  } else if (substr(x, 1, 2) == "ML") {
    
    #create sf object from SpatVector of extracted data
    table <- st_as_sf(terra::extract(r, ML_points, xy = TRUE, ID = FALSE, 
                                     exact = FALSE, touches = FALSE), 
                      coords = c('x', 'y'), remove = TRUE)
    
    
    ML_points[[newcolumn]] <- table[[oldcolumn]]
    
  }
  
  BR_points <- BR_points %>% drop_na()
  LR_points <- LR_points %>% drop_na()
  ML_points <- ML_points %>% drop_na()
  NM_points <- NM_points %>% drop_na()
  
  assign('BR_points', BR_points, envir = .GlobalEnv)
  assign('LR_points', LR_points, envir = .GlobalEnv)
  assign('NM_points', NM_points, envir = .GlobalEnv)
  assign('ML_points', ML_points, envir = .GlobalEnv)
  
}


setwd("C:/Users/swanj/Documents/Johnsonetal2026_FuelBreaks_UWR/Outputs/raster/")

raster_list <- list.files(pattern = ".tif")

raster_list <- raster_list[!grepl(x = raster_list, pattern = "xml")]

lapply(raster_list, sample_raster)

##SAMPLE FUEL MODEL AND SLOPE
setwd("C:/Users/swanj/Documents/Johnsonetal2026_FuelBreaks_UWR/Flammap/")

LR_UT_FM <- rast("LR_data/LR_inputs/limestoneridge/limestoneridge/limestoneridge.tif", lyr = 4)
LR_FB_FM <- rast("LR_data/LR_inputs/limestoneridge/LimestoneRidge_FB_GR1/LimestoneRidge_FB_GR1.tif", lyr = 4)
LR_IN_FM <- rast("LR_data/LR_inputs/limestoneridge/LimestoneRidge_Ind_GR1/LimestoneRidge_Ind_GR1.tif", lyr = 4)
LR_FI_FM <- rast("LR_data/LR_inputs/limestoneridge/LimestoneRidge_Firing_GR1/LimestoneRidge_Firing_GR1.tif", lyr = 4)
LR_SL <- rast("LR_data/LR_inputs/limestoneridge/limestoneridge/limestoneridge.tif", lyr = 2)
BR_UT_FM <- rast("BR_data/BR_inputs/boundaryridge.tif", lyr = 4)
BR_FB_FM <- rast("BR_data/BR_inputs/BoundaryRidge_FB_GR1.tif", lyr = 4)
BR_IN_FM <- rast("BR_data/BR_inputs/BoundaryRidge_Ind/BoundaryRidge_Ind_GR1.tif", lyr = 4)
BR_FI_FM <- rast("BR_data/BR_inputs/BoundaryRidge_Firing/BoundaryRidge_Firing_GR1.tif", lyr = 4)
BR_SL <- rast("BR_data/BR_inputs/boundaryridge.tif", lyr = 2)
ML_UT_FM <- rast("ML_data/ML_inputs/mtlowe/mtlowe.tif", lyr = 4)
ML_FB_FM <- rast("ML_data/ML_inputs/Mtlowe_FB_GR1/Mtlowe_FB_GR1.tif", lyr = 4)
ML_IN_FM <- rast("ML_data/ML_inputs/Mtlowe_Ind_GR1/Mtlowe_Ind_GR1.tif", lyr = 4)
ML_FI_FM <- rast("ML_data/ML_inputs/Mtlowe_Firing_GR1/Mtlowe_Firing_GR1.tif", lyr = 4)
ML_SL <- rast("ML_data/ML_inputs/mtlowe/mtlowe.tif", lyr = 2)
NM_UT_FM <- rast("NM_data/NM_inputs/northmt/northmt.tif", lyr = 4)
NM_FB_FM <- rast("NM_data/NM_inputs/northmt_fuelbreak_GR1/northmt_fuelbreak_GR1.tif", lyr = 4)
NM_IN_FM <- rast("NM_data/NM_inputs/Northmt_Ind_GR1/Northmt_Ind_GR1.tif", lyr = 4)
NM_FI_FM <- rast("NM_data/NM_inputs/Northmt_Firing_GR1/Northmt_Firing_GR1.tif", lyr = 4)
NM_SL <- rast("NM_data/NM_inputs/northmt/northmt.tif", lyr = 2)

raster_list <- c('LR_FB_FM', 'LR_FI_FM', 'LR_IN_FM', 'LR_SL', 'LR_UT_FM', 'BR_FB_FM', 'BR_FI_FM', 
                 'BR_IN_FM', 'BR_SL', 'BR_UT_FM', 'NM_FB_FM', 'NM_FI_FM', 'NM_IN_FM', 'NM_SL', 
                 'NM_UT_FM', 'ML_FB_FM', 'ML_FI_FM', 'ML_IN_FM', 'ML_SL', 'ML_UT_FM')

sample_raster <- function(raster_list) {
  
  newcolumn <- ifelse(str_length(raster_list) == 5, 
                      yes = "Slope",
                      no = paste0(substr(raster_list, 4, 5), "_FM"))
  
  oldcolumn <- ifelse(str_length(raster_list) == 5, 
                      yes = 'Slope',
                      no = 'Fuel Model')
  
  r <- get(raster_list)
  
  #Conditional matching of file names
  if (substr(raster_list, 1, 2) == "BR") {
    
    #create sf object from SpatVector of extracted data
    table <- st_as_sf(terra::extract(r, BR_points, xy = TRUE, ID = FALSE), 
                      coords = c('x', 'y'), remove = TRUE, crs = st_crs("epsg:32611"))
    
    BR_points[[newcolumn]] <- table[[oldcolumn]]
    
  } else if (substr(raster_list, 1, 2) == "LR") {
    
    #create sf object from SpatVector of extracted data
    table <- st_as_sf(terra::extract(r, LR_points, xy = TRUE, ID = FALSE), 
                      coords = c('x', 'y'), remove = TRUE, crs = st_crs("epsg:32611"))
    
    LR_points[[newcolumn]] <- table[[oldcolumn]]
    
  } else if (substr(raster_list, 1, 2) == "NM") {
    
    #create sf object from SpatVector of extracted data
    table <- st_as_sf(terra::extract(r, NM_points, xy = TRUE, ID = FALSE), 
                      coords = c('x', 'y'), remove = TRUE, crs = st_crs("epsg:32611"))
    
    NM_points[[newcolumn]] <- table[[oldcolumn]]
    
  } else if (substr(raster_list, 1, 2) == "ML") {
    
    #create sf object from SpatVector of extracted data
    table <- st_as_sf(terra::extract(r, ML_points, xy = TRUE, ID = FALSE), 
                      coords = c('x', 'y'), remove = TRUE, crs = st_crs("epsg:32611"))
    
    
    ML_points[[newcolumn]] <- table[[oldcolumn]]
    
  }
  
  assign('BR_points', BR_points, envir = .GlobalEnv)
  assign('LR_points', LR_points, envir = .GlobalEnv)
  assign('NM_points', NM_points, envir = .GlobalEnv)
  assign('ML_points', ML_points, envir = .GlobalEnv)
  
}



# Apply function to raster list
lapply(raster_list, sample_raster)

#Distance, direction, width

rm(list = ls()[!grepl("BR_points|LR_points|NM_points|ML_points", ls())])

##transforming point files to UTM for distance calculations
BR_points_t <- st_transform(BR_points, crs = "epsg:32611")
LR_points_t <- st_transform(LR_points, crs = "epsg:32611")
ML_points_t <- st_transform(ML_points, crs = "epsg:32611")
NM_points_t <- st_transform(NM_points, crs = "epsg:32611")

#combine files
master_points <- rbind(BR_points_t, LR_points_t, ML_points_t, NM_points_t)

rm(list = ls()[!grepl("master_points", ls())])

#read in master polygon files
setwd("C:/Users/swanj/Documents/Johnsonetal2026_FuelBreaks_UWR/Fuelbreak_polygons/")
master_fi_polygons <- st_read("master_fi_merged_best.shp")
master_fb_polygons <- st_read("master_fb_polygons.shp")

#calculating azimuth from point to fuel break, requires data frames to be in environment

# Initialize a vector to store the azimuths
fb_azimuths <- numeric(nrow(master_points))
fi_azimuths <- numeric(nrow(master_points))

# Vector of nearest polygon index attachd to points df
master_points$nearest_idx_fb <- st_nearest_feature(master_points, master_fb_polygons)
master_points$nearest_idx_fi <- st_nearest_feature(master_points, master_fi_polygons)

# Iterate over each point
for (i in 1:nrow(master_points)) {
  point <- master_points[i, ]
  
  # Isolate the nearest polygon
  nearest_fb_polygon <- master_fb_polygons[point$nearest_idx_fb, ]
  nearest_fi_polygon <- master_fi_polygons[point$nearest_idx_fi, ]
  
  #linestring between point and nearest point on polygon
  nearest_line_fb <- st_nearest_points(point, master_fb_polygons[nearest_fb_polygon,])
  nearest_points_fb <-  st_coordinates(nearest_line_fb)
  deltas_fb <- nearest_points_fb[2,][1:2] - nearest_points_fb[1,][1:2]
  deltaE_fb <- as.numeric(deltas_fb[1])
  deltaN_fb <- as.numeric(deltas_fb[2])
  
  azimuth_fb <- atan2(deltaE_fb, deltaN_fb) * 180 / pi  # Updated to use atan2 for better quadrant handling
  
  # Correct azimuth for negative angles
  if (azimuth_fb < 0) azimuth_fb <- azimuth_fb + 360
  
  # Store the closest azimuth in the vector
  fb_azimuths[i] <- azimuth_fb
  
  nearest_line_fi <- st_nearest_points(point, master_fi_polygons[nearest_fi_polygon,])
  nearest_points_fi <-  st_coordinates(nearest_line_fi)
  deltas_fi <- nearest_points_fi[2,][1:2] - nearest_points_fi[1,][1:2]
  deltaE_fi <- as.numeric(deltas_fi[1])
  deltaN_fi <- as.numeric(deltas_fi[2])
  
  azimuth_fi <- atan2(deltaE_fi, deltaN_fi) * 180 / pi  # Updated to use atan2 for better quadrant handling
  
  # Correct azimuth for negative angles
  if (azimuth_fi < 0) azimuth_fi <- azimuth_fi + 360
  
  # Store the closest azimuth in the vector
  fi_azimuths[i] <- azimuth_fi
}

# Add the azimuths to the points data frame
master_points$fb_az <- fb_azimuths
master_points$fi_az <- fi_azimuths

#Save udated points df
path <- "C:/Users/swanj/Documents/Johnsonetal2026_FuelBreaks_UWR/Outputs/data_frames/"

st_write(master_points, dsn = paste0(path, "master_points_az.shp"), overwrite = TRUE, append = FALSE)

#create variable for alignment of angle from fuel break to point with wind direction
#1 indicates alignment, -1 incdicates 180 degrees apart, 0 indicates orthogonal 
master_points <- master_points %>% mutate(wnd_fb = case_when(
  landscape == "BR" ~ cos((fb_az - 225) / 180 * pi),
  landscape == "LR" ~ cos((fb_az - 225) / 180 * pi),
  landscape == "NM" ~ cos((fb_az - 225) / 180 * pi),
  landscape == "ML" ~ cos((fb_az - 315) / 180 * pi),
), wnd_fi = case_when(
  landscape == "BR" ~ cos((fi_az - 225) / 180 * pi) ,
  landscape == "LR" ~ cos((fi_az - 225) / 180 * pi),
  landscape == "NM" ~ cos((fi_az - 225) / 180 * pi),
  landscape == "ML" ~ cos((fi_az - 315) / 180 * pi),
))

master_points <- master_points %>% select(-c(nearest_idx_fb, nearest_idx_fi))

st_write(obj = master_points, dsn = "C:/Users/swanj/Documents/Johnsonetal2026_FuelBreaks_UWR/Outputs/data_frames/master_points_haz.shp", 
         overwrite = TRUE, append = FALSE)

#Function to calculate distance from poing to treatment
calculate_distance <- function(master_points) {
  
  # Calculate distances
  distances_fb <- as.data.frame(st_distance(master_points, master_fb_polygons))
  master_points$fb_dis <- as.double(apply(distances_fb, 1, min))
  
  distances_fi <- as.data.frame(st_distance(master_points, master_fi_polygons))
  master_points$fi_dis <- as.double(apply(distances_fi, 1, min))
  
  assign("master_points", master_points, envir = .GlobalEnv)
  
}

calculate_distance(master_points)

st_write(obj = master_points, dsn = "C:/Users/swanj/Documents/Johnsonetal2026_FuelBreaks_UWR/Outputs/data_frames/master_points_haz.shp", 
         overwrite = TRUE, append = FALSE)

#Disaggregate all fb polygons
master_fb_polygons_dis <- st_cast(master_fb_polygons, to = "POLYGON")

#Fix bad row names
rownames(master_fb_polygons_dis) <- seq_len(nrow(master_fb_polygons_dis))
master_fb_polygons_dis$FID <- seq_len(nrow(master_fb_polygons_dis))

#Disaggregate all fi polygons
master_fi_polygons_dis <- st_cast(master_fi_polygons, to = "POLYGON")

#Fix bad row names
rownames(master_fi_polygons_dis) <- seq_len(nrow(master_fi_polygons_dis))
master_fi_polygons_dis$FID <- seq_len(nrow(master_fi_polygons_dis))

#Need to break all master_polygons into spatially discrete polygons for this to work
# Function to compute polygon width at points
compute_polygon_width_segments <- function(polygon) {
  
  for (i in seq_len(nrow(polygon))) {  
    message("Processing polygon ", i, " of ", nrow(polygon))
    
    # Convert a single polygon to a boundary
    polygon_boundary <- st_boundary(polygon[i, ])
    
    if (st_geometry_type(polygon_boundary) == "MULTILINESTRING") {
      polygon_boundary <- st_line_merge(polygon_boundary)
    }
    
    # Densify along polygon boundary to ensure points at least every 10 meters
    polygon_dense <- st_segmentize(polygon_boundary, dfMaxLength = 10)
    
    # Cast to LINESTRING
    polygon_linestring <- st_cast(polygon_dense, "LINESTRING") 
    
    # Compute total boundary length
    line_length <- as.numeric(st_length(polygon_linestring))
    
    if (length(line_length) > 1) line_length <- max(line_length)  # Ensure single value
    
    # Sample points, ensuring at least one
    if (line_length < 10) {
      sample_points <- st_cast(st_sample(polygon_linestring, size = 4, type = "regular"), "POINT")
    } else {
      sample_points <- st_line_sample(polygon_linestring, density = 1/10, type = "regular")
    }
    if (length(sample_points) == 0) {
      sample_points <- st_cast(st_sample(polygon_linestring, size = 2, type = "regular"), "POINT")
    }
    
    # Convert to sf object
    sample_points_sf <- st_as_sf(data.frame(id = seq_along(sample_points)), 
                                 geometry = sample_points) %>%
      filter(!st_is_empty(geometry)) %>%  # Drop empty geometries
      select(-id)  # Remove 'id' column
    
    sample_points_sf <- st_cast(sample_points_sf, "POINT")  # Ensure correct geometry type
    
    # Initialize width vector
    width_vect <- numeric(nrow(sample_points_sf))
    
    for (b in seq_len(nrow(sample_points_sf))) {
      point <- sample_points_sf[b, ]
      
      # Determine the next point for line segment calculation
      if (b == nrow(sample_points_sf)) {
        next_point <- sample_points_sf[b - 1, ]
      } else {
        next_point <- sample_points_sf[b + 1, ]
      }
      
      # Check for NA coordinates
      if (any(is.na(st_coordinates(point))) || any(is.na(st_coordinates(next_point)))) {
        message("Skipping point ", b, " due to NA coordinates")
        width_vect[b] <- NA
        next
      }
      
      # Compute the perpendicular angle
      angle <- atan2(
        st_coordinates(st_geometry(next_point))[2] - st_coordinates(st_geometry(point))[2], 
        st_coordinates(st_geometry(next_point))[1] - st_coordinates(st_geometry(point))[1]
      )
      
      perp_angle <- angle + pi / 2  # 90-degree perpendicular angle
      
      # Make a line to intersect the polygon for width calculation
      line_length <- 3000  # Ensure it crosses the polygon
      line_coords <- rbind(
        st_coordinates(st_geometry(point)) + line_length * c(cos(perp_angle), sin(perp_angle)),
        st_coordinates(st_geometry(point)) - line_length * c(cos(perp_angle), sin(perp_angle))
      )
      
      line <- st_linestring(line_coords)
      line_sf <- st_sfc(line, crs = st_crs(point))
      
      # Find the section of the line that falls within the polygon
      inside_lines <- st_intersection(st_geometry(polygon[i, ]), line_sf)
      
      # Ensure `inside_lines` is valid
      if (!inherits(inside_lines, "sf")) {
        inside_lines <- st_as_sf(inside_lines)
      }
      
      # Extract only LINESTRING geometries if needed
      inside_lines <- inside_lines %>%
        st_collection_extract("LINESTRING")
      
      # Ensure inside_lines is not empty before proceeding
      if (nrow(inside_lines) > 0) {
        # Compute distances from `point` to all segments
        distances <- sapply(seq_len(nrow(inside_lines)), function(idx) st_distance(point, inside_lines[idx, ]))
        
        # Find the closest segment
        min_dist_idx <- which.min(distances)
        
        # Compute width as the length of the closest touching segment
        width_vect[b] <- as.numeric(st_length(inside_lines[min_dist_idx, ]))
      } else {
        width_vect[b] <- NA  # No valid intersection
      }
    }
    
    # Assign widths to sample points
    sample_points_sf$width <- width_vect
    sample_points_sf$landscape <- paste0(polygon$landscape[i], polygon$FID[i])
    
    # Get the variable name as a string
    polygon_name <- deparse(substitute(polygon))
    
    # Check if "fb" or "fi" is in the variable name
    fborfi <- ifelse(grepl("fb", polygon_name), "_fbwidths_", "_fiwidths_")
    
    #output_name
    output_name <- paste0(polygon$landscape[i], fborfi, polygon$FID[i])
    
    # Save the results
    output_path <- paste0("C:/Users/swanj/Documents/Johnsonetal2026_FuelBreaks_UWR/Outputs/data_frames/", output_name, ".shp")
    
    if (all(is.na(width_vect))) {
      message("All widths are NA for polygon ", i, ". Skipping file writing.")
    } else {
      st_write(sample_points_sf, dsn = output_path, overwrite = TRUE, append = FALSE)
      assign(output_name, sample_points_sf, envir = .GlobalEnv)
    }
    
    gc()  # Free memory
  }
}

#Have to run separately because it's maxing out my memory
compute_polygon_width_segments(master_fi_polygons_dis) 
compute_polygon_width_segments(master_fb_polygons_dis)# Have to do #15 separately, with 5m spacing

#Combine fb width points into a single df
master_fbwidth_points <- rbind(brfb_fbwidths_1, brfb_fbwidths_2, lrfb_fbwidths_3, lmfb_fbwidths_4, lmfb_fbwidths_5, 
                               lmfb_fbwidths_6, lmfb_fbwidths_7, nmfb_fbwidths_8, nmfb_fbwidths_9, nmfb_fbwidths_10,
                               nmfb_fbwidths_11, nmfb_fbwidths_12, nmfb_fbwidths_13, nmfb_fbwidths_14, nmfb_fbwidths_15,
                               nmfb_fbwidths_16, nmfb_fbwidths_17)

#combine fi width points into a single df
master_fiwidth_points <- rbind(lrfb_fiwidths_1, nmfb_fiwidths_2, nmfb_fiwidths_3, nmfb_fiwidths_4, nmfb_fiwidths_5, 
                               brfb_fiwidths_6, brfb_fiwidths_7, lmfb_fiwidths_8, lmfb_fiwidths_9)

#Filter out values <2m and over 800m
master_fiwidth_points <- master_fiwidth_points %>% filter(width > 2, width < 800)
master_fbwidth_points <- master_fbwidth_points %>% filter(width > 2, width < 800)

setwd("C:/Users/swanj/Documents/Johnsonetal2026_FuelBreaks_UWR/Outputs/data_frames/")

st_write(master_fbwidth_points, dsn = "master_fbwidth_points.shp", overwrite = TRUE, append = FALSE)
st_write(master_fiwidth_points, dsn = "master_fiwidth_points.shp", overwrite = TRUE, append = FALSE)

#Make columns with the nearest width value to each point
master_points$fb_width_idx <- st_nearest_feature(master_points, master_fbwidth_points)

master_points$fi_width_idx <- st_nearest_feature(master_points, master_fiwidth_points)

if (!is.null(master_fbwidth_points$width)) {
  master_points$fb_width <- master_fbwidth_points$width[master_points$fb_width_idx]
} else {
  master_points$fb_width <- NA  # Assign NA if 'width' column does not exist
}

if (!is.null(master_fiwidth_points$width)) {
  master_points$fi_width <- master_fiwidth_points$width[master_points$fi_width_idx]
} else {
  master_points$fi_width <- NA  # Assign NA if 'width' column does not exist
}

master_points <- master_points %>% select(-c(fb_width_idx, fi_width_idx, fi_az, fb_az))

setwd('C:/Users/swanj/Documents/Johnsonetal2026_FuelBreaks_UWR/DEMs/')
TPI <- rast("9by9TPI.tif")
st_set_crs(master_points, crs(TPI))
TPI_class <- rast("TPI_9class_2.tif")
setwd("C:/Users/swanj/Documents/Johnsonetal2026_FuelBreaks_UWR/Fuelbreak_polygons/")
master_fi_polygons <- st_read("master_fi_merged_best.shp")
master_fb_polygons <- st_read("master_fb_polygons.shp")

# Initialize a vector to store the azimuths
fb_TPI <- numeric(nrow(master_points))
fi_TPI <- numeric(nrow(master_points))

# Vector of nearest polygon index attachd to points df
master_points$nearest_idx_fb <- st_nearest_feature(master_points, master_fb_polygons)
master_points$nearest_idx_fi <- st_nearest_feature(master_points, master_fi_polygons)

# Iterate over each point
for (i in 1:nrow(master_points)) {
  point <- master_points[i, ]
  
  # Isolate the nearest polygon
  nearest_fb_polygon <- master_fb_polygons[point$nearest_idx_fb, ]
  nearest_fi_polygon <- master_fi_polygons[point$nearest_idx_fi, ]
  
  #linestring between point and nearest point on polygon
  nearest_line_fb <- st_nearest_points(point, master_fb_polygons[nearest_fb_polygon,])
  nearest_points_fb <-  st_coordinates(nearest_line_fb)
  tpfb <- st_as_sf(st_sfc(st_point(nearest_points_fb[2,][1:2]), crs = crs(master_fb_polygons)))
  
  TPI_fb_pt <- terra::extract(x = TPI, y = tpfb)
  
  # Store the closest azimuth in the vector
  fb_TPI[i] <- as.numeric(TPI_fb_pt[2])
  
  nearest_line_fi <- st_nearest_points(point, master_fi_polygons[nearest_fi_polygon,])
  nearest_points_fi <-  st_coordinates(nearest_line_fi)
  tpfi <- st_as_sf(st_sfc(st_point(nearest_points_fi[2,][1:2]), crs = crs(master_fb_polygons)))
  
  TPI_fi_pt <- terra::extract(x = TPI, y = tpfi)
  
  # Store the closest azimuth in the vector
  fi_TPI[i] <- as.numeric(TPI_fi_pt[2])
}

master_points$fb_TPI <- fb_TPI
master_points$fi_TPI <- fi_TPI

setwd("C:/Users/swanj/Documents/Johnsonetal2026_FuelBreaks_UWR/Outputs/data_frames/")

st_write(obj = master_points, dsn = "master_points_haz.shp", overwrite = TRUE, append = FALSE)

#Compute sinuosity, length of fuel break, perimeter index, and area treated in a buffer

# Compute perimeter length
fb_length <- function(polygon) {
  
  #Create empty vectors for storage
  lengthvect <- numeric(nrow(polygon))
  
  for (i in seq_len(nrow(polygon))) {  
    message("Processing polygon ", i, " of ", nrow(polygon))
    
    #Get perimeter value
    perimeter <- as.numeric(st_length(st_boundary(polygon[i, ])))  # Compute perimeter for reference
    #store in vector
    lengthvect[i] <- perimeter / 2
  }
  
  # Store computed values in the spatial data frame
  polygon$length <- lengthvect
  
  return(polygon)
  
}

#Run length function and store dfs 
master_fb_polygons_dis <- fb_length(master_fb_polygons_dis)

#Make columns with the nearest fb value to each point
master_points$fb_length_idx <- st_nearest_feature(master_points, master_fb_polygons_dis)

if (!is.null(master_fb_polygons$length)) {
  master_points$fb_length <- master_fb_polygons_dis$length[master_points$fb_length_idx]
} else {
  master_points$fb_length <- NA  # Assign NA if 'width' column does not exist
}

#Clean up messy environment
rm(list = ls()[!grepl("master_points|master_fb_polygons_dis|master_fi_polygons_dis", ls())])


#Measures the average distance to the nearest neighbor polygon, normalized by the polygon's perimeter.
#Higher values → More isolated polygons.
#lower values → Closer clustering of polygons.
proximity_index <- function(polygon, polygon_name) {
  for (i in seq_len(nrow(polygon))) {  
    message("Processing polygon ", i, " of ", nrow(polygon))
    
    # Convert polygon to boundary
    polygon_boundary <- st_boundary(polygon[i, ])
    
    if (st_geometry_type(polygon_boundary) == "MULTILINESTRING") {
      polygon_boundary <- st_line_merge(polygon_boundary)
    }
    
    # Densify boundary (every 10m)
    polygon_dense <- st_segmentize(polygon_boundary, dfMaxLength = 10)
    polygon_linestring <- st_cast(polygon_dense, "LINESTRING") 
    
    # Compute total boundary length
    line_length <- as.numeric(st_length(polygon_linestring))
    
    if (length(line_length) > 1) line_length <- max(line_length)  # Ensure single value
    
    # Sample points, ensuring at least one
    if (line_length < 10) {
      sample_points <- st_cast(st_sample(polygon_linestring, size = 1, type = "regular"), "POINT")
    } else {
      sample_points <- st_line_sample(polygon_linestring, density = 1/10, type = "regular")
    }
    if (length(sample_points) == 0) {
      sample_points <- st_cast(st_sample(polygon_linestring, size = 1, type = "regular"), "POINT")
    }
    
    # Convert to sf object
    sample_points_sf <- st_as_sf(data.frame(id = seq_along(sample_points)), geometry = sample_points) %>%
      filter(!st_is_empty(geometry)) %>% select(-id)
    
    sample_points_sf <- st_cast(sample_points_sf, "POINT")  
    
    # Initialize proximity vector
    proximity_index_vect <- numeric(nrow(sample_points_sf))
    
    # Exclude self when computing distances
    other_polygons <- polygon[-i, ]
    
    # Compute perimeter
    perimeter <- as.numeric(st_length(st_boundary(polygon[i, ])))
    
    for (b in seq_len(nrow(sample_points_sf))) {
      # Get distances to all other polygons
      distances <- as.numeric(st_distance(sample_points_sf[b, ], other_polygons))
      
      # Remove zero distances (touching polygons)
      nonzero_distances <- distances[distances > 0]
      
      # Compute proximity index
      nearest_distance <- ifelse(length(nonzero_distances) > 0, min(nonzero_distances), NA)
      proximity_index_vect[b] <- nearest_distance / perimeter
    }
    
    # Assign proximity index to sample points
    sample_points_sf$prox_idx <- proximity_index_vect
    sample_points_sf$landscape <- paste0(polygon$landscape[i], polygon$FID[i]) 
    
    polygon_name <- deparse(substitute(polygon))
    
    # Determine output suffix based on polygon name
    fborfi_prox <- ifelse(grepl("fb", polygon_name), "_fbprox_", "_fiprox_")
    
    # Generate output filename
    output_name_prox <- paste0(as.character(polygon$landscape[i]), fborfi_prox, as.character(polygon$FID[i]))
    
    # Define output path
    output_path_prox <- paste0("C:/Users/swanj/Documents/Johnsonetal2026_FuelBreaks_UWR/Outputs/data_frames/", output_name_prox, ".shp")
    
    # Save results
    if (!all(is.na(proximity_index_vect))) {
      st_write(sample_points_sf, dsn = output_path_prox, overwrite = TRUE, append = FALSE)
      assign(output_name_prox, sample_points_sf, envir = .GlobalEnv)
    } else {
      message("All proximity index values are NA for polygon ", i, ". Skipping file writing.")
    }
  }
}

#Run proximity index 
proximity_index(master_fb_polygons_dis)
proximity_index(master_fi_polygons_dis)

#Combine fb width points into a single df
master_fbprox_points <- rbind(brfb_fbprox_1, brfb_fbprox_2, lrfb_fbprox_3, lmfb_fbprox_4, lmfb_fbprox_5, 
                              lmfb_fbprox_6, lmfb_fbprox_7, nmfb_fbprox_8, nmfb_fbprox_9, nmfb_fbprox_10,
                              nmfb_fbprox_11, nmfb_fbprox_12, nmfb_fbprox_13, nmfb_fbprox_14, nmfb_fbprox_15,
                              nmfb_fbprox_16, nmfb_fbprox_17)

#combine fi width points into a single df
master_fiprox_points <- rbind(lrfb_fiprox_1, nmfb_fiprox_2, nmfb_fiprox_3, nmfb_fiprox_4, nmfb_fiprox_5, 
                              brfb_fiprox_6, brfb_fiprox_7, lmfb_fiprox_8, lmfb_fiprox_9)

#Make columns with the nearest width value to each point
master_points$fb_prox_idx <- st_nearest_feature(master_points, master_fbprox_points)

master_points$fi_prox_idx <- st_nearest_feature(master_points, master_fiprox_points)

if (!is.null(master_fbprox_points$prox_idx)) {
  master_points$fb_proxix <- master_fbprox_points$prox_idx[master_points$fb_prox_idx]
} else {
  master_points$fb_proxix <- NA  # Assign NA if 'prox' column does not exist
}

if (!is.null(master_points$fi_prox_idx)) {
  master_points$fi_proxix <- master_fiprox_points$prox_idx[master_points$fi_prox_idx]
} else {
  master_points$fi_proxix <- NA  # Assign NA if 'proz' column does not exist
}

#Lower sinuosity values (closer to 1) indicate straighter features, higher values are wavier
compute_sinuosity_and_artrtd <- function(polygon) {
  
  for (i in seq_len(nrow(polygon))) {  
    message("Processing polygon ", i, " of ", nrow(polygon))
    
    # Convert a single polygon to a boundary
    polygon_boundary <- st_boundary(polygon[i, ])
    
    if (st_geometry_type(polygon_boundary) == "MULTILINESTRING") {
      polygon_boundary <- st_line_merge(polygon_boundary)
    }
    
    # Densify along polygon boundary to ensure points at least every 250 meters
    polygon_dense <- st_segmentize(polygon_boundary, dfMaxLength = 250)
    
    # Cast to LINESTRING
    polygon_linestring <- st_cast(polygon_dense, "LINESTRING") 
    
    # Sample points every 250m
    # Compute total length of the polygon boundary
    line_length <- as.numeric(st_length(polygon_linestring))
    
    if (length(line_length) > 1) line_length <- max(line_length)  # Ensure single value
    
    
    # If the length is smaller than 250m, take at least 1 sample at midpoint
    if (line_length < 250) {
      sample_points <- st_cast(st_sample(polygon_linestring, size = 1, type = "regular"), "POINT")
    } else {
      # Otherwise, sample every 250m as usual
      sample_points <- st_line_sample(polygon_linestring, density = 1/250, type = "regular")
    }
    
    # Convert to sf object
    sample_points_sf <- st_as_sf(data.frame(id = seq_along(sample_points)), 
                                 geometry = sample_points) %>%
      filter(!st_is_empty(geometry)) %>%  # Drop empty geometries
      select(-id)  # Remove 'id' column
    
    sample_points_sf <- st_cast(sample_points_sf, "POINT")  # Ensure correct geometry type
    
    # Initialize width vector
    sinuosity_vect <- numeric(nrow(sample_points_sf))
    
    for (b in seq_len(nrow(sample_points_sf))) {
      
      # Create a 500m buffer around the point for sinuosity
      buffer_sinu <- st_buffer(sample_points_sf[b,], dist = 250)
      
      #Extract portion of polygon in the buffer
      polygon_section <- st_intersection(polygon[i,], buffer_sinu)
      
      # Check if the intersection is empty
      if (st_is_empty(polygon_section)) {
        sinuosity_vect[b] <- NA
        next
      }
      
      # Extract the boundary of the polygon section
      polygon_section_boundary <- st_boundary(polygon_section)
      
      # Ensure the boundary is a LINESTRING
      polygon_section_boundary <- st_collection_extract(polygon_section_boundary, "LINESTRING")
      
      # Get the longest connected boundary path (not full perimeter)
      actual_length <- as.numeric(st_length(polygon_section_boundary))/2 # Pick the longest path
      
      # Step 4: Compute straight-line distance across the buffer (diameter)
      straight_distance <- 500  # Buffer is a circle, so diameter = 2 * radius
      
      # Step 5: Compute sinuosity
      sinuosity <- as.numeric(actual_length / straight_distance)
      
      #store in vector
      sinuosity_vect[b] <- sinuosity
    }
    
    # Assign widths to sample points
    sample_points_sf$sinuosity <- sinuosity_vect
    sample_points_sf$landscape <- paste0(polygon$landscape[i], polygon$FID[i])
    
    polygon_dense <- st_segmentize(polygon_boundary, dfMaxLength = 500)
    
    # Cast to LINESTRING
    polygon_linestring <- st_cast(polygon_dense, "LINESTRING") 
    
    # Compute total length of the polygon boundary
    line_length <- as.numeric(st_length(polygon_linestring))
    
    #In case there are multiple lines
    if (length(line_length) > 1) line_length <- max(line_length)  # Ensure single value
    
    # If the length is smaller than 250m, take at least 1 sample at midpoint
    if (line_length < 500) {
      sample_points <- st_cast(st_sample(polygon_linestring, size = 1, type = "regular"), "POINT")
    } else {
      # Otherwise, sample every 250m as usual
      sample_points <- st_line_sample(polygon_linestring, density = 1/500, type = "regular")
    }
    
    sample_points_sf_at <- st_as_sf(data.frame(id = seq_along(sample_points)), 
                                    geometry = sample_points) %>%
      filter(!st_is_empty(geometry)) %>%  # Drop empty geometries
      select(-id)  # Remove 'id' column
    
    sample_points_sf_at <- st_cast(sample_points_sf_at, "POINT")  # Ensure correct geometry type
    
    area_treated_vect <- numeric(nrow(sample_points_sf_at))
    
    for (b in seq_len(nrow(sample_points_sf_at))) {
      
      #Create 1000m buffer around point for area treated
      buffer_areatrtd <- st_buffer(sample_points_sf_at[b,], dist = 500)
      
      #Extract portion of all polygons in the buffer
      polygon_section_all <- st_intersection(polygon, buffer_areatrtd)
      
      
      if (all(st_is_empty(polygon_section_all))) {
        area_treated_vect[b] <- NA
      } else {
        #calculate area treated as a proportion
        area_treated_vect[b] <- as.numeric(sum(st_area(polygon_section_all))) / (pi*500^2) #calculate area treated as a proportion
      }
      
    }
    
    # Get the variable name as a string
    sample_points_sf_at$area_treated <- area_treated_vect
    sample_points_sf_at$landscape <- paste0(polygon$landscape[i], polygon$FID[i])
    
    polygon_name <- deparse(substitute(polygon))
    
    # Check if "fb" or "fi" is in the variable name
    fborfi_sinu <- ifelse(grepl("fb", polygon_name), "_fbsinu_", "_fisinu_")
    
    #output_name sinuosity
    output_name_sinu <- paste0(polygon$landscape[i], fborfi_sinu, polygon$FID[i])
    
    # Save the results
    output_path_sinu <- paste0("C:/Users/swanj/Documents/Johnsonetal2026_FuelBreaks_UWR/Outputs/data_frames/", 
                               output_name_sinu, ".shp")
    
    if (all(is.na(sinuosity_vect))) {
      message("All widths are NA for polygon ", i, ". Skipping file writing.")
    } else {
      st_write(sample_points_sf, dsn = output_path_sinu, overwrite = TRUE, append = FALSE)
      assign(output_name_sinu, sample_points_sf, envir = .GlobalEnv)
    }
    
    # Check if "fb" or "fi" is in the variable name
    fborfi_at <- ifelse(grepl("fb", polygon_name), "_fbat_", "_fiat_")
    
    #output_name sinuosity
    output_name_at <- paste0(polygon$landscape[i], fborfi_at, polygon$FID[i])
    
    # Save the results
    output_path_at <- paste0("C:/Users/swanj/Documents/Johnsonetal2026_FuelBreaks_UWR/Outputs/data_frames/", 
                             output_name_at, ".shp")
    
    
    if (all(is.na(area_treated_vect))) {
      message("All widths are NA for polygon ", i, ". Skipping file writing.")
    } else {
      st_write(sample_points_sf_at, dsn = output_path_at, overwrite = TRUE, append = FALSE)
      assign(output_name_at, sample_points_sf_at, envir = .GlobalEnv)
    }
    
    gc()  # Free memory
  }
}

#Run function
compute_sinuosity_and_artrtd(master_fi_polygons_dis)
compute_sinuosity_and_artrtd(master_fb_polygons_dis)

#Combine fb sinu points into a single df
master_fbsinu_points <- rbind(brfb_fbsinu_1, brfb_fbsinu_2, lrfb_fbsinu_3, lmfb_fbsinu_4, lmfb_fbsinu_5, 
                              lmfb_fbsinu_6, lmfb_fbsinu_7, nmfb_fbsinu_8, nmfb_fbsinu_9, nmfb_fbsinu_10,
                              nmfb_fbsinu_11, nmfb_fbsinu_12, nmfb_fbsinu_13, nmfb_fbsinu_14, nmfb_fbsinu_15,
                              nmfb_fbsinu_16, nmfb_fbsinu_17)

#combine fi sinu points into a single df
master_fisinu_points <- rbind(lrfb_fisinu_1, nmfb_fisinu_2, nmfb_fisinu_3, nmfb_fisinu_4, nmfb_fisinu_5, 
                              brfb_fisinu_6, brfb_fisinu_7, lmfb_fisinu_8, lmfb_fisinu_9)

#Make columns with the nearest width value to each point
master_points$fb_sinu_idx <- st_nearest_feature(master_points, master_fbsinu_points)

master_points$fi_sinu_idx <- st_nearest_feature(master_points, master_fisinu_points)

if (!is.null(master_fbsinu_points$sinuosity)) {
  master_points$fb_sinu <- master_fbsinu_points$sinuosity[master_points$fb_sinu_idx]
} else {
  master_points$fb_sinu <- NA  # Assign NA if 'prox' column does not exist
}

if (!is.null(master_fisinu_points$sinuosity)) {
  master_points$fi_sinu <- master_fisinu_points$sinuosity[master_points$fi_sinu_idx]
} else {
  master_points$fi_sinu <- NA  # Assign NA if 'proz' column does not exist
}

#Combine fb sinu points into a single df
master_fbat_points <- rbind(brfb_fbat_1, brfb_fbat_2, lrfb_fbat_3, lmfb_fbat_4, lmfb_fbat_5, 
                            lmfb_fbat_6, lmfb_fbat_7, nmfb_fbat_8, nmfb_fbat_9, nmfb_fbat_10,
                            nmfb_fbat_11, nmfb_fbat_12, nmfb_fbat_13, nmfb_fbat_14, nmfb_fbat_15,
                            nmfb_fbat_16, nmfb_fbat_17)

#combine fi sinu points into a single df
master_fiat_points <- rbind(lrfb_fiat_1, nmfb_fiat_2, nmfb_fiat_3, nmfb_fiat_4, nmfb_fiat_5, 
                            brfb_fiat_6, brfb_fiat_7, lmfb_fiat_8, lmfb_fiat_9)

#Make columns with the nearest width value to each point
master_points$fb_at_idx <- st_nearest_feature(master_points, master_fbat_points)

master_points$fi_at_idx <- st_nearest_feature(master_points, master_fiat_points)

if (!is.null(master_fbat_points$area_treated)) {
  master_points$fb_at <- master_fbat_points$area_treated[master_points$fb_at_idx]
} else {
  master_points$fb_at <- NA  # Assign NA if 'prox' column does not exist
}

if (!is.null(master_fiat_points$area_treated)) {
  master_points$fi_at <- master_fiat_points$area_treated[master_points$fi_at_idx]
} else {
  master_points$fi_at <- NA  # Assign NA if 'proz' column does not exist
}


rm(list = ls()[!grepl("master_points", ls())])

setwd("C:/Users/swanj/Documents/Johnsonetal2026_FuelBreaks_UWR/Outputs/data_frames/")

st_write(obj = master_points, dsn = "master_points_haz.shp", overwrite = TRUE, append = FALSE)

#Add coordinates for random effect
master_points$xcds <- as.character(st_coordinates(master_points)[,1])
master_points$ycds <- as.character(st_coordinates(master_points)[,2])

#Setting up data for modeling with all data, interaction terms, and categorizing fuel models
#excludes coordinates that don't show up four times in the df for each of the scenarios
master_points_longer <- master_points %>%
  pivot_longer(cols = contains("_"), 
               names_to = c("scenario", ".value"), 
               names_pattern = "([a-zA-Z]+)_(.*)") %>%
  rename(hazard = haz) %>%
  mutate(scenario = case_when(
    scenario == "UT" ~ "untreated", 
    scenario == "IN" ~ "firebreak",
    scenario == "FB" ~ "unstaffed",
    scenario == "FI" ~ "firing"),
    scenario = factor(scenario, 
                      levels = c("untreated", "unstaffed", 
                                 "firebreak", "firing")),
    FB_cats = case_when(
      FM %in% c(91, 99, 93) ~ "NB",
      FM %in% c(101, 102, 103) ~ "GR",
      FM %in% c(121, 122, 123) ~ "GS",
      FM %in% c(141, 142, 143, 144, 145, 147) ~ "SH",
      FM %in% c(161, 162, 163, 165) ~ "TU",
      FM %in% c(182, 183, 184, 185, 186, 187, 188, 189) ~ "TL",
      FM %in% c(202) ~ "SB")) %>%
  mutate(FB_cats = factor(FB_cats, levels = c("GR", "GS", "SH", "TU", "TL", "SB"))) %>%
  filter(FB_cats1 != "NB") %>% #Lose 58 points that overlapped with and sampled NB pixels
  select(-c(FM)) %>%
  group_by(coords) %>%
  filter(n() == 4) %>%
  ungroup()

#Save new master points and master_points_longer in final edits folder
setwd("C:/Users/swanj/Documents/Johnsonetal2026_FuelBreaks_UWR/Outputs/data_frames/")

st_write(master_points_longer, "master_points_longer.gpkg", append = FALSE)

#Boundary Ridge modeling df
BR_points_l <- master_points_longer %>%
  filter(landscape == "BR") %>%
  mutate(length = as.vector(scale(length)), #Scale all variables
         wdth = as.vector(scale(wdth)),
         wnd = as.vector(scale(wnd)),
         logdis = as.vector(scale(logdis)),
         TPI = as.vector(scale(TPI)),
         proxix = as.vector(scale(proxix)),
         sinu = as.vector(scale(sinu)),
         Slope = as.vector(scale(Slope)),
         scenario = factor(x = scenario, 
                           levels = c("untreated", "unstaffed", 
                                      "firebreak", "firing")),
         FB_cats = factor(x = FB_cats, 
                          levels = c("GR", "GS", "SH", "TU", "TL")))

#Limestone Ridge modeling df
LR_points_l <- master_points_longer %>%
  filter(landscape == "LR") %>%
  mutate(wdth = as.vector(scale(wdth)),
         wnd = as.vector(scale(wnd)),
         logdis = as.vector(scale(logdis)),
         TPI = as.vector(scale(TPI)),
         sinu = as.vector(scale(sinu)),
         Slope = as.vector(scale(Slope)),
         scenario = factor(x = scenario, 
                           levels = c("untreated", "unstaffed", 
                                      "firebreak", "firing")),
         FB_cats = factor(x = FB_cats, 
                          levels = c("GR", "GS", "SH", "TU", "TL")))

#North Mountain modeling df
NM_points_l <- master_points_longer %>%
  filter(landscape == "NM") %>%
  mutate(length = as.vector(scale(length)), #Scale all variables
         wdth = as.vector(scale(wdth)),
         wnd = as.vector(scale(wnd)),
         logdis = as.vector(scale(logdis)),
         TPI = as.vector(scale(TPI)),
         proxix = as.vector(scale(proxix)),
         sinu = as.vector(scale(sinu)),
         Slope = as.vector(scale(Slope)),
         scenario = factor(x = scenario, 
                           levels = c("untreated", "unstaffed", 
                                      "firebreak", "firing")),
         FB_cats = factor(x = FB_cats, 
                          levels = c("GR", "GS", "SH", "TU", "TL")))

#Mount Lowe modeling df
ML_points_l <- master_points_longer  %>%
  filter(landscape == "ML",
         FB_cats != "SB") %>% #Remove SB fuel models. Only 20 observations
  mutate(length = as.vector(scale(length)),
         wdth = as.vector(scale(wdth)),
         wnd = as.vector(scale(wnd)),
         logdis = as.vector(scale(logdis)),
         TPI = as.vector(scale(TPI)),
         proxix = as.vector(scale(proxix)),
         sinu = as.vector(scale(sinu)),
         Slope = as.vector(scale(Slope)),
         scenario = factor(x = scenario, 
                           levels = c("untreated", "unstaffed", 
                                      "firebreak", "firing")),
         FB_cats = factor(x = FB_cats, 
                          levels = c("GR", "GS", "SH", "TU", "TL")))

#Save dfs
setwd("C:/Users/swanj/Documents/Johnsonetal2026_FuelBreaks_UWR/Outputs/data_frames/")
st_write(BR_points_l, "BR_points_final.gpkg")
st_write(LR_points_l, "LR_points_final.gpkg", append = FALSE)
st_write(NM_points_l, "NM_points_final.gpkg")
st_write(ML_points_l, "ML_points_final.gpkg", append = FALSE)