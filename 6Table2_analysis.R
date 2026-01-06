library(tidyverse)
library(sf)

#Read in UWR rasters to create summary metrics
setwd("C:/Users/swanj/Documents/Research/fuel_break_systems/Spatialdata/CFL_BP_rasters/JohnsEdits_Final/withconstant/")

#Read in tifs
UWR_files <- list.files(pattern = ".tif")
UWR_files <- UWR_files[!grepl("xml", UWR_files)]
lapply(UWR_files, function(x) {
  assign(substr(x,1,9), rast(x), envir = .GlobalEnv)
})

#read in master fb and fi polygons
setwd("C:/Users/swanj/Documents/Research/fuel_break_systems/Spatialdata/sf_for_distance/")

master_fi_polygons_dis <- st_read("master_fi_merged_best.shp")
master_fb_polygons_dis <- st_read("master_fb_poly_dis.shp")

#Find polygon areas
nmfbarea <- sum(as.numeric(st_area(master_fb_polygons_dis[master_fb_polygons_dis$landscape == 'nmfb',])))
nmfiarea <- sum(as.numeric(st_area(master_fi_polygons_dis[master_fi_polygons_dis$landscape == 'nmfb',])))
mlfbarea <- sum(as.numeric(st_area(master_fb_polygons_dis[master_fb_polygons_dis$landscape == 'lmfb',])))
mlfiarea <- sum(as.numeric(st_area(master_fi_polygons_dis[master_fi_polygons_dis$landscape == 'lmfb',])))
lrfbarea <- sum(as.numeric(st_area(master_fb_polygons_dis[master_fb_polygons_dis$landscape == 'lrfb',])))
lrfiarea <- sum(as.numeric(st_area(master_fi_polygons_dis[master_fi_polygons_dis$landscape == 'lrfb',])))
brfbarea <- sum(as.numeric(st_area(master_fb_polygons_dis[master_fb_polygons_dis$landscape == 'brfb',])))
brfiarea <- sum(as.numeric(st_area(master_fi_polygons_dis[master_fi_polygons_dis$landscape == 'brfb',])))

#Find the total area of the landscapes including NAs
# Reproject to UTM Zone 11N (Change based on your location)
lr_ext <- ext(LR_UT_haz)
lr_width <- lr_ext[2] - lr_ext[1]  # Xmax - Xmin
lr_height <- lr_ext[4] - lr_ext[3] # Ymax - Ymin

nm_ext <- ext(NM_UT_haz)
nm_width <- nm_ext[2] - nm_ext[1]  # Xmax - Xmin
nm_height <- nm_ext[4] - nm_ext[3] # Ymax - Ymin

ml_ext <- ext(ML_UT_haz)
ml_width <- ml_ext[2] - ml_ext[1]  # Xmax - Xmin
ml_height <- ml_ext[4] - ml_ext[3] # Ymax - Ymin

br_ext <- ext(BR_UT_haz)
br_width <- br_ext[2] - br_ext[1]  # Xmax - Xmin
br_height <- br_ext[4] - br_ext[3] # Ymax - Ymin

# Compute current area
nmarea <- as.numeric(nm_width * nm_height)
mlarea <- as.numeric(ml_width * ml_height)
lrarea <- as.numeric(lr_width * lr_height)
brarea <- as.numeric(br_width * br_height)

#Percentages treated
nmfbperc <- nmfbarea/nmarea
nmfiperc <- nmfiarea/nmarea
mlfbperc <- mlfbarea/mlarea
mlfiperc <- mlfiarea/mlarea
lrfbperc <- lrfbarea/lrarea
lrfiperc <- lrfiarea/lrarea
brfbperc <- brfbarea/brarea
brfiperc <- brfiarea/brarea

#Create data frame for treatment percentages, landscape sizes, and normalization metrics
percent_treateddf <- data.frame(row.names = c("nm", "ml", "lr", "br"),
                                fbperc = c(nmfbperc, mlfbperc, lrfbperc, brfbperc),
                                fiperc = c(nmfiperc, mlfiperc, lrfiperc, brfiperc),
                                fbarea = c(nmfbarea/10000, mlfbarea/10000, lrfbarea/10000, brfbarea/10000),
                                fiarea = c(nmfiarea/10000, mlfiarea/10000, lrfiarea/10000, brfiarea/10000),
                                lscparea = c(nmarea/10000, mlarea/10000, lrarea/10000, brarea/10000))

percent_treateddf <- format(percent_treateddf, scientific = FALSE)

#Mutate normalization metrics into df
percent_treateddf <- percent_treateddf %>% 
  mutate(normfbmetric = as.numeric(lscparea) / as.numeric(fbarea),
         normfimetric = as.numeric(lscparea) / as.numeric(fiarea))

#save table
setwd("C:/Users/swanj/Documents/Research/fuel_break_systems/Figures")
write.csv(percent_treateddf, "percent_treated_table.csv")

#Read in fire size lists
setwd("C:/Users/swanj/Documents/Research/fuel_break_systems/Spatialdata/Fire_sizelist")

perimeter_files <- list.files(pattern = '.csv')

lapply(perimeter_files, function(x) {
  #Read in csv as df and select columns
  df <- read_csv(x) %>% 
    select(c(FIRE_NUM, 'FireSize Ha', XStart, YStart)) %>%
    rename(FireSize.Ha = 'FireSize Ha')
  
  #grab raster and crs of raster for points
  if (str_detect(x, "BR")) {
    rast <- BR_UT_haz
    crsrast <- crs(BR_UT_haz)
  } else if (str_detect(x, "LR")) {
    rast <- LR_UT_haz
    crsrast <- crs(LR_UT_haz)
  } else if (str_detect(x, "NM")) {
    rast <- NM_UT_haz
    crsrast <- crs(NM_UT_haz)
  } else {
    rast <- ML_UT_haz
    crsrast <- crs(ML_UT_haz)
  }
  
  #convert firesize list to df
  fire_size_points <- st_as_sf(df, coords = c("XStart", "YStart"), crs = crsrast)
  
  #Clip points to raster
  cropped_fires <- st_crop(fire_size_points, rast)
  
  cropped_fires$scenario <- ifelse(substr(x, 4,5) == "UT", "Untreated",
                                   ifelse(substr(x,4,5) == "FB", "Fuel break",
                                          ifelse(substr(x,4,5) == "IN", "Firebreak",
                                                 "Firing")))
  
  #Save cropped fires as shapefiles
  #st_write(cropped_fires, paste0(substr(x, 1, 6), "cropped_fires.shp"), overwrite = TRUE)
  
  #Assign to envirionment
  assign(substr(x, 1, 14), cropped_fires, envir = .GlobalEnv)
})

#Remove  file lists
rm(perimeter_files, UWR_files)

LR_firesize <- rbind(LR_UT_firesize, LR_FB_firesize, LR_IN_firesize, LR_FI_firesize)
BR_firesize <- rbind(BR_UT_firesize, BR_FB_firesize, BR_IN_firesize, BR_FI_firesize)
NM_firesize <- rbind(NM_UT_firesize, NM_FB_firesize, NM_IN_firesize, NM_FI_firesize)
ML_firesize <- rbind(ML_UT_firesize, ML_FB_firesize, ML_IN_firesize, ML_FI_firesize)

#Make fire size histograms
NM_firesize %>%
  ggplot(aes(x = FireSize.Ha)) +
  geom_histogram() +
  facet_wrap( ~ factor(scenario, levels = c("Untreated", "Fuel break", "Firebreak", "Firing")), ) +
  xlab(label = "Fire Size in Hectares") 

ML_firesize %>%
  ggplot(aes(x = FireSize.Ha)) +
  geom_histogram() +
  facet_wrap( ~ factor(scenario, levels = c("Untreated", "Fuel break", "Firebreak", "Firing")), ) +
  xlab(label = "Fire Size in Hectares")

BR_firesize %>%
  ggplot(aes(x = FireSize.Ha)) +
  geom_histogram() +
  facet_wrap( ~ factor(scenario, levels = c("Untreated", "Fuel break", "Firebreak", "Firing")), ) +
  xlab(label = "Fire Size in Hectares")

LR_firesize %>%
  ggplot(aes(x = FireSize.Ha)) +
  geom_histogram() +
  facet_wrap( ~ factor(scenario, levels = c("Untreated", "Fuel break", "Firebreak", "Firing")), ) +
  xlab(label = "Fire Size in Hectares")

#Get mean values for each raster
#create data frame of raster statistics
data_list <- ls(pattern = "R_|M_|L_")

# Define the desired order based on detected strings
order_priority <- c("UT", "FB", "IN", "FI")

# Function to assign a numeric rank based on detected string
get_order_rank <- function(x) {
  match_value <- str_extract(x, paste(order_priority, collapse = "|"))
  return(match(match_value, order_priority)) # Assigns numeric ranking
}

# Apply ranking function to list elements
rank_values <- sapply(data_list, get_order_rank)

# Reorder list based on detected string values
data_list <- data_list[order(rank_values)]

fun_list <- list(mean = mean, sd = sd, median = stats::median)

df <- data.frame(matrix(0, nrow = 0, ncol = 6))
colnames(df) <- c("mean_hazard", "sd_hazard", "median_hazard",
                  "mean_fs", "sd_fs", "median_fs")

for (i in data_list) {
  rowname1 <- ifelse(str_detect(i, "BR"), "BR", 
                     ifelse(str_detect(i, "LR"), "LR", 
                            ifelse(str_detect(i, "NM"), "NM", "ML")))
  
  rowname2 <- ifelse(str_detect(i, "UT"), "_untreated", 
                     ifelse(str_detect(i, "FB"), "_unstaffed", 
                            ifelse(str_detect(i, "FI"), "_firing", "_firebreak")))
  
  for (y in names(fun_list)) {
    if (str_detect(i, "haz") == TRUE) {
      value <- global(get(i), fun = fun_list[[y]], na.rm = TRUE)
      df[paste0(rowname1, rowname2), paste0(y, "_hazard")] <- value
    } else if (str_detect(i, "firesize") == TRUE) {
      value <- get(i) %>%
        st_drop_geometry() %>%
        as.data.frame()
      value <- value %>% summarise_at('FireSize.Ha', fun_list[[y]])
      df[paste0(rowname1, rowname2), paste0(y, "_fs")] <- value
    } 
  }
}

#Create differenced hazard column
df <- df %>%
  mutate(rows = rownames(.),
         landscape = str_sub(rows, 1, 2),  # Extracts first two letters as landscape
         treatment = str_replace(rows, "^[A-Z]{2}_", "")) %>%  # Extracts treatment dynamically
  group_by(landscape) %>%
  mutate(untreated_hazard = mean_hazard[treatment == "untreated"],  # Get Untreated hazard for each landscape
         differenced = ifelse(treatment != "untreated", mean_hazard - untreated_hazard, NA)) %>%
  ungroup() 

df <- df %>% group_by(landscape, treatment) %>%
  mutate(norm_metric = case_when(
    landscape == "BR" & treatment %in% c("untreated", "unstaffed", "firebreak") ~ percent_treateddf$normfbmetric[row.names(percent_treateddf) == "br"],
    landscape == "BR" & treatment == "firing" ~ percent_treateddf$normfimetric[row.names(percent_treateddf) == "br"],
    landscape == "LR" & treatment %in% c("untreated", "unstaffed", "firebreak") ~ percent_treateddf$normfbmetric[row.names(percent_treateddf) == "lr"],
    landscape == "LR" & treatment == "firing" ~ percent_treateddf$normfimetric[row.names(percent_treateddf) == "lr"],
    landscape == "NM" & treatment %in% c("untreated", "unstaffed", "firebreak") ~ percent_treateddf$normfbmetric[row.names(percent_treateddf) == "nm"],
    landscape == "NM" & treatment == "firing" ~ percent_treateddf$normfimetric[row.names(percent_treateddf) == "nm"],
    landscape == "ML" & treatment %in% c("untreated", "unstaffed", "firebreak") ~ percent_treateddf$normfbmetric[row.names(percent_treateddf) == "ml"],
    landscape == "ML" & treatment == "firing" ~ percent_treateddf$normfimetric[row.names(percent_treateddf) == "ml"]),
    norm_difference = differenced * norm_metric,
    perc_change = (differenced / untreated_hazard)*100)

setwd("C:/Users/swanj/Documents/Research/fuel_break_systems/Figures")
write.csv(df, "DescriptiveStats_Table_Forpaper.csv", append = FALSE)

##total area burned
rm(perimeter_files)

fs_list <- ls(pattern = "firesize")

# Apply ranking function to list elements
rank_values <- sapply(fs_list, get_order_rank)

# Reorder list based on detected string values
fs_list <- fs_list[order(rank_values)]

# Create an empty data frame with the correct number of rows
fs_df <- data.frame(matrix(0, nrow = length(fs_list), ncol = 0))

# Assign row names
rownames(fs_df) <- fs_list

#calculate per treatment hectare reduction in area burned
arburn_redperac <- lapply(fs_list, function(x) {
  df <- get(x)
  if (str_detect(x, "BR")) {
    reduc <- sum(BR_UT_firesize$FireSize.Ha, na.rm = TRUE) - sum(df$FireSize.Ha, na.rm = TRUE)
    ifelse(str_detect(x, "FI"), reduc / 689.71,
           ifelse(str_detect(x, "UT"), 0, reduc / 282.35))
  } else if (str_detect(x, "LR")) {
    reduc <- sum(LR_UT_firesize$FireSize.Ha, na.rm = TRUE) - sum(df$FireSize.Ha, na.rm = TRUE)
    ifelse(str_detect(x, "FI"), reduc / 94.11,
           ifelse(str_detect(x, "UT"), 0, reduc / 17.92))  
  } else if (str_detect(x, "NM")) {
    reduc <- sum(NM_UT_firesize$FireSize.Ha, na.rm = TRUE) - sum(df$FireSize.Ha, na.rm = TRUE)
    ifelse(str_detect(x, "FI"), reduc / 632.63,
           ifelse(str_detect(x, "UT"), 0, reduc / 135.63))  
  } else if (str_detect(x, "ML")) {
    reduc <- sum(ML_UT_firesize$FireSize.Ha, na.rm = TRUE) - sum(df$FireSize.Ha, na.rm = TRUE)
    ifelse(str_detect(x, "FI"), reduc / 872.47,
           ifelse(str_detect(x, "UT"), 0, reduc / 203.75))  
  }
})

#Final table of area burned reduction per treatment hectare
fs_df$ab_redperac <- unlist(arburn_redperac)

##Calculate fire size change
# ##Calculate dectile change for fire size
# #Remove the ignition that didn't occur in the treated areas on LR landscape 
# differences <- which(LR_UT_firesize$geometry != LR_IN_firesize$geometry, arr.ind = TRUE)
# 
# #(FIRE_NUM = 8949)
LR_UT_firesize <- LR_UT_firesize %>% filter(FIRE_NUM != 8949) 

LR_fi_list <- list(LR_UT_firesize, LR_FB_firesize, LR_IN_firesize,LR_FI_firesize)
LR_fi_list <- lapply(LR_fi_list, st_drop_geometry)
names(LR_fi_list) <- c("LR_UT_firesize", "LR_FB_firesize", "LR_IN_firesize", "LR_FI_firesize")
BR_fi_list <- list(BR_UT_firesize, BR_FB_firesize, BR_IN_firesize, BR_FI_firesize)
BR_fi_list <- lapply(BR_fi_list, st_drop_geometry)
names(BR_fi_list) <- c("BR_UT_firesize", "BR_FB_firesize", "BR_IN_firesize", "BR_FI_firesize")
NM_fi_list <- list(NM_UT_firesize, NM_FB_firesize, NM_IN_firesize, NM_FI_firesize)
NM_fi_list <- lapply(NM_fi_list, st_drop_geometry)
names(NM_fi_list) <- c("NM_UT_firesize", "NM_FB_firesize", "NM_IN_firesize", "NM_FI_firesize")
ML_fi_list <- list(ML_UT_firesize, ML_FB_firesize, ML_IN_firesize, ML_FI_firesize)
ML_fi_list <- lapply(ML_fi_list, st_drop_geometry)
names(ML_fi_list) <- c("ML_UT_firesize", "ML_FB_firesize", "ML_IN_firesize", "ML_FI_firesize")

fi_list <- list(LR_fi_list, BR_fi_list, NM_fi_list, ML_fi_list)

names(fi_list) <- c("LR_fi_list", "BR_fi_list", "NM_fi_list", "ML_fi_list")

#Calculate fire dectiles
imap(fi_list, function(x, i) {
  
  #Set control to scenario with largest fires
  cntrlidx <- 
    
    
    cntrlidx <- which(str_detect(names(x), "UT"))
  cntlnm <- names(x)[[cntrlidx]]
  cntrl <- x[[cntrlidx]]
  maxidx <- ifelse(str_detect(i, "BR"), 
                   which(str_detect(names(x), "UT")),
                   which(str_detect(names(x), "FB")))
  maxnm <- names(x)[[maxidx]]
  max <- x[[maxidx]]
  fbidx <- which(str_detect(names(x), "FB"))
  fbnm <- names(x)[[fbidx]]
  fb <- x[[fbidx]]
  indidx <- which(str_detect(names(x), "IN"))
  innm <- names(x)[[indidx]]
  ind <- x[[indidx]]
  fiidx <- which(str_detect(names(x), "FI"))
  finm <- names(x)[[fiidx]]
  fi <- x[[fiidx]]
  
  df <- data.frame(matrix(0, nrow = 4, ncol = 0))
  row.names(df) <- c("UT", "FB", "IN", "FI")
  
  control_dectile_breaks <- quantile(
    cntrl[['FireSize.Ha']], 
    probs = seq(0, 1, by = 0.2), 
    na.rm = TRUE, 
    type = 7
  )
  
  # # Ensure the range includes all values
  control_dectile_breaks[1] <- 0.09
  control_dectile_breaks[length(control_dectile_breaks)] <- max(max[['FireSize.Ha']], na.rm = TRUE)
  # 
  # print(summary(control_dectile_breaks))  # Debug breakpoints
  
  # Identify unclassified fires
  fires_outside_breaks <- function(df, breaks) {
    df %>% filter(FireSize.Ha < min(breaks) | FireSize.Ha > max(breaks))
  }
  
  # Check for issues in treated landscapes
  print(fires_outside_breaks(fb, control_dectile_breaks), n = Inf)
  print(fires_outside_breaks(ind, control_dectile_breaks), n = Inf)
  print(fires_outside_breaks(fi, control_dectile_breaks), n = Inf)
  
  classify_fire_size <- function(df) {
    df %>%
      mutate(
        fire_size_class = cut(
          .data[['FireSize.Ha']],
          breaks = control_dectile_breaks,  # Define size class boundaries
          include.lowest = TRUE
        )
      )
  }
  
  # Classify fire sizes in the control data frame
  control_classified <- classify_fire_size(cntrl)
  
  # Count fires in each size class for the control scenario
  control_summary <- control_classified %>%
    count(fire_size_class, name = "control_count")
  
  trt_lst <- list(FB = fb, IN = ind, FI = fi)
  
  # Combine treatment results into new columns
  combined_result <- reduce(names(trt_lst), function(df, treatment_name) {
    treatment_df <- trt_lst[[treatment_name]]
    
    treatment_classified <- classify_fire_size(treatment_df)
    treatment_summary <- treatment_classified %>%
      count(fire_size_class, name = paste0(treatment_name, "_count"))
    
    # Compare control vs treatment
    treatment_comparison <- control_summary %>%
      full_join(treatment_summary, by = "fire_size_class") %>%
      mutate(
        !!paste0(treatment_name, "_change") := !!sym(paste0(treatment_name, "_count")) - control_count,
        !!paste0(treatment_name, "_percent_change") := (!!sym(paste0(treatment_name, "_change")) / control_count) * 100
      ) %>%
      select(-control_count)  # Remove duplicate columns if needed
    
    # Merge with the main result
    df %>%
      full_join(treatment_comparison, by = "fire_size_class")
  }, .init = control_summary)
  
  #Save to CSV
  write.csv(
    combined_result,
    file = paste0("C:/Users/swanj/Documents/Research/fuel_break_systems/Figures/",
                  i, "_firechange_combined.csv"),
    row.names = FALSE,
    append = FALSE
  )
  
  # Assign to the global environment for further use
  assign(paste0(i, "_firechange_combined"), combined_result, envir = .GlobalEnv)
})

