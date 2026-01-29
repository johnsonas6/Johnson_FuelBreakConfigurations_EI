library(tidyverse)
library(sf)
library(terra)

##Figures

##Paper figures
#Make UWR vs BP and CFL heatmap
setwd("C:/Users/swanj/Documents/Johnsonetal2026_FuelBreaks_UWR/Outputs/raster/BP/")

#Get raster file names
BP_list <- list.files(pattern = "UT")
BP_list <- BP_list[!grepl("xml", BP_list)]

setwd("C:/Users/swanj/Documents/Johnsonetal2026_FuelBreaks_UWR/Outputs/raster/CFL/")
CFL_list <- list.files(pattern = "UT")
CFL_list <- CFL_list[!grepl("xml", CFL_list)]

setwd("C:/Users/swanj/Documents/Johnsonetal2026_FuelBreaks_UWR/Outputs/raster/UWR/")
UWR_list <- list.files(pattern = "UT")
UWR_list <- UWR_list[!grepl("xml", UWR_list)]

raster_list <- c(BP_list, CFL_list, UWR_list)

#Import rasters
lapply(raster_list, function(x) {
  if (str_detect(x, "CFL")) {
    setwd("C:/Users/swanj/Documents/Johnsonetal2026_FuelBreaks_UWR/Outputs/raster/CFL/")
    r <- rast(x)
    assign(substr(x,1,9), r, envir = .GlobalEnv)
  }
  else if (str_detect(x, "BP")) {
    setwd("C:/Users/swanj/Documents/Johnsonetal2026_FuelBreaks_UWR/Outputs/raster/BP/")
    r <- rast(x)
    assign(substr(x,1,8), r, envir = .GlobalEnv)
  }
  else {
    setwd("C:/Users/swanj/Documents/Johnsonetal2026_FuelBreaks_UWR/Outputs/raster/UWR/")
    r <- rast(x)
    assign(substr(x,1,9), r, envir = .GlobalEnv)
  }
})


#Make data frame with column for landscape name/trtmnt, BP, CFL, and hazard
setwd("C:/Users/swanj/Documents/Johnsonetal2026_FuelBreaks_UWR/Outputs/data_frames/")

BR_points <- st_read("BR_point_samples.shp") %>%
  select(-FID) %>%
  mutate(landscape = "BR",
         ID = row_number())
LR_points <- st_read("LR_point_samples.shp") %>%
  select(-FID) %>%
  mutate(landscape = "LR",
         ID = row_number())
NM_points <- st_read("NM_point_samples.shp") %>%
  select(-FID) %>%
  mutate(landscape = "NM",
         ID = row_number())
ML_points <- st_read("ML_point_samples.shp") %>%
  select(-FID) %>%
  mutate(landscape = "ML",
         ID = row_number())

sample_raster <- function(x) {
  
  r <- get(x)
  
  #Conditional matching of file names
  if (substr(x, 1, 2) == "BR") {
    
    #create sf object from SpatVector of extracted data
    table <- terra::extract(r, BR_points, xy = FALSE, ID = TRUE, weights = FALSE, 
                            exact = FALSE, touches = FALSE)
    
    BR_points <- BR_points %>% left_join(table, by = join_by("ID"))
    
  } else if (substr(x, 1, 2) == "LR") {
    
    #create sf object from SpatVector of extracted data
    table <- terra::extract(r, LR_points, xy = FALSE, ID = TRUE, weights = FALSE, 
                            exact = FALSE, touches = FALSE)
    
    LR_points <- LR_points %>% left_join(table, by = join_by("ID"))
    
  } else if (substr(x, 1, 2) == "NM") {
    
    ##create sf object from SpatVector of extracted data
    table <- terra::extract(r, NM_points, xy = FALSE, ID = TRUE, weights = FALSE, 
                            exact = FALSE, touches = FALSE)
    
    NM_points <- NM_points %>% left_join(table, by = join_by("ID"))
    
  } else if (substr(x, 1, 2) == "ML") {
    
    # ##create sf object from SpatVector of extracted data
    table <- terra::extract(r, ML_points, xy = FALSE, ID = TRUE, weights = FALSE, 
                            exact = FALSE, touches = FALSE)
    
    ML_points <- ML_points %>% left_join(table, by = join_by("ID"))
    
  }
  
  assign('BR_points', BR_points, envir = .GlobalEnv)
  assign('LR_points', LR_points, envir = .GlobalEnv)
  assign('NM_points', NM_points, envir = .GlobalEnv)
  assign('ML_points', ML_points, envir = .GlobalEnv)
  
}

all_rasters <- ls(pattern = "UT")

lapply(all_rasters, sample_raster)

#Project jp dfs to UTM
jp_list <- list('BR_points', 'LR_points', 'ML_points', 'NM_points')

lapply(jp_list, function(x) {
  b <- get(x)
  b <- st_transform(b, crs = 32612)  # Transform the object
  assign(x, b, envir = .GlobalEnv)  # Assign to global environment
})

#Combine just point data frames
points <- bind_rows(BR_points, LR_points, ML_points, NM_points)

points <- points %>%
  select(c(UWR, Burn_Probability, CFL)) %>%
  st_drop_geometry() %>%
  na.omit()

#CFL_cap <- round(range(points$CFL)[2] * 2) / 2

CFL_cap <- ceiling(range(points$CFL)[2])

CFL_breaks <- seq(0, CFL_cap, by = 1)

CFL_floor <- CFL_breaks[2]

#CFL_labels <- as.character(seq(CFL_floor, CFL_cap, by = 0.5))
CFL_labels <- as.character(seq(CFL_floor, CFL_cap, by = 1))

#Set same number of breaks as CFL
BP_cap <- range(points$Burn_Probability)[2]

BP_split <- BP_cap/9

BP_breaks <- seq(0, BP_cap, by = BP_split)

BP_floor <- BP_breaks[2]

BP_labels <- as.character(round(seq(BP_floor, BP_cap, by = BP_split), digits = 3))

points_hm <- points %>%
  mutate(
    Conditional_Flame_Length = cut(CFL, 
                                   breaks = CFL_breaks, 
                                   labels = CFL_labels),
    
    Burn_Probability = cut(Burn_Probability, 
                           breaks = BP_breaks, 
                           labels = BP_labels)) %>%
  
  group_by(Burn_Probability, Conditional_Flame_Length) %>%
  
  summarize(UWR = mean(UWR)) %>%
  
  na.omit()


#Make Heatmap


cfl_bp_haz_hm <- ggplot(data = points_hm,
                        mapping = aes(
                          x = Conditional_Flame_Length,
                          y = Burn_Probability,
                          fill = UWR
                        )) +
  geom_tile(color = 'white') +
  scale_fill_gradientn(
    breaks = scales::breaks_extended(n = 5),
    colors = c("#3ea4f0", '#FFF44F', "#c12422"),
    name = "Uncontrollable\nWildfire\nRisk",
    guide = guide_colorbar(barwidth = 2, barheight = 7.5))  +
  coord_fixed() +
  theme(legend.position = c(1.19, 0.75),
        legend.ticks = element_line(size = 0.5),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 18),
        panel.border = element_rect(color = "black", fill = NA, size = 1),
        legend.background = element_rect(color = "black", fill = NA, size = 0.5),
        panel.background = element_rect(color = "white", fill = "white"),
        legend.spacing = unit(0.5, "cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 14),
        axis.title.x = element_text(size=18, 
                                    margin = margin(t = 10, r = 10, b = 10, l = 10)),
        axis.title.y = element_text(size=18, 
                                    margin = margin(t = 10, r = 10, b = 10, l = 10), 
                                    angle = 90)) +
  labs(x = "Conditional Flame Length (m)", y = "Burn Probability")

setwd("C:/Users/swanj/Documents/Johnsonetal2026_FuelBreaks_UWR/Outputs/figures/")
ggsave(plot = cfl_bp_haz_hm, filename = "UWR_heatmap_Forpaper.png", 
       width = 10, height = 6, 
       dpi = 300)

#WEIGHTS FIGURE
#Plotting breakpoints of suppression effectiveness
#X is flame lengths or fireline intensity
#Byram's equation is FL in meters = 0.0775*FLI(KW/m) ^ 0.46
#1000 1.86m- handcrew, 
#2000 2.55m- Retardent drops hold fire ~1 hr (ground and air), Budd et al. and Loane and Gould
#3000 3.08m- Budd et al. hypothesize that air tanker drops and ground forces could contain this
#5000 3.9m- Aircraft and handcrews SOMETIMES succesful when working together, Budd et al.
#6700 4.49m - Upper end of aircraft suppression failing Plucinski MP. Evaluation of the effectiveness of the 10 tanker air carrier DC-10 air tanker, Victoria 2010. Technical Report. East Melbourne: Bushfire Cooperative Research Centre; 2010.from Burgan 1979 and Plucinski 2019
x1 <- c(0.25, 1.86, 2.55, 3.08, 3.9, 4.49, 9.75)
#Y is some arbitrary measure of suppression difficulty 
#reflective of the number and type of resources needed, implications about their
#availability and impact 
y1 <- c(1, 5, 15, 20, 30, 40, 49.999)

plot(x = x1, y = y1)

#Making a population growth model to assign weights to FIL categories
data <- data.frame(x1,y1)

#phi1 is the max weight value
modelphi1 <- 50

#phi2 and phi3 are derived from the 2 parameter logit models intercept and x coefficient
modelphi2 <- coef(lm(logit(y1/50)~x1))[1]
modelphi3 <- coef(lm(logit(y1/50)~x1))[2]

#Making population growth model from coefficients using non-linear least squares
weights <-nls(y1~phi1/(1+exp(-(phi2+phi3*x1))),
              start=list(phi1=modelphi1, phi2=modelphi2, phi3=1.572869 ), trace=TRUE)

#Set coefficients for weights equation
phi1<-coef(weights)[1]
phi2<-coef(weights)[2]
phi3<-coef(weights)[3]
FIL <- c(-10, 0.25, 0.75, 1.25, 1.75, 2.25, 2.75, 3.25, 3.75, 4.25, 4.75, 5.25, 5.75, 6.25, 6.75, 7.25, 
         7.75, 8.25, 8.75, 9.25, 9.75, 15)
hazard_weights <- phi1/(1+exp(-(phi2+phi3*FIL)))

predicted_weights <- data.frame(FIL, hazard_weights)

#Create plot
weight_plot <- data %>%
  ggplot(aes(x = x1, y = y1)) +
  
  # Add line and points
  geom_line(data = predicted_weights, aes(x = FIL, y = hazard_weights), size = 1.2, color = "black") +
  
  geom_point(aes(x = x1, y = y1), size = 3, color = "orange") +
  
  # Labels
  labs(
    x = "Flame Length (m)",
    y = "Hazard Weight"
  ) +
  
  # Axis ticks
  scale_x_continuous(breaks = seq(0, 10, 2), expand = c(0,0)) +
  scale_y_continuous(breaks = seq(0, 50, 10), expand = expansion(mult = c(0.01, 0.01))) +
  
  # Theme customization
  theme_minimal(base_size = 14) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text = element_text(size = 14, color = "black"),
    axis.title.x = element_text(size = 18, margin = margin(t = 10)),
    axis.title.y = element_text(size = 18, margin = margin(r = 10)),
    panel.background = element_rect(fill = "white", color = NA),
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "white", color = NA)
  )

ggsave(weight_plot, filename = "C:/Users/swanj/Documents/Johnsonetal2026_FuelBreaks_UWR/Outputs/figures/weight_plot.png", 
       width = 10, height = 6, dpi = 300)

#Distance panel figure

#Upload point sfs
setwd("C:/Users/swanj/Documents/Johnsonetal2026_FuelBreaks_UWR/Outputs/data_frames/")

BR_points_l <- st_read("BR_points_final.gpkg")
LR_points_l <- st_read("LR_points_final.gpkg")
ML_points_l <- st_read("ML_points_final.gpkg")
NM_points_l <- st_read("NM_points_final.gpkg")

#Unscale logdis and wnd
master_points_longer <- st_read("master_points_longer.gpkg") %>%
  st_drop_geometry

#Pull out scale transformation variables
BRdisvars <- master_points_longer %>%
  filter(landscape == "BR") %>%
  summarise(mean_logdis = mean(logdis, na.rm = TRUE),
            sd_logdis = sd(logdis, na.rm = TRUE))

BRdismean <- BRdisvars[,1]
BRdissd <- BRdisvars[,2]

BRwndvars <- master_points_longer %>%
  filter(landscape == "BR") %>%
  summarise(mean_wnd = mean(wnd, na.rm = TRUE),
            sd_wnd = sd(wnd, na.rm = TRUE))

BRwndmean <- BRwndvars[,1]
BRwndsd <- BRwndvars[,2]

LRdisvars <- master_points_longer %>%
  filter(landscape == "LR") %>%
  summarise(mean_logdis = mean(logdis, na.rm = TRUE),
            sd_logdis = sd(logdis, na.rm = TRUE))

LRdismean <- LRdisvars[,1]
LRdissd <- LRdisvars[,2]

LRwndvars <- master_points_longer %>%
  filter(landscape == "LR") %>%
  summarise(mean_wnd = mean(wnd, na.rm = TRUE),
            sd_wnd = sd(wnd, na.rm = TRUE))

LRwndmean <- LRwndvars[,1]
LRwndsd <- LRwndvars[,2]

MLdisvars <- master_points_longer %>%
  filter(landscape == "ML") %>%
  summarise(mean_logdis = mean(logdis, na.rm = TRUE),
            sd_logdis = sd(logdis, na.rm = TRUE))

MLdismean <- MLdisvars[,1]
MLdissd <- MLdisvars[,2]

MLwndvars <- master_points_longer %>%
  filter(landscape == "ML") %>%
  summarise(mean_wnd = mean(wnd, na.rm = TRUE),
            sd_wnd = sd(wnd, na.rm = TRUE))

MLwndmean <- MLwndvars[,1]
MLwndsd <- MLwndvars[,2]

NMdisvars <- master_points_longer %>%
  filter(landscape == "NM") %>%
  summarise(mean_logdis = mean(logdis, na.rm = TRUE),
            sd_logdis = sd(logdis, na.rm = TRUE))

NMdismean <- NMdisvars[,1]
NMdissd <- NMdisvars[,2]

NMwndvars <- master_points_longer %>%
  filter(landscape == "NM") %>%
  summarise(mean_wnd = mean(wnd, na.rm = TRUE),
            sd_wnd = sd(wnd, na.rm = TRUE))

NMwndmean <- NMwndvars[,1]
NMwndsd <- NMwndvars[,2]

BR_points_l <- BR_points_l %>%
  mutate(logdis = logdis * BRdissd + BRdismean,
         wnd = wnd * BRwndsd + BRwndmean)

LR_points_l <- LR_points_l %>%
  mutate(logdis = logdis * LRdissd + LRdismean,
         wnd = wnd * LRwndsd + LRwndmean)

ML_points_l <- ML_points_l %>%
  mutate(logdis = logdis * MLdissd + MLdismean,
         wnd = wnd * MLwndsd + MLwndmean)

NM_points_l <- NM_points_l %>%
  mutate(logdis = logdis * NMdissd + NMdismean,
         wnd = wnd * NMwndsd + NMwndmean)

#Bind point sfs together
master_points_graph <- rbind(BR_points_l, LR_points_l, NM_points_l, ML_points_l)

master_points_graph <- master_points_graph %>%
  mutate(dis = 10^logdis)

#Limit distance to 4000 and check relationships
master_points_graph_dislim <- master_points_graph %>% filter(dis <= 4000)

#Graph of predicted hazard over unscaled log distance faceted by landscape
distance_graph_lim_realdis <- master_points_graph_dislim %>%
  mutate(scenario = factor(scenario, levels = c("untreated", "unstaffed", "firebreak", "firing")),
         landscape = case_when(
           landscape == "LR" ~ "Single Segment",
           landscape == "BR" ~ "Branching Network",
           landscape == "NM" ~ "Multiple Segment Network",
           landscape == "ML" ~ "Enclosed Network"
         )) %>% 
  ggplot(aes(x = dis, y = predicted, 
             color = scenario)) +
  facet_wrap(~landscape, nrow = 2, ncol = 2) +
  geom_smooth(method = "lm", se = TRUE) +
  scale_x_continuous(
    limits = c(0, 4000),
    breaks = scales::breaks_extended(n = 8),
    labels = c("0", ".5", "1", "1.5", "2", "2.5", "3", "3.5", "4")
  ) +
  labs(x = "Distance (km)", 
       y = "Uncontrollable Wildfire Risk (UWR)", 
       color = "Leverage\nCondition") +
  theme(axis.text = element_text(size = 14),
        axis.title.x = element_text(size=18, 
                                    margin = margin(t = 10, r = 10, b = 10, l = 10)),
        axis.title.y = element_text(size=18, 
                                    margin = margin(t = 10, r = 10, b = 10, l = 10)),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, size = 1),
        strip.text.x.top = element_text(size = 18),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 18),
        legend.background = element_rect(color = "black", fill = NA, size = 1),
        legend.box.background = element_rect(color = "white", fill = 'white', size = 1),
        legend.position = c(1.13, 0.5), 
        plot.margin = margin(50, 120, 10, 10, unit = "pt"))

setwd("C:/Users/swanj/Documents/Johnsonetal2026_FuelBreaks_UWR/Outputs/figures/")
ggsave("distance_graph.png", distance_graph_lim_realdis, width = 9, height = 10)

#Make Boundary Ridge wind and distance heat map
master_points_graph <- master_points_graph %>%
  mutate(landscape = case_when(
    landscape == "BR" ~ "Branching",
    landscape == "LR" ~ "Segment",
    landscape == "ML" ~ "Enclosed",
    landscape == "NM" ~ "Multiple"
  ))

#BR data clean up
BRpoints_winddishm <- master_points_graph  %>%
  filter(landscape == "Branching") %>%
  select(c(predicted, wnd, dis, scenario, coords)) %>%
  st_drop_geometry() %>%
  na.omit()

#Create breaks
BRpoints_winddishm <- BRpoints_winddishm %>%
  mutate(
    wnd = cut(wnd, 
              breaks = c(-1, -0.75, -0.05, 
                         -0.25, 0, 0.25, 
                         0.5, 0.75, 1), 
              labels = c("-1--0.75", "-0.75--0.5", "-0.05--0.25", 
                         "-0.25-0", "0-0.25", "0.25-0.5", 
                         "0.5-0.75", "0.75-1")),
    dis = cut(dis, 
              breaks = c(0, 100, 200, 300, 
                         400, 500, 750, 
                         1000, 1500, 2000, 
                         2500, 3000), 
              labels = c(".1", ".2", ".3", 
                         ".4", ".5", ".75", 
                         "1", "1.5", "2", 
                         "2.5", "3"))) %>%
  mutate(
    wnd = factor(wnd, 
                 levels = c("-1--0.75", "-0.75--0.5", "-0.05--0.25", 
                            "-0.25-0", "0-0.25", "0.25-0.5", 
                            "0.5-0.75", "0.75-1")),
    dis = factor(dis,
                 levels = c(".1", ".2", ".3", 
                            ".4", ".5", ".75", 
                            "1", "1.5", "2", 
                            "2.5", "3"))) %>%
  na.omit() %>%
  group_by(coords) %>%
  filter(n() == 4) %>%
  ungroup() %>%
  select(scenario, wnd, dis, predicted) %>%
  group_by(scenario, wnd, dis) %>%
  summarize(predicted = mean(predicted))

#BR Heatmap
BRwnddis_hm <- BRpoints_winddishm %>%
  mutate(scenario = factor(scenario, 
                           levels = c("untreated", "unstaffed", "firebreak", "firing"),
                           labels = c("Untreated", "Unstaffed", "Firebreak", "Firing"))) %>%
  ggplot(mapping = aes(
    x = dis,
    y = wnd,
    fill = predicted
  )) +
  geom_tile(color = 'white') +
  scale_fill_gradientn(
    breaks = scales::breaks_extended(n = 5),
    colors = c("#3ea4f0", '#FFF44F', "#c12422"),
    name = "Uncontrollable\nWildfire\nRisk",
    guide = guide_colorbar(barwidth = 2, barheight = 7.5))  +
  coord_fixed() +
  facet_wrap(~scenario, nrow = 2, ncol = 2) +
  theme(legend.position = c(1.19, 0.75),
        legend.ticks = element_line(size = 0.5),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 18, margin = margin(b=5)),
        panel.border = element_rect(color = "black", fill = NA, size = 1),
        legend.background = element_rect(color = "black", fill = NA, size = 0.5),
        panel.background = element_rect(color = "white", fill = "white"),
        legend.spacing = unit(0.5, "cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size = 18, margin = margin(b=2)),
        axis.text = element_text(size = 14),
        axis.title.x = element_text(size=18, 
                                    margin = margin(t = 10, r = 10, b = 10, l = 10)),
        axis.title.y = element_text(size=18, 
                                    margin = margin(t = 10, r = 10, b = 10, l = 10), 
                                    angle = 90)) +
  labs(x = "Distance (km)", y = "Wind Alignment")

setwd("C:/Users/swanj/Documents/Johnsonetal2026_FuelBreaks_UWR/Outputs/figures/")
ggsave(plot = BRwnddis_hm, filename = "BR_wnddis_hm.png", 
       width = 15, height = 6, 
       dpi = 300)

##SI Figures

#Make graphs for UWR vs BP and CFL
setwd("C:/Users/swanj/Documents/Johnsonetal2026_FuelBreaks_UWR/Outputs/raster/")

#Read in tifs
UWR_files <- list.files(pattern = "UT_hazard")
UWR_files <- UWR_files[!grepl("xml", UWR_files)]

#Extract points at 150m grid spacing. 
lapply(UWR_files, function(file_path) {
  # Read the raster file
  r <- rast(file_path)
  
  # Get raster extent and resolution
  ext <- ext(r)  # Get extent
  res_x <- res(r)[1]  # X resolution (30m)
  res_y <- res(r)[2]  # Y resolution (30m)
  
  # Define 1000 spacing so this is faster
  spacing <- 1000
  
  # Generate regular grid coordinates
  x_seq <- seq(ext[1], ext[2], by = spacing)  # X-coordinates
  y_seq <- seq(ext[3], ext[4], by = spacing)  # Y-coordinates
  
  # Create grid of points
  grid_points <- expand.grid(x = x_seq, y = y_seq)
  
  # Extract raster values at grid points
  values <- terra::extract(r, grid_points, ID = FALSE, xy = TRUE)
  
  # Convert to sf points
  values_sf <- st_as_sf(values, coords = c("x", "y"), crs = crs(r), remove = TRUE)
  
  # Remove NA values if needed
  values_sf <- values_sf[!is.na(values_sf$UWR), ]
  
  #assign to environment
  assign(paste0(substr(file_path, 1, 2), "_points"), values_sf, envir = .GlobalEnv)
})

##Making new BP and CFL rasters so I don't have to reproject

setwd("C:\\Users\\swanj\\Documents\\Johnsonetal2026_FuelBreaks_UWR\\Flammap\\")
brfm <- rast("BR_data/BR_inputs/boundaryridge.tif", lyrs = 4)
lrfm <- rast("LR_data/LR_inputs/limestoneridge/limestoneridge/limestoneridge.tif", lyrs = 4)
mlfm <- rast("ML_data/ML_inputs/mtlowe/mtlowe.tif", lyrs = 4)
nmfm <- rast("NM_data/NM_inputs/northmt/northmt.tif", lyrs = 4)


make_CFL_rasters <- function(x){
  setwd("C:\\Users\\swanj\\Documents\\Johnsonetal2026_FuelBreaks_UWR\\Flammap\\FLP_files\\")
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
  
  landscape <- ifelse(substr(x,1,2) == "LR", "lrfm",
                      ifelse(substr(x,1,2) == "ML", 'mlfm',
                             ifelse(substr(x,1,2) == "BR", 'brfm',
                                    "nmfm")))
  
  mask <- get(landscape)
  
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
  
  points_envir <- ifelse(substr(x,1,2) == "LR", "LR_points",
                         ifelse(substr(x,1,2) == "ML", 'ML_points',
                                ifelse(substr(x,1,2) == "BR", 'BR_points',
                                       "NM_points")))
  
  points <- get(points_envir)
  
  points <- points %>%
    mutate(ID = row_number())
  
  values <- terra::extract(b, points, ID = TRUE)
  
  points <- inner_join(points, values, by = join_by("ID"))
  
  
  assign(paste0(substr(x, 1, 6), "CFL.tif"), b, envir = .GlobalEnv)
  assign(points_envir, points, envir = .GlobalEnv)
}

#Run function on all FLP csvs
setwd("C:\\Users\\swanj\\Documents\\Johnsonetal2026_FuelBreaks_UWR\\Flammap\\FLP_files\\")
UWR_csvs <- list.files(pattern = "UT_FLPm")
lapply(UWR_csvs, make_CFL_rasters)

setwd("C:\\Users\\swanj\\Documents\\Johnsonetal2026_FuelBreaks_UWR\\Flammap\\FLP_files\\")
BP_rast <- list.files(pattern = "UT_BP")
BP_rast <- BP_rast[!grepl("xml", BP_rast)]
lapply(BP_rast, function(x) {
  
  b <- rast(x)
  
  
  points_envir <- ifelse(substr(x,1,2) == "LR", "LR_points",
                         ifelse(substr(x,1,2) == "ML", 'ML_points',
                                ifelse(substr(x,1,2) == "BR", 'BR_points',
                                       "NM_points")))
  
  points <- get(points_envir)
  
  
  values <- terra::extract(b, points, ID = TRUE)
  
  points <- inner_join(points, values, by = join_by("ID"))
  
  assign(points_envir, points, envir = .GlobalEnv)
  
})

LR_points$landscape <- "LR"
BR_points$landscape <- "BR"
ML_points$landscape <- "ML"
NM_points$landscape <- "NM"

lapply(c("LR_points", "BR_points", "ML_points", "NM_points"), function(x) {
  
  points <- get(x)
  
  points <- st_transform(points, crs(BR_UT_CFL.tif))
  
  assign(x, points, envir = .GlobalEnv)
  
})

all_points <- rbind(LR_points, BR_points, ML_points, NM_points)

#create weights column
x1 <- c(0.25, 1.86, 2.55, 3.08, 3.9, 4.49, 9.75)
#Y is some arbitrary measure of suppression difficulty 
#reflective of the number and type of resources needed, implications about their
#availability and impact 
y1 <- c(1, 5, 15, 20, 30, 40, 49.999)

plot(x = x1, y = y1)

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
predict<-data.frame(x2,y2) #create the prediction data frame#And add a nice plot 

FIL <- c(0.25, 0.75, 1.25, 1.75, 2.25, 2.75, 3.25, 3.75, 4.25, 4.75, 5.25, 5.75, 6.25, 6.75, 7.25, 
         7.75, 8.25, 8.75, 9.25, 9.75)
weights <- phi1/(1+exp(-(phi2+phi3*FIL)))

predicted_weights <- data.frame(FIL, weights)

all_points <- all_points %>%
  mutate(weights = case_when(
    CFL <= 0.5 ~ predicted_weights[1,2],
    CFL > 0.5 & CFL <= 1 ~ predicted_weights[2,2],
    CFL > 1 & CFL <= 1.5 ~ predicted_weights[3,2],
    CFL > 1.5 & CFL <= 2 ~ predicted_weights[4,2],
    CFL > 2 & CFL <= 2.5 ~ predicted_weights[5,2],
    CFL > 2.5 & CFL <= 3 ~ predicted_weights[6,2],
    CFL > 3 & CFL <= 3.5 ~ predicted_weights[7,2],
    CFL > 3.5 & CFL <= 4 ~ predicted_weights[8,2],
    CFL > 4 & CFL <= 4.5 ~ predicted_weights[9,2],
    CFL > 4.5 & CFL <= 5 ~ predicted_weights[10,2],
    CFL > 5 & CFL <= 5.5 ~ predicted_weights[11,2],
    CFL > 5.5 & CFL <= 6 ~ predicted_weights[12,2],
    CFL > 6 & CFL <= 6.5 ~ predicted_weights[13,2],
    CFL > 6.5 & CFL <= 7 ~ predicted_weights[14,2]
  ))

#Make CFL over BP plot
ggplot(all_points, aes(y = `Burn Probability`, x = CFL)) +
  geom_point(geom_alpha = 0.6) +
  geom_smooth(method = "lm") +
  theme_minimal()

# Pivot to long format for ggplot faceting
all_points_long <- bind_rows(
  all_points %>% select(x = CFL, UWR) %>% mutate(panel = "CFL"),
  all_points %>% select(x = `Burn Probability`, UWR) %>% mutate(panel = "Burn Probability"),
  all_points %>% select(x = weights, UWR) %>% mutate(panel = "weights")
)

# Plot
ggplot(all_points_long, aes(x = x, y = UWR)) +
  geom_point(alpha = 0.6) +
  facet_wrap(~ panel, scales = "free_x") +
  theme_minimal() +
  labs(title = "UWR over covariates")