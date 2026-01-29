library(sf)
library(glmmTMB)
library(tidyverse)
library(DHARMa)
library(parallel)
library(snow)
library(cv)
library(performance)

#Read in modeling data frames
setwd("C:/Users/swanj/Documents/Johnsonetal2026_FuelBreaks_UWR/Outputs/data_frames/")
BR_points_l <- st_read("BR_points_final.gpkg")
LR_points_l <- st_read("LR_points_final.gpkg")
NM_points_l <- st_read("NM_points_final.gpkg")
ML_points_l <- st_read("ML_points_final.gpkg")

#Boundary Ridge Model
BRmodelallvars <- glmmTMB(hazard ~ scenario + logdis + wnd + TPI +  wdth + 
                            Slope + FB_cats + sinu + proxix + length +
                            (scenario * logdis) + (scenario * wnd) + (scenario * TPI) + 
                            (scenario * wdth) + (scenario * sinu) + (scenario * proxix) + 
                            (scenario * length) + (1 | coords), 
                          data = BR_points_l, family = gaussian(), 
                          control = glmmTMBControl(optCtrl=list(iter.max=1e3,eval.max=1e3)),
                          REML = TRUE, na.action = na.fail)

#Testing residuals
BR_points_l$residuals <- residuals(BRmodelallvars, type = "pearson")

#predict using og data
BR_points_l$predicted <- predict(BRmodelallvars, newdata = BR_points_l)

#plotting predicted against residuals
plot(x = BR_points_l$predicted, y = BR_points_l$residuals)

#Join with points
BR_points_l <- master_points_longer %>%
  filter(landscape == "BR") %>%
  select(geometry, coords, scenario) %>%
  right_join(BR_points_l, by = join_by("scenario", "coords"))

#variogram of residuals (looks good)
BRresidvario <- variogram(object = residuals ~ 1, data = BR_points_l)


#Limestone Ridge model
LRmodelallvars <- glmmTMB(hazard ~ scenario + logdis + wnd + wdth + 
                            Slope + FB_cats + sinu + TPI + 
                            (scenario * logdis) + (scenario * wnd) + (scenario * TPI) + 
                            (scenario * wdth) + (scenario * sinu) + (1 | coords), 
                          data = LR_points_l, family = gaussian(), 
                          control = glmmTMBControl(optCtrl=list(iter.max=1e3,eval.max=1e3)),
                          REML = TRUE, na.action = na.fail)

#Testing residuals
LR_points_l$residuals <- residuals(LRmodelallvars, type = "pearson")

#predict using og data
LR_points_l$predicted <- predict(LRmodelallvars, newdata = LR_points_l)

#plotting predicted against residuals
plot(x = LR_points_l$predicted, y = LR_points_l$residuals)

#Join with points
LR_points_l <- master_points_longer %>%
  filter(landscape == "LR") %>%
  select(geometry, coords, scenario) %>%
  right_join(LR_points_l, by = join_by("scenario", "coords"))

#Residual correlation - looks ok
LRresidvario <- variogram(residuals ~ 1, LR_points_l)


#North Mountain Model
NMmodelallvars <- glmmTMB(hazard ~ scenario + logdis + wnd + TPI +  wdth + 
                            Slope + FB_cats + sinu + proxix + length +
                            (scenario * logdis) + (scenario * wnd) + (scenario * TPI) + 
                            (scenario * wdth) + (scenario * sinu) + (scenario * proxix) + 
                            (scenario * length) + (1 | coords), 
                          data = NM_points_l, family = gaussian(), 
                          control = glmmTMBControl(optCtrl=list(iter.max=1e3,eval.max=1e3)),
                          REML = TRUE, na.action = na.fail)


#Testing residuals
NM_points_l$residuals <- residuals(NMmodelallvars, type = "pearson")

#predict using og data
NM_points_l$predicted <- predict(NMmodelallvars, newdata = NM_points_l)

#plotting predicted against residuals
plot(x = NM_points_l$predicted, y = NM_points_l$residuals)

#Join with points
NM_points_l <- master_points_longer %>%
  filter(landscape == "NM") %>%
  select(geometry, coords, scenario) %>%
  right_join(NM_points_l, by = join_by("scenario", "coords"))

#Residual correlation
#If the variance between two points changes with distance, points are autocorrelated, where the curve plateus they are no longer correlated
NMresidvario <- variogram(residuals ~ 1, NM_points_l)

#Mount Lowe model
MLmodelallvars <- glmmTMB(hazard ~ scenario + logdis + wnd + TPI +  wdth + 
                            Slope + FB_cats + sinu + proxix + length +
                            (scenario * logdis) + (scenario * wnd) + (scenario * TPI) + 
                            (scenario * wdth) + (scenario * sinu) + (scenario * proxix) + 
                            (scenario * length) + (1 | coords), 
                          data = ML_points_l, family = gaussian(), 
                          control = glmmTMBControl(optCtrl=list(iter.max=1e3,eval.max=1e3)),
                          REML = TRUE, na.action = na.fail)


#Testing residuals
ML_points_l$residuals <- residuals(MLmodelallvars, type = "pearson")

#predict using og data
ML_points_l$predicted <- predict(MLmodelallvars, newdata = ML_points_l)

#plotting predicted against residuals
plot(x = ML_points_l$predicted, y = ML_points_l$residuals)

#Join with points
ML_points_l <- master_points_longer %>%
  filter(landscape == "ML",
         FB_cats != "SB") %>%
  select(geometry, coords, scenario) %>%
  right_join(ML_points_l, by = join_by("scenario", "coords"))

#Residual correlation - looks good/ok
MLresidvario <- variogram(residuals ~ 1, data = ML_points_l)

##MODEL VALIDATION

#Cross validation
#cross-validation criterion = MSE averaged across all folds, computed for all of the n values of the response y
#   based on fitted values yˆfrom the model fit to all of the data.
#bias-adjusted cross-validation criterion = MSE averaged across all folds, adjusted for bias
#95% CI for bias-adjusted CV criterion = 95% confidence interval for the bias-adjusted CV criterion, so this includes standard deviations of MSE
#full-sample criterion = MSE for the full sample

LRcv <- cv(model = LRmodelallvars,
           data = insight::get_data(LRmodel),
           k = 10,
           ncores = 15)
#R RNG seed set to 148343
#10-Fold Cross Validation
#criterion: mse
#cross-validation criterion = 0.0351263
#bias-adjusted cross-validation criterion = 0.03414336
#95% CI for bias-adjusted CV criterion = (0.02937225, 0.03891446)
#full-sample criterion = 0.0189688

BRcv <- cv::cv(model = BRmodelallvars,
               data = insight::get_data(BRmodel),
               k = 10,
               ncores = 15)
#R RNG seed set to 142239
#10-Fold Cross Validation
#criterion: mse
#cross-validation criterion = 0.03797794
#bias-adjusted cross-validation criterion = 0.03660142
#95% CI for bias-adjusted CV criterion = (0.03378173, 0.03942111)
#full-sample criterion = 0.01825461

MLcv <- cv::cv(model = MLmodelallvars,
               data = insight::get_data(MLmodel),
               k = 10,
               ncores = 15)
#R RNG seed set to 646578
#10-Fold Cross Validation
#criterion: mse
#cross-validation criterion = 0.03462634
#bias-adjusted cross-validation criterion = 0.03353204
#95% CI for bias-adjusted CV criterion = (0.03134398, 0.03572011)
#full-sample criterion = 0.01786038

NMcv <- cv::cv(model = NMmodelallvars,
               data = insight::get_data(NMmodel),
               k = 10,
               ncores = 15)
#R RNG seed set to 150894
#10-Fold Cross Validation
#criterion: mse
#cross-validation criterion = 0.06753085
#bias-adjusted cross-validation criterion = 0.06545556
#95% CI for bias-adjusted CV criterion = (0.06081157, 0.07009956)
#full-sample criterion = 0.03536383

r2(LRmodelallvars)
#Conditional R2: 0.992
#Marginal R2: 0.329
r2(BRmodelallvars)
# Conditional R2: 0.993
#Marginal R2: 0.417
r2(NMmodelallvars)
#Conditional R2: 0.992
#Marginal R2: 0.662
r2(MLmodelallvars)
#Conditional R2: 0.996
#Marginal R2: 0.600


#Save models
setwd("C:/Users/swanj/Documents/Research/fuel_break_systems/Models/")
saveRDS(LRmodelallvars, "LRmodel_forpaper.rds")
saveRDS(BRmodelallvars, "BRmodel_forpaper.rds")
saveRDS(MLmodelallvars, "MLmodel_forpaper.rds")
saveRDS(NMmodelallvars, "NMmodel_forpaper.rds")