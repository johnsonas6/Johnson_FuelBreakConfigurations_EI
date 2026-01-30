# Johnson\_FuelBreakConfigurations\_EI

Analytical code for Johnson et al. 2026 "Configuration of fuel break networks influence landscape-level fire-risk in California"

Data available on Zenodo: https://doi.org/10.5281/zenodo.18422834

Run Scripts in number order to recreate results. File paths to files downloadable from Zenodo are below scripts:


1UWR - Creates Uncontrollable Wildfire Risk Rasters.



Inputs: 

1\) Flame Length Probability csvs 

(Johnsonetal2026\_FuelBreaks\_UWR\\Flammap\\FLP\_files)

2\) 40FBFM fuel model input raster used in fire modeling to create Flame Length Probability 

(Johnsonetal2026\_FuelBreaks\_UWR\\Flammap\\LR|BR|NM|ML\_data\\LR|BR|NM|ML\_inputs)



Outputs: 

1\) Uncontrollable Wildfire Risk raster

(Johnsonetal2026\_FuelBreaks\_UWR\\Outputs\\raster)



2TPI - Creates Topographic Position Index rasters from USGS dems.



Inputs: 

1\) USGS Digital Elevation Models 

(Johnsonetal2026\_FuelBreaks\_UWR\\DEMs)



4Create\_Model\_dfs - Creates model data frames from sample points across UWR landscapes. Performs a bunch of vector functions to calculate spatial relationships for fuel breaks and extracts raster values to fuel breaks.



Inputs: 

1)UWR rasters (Johnsonetal2026\_FuelBreaks\_UWR\\Outputs\\raster)

2)40FBFM fuel model and slope input raster used in fire modeling to create Flame Length Probability

(Johnsonetal2026\_FuelBreaks\_UWR\\Flammap\\LR|BR|NM|ML\_data\\LR|BR|NM|ML\_inputs)

3)merged fuel break shapefiles

(Johnsonetal2026\_FuelBreaks\_UWR\\Fuelbreak\_polygons)

4)merged fuel break with firing operation shapefiles

(Johnsonetal2026\_FuelBreaks\_UWR\\Fuelbreak\_polygons)



Outputs: (all outputs in Johnsonetal2026\_FuelBreaks\_UWR\\Outputs\\data\_frames)

1)Initial sampling points

2)Master sampling data frames

3)Long pivot sampling data frame for modeling



5Models - Runs LMMs on data frames created in the previous step.



Inputs:

1)Long pivot sampling data frame for modeling

(Johnsonetal2026\_FuelBreaks\_UWR\\Outputs\\data\_frames)



Outputs:

2)RDS models

Johnsonetal2026\_FuelBreaks\_UWR\\Outputs\\models



6BPandCFL - Creates Burn Probability and Conditional Flame Length Rasters.



Inputs: 

1\) Flame Length Probability csvs (Johnsonetal2026\_FuelBreaks\_UWR\\Flammap\\FLP\_files)

2\) 40FBFM fuel model input raster used in fire modeling to create Flame Length Probability

(Johnsonetal2026\_FuelBreaks\_UWR\\Flammap\\LR|BR|NM|ML\_data\\LR|BR|NM|ML\_inputs)



Outputs: (all outputs in (Johnsonetal2026\_FuelBreaks\_UWR\\Outputs\\raster))

1\) Burn Probability Rasters

2\) Conditional Flame Length Rasters



7Table2\_analysis.R



Inputs:

1\) Burn Probability Rasters 

(Johnsonetal2026\_FuelBreaks\_UWR\\Outputs\\raster)

2\) Conditional Flame Length Rasters 

(Johnsonetal2026\_FuelBreaks\_UWR\\Outputs\\raster)

3\) fire size csvs 

(Johnsonetal2026\_FuelBreaks\_UWR\\Flammap\\FireSize\_lists)

4\) UWR rasters 

(Johnsonetal2026\_FuelBreaks\_UWR\\Outputs\\raster)





Outputs:

2)Tables for paper and SI

(Johnsonetal2026\_FuelBreaks\_UWR\\Outputs\\tables)





8Figures  - Code for figures from the main paper and supplementary information.



Inputs:

1)UWR rasters

(Johnsonetal2026\_FuelBreaks\_UWR\\Outputs\\raster)



Outputs:

1)Figures

(Johnsonetal2026\_FuelBreaks\_UWR\\Outputs\\figures)



