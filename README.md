# Johnson_FuelBreakConfigurations_EI
Analytical code for Johnson et al. 2026 "Configuration of fuel break networks influence landscape-level fire-risk in California"

Run Scripts in number order to recreate results.

1UWR - Creates Uncontrollable Wildfire Risk Rasters.

2BPandCFL - Creates Burn Probability and Conditional Flame Length Rasters.

3TPI - Creates Topographic Position Index rasters from USGS dems.

4Create_Model_dfs - Creates model data frames. Performs a bunch of vector functions to calculate spatial relationships for fuel breaks.

5Models - Runs LMMs on data frames created in the previous step.

7Figures  - Code for figures from the main paper and supplementary information.
