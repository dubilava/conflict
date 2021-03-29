# Replication Material

This repository hosts the data and R codes for the paper "Commodity Price Shocks and the Seasonality of Conflict," co-authored with Kadir Atalay.

## Data

- The conflict data were obtained from Armed Conflict Location & Event Data Project (ACLED), available at: [http://acleddata.com](www.acleddata.com). For details, see: Raleigh, C., A. Linke, H. Hegre, and J. Karlsen (2010). Introducing ACLED: "An Armed Conflict Location and Event Dataset: Special Data Feature." Journal of Peace Research 47(5): 651–660.

- The crop harvest data were obtained from the Center for Sustainability and the Global Environment, Nelson Institute at University of Wisconsin--Madison, available at: [https://nelson.wisc.edu/sage/data-and-models/crop-calendar-dataset/index.php](https://nelson.wisc.edu/sage/data-and-models/crop-calendar-dataset/index.php). For details, see: Sacks, W. J., D. Deryng, J. A. Foley, and N. Ramankutty (2010). "Crop Planting Dates: An Analysis of Global Patterns." Global Ecology and Biogeography 19(5): 607–620.

## R Codes

- regressions.r replicates the main results of the study
- illustrations.r generates the figures of the main text as well as some of the figures of the appendix
- robustness.r replicates some of the robustness checls of the study
- sensitivity.r generates two figures that address the sensitivity of the main results to omitting a year or a lattitude from the data
- falsification.r generates the figure that displays parameter estimates from a simulation exerise that randomizes the harvest seasons across the locations

