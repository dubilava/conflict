# Replication Material

This repository hosts data and R codes for the paper "Agricultural Windfalls and the Seasonality of Political Violence in Africa," co-authored with Justin Hastings and Kadir Atalay.

## Data

The data used in the analysis are stored in the data_violence_acled.RData file. This file compiles the data from the following sources:

- The *conflict* data, obtained from Armed Conflict Location & Event Data Project (ACLED) available at: [http://acleddata.com](www.acleddata.com). For details, see: Raleigh, C., A. Linke, H. Hegre, and J. Karlsen (2010). Introducing ACLED: "An Armed Conflict Location and Event Dataset: Special Data Feature." Journal of Peace Research 47(5): 651–660.
  * As a robustness check, we also consider the Uppsala Conflict Data Program (UCDP) Georeferenced Event Dataset (global version 21.1) available at: [https://ucdp.uu.se](https://ucdp.uu.se). 

- The *commodity price* data, obtained from the International Monetary Fund's online portal for Primary Commodity Prices, available at [https://www.imf.org/en/Research/commodity-prices](https://www.imf.org/en/Research/commodity-prices).

- The *crop harvest* data, obtained from the Center for Sustainability and the Global Environment, Nelson Institute at University of Wisconsin--Madison, available at: [https://nelson.wisc.edu/sage/data-and-models/crop-calendar-dataset/index.php](https://nelson.wisc.edu/sage/data-and-models/crop-calendar-dataset/index.php). For details, see: Sacks, W. J., D. Deryng, J. A. Foley, and N. Ramankutty (2010). "Crop Planting Dates: An Analysis of Global Patterns." Global Ecology and Biogeography 19(5): 607–620.
  * An alternative source of the data (apparently more nuanced data but doesn't include harvest calendars) is EathStat, available at [http://www.earthstat.org/](http://www.earthstat.org/)

- The *population* data obtained from the Center for International Earth Science Information Network at Columbia University available at [https://sedac.ciesin.columbia.edu](https://sedac.ciesin.columbia.edu).

- The *weather* data are the ERA5 reanalysis gridded data on daily 2m above the surface air temperatures and monthly averaged total precipitation from European Centre for Medium-Range Weather Forecasts (ECMWF) Copernicus Project, available at [https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-single-levels](https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-single-levels).  


## R Codes

The R codes to replicate the study are as follows:

- 01-baseline.r replicates the main results of the study, plots the relevant figures.
- 02-illustrations.r generates a set of descriptive figures of the main text as well as some of the figures of the appendix.
- 03-robustness_tables.r replicates some of the robustness checks of the study that are presented as Appendix Tables.
- 04-robustness_latitudes.r generates a figure that addressed the sensitivity of the main results to omitting locations based on latitudes.
- 05-robustness_years.r generates a figure that addressed the sensitivity of the main results to the time-frame of the data.
- 06-robustness_datasets.r generates the figure that compares estimated seasonal patterns between ACLED and UCDP data.
- 07-mechanism_rain.r generates the figure that compares estimated seasonal patterns between 'average' and 'rainy' growing seasons.
- 08-mechanism_temp.r generates the figure that compares estimated seasonal patterns between 'average' and 'hot' growing seasons.
- 09-mechanism_dose.r generates the figure that compares estimated seasonal patterns across different cropland area fractions.


## License

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

