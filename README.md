# Replication Material

This repository hosts data and R codes for the paper "Commodity Price Shocks and the Seasonality of Political Violence in Africa," co-authored with Kadir Atalay and Justin Hastings.

## Data

The data used in the analysis are stored in the data_violence_acled.RData file. This file compiles the data from the following sources:

- The conflict data, obtained from Armed Conflict Location & Event Data Project (ACLED) available at: [http://acleddata.com](www.acleddata.com). For details, see: Raleigh, C., A. Linke, H. Hegre, and J. Karlsen (2010). Introducing ACLED: "An Armed Conflict Location and Event Dataset: Special Data Feature." Journal of Peace Research 47(5): 651–660.
  * As a robustness check, we also consider the Uppsala Conflict Data Program (UCDP) Georeferenced Event Dataset (global version 21.1) available at: [https://ucdp.uu.se](https://ucdp.uu.se). 

- The commodity price data, obtained from the International Monetary Fund's online portal for Primary Commodity Prices, available at [https://www.imf.org/en/Research/commodity-prices](https://www.imf.org/en/Research/commodity-prices).   * For a robustness check, we also consider selected price indices from the the Food and Agricultural Organization of the United Nation's online portal for World Food Situation available at: [https://www.fao.org/worldfoodsituation/foodpricesindex/en](https://www.fao.org/worldfoodsituation/foodpricesindex/en).

- The crop harvest data, obtained from the Center for Sustainability and the Global Environment, Nelson Institute at University of Wisconsin--Madison, available at: [https://nelson.wisc.edu/sage/data-and-models/crop-calendar-dataset/index.php](https://nelson.wisc.edu/sage/data-and-models/crop-calendar-dataset/index.php). For details, see: Sacks, W. J., D. Deryng, J. A. Foley, and N. Ramankutty (2010). "Crop Planting Dates: An Analysis of Global Patterns." Global Ecology and Biogeography 19(5): 607–620.
  * An alternative source of the data (apparently more nuanced data but doesn't include harvest calendars) is EathStat, available at [http://www.earthstat.org/](http://www.earthstat.org/)

- The population data obtained from the Center for International Earth Science Information Network at Columbia University available at [https://sedac.ciesin.columbia.edu](https://sedac.ciesin.columbia.edu).


## R Codes

The R codes to replicate the study are as follows:

- regressions.r replicates the main results of the study.
- illustrations.r generates the figures of the main text as well as some of the figures of the appendix.
- robustness.r replicates some of the robustness checls of the study.
- sensitivity.r generates two figures that address the sensitivity of the main results to omitting a year or a lattitude from the data.
- falsification.r generates the figure that displays parameter estimates from a simulation exerise that randomizes the harvest seasons across the locations.


## License

<a rel="license" href="http://creativecommons.org/licenses/by/4.0/"><img alt="Creative Commons License" style="border-width:0" src="https://i.creativecommons.org/l/by/4.0/88x31.png" /></a><br />This work is licensed under a <a rel="license" href="http://creativecommons.org/licenses/by/4.0/">Creative Commons Attribution 4.0 International License</a>.

