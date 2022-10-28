# Replication Material

This repository hosts data and R codes for the paper "[Agricultural Windfalls and the Seasonality of Political Violence in Africa](https://arxiv.org/abs/2202.07863)," co-authored with Justin Hastings and Kadir Atalay.

## Data

The data used for generating the main results of the study are stored in the data_violence_acled.RData file. This file compiles the data from the following sources:

- The *conflict* data, obtained from Armed Conflict Location & Event Data Project (ACLED) available at: [http://acleddata.com](www.acleddata.com). For details, see: Raleigh, C., A. Linke, H. Hegre, and J. Karlsen (2010). Introducing ACLED: "An Armed Conflict Location and Event Dataset: Special Data Feature." Journal of Peace Research 47(5): 651–660.
  * As a robustness check, we also consider the Uppsala Conflict Data Program (UCDP) Georeferenced Event Dataset (global version 21.1) available at: [https://ucdp.uu.se](https://ucdp.uu.se). 

- The *commodity price* data, obtained from the International Monetary Fund's online portal for Primary Commodity Prices, available at [https://www.imf.org/en/Research/commodity-prices](https://www.imf.org/en/Research/commodity-prices).

- The *crop harvest* data, obtained from the Center for Sustainability and the Global Environment, Nelson Institute at University of Wisconsin--Madison, available at: [https://sage.nelson.wisc.edu/data-and-models/datasets/crop-calendar-dataset](https://sage.nelson.wisc.edu/data-and-models/datasets/crop-calendar-dataset). For details, see: Sacks, W. J., D. Deryng, J. A. Foley, and N. Ramankutty (2010). "Crop Planting Dates: An Analysis of Global Patterns." Global Ecology and Biogeography 19(5): 607–620.
  * An alternative source of the similar data (potentially more nuanced data but don't seem to include harvest calendars) is EathStat, available at [http://www.earthstat.org/](http://www.earthstat.org/)

- The *population* data obtained from the Center for International Earth Science Information Network at Columbia University available at [https://sedac.ciesin.columbia.edu](https://sedac.ciesin.columbia.edu).

- The *weather* data are the ERA5 reanalysis gridded data on daily 2m above the surface air temperatures and monthly averaged total precipitation from European Centre for Medium-Range Weather Forecasts (ECMWF) Copernicus Project, available at [https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-single-levels](https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-single-levels). 

The additional data used for generating some of the robustness checks are from the following sources:

- The mining data are from Berman, N, Couttenier, M., Rohner, D., and M. Thoenig (2017). "This Mine Is Mine! How Minerals Fuel Conflicts in Africa." American Economic Review, 107 (6): 1564-1610. Available at [https://www.aeaweb.org/articles?id=10.1257/aer.20150774](https://www.aeaweb.org/articles?id=10.1257/aer.20150774)

- The pastoral suitability data are from Beck, J., and A. Sieber (2010). "Is the Spatial Distribution of Mankind’s Most Basic Economic Traits Determined by Climate and Soil Alone?" PloS One 5(5), e10416. Available at [https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0010416](https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0010416)


## R Codes

The R codes to replicate the study are as follows:

- [01-tables.r](01-tables.r): Tables 1-3 (main text), and Appendix Tables B35-B39.
- [02-robustness.r](03-robustness_tables.r): Appendix Tables B1-B34.
- [03-illustrations.r](02-illustrations.r): Figures 1-6 (main text), and Appendix Figures C1-C7.


## License

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

