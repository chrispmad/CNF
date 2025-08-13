library(sf)
library(tidyverse)

base_dir = stringr::str_extract(getwd(),"C:\\/Users\\/[a-zA-Z]+")
onedrive_wd = "//SFP.IDIR.BCGOV/S140/S40203/WFC AEB/General/2 SCIENCE - Invasives/AIS_R_Projects/LargeDataFiles/"
lan_root = "//SFP.IDIR.BCGOV/S140/S40203/WFC AEB/General/"

bc = bcmaps::bc_bound()

scbt = sf::read_sf(paste0(base_dir,"/Downloads/wetransfer_south-coast-bull-trout-gis-files_2025-02-13_0022/BTDesignatableUnits.shp"))

scbt_t = sf::st_intersection(scbt, bc)

scbt_t = scbt_t[scbt_t$Designatab == "DU1 - Southcoast BC populations",]

scbt_t = scbt_t |> sf::st_transform(4326)

scbt_t = scbt_t |> 
  dplyr::summarise(Common_Name_EN = "Bull Trout",
                   Scientific_Name = 'Salvelinus confluentus',
                   Population_EN = 'South Coast',
                   Taxon = 'Fishes',
                   Ecotype = 'Freshwater') |> 
  dplyr::rename(geom = geometry)

ggplot() + geom_sf(data = scbt_t, aes(fill = Common_Name_EN))

# Bring in the SAR polygon file.
sar = sf::read_sf(paste0(onedrive_wd,"CNF/dfo_sara_and_crit_hab_and_sockeye_data.gpkg"))

sar = sar |> sf::st_transform(4326)

# remove bull trout points that fall within the new polygon.
sar_d = sar |> 
  # sf::st_join(scbt_t)
  dplyr::filter(!(Common_Name_EN == 'Bull Trout' & Population_EN == 'South Coast'))

# sar_d = sar_d |> 
#   dplyr::filter(is.na(Designatab))

sar_c = sar_d |> 
  dplyr::bind_rows(scbt_t)

ggplot() + geom_sf(data = sar_c)

sf::write_sf(sar_c, paste0(onedrive_wd,"CNF/dfo_sara_and_crit_hab_bulltrout_and_sockeye_data.gpkg"))

