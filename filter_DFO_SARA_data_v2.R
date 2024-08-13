
library(sf)
library(rgovcan)
library(readxl)
library(tidyverse)
library(gdalUtilities)
library(tidyverse)

ensure_multipolygons <- function(X) {
  tmp1 <- tempfile(fileext = ".gpkg")
  tmp2 <- tempfile(fileext = ".gpkg")
  st_write(X, tmp1)
  ogr2ogr(tmp1, tmp2, f = "GPKG", nlt = "MULTIPOLYGON")
  Y <- st_read(tmp2)
  st_sf(st_drop_geometry(X), geom = st_geometry(Y))
}

bc = bcmaps::bc_bound() |> dplyr::summarise()

# If we haven't yet split this by species, do it now.
if(!file.exists("data/dfo_occ_data_by_species/dfo_occ_bull trout.gpkg")){
  dfo_sara_occ_data = st_read(dsn = 'data/Distribution_Repartition/Distribution_FGP.gdb/',
                              layer = 'DFO_SARA_Dist_2023_FGP_EN')
  
  dfo_sara_occ_data = dplyr::rename(dfo_sara_occ_data, geom = Shape)
  
  # Split data by species?
  purrr::iwalk(unique(dfo_sara_occ_data$Common_Name_EN), ~ {
    print(.x)
    sf::write_sf(dfo_sara_occ_data[dfo_sara_occ_data$Common_Name_EN == .x,],
                 paste0("data/dfo_occ_data_by_species/dfo_occ_",stringr::str_to_lower(.x),".gpkg"))
  })
}

if(!file.exists("data/dfo_occ_data_by_species/dfo_occ_bull trout_cleaned.gpkg")){
  # Replace complex bulltrout geometry with the giant bulltrout polygon I drew.
  bt_big_shape = sf::read_sf("C:/Users/CMADSEN/OneDrive - Government of BC/data/CNF/handmade_bulltrout_NE_BC_polygon.gpkg")
  
  bt = sf::read_sf("data/dfo_occ_data_by_species/dfo_occ_bull trout.gpkg")
  
  bt_big_shape = st_transform(bt_big_shape, st_crs(bt))
  
  bt_rows_to_drop = bt |> 
    sf::st_intersects(bt_big_shape)
  
  bt_within_polygon = bt[!is.na(as.numeric(bt_rows_to_keep)),]
  
  bt_outside_polygon = bt[is.na(as.numeric(bt_rows_to_keep)),]
  
  bt_outside_polygon = dplyr::bind_rows(
    bt_outside_polygon,
    bt_big_shape
  )
  
  ggplot() + geom_sf(data = bt_big_shape) + geom_sf(data = bt_outside_polygon)
  
  sf::write_sf(bt_outside_polygon, "data/dfo_occ_data_by_species/dfo_occ_bull trout_cleaned.gpkg")
}

bc = sf::st_transform(bc, 3857)

bc_bbox = sf::st_as_sf(sf::st_as_sfc(sf::st_bbox(bc)))

files_to_clean = list.files(path = "data/dfo_occ_data_by_species/",
           pattern = ".gpkg")

# Make sure we are using the cleaned bulltrout object!
files_to_clean = files_to_clean[files_to_clean != 'dfo_occ_bull trout.gpkg']

purrr::iwalk(
  files_to_clean, ~ {
    
    print(.x)
    
    if(!file.exists(paste0("data/dfo_occ_data_by_species_BC/",.x))){
      # dat = sf::read_sf(paste0("data/dfo_occ_data_by_species/",.x))
      dat = sf::read_sf(paste0("data/dfo_occ_data_by_species/",.x))
      
      # Make sure geometry is clean
      dat = suppressWarnings(suppressMessages(ensure_multipolygons(dat)))
      
      # Filter to just BC bounding box.
      dat = tryCatch(
        expr = dat |> 
          sf::st_filter(bc_bbox),
        error = function(e) return(NULL)
      )
      
      if(nrow(dat) > 0){
        
        # Filter to BC polygon.
        dat = tryCatch(
          expr = dat |> 
            sf::st_filter(bc),
          error = function(e) return(NULL)
        )
        
        if(nrow(dat) > 0){
          
          sf::write_sf(dat, paste0("data/dfo_occ_data_by_species_BC/",.x))
          
          print('wrote to file...')
        }
      }
    }
  }
)

# Now combine all of our DFO occurrences in BC into one file :D

dfo_sara_bc = list.files(path = 'data/dfo_occ_data_by_species_BC/',
           full.names = T) |> 
  lapply(\(x) sf::read_sf(x)) |> 
  dplyr::bind_rows()

dfo_sara_bc |> 
  sf::write_sf("C:/Users/CMADSEN/OneDrive - Government of BC/data/CNF/dfo_occurrences_in_BC_all_species.gpkg")
