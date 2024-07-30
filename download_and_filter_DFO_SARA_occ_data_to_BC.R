# Title: Explore Federal SARA Critical Habitat spatial files
#
# Date: 2024-01-23
#
# Author(s): Chris Madsen (chris.madsen@gov.bc.ca)
# 
# Description: This script explores the federal dataset.

library(sf)
library(rgovcan)
library(readxl)
library(tidyverse)
library(gdalUtilities)

# ---------------------------------
# Options

# Do we want to jump into a specific part of this lengthy pipeline?
jump_point_options = c("scratch",
                       "filter_for_SARA_fish",
                       "filter_with_BC_bounding_box",
                       "trim_data_to_bc_polygon",
                       "filter_for_WD_susceptible_species",
                       "filter_for_fraser_columbia_priority_region",
                       "none" # I don't want to repeat any data analysis!
)

jump_point = jump_point_options[5]

ensure_multipolygons <- function(X) {
  tmp1 <- tempfile(fileext = ".gpkg")
  tmp2 <- tempfile(fileext = ".gpkg")
  st_write(X, tmp1)
  ogr2ogr(tmp1, tmp2, f = "GPKG", nlt = "MULTIPOLYGON")
  Y <- st_read(tmp2)
  st_sf(st_drop_geometry(X), geom = st_geometry(Y))
}

species = read_excel('data/SARA-listed_Canadian_FW_fish.xlsx')

cosewic_species = c("Chinook Salmon",
                    "Coho Salmon",
                    "Eulachon",
                    "Sockeye Salmon",
                    "Steelhead Salmon",
                    "White Sturgeon")

species_w_cos = tibble(
  grouping = c(rep('SARA',nrow(species)),rep('COSEWIC',length(cosewic_species))),
  species = c(species$Species,cosewic_species)
)

species_w_cos_searcher = paste0("(",paste0(unique(species_w_cos$species),collapse='|'),")")

dus = read_excel('data/Copy of Measures-for-Species.xlsx')

wd_sensitive = tibble(scientific = c("Oncorhynchus clarkii",
                                  "Oncorhynchus kisutch",
                                  "Oncorhynchus mykiss",
                                  "Oncorhynchus nerka",
                                  "Oncorhynchus tshawytscha",
                                  "Prosopium williamsoni",
                                  "Salmo salar",
                                  "Salmo trutta",
                                  "Salvelinus confluentus",
                                  "Salvelinus fontinalis"),
                      common = c('Cutthroat Trout',
                                 'Coho Salmon',
                                 'Rainbow Trout',
                                 'Sockeye Salmon',
                                 'Chinook Salmon',
                                 'Mountain Whitefish',
                                 'Atlantic Salmon',
                                 'Brown Trout',
                                 'Bull Trout',
                                 'Brook Trout')
)

wd_sensitive_searcher = wd_sensitive |> 
  summarise(scientific_pattern = paste0('(',paste0(scientific, collapse = '|'),')'),
            common_pattern = paste0('(',paste0(common, collapse = '|'),')'))

# Ensure we have the DFO dataset in its filtered, final form:
# 3 filters applied to original DFO dataset:
# (1) SARA-listed freshwater fish species
# (2) Those species sensitive to whirling disease
# (3) Polygons are located in BC
# (4) Points are located in the Fraser / Columbia River basins

# Not Currently Applied: Just SARA-listed POPULATIONS / DUs

# if(jump_point == 'cut_down_columns'){

if(jump_point == 'filter_for_fraser_columbia_priority_region'){
  
  if(jump_point == 'filter_for_WD_susceptible_species'){
    
    if(jump_point == 'trim_data_to_bc_polygon'){
      
      if(jump_point == 'filter_with_BC_bounding_box'){
        
        if(jump_point == 'filter_for_SARA_fish'){
          # Need to filter SARA-listed DFO national dataset for just 
          # SARA-listed Canadian FW fish species.
          
          if(jump_point == 'scratch'){
            # Necessary to download Canada-wide species-at-risk records!
            
            dfo_sara_ranges = rgovcan::govcan_get_record(record_id = 'e0fabad5-9379-4077-87b9-5705f28c490b')
            dfo_sara_ranges_resources = govcan_get_resources(dfo_sara_ranges)
            govcan_dl_resources(dfo_sara_ranges_resources, path = 'data/')
            # The above download takes a long time!
          } else {
            # Unzip the download by hand (for now?)
            # DFO_distribution_layers = st_layers('data/Distribution_Repartition/Distribution_FGP.gdb/')
            dfo_sara_occ_data = st_read(dsn = 'data/Distribution_Repartition/Distribution_FGP.gdb/',
                                        layer = 'DFO_SARA_Dist_2023_FGP_EN')
            
            dfo_sara_occ_data = dplyr::rename(dfo_sara_occ_data, geom = Shape)
            
            # Filter by Federal SARA-listed fish species list AND COSEWIC!
            dfo_sara_occ_data_fish = dfo_sara_occ_data |>
              # dplyr::filter(Common_Name_EN %in% species$Species)
              dplyr::filter(stringr::str_detect(stringr::str_to_title(Common_Name_EN),species_w_cos_searcher))
            
            dfo_sara_occ_data_fish_no_geom = sf::st_drop_geometry(dfo_sara_occ_data_fish)
            dfo_national_dataset_SARA_and_COSEWIC_count = dplyr::count(dfo_sara_occ_data_fish_no_geom, Common_Name_EN, sort = T)
            
            write.csv(dfo_national_dataset_SARA_and_COSEWIC_count, 'output/dfo_national_dataset_SARA_and_COSEWIC_count.csv', row.names = F)
            
            # Write out to data/ folder
            write_sf(dfo_sara_occ_data_fish,
                     'data/dfo_national_sara_cosewic_fishes.gpkg')
            rm(dfo_sara_occ_data)
            rm(dfo_sara_occ_data_fish)
            gc()
          }
        } else {
          dfo_sara_occ_data_fish = read_sf('data/dfo_sara_fw_fishes.gpkg')
        }
        # See if you can do a spatial overlay with BC... this could be a useful dataset to have
        # at our fingertips.
        bc = bcmaps::bc_bound() |> 
          dplyr::summarise() |> 
          st_bbox() |> 
          st_as_sfc() |> 
          st_transform(crs = 3857)
        
        # dfo_sara_occ_data_fish_centroids = sf::st_centroid(dfo_sara_occ_data_fish)
          
        # This file has some weird geometries that don't allow for 
        # typical intersects... going to have to go row-wise and use tryCatch
        # in case of errors. 'Twill be slow!
        dfo_sara_in_BC = list() #Container for results
        
        for(i in 1:nrow(dfo_sara_occ_data_fish)){
          dat = dfo_sara_occ_data_fish[i,]
          print(i)
          # Try to correct weird geometries... If they are not 
          # polygons or linestrings, 'correct' them to be so.
          if(!st_geometry_type(dat) %in% c("POLYGON","MULTIPOLYGON","MULTILINESTRING")){
            dat = ensure_multipolygons(dat)
          }
          
          # Test to see if an overlap can be performed.
          workable_geometry = tryCatch(
            sf::st_intersects(dat$geom, bc, sparse = F)[1],
            error = function(e) return('error in geometry')
          )
          if(workable_geometry != 'error in geometry'){
            if(sf::st_intersects(dat$geom, bc, sparse = F)[1]){
              dfo_sara_in_BC[[i]] <- dat
            } else {
              dfo_sara_in_BC[[i]] <- NULL
            }
          } else {
            # An overlap could not be performed; inform console!
            print('Error in geometry!')
            dfo_sara_in_BC[[i]] <- NULL
          }
        }
        
        dfo_sara_in_BC_bbox_bound = dplyr::bind_rows(dfo_sara_in_BC)
        
        write_sf(dfo_sara_in_BC_bbox_bound, 'data/dfo_sara_in_BC_bbox.gpkg')
        
        rm(dfo_sara_in_BC)
        gc()
        
      } else {
        dfo_sara_in_BC_bbox_bound = read_sf('data/dfo_sara_in_BC_bbox.gpkg')
      }
      
      # Clean up by just keeping things inside BC.
      bc = bcmaps::bc_bound() |> 
        dplyr::summarise() |> 
        st_transform(3857)
      
      dfo_sara_in_BC = dfo_sara_in_BC_bbox_bound |> 
        dplyr::filter(sf::st_intersects(geom, bc, sparse = F))
      rm(dfo_sara_in_BC_bbox_bound)
      gc()
      write_sf(dfo_sara_in_BC, 'data/dfo_sara_in_BC_boundary.gpkg')
    } else {
      dfo_sara_in_BC = read_sf('data/dfo_sara_in_BC_boundary.gpkg')
    }
    
    dfo_sara_in_BC_no_geom = sf::st_drop_geometry(dfo_sara_in_BC)
    dfo_sara_in_BC_no_geom |> dplyr::count(Common_Name_EN, sort = T)
    
    write.csv(dfo_sara_in_BC_no_geom, 'output/count_of_SARA_fish_species_polygons_within_BC_borders.csv')
    
    rm(dfo_sara_in_BC_no_geom)
    
    # Search through the DFO SARA occ data for just whirling disease susceptible species.
    dfo_sara_in_BC_WD_fish = dfo_sara_in_BC |> 
      dplyr::filter(stringr::str_detect(Scientific_Name, wd_sensitive_searcher$scientific_pattern) | stringr::str_detect(Common_Name_EN, wd_sensitive_searcher$common_pattern))
    
    dfo_sara_in_BC_WD_fish_no_geom = sf::st_drop_geometry(dfo_sara_in_BC_WD_fish)
    dfo_sara_in_BC_WD_fish_no_geom |> dplyr::count(Common_Name_EN, sort = T)
    
    rm(dfo_sara_in_BC)
    gc()
    write_sf(dfo_sara_in_BC_WD_fish, 
             'data/dfo_sara_in_BC_WD_sensitive_fish.gpkg')
  } else {
    dfo_sara_in_BC_WD_fish = read_sf('data/dfo_sara_in_BC_WD_sensitive_fish.gpkg')
  }
  
  print(paste0("About to filter DFO dataset to just Fraser / Columbia River Watersheds / Areas at ",Sys.time()))
  
  # Make polygon for Fraser River and Colombia River basin polygon
  fr = read_sf("W:/CMadsen/shared_data_sets/fraser_watershed_priority_area.gpkg") |> 
    dplyr::summarise()
  cl = read_sf("W:/CMadsen/shared_data_sets/columbia_watershed_priority_area.gpkg") |> 
    dplyr::summarise()
  
  pr = fr |> 
    bind_rows(cl) |> 
    dplyr::summarise() |> 
    st_transform(crs = st_crs(dfo_sara_in_BC_WD_fish))
  
  print(paste0("Created Fraser / Columbia priority region polygon",Sys.time()))
  
  dfo_sara_in_fras_col_priority_area = dfo_sara_in_BC_WD_fish |> 
    dplyr::filter(sf::st_intersects(geom, pr, sparse = F))
  
  print("Performed spatial overlay!")
  
  rm(dfo_sara_in_BC_WD_fish)
  sf::write_sf(dfo_sara_in_fras_col_priority_area, 'data/dfo_sara_in_col_fras_priority_area.gpkg')
  print(paste0("Finished writing dataset at ",Sys.time()))
  
} else {
  dfo_sara_in_fras_col_priority_area = sf::read_sf('data/dfo_sara_in_col_fras_priority_area.gpkg')
}

ggplot() + 
  geom_sf(data = bcmaps::bc_bound()) + 
  geom_sf(data = pr, fill = 'lightblue') +
  geom_sf(data = dfo_sara_in_fras_col_priority_area,
          aes(fill = Common_Name_EN, col = Common_Name_EN))

ggsave('output/dfo_sara_in_fras_col_priority_area.png',
       width = 8, height = 8)
  