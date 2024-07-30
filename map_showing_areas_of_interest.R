library(bcinvadeR)
library(tidyverse)
library(sf)

# Get occurrence data for WD-suspectible species

if(!file.exists('data/sens_fish_sp_occurrence_data.gpkg')){
  
  WD_sensitive_species_list = c(
    'cutthroat trout',
    'coho salmon',
    'rainbow trout',
    'sockeye salmon',
    'chinook salmon',
    'mountain whitefish',
    'Atlantic salmon',
    'brown trout',
    'bull trout',
    'brook trout')
    
  occ_data = lapply(WD_sensitive_species_list,
                    \(x) tryCatch(
                      grab_aq_occ_data(x),
                      error = function(e) return(NULL)
                      )
  )
  
  
  occ_dat_b = occ_data |> 
    dplyr::bind_rows()
  
  sf::write_sf(occ_dat_b, 'data/sens_fish_sp_occurrence_data.gpkg')
} else {
  occ_dat_b = sf::read_sf('data/sens_fish_sp_occurrence_data.gpkg')
}

SARA_species_list = c(
  'cutthroat trout',
  'White Sturgeon',
  'bull trout',
  'sculpin',
  'pygmy sculpin',
  'green sturgeon',
  'mountain sucker',
  'Nooksack Dace',
  'Rainbow trout',
  'Rocky Mountain Ridged Mussel',
  'Rocky Mountain Sculpin',
  'Salish Sucker',
  'Shorthead Sculpin',
  'Speckled Dace')

# Only keep species that are also Aquatic SARA listed species.
occ_dat_b = occ_dat_b |> 
  dplyr::filter(Species %in% c('Cutthroat Trout','Rainbow Trout',
                               'Bull Trout'))

# See number of rows by data source.
occ_dat_b |> 
  sf::st_drop_geometry() |> 
  count(DataSource)

# See number of rows by species
occ_dat_b |> 
  sf::st_drop_geometry() |> 
  count(Species, sort = T)

# Bring in species at risk layer.
sp = read_sf('W:/CMadsen/Projects/SpeciesAtRisk/species_ecosystems_at_risk_publically_available.shp') |> 
  st_transform(4326)

sp = st_make_valid(sp)

# Make polygon for Fraser River and Colombia River basin polygon
fr = read_sf("W:/CMadsen/shared_data_sets/fraser_watershed_priority_area.gpkg") |> 
  dplyr::summarise()
cl = read_sf("W:/CMadsen/shared_data_sets/columbia_watershed_priority_area.gpkg") |> 
  dplyr::summarise()

pr = fr |> 
  bind_rows(cl) |> 
  dplyr::summarise()

# Federal critical habitat
fch = sf::read_sf('W:/CMadsen/shared_data_sets/DFO_SARA_CriticalHabitat_in_ColFras_Regions.shp')

# Just keep occ_dat and aquatic SAR inside our priority region
occ_dat_f = occ_dat_b |> 
  dplyr::filter(sf::st_intersects(geom, pr, sparse = F))

sp_f = sp |> 
  dplyr::filter(sf::st_intersects(geometry, pr, sparse = F))

# Get surrounding polygons (e.g. Alberta, Washington, rest of BC, etc.)
stateprov_bg = rnaturalearth::ne_states(country = c("Canada","United States of America")) |>
  st_as_sf() |>
  dplyr::select(state_prov = name,
                country = admin) |> 
  dplyr::filter(state_prov %in% c("Washington","British Columbia","Alberta","Idaho","Montana")) |> 
  dplyr::mutate(label = state_prov) |> 
  dplyr::mutate(label = ifelse(label == 'British Columbia',NA,label))

species_dat = dplyr::bind_rows(
  sp_f |> dplyr::select(Species = ENG_NAME_F, 
                        SCI_NAME) |> 
    dplyr::mutate(DataSource = 'Aquatic SAR CDC'),
  occ_dat_f
)

base_plot = ggplot() + 
  geom_sf(data = stateprov_bg, fill = 'lightgrey',col = 'grey') + 
  geom_sf_text(data = stateprov_bg, aes(label = label), nudge_y = 1) +
  geom_sf(data = pr, fill = 'lightblue')

base_plot + 
  geom_sf(data = sp_f, fill = 'gold') +
  geom_sf(data = occ_dat_f, aes(col = DataSource)) +
  labs(title = 'Sensitive Fish Species Occurrence Data Abounds') + 
  coord_sf(xlim = c(-128,-114), ylim = c(48,56.5))

ggsave(filename = 'output/Fig1_Sens_Fish_Occ_Data_Abounds.png',
       width = 8, height = 8)

# Let's try converting our sensitive fish species into a raster / heatmap / grid?

library(terra)
templ_r = terra::rast(pr, res = 0.1)
occ_dat_r = rasterize(occ_dat_f, templ_r, fun = 'count')
plot(occ_dat_r)
occ_dat_polys = as.polygons(occ_dat_r)
occ_dat_sf = st_as_sf(occ_dat_polys)

base_plot + 
  geom_sf(data = occ_dat_sf, aes(fill = count)) +
  coord_sf(xlim = c(-128,-114), ylim = c(48,56.5)) +
  labs(title = 'Sensitive Fish Species Localized Hotspots',
       subtitle = 'Grid Cell Resolution ~24,000 km^2') +
  scale_fill_viridis_c()

ggsave(filename = 'output/Fig2_Sens_Fish_Localized_Hotspots.png',
       width = 8, height = 8)

# Could we bin the occurrences?

ggplot(occ_dat_sf) + geom_histogram(aes(count))

occ_dat_sf = occ_dat_sf |> 
  dplyr::mutate(count_b = case_when(
    count <= 250 ~ '1 - 250',
    count > 250 & count <= 750 ~ '251 - 750',
    T ~ '751 - 1989'
  ))

base_plot + 
  geom_sf(data = occ_dat_sf, aes(fill = count_b)) +
  coord_sf(xlim = c(-128,-114), ylim = c(48,56.5)) +
  labs(title = 'Sensitive Fish Species Binned Counts',
       subtitle = 'Grid Cell Resolution ~24,000 km^2') +
  scale_fill_viridis_d()

ggsave(filename = 'output/Fig3_Sens_Fish_Binned_Counts.png',
       width = 8, height = 8)

# Add the Aquatic SAR back onto the map

base_plot + 
  geom_sf(data = occ_dat_sf, aes(fill = count_b)) +
  geom_sf(data = sp_f, fill = 'orange', col = 'orange') + 
  geom_sf(data = fch, fill = 'red', col = 'red') +
  coord_sf(xlim = c(-128,-114), ylim = c(48,56.5)) +
  labs(title = 'Sensitive Fish Species Plus Aquatic SAR',
       subtitle = 'Grid Cell Resolution ~24,000 km^2',
       fill = 'Abundance of \nSensitive \nFish Species',
       x = 'Longitude', y = 'Latitude',
       caption = 'Aquatic SAR in orange, SARA Critical Habitat in red') +
  scale_fill_viridis_d()

ggsave(filename = 'output/Fig4_Sens_Fish_plus_Aquatic_SAR.png',
       width = 8, height = 8)

# Get BC cities for map

# cities = bcmaps::bc_cities() |> 
#   dplyr::filter(NAME %in% c("Vancouver","Kelowna","Prince George","Quesnel","Golden"))
# 
# base_plot + 
#   geom_sf(data = occ_dat_sf, aes(fill = count_b)) +
#   geom_sf(data = sp_f, fill = 'orange', col = 'orange') + 
#   geom_sf_text(data = cities, aes(label = NAME), col = 'white') +
#   coord_sf(xlim = c(-128,-114), ylim = c(48,56.5)) +
#   labs(title = 'Sensitive Fish Species Plus SAR',
#        subtitle = 'Grid Cell Resolution ~24,000 km^2',
#        fill = 'Abundance of \nSensitive \nFish Species',
#        x = 'Longitude', y = 'Latitude') +
#   scale_fill_viridis_d()
