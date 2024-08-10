---
  title: "Watershed-scale Analysis of SAR and High-Priority Waterbodies"
author: "Chris Madsen and John Phelan"
date: "`r Sys.Date()`"
output:
  rmdformats::robobook:
  pdf_document: default
editor_options: 
  chunk_output_type: console
---
  
  ```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE,
                      fig.width = 10, fig.height = 8)
library(tidyverse)
library(sf) # shapes
library(viridis) # colors
library(bcdata)
library(bcmaps)
library(raster)
library(geodata)
library(dismo) # getting the climate models and other cool things
library(ggpattern) # hatching ggplot
library(RColorBrewer) # colors
library(ggspatial) # scale bar
library(patchwork) # For combining ggplot figures in cool ways

# Trace path back to Users/FLAST to then construct path to Onedrive data folder.
path_to_user = str_extract(getwd(), ".*Users/[A-Z]+")
onedrive_path = paste0(path_to_user,"/OneDrive - Government of BC/data/CNF/")
```

# Priority of Waterbodies Established via the ZQ Mussel Prioritization Model.

## Figures {.tabset}

```{r reading_in_data}
# Zebra Quagga Prioritization Model
zqpm <-read_sf(paste0(onedrive_path,"Waterbodies_with_binned_and_original_values.gpkg"))

# Watersheds of some scale
if(!file.exists(paste0(onedrive_path,'watershed_groups.gpkg'))){
  ws = bcdc_query_geodata('freshwater-atlas-watershed-groups') %>%   
    # This one has 246 rows
    collect()
  # Simplify the geometry! Woo
  ws_s = rmapshaper::ms_simplify(ws)
  sf::write_sf(ws_s,paste0(onedrive_path,'watershed_groups.gpkg'))
} else {
  ws = sf::read_sf(paste0(onedrive_path,'watershed_groups.gpkg'))
}

# Regions (just in case)
regs = bcmaps::nr_regions()

# DFO SARA spatial data.
sara = read_sf(paste0(onedrive_path,"DFO_SARA_occ_data_QGIS_simplified.gpkg"))

# sciName<-unique(sara$Scientific_Name)
# 
# tax_class<-c("Lampreys", "ray-finned fishes", "bivalves")

# if(!file.exists(paste0(onedrive_path,"SAR_occurences.gpkg"))){
#   spp<-bcdc_query_geodata("species-and-ecosystems-at-risk-publicly-available-occurrences-cdc") %>% 
#     filter(TAX_CLASS %in% tax_class) %>% 
#     collect()
#   sf::write_sf(spp,paste0(onedrive_path,'SAR_occurences.gpkg'))
# }else{
#   spp<-read_sf(paste0(onedrive_path,"SAR_occurences.gpkg"))
# }
```

The list of SARA-listed species included in this assessment are as follows: `r paste0(unique(sara$Common_Name_EN), collapse = ", ")`.

```{r filter_destination_waterbodies_and_find_downstream_networks_for_each}

# Find the highest priority waterbodies based on the ZQM model.
zqpmUseFilt <- zqpm %>% arrange(desc(InvasionRisk)) %>% 
  dplyr::select(WATERSH, WATERBO, GNIS_NA, InvasionRisk) %>% 
  filter(!GNIS_NA %in% c("Dry Storage", "Pacific Ocean")) %>% 
  filter(InvasionRisk >= 3)

# Subset the subwatersheds for only those with high-priority waterbodies inside.
ws_high_priority <- st_filter(ws, zqpmUseFilt)

ws = st_simplify(ws)
```

```{r}
# For each waterbody, find its downstream course.
t = zqpmUseFilt[2,]


```


```{r}
# Note: the script below actually includes a whole bunch of garbage old code, so it can't be run as is. For now, we can just use the geopackage that's created partway through the code. This geopackage is composed of many, many (multi)linestrings.
source("scripts/find_downstream_pieces_of_waterbodies.R")

downstream_pieces = sf::read_sf("data/all_downstream_pieces_with_source_priority_wb_names.gpkg")
# These are all linestrings.
```

```{r}
# Summarise all downstream pieces into one mega geometry object.
ds_sum = dplyr::summarise(downstream_pieces)

# Also summarise them by source waterbody.
ds_sum_by_wb = downstream_pieces |> 
  dplyr::group_by(source_priority_wb) |> 
  dplyr::summarise()

# Find who is downstream of who.
st_touches(ds_sum_by_wb)
# Do an overlap with SARA species here.
sara_by_ds_geometries = sara |> 
  sf::st_transform(3005) |> 
  sf::st_join(ds_sum_by_wb) |> 
  sf::st_drop_geometry()

sara_by_ds_geometries |> 
  dplyr::filter(!is.na(source_priority_wb)) |>
  dplyr::select(Common_Name_EN, source_priority_wb) |> 
  dplyr::distinct()
# Now, for 
downstream_pieces |> 
  sf::st_drop_geometry() |> 
  dplyr::select(source_priority_wb,upstream_waterbodies) |> 
  dplyr::distinct()
# For each waterbody, remove all other waterbodies from this downstream pieces summed
# shape, then find overlapping pieces.
all_wbs = dplyr::summarise(zqpmUseFilt)

ds_sum_w_holes = sf::st_difference(ds_sum, all_wbs)

ggplot() + geom_sf(data = ds_sum_w_holes)

# Now add in the waterbody in question then find stuff in physical connection.
ds_sum_w_holes_plus_wb = dplyr::bind_rows(
  zqpmUseFilt[2,],
  ds_sum_w_holes
) |> 
  dplyr::summarise()

ggplot() + geom_sf(data = ds_sum_w_holes_plus_wb)


# Find the linestring piece that overlaps with the waterbody in question.
sf::st_join(downstream_pieces, wb |> summarise(in_network = TRUE)) |> 
  
  
  zqpm_w_ds = zqpmUseFilt

for(i in 1:nrow(zqpmUseFilt)){
  wb = zqpmUseFilt[i,]
  if(wb$GNIS_NA == 'Columbia River') next
  
  # Find the stream skeleton piece for the lake, if it's a lake.
  if(sf::st_geometry_type(wb) %in% c("MULTIPOLYGON","POLYGON")){
    wb_str = bcdc_query_geodata('freshwater-atlas-stream-network') |> 
      filter(INTERSECTS(wb)) |> 
      collect() |> 
      sf::st_zm() |> 
      dplyr::summarise()
  } else {
    wb_str = wb
  }
  
  ggplot() + geom_sf(data = wb) + geom_sf(data = wb_str, col = 'red')
  
  ggplot() + geom_sf(data = ds_sum_w_holes) + 
    geom_sf(data = all_wbs, col = 'purple') +
    geom_sf(data = wb, col = 'blue') + 
    geom_sf(data = wb_str, col = 'red')
  
  # Find overlapping segments with the wb_str.
  wb_w_ds = sf::st_combine(sf::st_buffer(wb_str, 50), ds_sum_w_holes)
  
  ggplot() + geom_sf(data = wb_w_ds)
  
  wb_w_ds |> sf::st_cast("LINESTRING")
  # wb_w_ds = sf::st_join(wb, ds_sum_w_holes)
  zqpm_w_ds = sf::st_set_geometry(zqpm_w_ds[i,], wb_w_ds$geom)
}

ggplot() + geom_sf(data = downstream_pieces)
ggplot() + geom_sf(data = zqpmUseFilt[zqpmUseFilt$GNIS_NA == 'Kalamalka Lake',])
ggplot() + 
  geom_sf(data = downstream_pieces[downstream_pieces$source_priority_wb == 'Kalamalka Lake',])
# Remove the waterbodies themselves from these downstream pieces.
downstream_pieces = sf::st_difference(downstream_pieces,
                                      dplyr::summarise(zqpmUseFilt))

# Summarise the SARA-listed fish species by downstream courses of priority waterbodies.
sara_by_downstream_overlap = sara |> 
  sf::st_transform(3005) |> 
  st_join(downstream_pieces)

# Summarise the different SARA-listed species present in each of the 
# high priority waterbodies.
sara_by_downstream = sara_by_downstream_overlap |> 
  sf::st_drop_geometry() |> 
  dplyr::filter(!is.na(WATERSHED_KEY)) |> 
  dplyr::select(source_priority_wb, Common_Name_EN) |> 
  # dplyr::select(WATERSHED_KEY, BLUE_LINE_KEY, GNIS_NAME, Common_Name_EN) |> 
  dplyr::distinct() |> 
  # dplyr::arrange(GNIS_NAME,Common_Name_EN) |> 
  dplyr::arrange(source_priority_wb,Common_Name_EN) |>
  # dplyr::group_by(WATERSHED_KEY, BLUE_LINE_KEY, GNIS_NAME) |> 
  dplyr::group_by(source_priority_wb) |> 
  dplyr::summarise(distinct_SARA = n(),
                   list_of_SARA = paste0(Common_Name_EN, collapse = ', ')) |> 
  dplyr::ungroup()

# Alternate methodology: find downstream networks of streams/rivers 
# for each high-priority waterbody; run the overlap with the subwatersheds
# using these networks rather than the waterbody geometry itself.


# Combine the list of downstream geometries into a single table.
ds_geom_bound = dplyr::bind_rows(downstream_geometries)

# This is going to need some hand correcting in QGIS.
# sf::write_sf(ds_geom_bound, "output/downstream_geoms_for_corrections.shp")

library(leaflet)
leaflet() |> 
  addTiles() |> 
  addPolygons(
    data = sf::st_transform(ds_geom_bound, 4326),
    label = ~GNIS_NAME
  )

# We could summarize these networks yet again to get a simple 
# overlap with the subwatersheds. 
ds_geom_sum = dplyr::summarise(ds_geom_bound)

# Find all subwatersheds that overlap with any of these downstream networks.
ws_high_priority = ws %>% 
  sf::st_filter(ds_geom_sum)

# Summarise the SARA-listed fish species by waterbody or a downstream portion.
sara_by_wb_overlap = sara |> 
  sf::st_transform(3005) |> 
  st_join(zqpmUseFilt)

# Summarise the different SARA-listed species present in each of the 
# high priority waterbodies.
sara_by_wb = sara_by_wb_overlap |> 
  sf::st_drop_geometry() |> 
  dplyr::filter(!is.na(WATERSH)) |> 
  dplyr::select(WATERSH, WATERBO, GNIS_NA, Common_Name_EN) |> 
  dplyr::distinct() |> 
  dplyr::arrange(GNIS_NA,Common_Name_EN) |> 
  dplyr::group_by(WATERSH, WATERBO, GNIS_NA) |> 
  dplyr::summarise(distinct_SARA = n(),
                   list_of_SARA = paste0(Common_Name_EN, collapse = ', ')) |> 
  dplyr::ungroup()

# Perform the same spatial overlap, but this time using the downstream networks.
# If we get the same numbers, I guess we don't need to worry?
sara_by_ds_overlap = sara |> 
  sf::st_transform(3005) |> 
  st_join(ds_geom_bound)

sara_by_ds = sara_by_ds_overlap |> 
  sf::st_drop_geometry() |> 
  dplyr::filter(!is.na(wb_name)) |> 
  dplyr::select(wb_name, Common_Name_EN) |> 
  dplyr::distinct() |> 
  dplyr::arrange(wb_name,Common_Name_EN) |> 
  dplyr::group_by(wb_name) |> 
  dplyr::summarise(distinct_SARA = n(),
                   list_of_SARA = paste0(Common_Name_EN, collapse = ', ')) |> 
  dplyr::ungroup()
# Join this summary back on to the waterbodies.

ggplot() + geom_sf(data = sara_by_wb)

sara_by_ds = sara |> 
  sf::st_transform(3005) |> 
  st_join(ds_geom_bound)
# Summarise the SARA-listed fish species by subwatershed.
# sara_by_ws = sara %>%
#   st_transform(3005) %>% 
#   st_join(ws %>% dplyr::select(WATERSHED_GROUP_NAME)) %>% 
#   sf::st_drop_geometry() %>% 
#   # dplyr::group_by(ENG_NAME) %>% 
#   # dplyr::summarise(WATERSHED_GROUP_NAME = paste0(WATERSHED_GROUP_NAME, collapse = ', '))
#   dplyr::select(Common_Name_EN,WATERSHED_GROUP_NAME)

sara_by_ws = sara_by_ws %>% 
  dplyr::distinct() %>% 
  dplyr::count(WATERSHED_GROUP_NAME, name = 'number_distinct_SAR')

ws = ws %>% 
  dplyr::left_join(sara_by_ws)

#plot(st_geometry(use_Spp_Overlap))

#use_Spp_Overlap_NA<-use_Spp_Overlap[!is.na(use_Spp_Overlap$number_distinct_SAR),]
#plot(st_geometry(use_Spp_Overlap_NA))

#streams - get some SAR and crosschecck with Use
ws_Sar<-ws[!is.na(ws$number_distinct_SAR),]
ws_Sar<-ws_Sar[ws_Sar$WATERSHED_GROUP_NAME %in% ws_high_priority$WATERSHED_GROUP_NAME,]

#get overlap for rivers with sar 
# zqpm_Overlap<-zqpmUseFilt[zqpmUseFilt$WATERSH %in% ws_Sar$WATERSHED_GROUP_ID,]
zqpm_Overlap<-zqpmUseFilt[zqpmUseFilt$WATERSH %in% ws_Sar$WATERSHED_GROUP_ID,]

zqpm_Overlap<-st_intersection(st_transform(ds_geom_sum, 3005), ws_Sar)

#used watersheds and sar
ws_high_priority_Overlap<-ws_high_priority[ws_high_priority$WATERSHED_GROUP_ID %in% 
                                             ws_Sar$WATERSHED_GROUP_ID,]

# Use suppressMessages just in case the bcmaps package is feeling chatty.
bc <- suppressMessages(bc_bound())

# Make a subset of the subwatershed layer: just those subwatersheds in the window of the high-priority waterbodies and their downstream networks.
ws_background = sf::st_crop(ws, ws_high_priority)



```

### High-Priority Waterbodies from ZQM Model
```{r high_Priority_waterbodies_by_Subwatershed}

# Combine the waterbodies flagged as high risk by the ZQ Mussel Prioritization Model with the downstream sections we found above; this allows us to plot a single sf object to the plot below and utilize a legend to easily explain what is what.

priority_wb_and_ds = dplyr::bind_rows(
  ds_geom_sum |> 
    dplyr::rename(geom = geometry) |> 
    dplyr::mutate(type = "Downstream\nNetwork"),
  zqpmUseFilt |> 
    dplyr::mutate(type = "High-priority\nWaterbody")
) |> 
  arrange(type)

# make map insert of BC with region highlighted.
map_inset = ggplot() + 
  geom_sf(data = bc) + 
  geom_sf(data = dplyr::summarise(ws_high_priority), fill = 'purple') +
  ggthemes::theme_map() + 
  theme(plot.background = element_rect(color = 'black'))

main_map = ggplot() +
  geom_sf(data = ws_background, fill = "lightgrey")+ # simplify geom?
  geom_sf(data=ws_high_priority, fill = "purple", color = "black", alpha = 0.3)+
  # geom_sf(data=ds_geom_sum, fill = "orange", color="orange")+
  # geom_sf(data=zqpmUseFilt, fill = "red", color="darkred")+
  geom_sf(data = priority_wb_and_ds,aes(fill = type, col = type)) +
  scale_fill_manual(values = c("Downstream\nNetwork" = "orange",
                               "High-priority\nWaterbody" = "red")) +
  scale_colour_manual(values = c("Downstream\nNetwork" = "orange",
                                 "High-priority\nWaterbody" = "red")) +
  labs(title = "High Priority waterbodies by subwatershed",
       subtitle = paste0("This includes downstream networks. \nTotal number of subwatersheds: ",nrow(ws_high_priority)),
       fill = "Waterbody\nType",
       colour = "Waterbody\nType")+
  ggthemes::theme_map()+
  theme(plot.title = element_text(size = rel(3), face = "bold"),
        plot.subtitle = element_text(size = rel(2)),
        legend.title = element_text(size = rel(1.8)),
        legend.text = element_text(size = rel(1.2)),
        legend.position = 'right'
  ) + 
  coord_sf(xlim = sf::st_bbox(ws_high_priority)[c(1,3)],
           ylim = sf::st_bbox(ws_high_priority)[c(2,4)]) + 
  ggspatial::annotation_scale()

# The following code to combine plots uses the {patchwork} package.
main_map + patchwork::inset_element(map_inset, right = 1, top = 1, left = 0.8, bottom = 0.75)
```

### Number of Distinct SAR sp.
```{r number_of_distinct_SAR_species_by_Subwatershed}
sp_colors<-c( "#EFFDB1ff","#BDD793ff","#8BB174ff","#598B56ff","#276537ff")
ggplot() + 
  geom_sf(data = ws, aes(fill = as.factor(number_distinct_SAR))) + 
  scale_fill_manual(values =  sp_colors, na.value = "grey")+
  labs(title = 'Number of Distinct SARA-listed Species by Subwatershed',
       subtitle = "(only whirling-disease-sensitive species included)",
       fill = "No. species at risk")+
  ggthemes::theme_map()+
  theme(plot.title = element_text(size = rel(3), face = "bold"),
        plot.subtitle = element_text(size = rel(2)),
        legend.title = element_text(size = rel(1.8)),
        legend.text = element_text(size = rel(1.2))
  )
```

### All Data
```{r all_data}
#define the color palette
sp_colors<-c( "#EFFDB1ff","#BDD793ff","#8BB174ff","#598B56ff","#276537ff")

# Combine high-priority waterbodies, downstream networks, and subwatersheds with counts of distinct SARA-listed species into one sf object (then we can use the fill for three different datsets!)

# sf_dat_for_plot = dplyr::bind_rows(
#   ws |> 
#     dplyr::filter(!is.na(number_distinct_SAR) | WATERSHED_GROUP_NAME %in% ws_high_priority$WATERSHED_GROUP_NAME) |> 
#     mutate(type = "Number SARA-listed Species") |> 
#     dplyr::select(),
#   ds_geom_sum |> 
#     dplyr::rename(geom = geometry) |> 
#     dplyr::mutate(type = "Downstream\nNetwork"),
#   zqpmUseFilt |> 
#     dplyr::mutate(type = "High-priority\nWaterbody")
# )

ggplot()+
  geom_sf(data = bc, fill = "lightgrey", color = "darkgrey", alpha = 0.7)+ 
  geom_sf(data = ws[!is.na(ws$number_distinct_SAR) | ws$WATERSHED_GROUP_NAME %in% ws_high_priority$WATERSHED_GROUP_NAME,], aes(fill = as.factor(number_distinct_SAR))) +
  scale_fill_manual(values =  sp_colors, na.value = "grey")+
  # geom_sf(data= ds_geom_sum, fill = "red", color="dark red") +
  geom_sf(data = priority_wb_and_ds, col = 'red', fill = 'red') +
  geom_sf(data = ws_high_priority, fill = "transparent", color = "purple", linewidth = 0.8) + 
  labs(title = "Number of SARA-listed Species and High Priority Subwatersheds",
       subtitle = "High priority subwatersheds outlined in purple\nHigh priority waterbodies and downstream Networks in red",
       fill = "No. species at risk") +
  annotation_scale(location = "br", width_hint = 0.5) +
  ggthemes::theme_map()+
  theme(plot.title = element_text(size = rel(1.8), face = "bold"),
        plot.subtitle = element_text(size = rel(1.6)),
        legend.title = element_text(size = rel(1.3)),
        legend.text = element_text(size = rel(1.1))
  )

```

### Only Overlapping Data
```{r}
ggplot()+
  geom_sf(data = bc, fill = "lightgrey", color = "darkgrey", alpha = 0.7)+ 
  geom_sf(data = ws_Sar, aes(fill = as.factor(number_distinct_SAR))) +
  scale_fill_manual(values =  sp_colors, na.value = "grey")+
  geom_sf(data= zqpm_Overlap, fill = "red", color="dark red") +
  geom_sf(data= ws_high_priority_Overlap, fill = "transparent", color = "purple", linewidth = 0.8) + 
  labs(title = "Number of SARA-listed Species and High Priority Subwatersheds",
       subtitle = "High priority subwatersheds outlined in purple \nHigh priority waterbodies and downstream network in red",
       fill = "No. species at risk") +
  annotation_scale(location = "br", width_hint = 0.5) +
  ggthemes::theme_map()+
  theme(plot.title = element_text(size = rel(1.8), face = "bold"),
        plot.subtitle = element_text(size = rel(1.6)),
        legend.title = element_text(size = rel(1.3)),
        legend.text = element_text(size = rel(1.1))
  )

```

### Cropped and road Network
```{r Road_Network_Plot}

roadNetwork<-bcdc_query_geodata("digital-road-atlas-dra-master-partially-attributed-roads") %>% 
  filter(BBOX(local(st_bbox(ws_high_priority_Overlap, crs = st_crs(ws_high_priority_Overlap))))) %>% 
  filter(!is.na(HIGHWAY_ROUTE_NUMBER)) %>% 
  collect()

ggplot()+
  geom_sf(data = st_crop(bc, ws_high_priority_Overlap), fill = "lightgrey", color = "darkgrey", alpha = 0.7)+
  geom_sf(data = ws_Sar, aes(fill = as.factor(number_distinct_SAR))) +
  scale_fill_manual(values =  sp_colors, na.value = "grey")+
  geom_sf(data= zqpm_Overlap, fill = "red", color="dark red") +
  geom_sf(data= ws_high_priority_Overlap, fill = "transparent", color = "purple", linewidth = 0.8) +
  geom_sf(data = roadNetwork, color = "Black", linewidth = 1, alpha = 0.4)+
  labs(title = "Number of Distict SAR Species and High use sub-watersheds",
       subtitle = "High use sub-watershed outlined in purple \nHigh use waterbodies and downstream network in red \nRoad networks in black",
       fill = "No. species at risk") +
  annotation_scale(location = "br", width_hint = 0.5) +
  coord_sf(xlim = sf::st_bbox(ws_high_priority)[c(1,3)],
           ylim = sf::st_bbox(ws_high_priority)[c(2,4)]) + 
  ggthemes::theme_map()+
  theme(plot.title = element_text(size = rel(2), face = "bold"),
        plot.subtitle = element_text(size = rel(1.8)),
        legend.title = element_text(size = rel(1.3)),
        legend.text = element_text(size = rel(1.1))
  )
```



