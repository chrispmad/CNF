if(!file.exists(paste0(onedrive_path,"all_downstream_pieces_with_source_priority_wb_names.gpkg"))){
  ######### remove the -1 to include Columbia River and delete the file downstream_geoms
  #############################################################################################
  #############################################################################################
  full_geometries = list()
  wb_names = unique(zqpmUseFilt$GNIS_NA)
  
  for(i in 1:length(unique(zqpmUseFilt$GNIS_NA))){
    
    print(i)
    
    the_name = wb_names[i]
    the_wb = zqpmUseFilt %>% filter(GNIS_NA == the_name)
    
    # Download the geometry for this waterbody (for rivers, can be just a piece
    # of the total river).
    if(!str_detect(the_name,'Lake')){
      result = bcdc_query_geodata('freshwater-atlas-stream-network') %>% 
        bcdata::filter(GNIS_NAME == the_name,
                       WATERSHED_GROUP_ID == the_wb$WATERSH,
                       WATERBODY_KEY == the_wb$WATERBO) %>% 
        collect() |> 
        sf::st_zm()
      
      unique_BLK = unique(result$BLUE_LINE_KEY)
      
      full_result = bcdc_query_geodata('freshwater-atlas-stream-network') %>% 
        bcdata::filter(BLUE_LINE_KEY == unique_BLK) %>% 
        collect() |> 
        sf::st_zm() |> 
        dplyr::mutate(GNIS_NAME = unique(result$GNIS_NAME),
                      FWA_WATERSHED_CODE = unique(result$FWA_WATERSHED_CODE)) |> 
        dplyr::group_by(wb_name = GNIS_NAME,
                        FWA_WATERSHED_CODE) |>
        dplyr::summarise() |> 
        dplyr::ungroup()
    } else {
      full_result = bcdc_query_geodata('freshwater-atlas-lakes') %>% 
        bcdata::filter(GNIS_NAME_1 == the_wb$GNIS_NA,
                       WATERSHED_GROUP_ID == the_wb$WATERSH,
                       WATERBODY_KEY == the_wb$WATERBO) %>% 
        collect() |> 
        sf::st_zm() |> 
        dplyr::mutate(GNIS_NAME_1 = the_name) |> 
        dplyr::group_by(wb_name = GNIS_NAME_1,
                        FWA_WATERSHED_CODE) |> 
        dplyr::summarise() |> 
        dplyr::ungroup()
    }
    
    # ggplot() + geom_sf(data = full_result) + geom_sf(data = the_wb, fill = 'red', col = 'red')
    
    # Add the list
    full_geometries[[i]] <- full_result
  }
  
  # Combine the list of waterbodies (lakes and rivers) into a single table.
  wb_tbl = dplyr::bind_rows(full_geometries)
  
  wb_tbl$upstream_waterbodies = NA
  
  # Let's find who is upstream of each row.
  for(i in 1:nrow(wb_tbl)){
    the_row = wb_tbl[i,]
    
    code_for_search = str_remove(the_row$FWA_WATERSHED_CODE, '-000000.*')
    
    all_upstream_wbs = wb_tbl |> 
      dplyr::filter(str_detect(FWA_WATERSHED_CODE, paste0(code_for_search,"-[^000000]"))) |> 
      pull(wb_name)
    
    wb_tbl$upstream_waterbodies[i] = paste0(all_upstream_wbs, collapse = ", ")
  }
  
  ggplot() + geom_sf(data = wb_tbl)
  # Save the table of full geometries of high-priority wbs with the 
  # new field describing which other high-priority wbs are upstream of each row.
  sf::write_sf(wb_tbl, paste0(onedrive_path,"high_priority_wbs_with_upstream_members.gpkg"))
  
  
  # For each row, find all the waterbody geometries that are downstream.
  wb_tbl_w_ds = wb_tbl
  
  for(i in 1:nrow(wb_tbl)){
    print(i)
    the_row = wb_tbl[i,]
    the_code = the_row$FWA_WATERSHED_CODE
    the_code_s = str_remove(the_code, "-000000.*")
    # Quick check - is this row a massive river that has no downstream geometries?
    # If so, don't continue this iteration of the loop!
    if(str_detect(the_row$FWA_WATERSHED_CODE,"-[^000000]")){
      
      # Find each downstream piece to download.
      number_geoms_to_download = str_count(the_code_s, "-")
      ds_geoms = list()
      
      for(y in 1:number_geoms_to_download){
        the_subcode = str_replace(the_code_s,paste0("(-[0-9]{6}){",y,"}$"),paste0(rep("-000000",y), collapse = ''))
        # Append 0's to fill up the subcode's FWA code to the proper length.
        digits_to_add = 143 - str_length(the_subcode) # Number of digits needed to add
        zeros_to_add = digits_to_add/7
        the_subcode = paste0(the_subcode,paste0(rep("-000000", zeros_to_add),collapse = ''))
        
        the_query = paste0("FWA_WATERSHED_CODE like '",the_subcode,"'",
                           collapse = '')
        the_query = bcdata:::CQL(the_query)
        
        # Now query the data warehouse for a stream with that name.
        ds_geom = tryCatch(
          expr = bcdc_query_geodata('freshwater-atlas-stream-network') |> 
            filter(the_query) |> 
            collect() |> 
            sf::st_zm() |> 
            dplyr::group_by(FWA_WATERSHED_CODE) |> 
            dplyr::summarise(),
          error = function(e) return(NULL)
        )
        
        if(!is.null(ds_geom)){
          # Inform this downstream piece which waterbody it is downstream of.
          ds_geoms[[y]] = ds_geom
        }
      }
      # We've now downloaded all the downstream pieces; they're stored in ds_geoms.
      ds_geoms = dplyr::bind_rows(ds_geoms)
      
      wb_and_ds_geoms = dplyr::bind_rows(
        wb_tbl_w_ds[i,],
        ds_geoms
      ) |> 
        dplyr::summarise()
      
      wb_tbl_w_ds[i,]$geometry = wb_and_ds_geoms$geometry
    }
  }
  
  ggplot() + 
    geom_sf(data = wb_tbl_w_ds[2,]) +
    geom_sf(data = wb_tbl_w_ds[3,], col = 'green') +
    geom_sf(data = wb_tbl_w_ds[4,], col = 'purple') +
    geom_sf(data = wb_tbl_w_ds[5,], col = 'orange')
  
  
  # Trying this once again... with fwa.connect...
  downstream_courses = purrr::map(
    wb_tbl$FWA_WATERSHED_CODE[-1], ~ {
      fwa.connect::trace_course_downstream(.x)
    })
  
  names(downstream_courses) <- wb_tbl$wb_name[-1]
  
  downstream_courses_b = dplyr::bind_rows(downstream_courses, 
                                          .id = 'source_priority_wb') |> 
    dplyr::bind_rows(
      wb_tbl |> 
        dplyr::filter(wb_name == 'Columbia River') |> 
        mutate(source_priority_wb = "Columbia River")
    )
  
  sf::write_sf(downstream_courses_b, paste0(onedrive_path,"all_downstream_pieces_with_source_priority_wb_names.gpkg"))
  
}else{
  downstream_geometries_b <- read_sf(paste0(onedrive_path,"all_downstream_pieces_with_source_priority_wb_names.gpkg"))
}