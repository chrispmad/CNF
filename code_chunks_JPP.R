#tmax<-getData('CMIP6', var='tmax', res=10, rcp=85, model='AC', year=70)
bioclim<-worldclim_global(var="bio", res=10, path = "./data") # gets all variables
plot(bioclim)# have a look
names(bioclim)# whats the name?

#https://www.worldclim.org/data/bioclim.html
renames<-c("Annual Mean Temperature", "Mean Diurnal Range (temp)", "Isothermality", "Temperature Seasonality", "Max Temperature of Warmest Month", "Min Temperature of Coldest Month", "Temperature Annual Range", "Mean Temperature of Wettest Quarter","Mean Temperature of Driest Quarter", "Mean Temperature of Warmest Quarter", "Mean Temperature of Coldest Quarter", "Annual Precipitation", "Precipitation of Wettest Month", "Precipitation of Driest Month", "Precipitation Seasonality", "Precipitation of Wettest Quarter", "Precipitation of Driest Quarter", "Precipitation of Warmest Quarter", "Precipitation of Coldest Quarter")
renames<-gsub(" ", "_", renames)

#https://www.carbonbrief.org/cmip6-the-next-generation-of-climate-models-explained/
#https://search.r-project.org/CRAN/refmans/geodata/html/cmip6.html
# explalation of ssp codes https://www.carbonbrief.org/cmip6-the-next-generation-of-climate-models-explained/
cmidata<-cmip6_world("ACCESS-CM2", ssp = "585", var = "bioc", res = 5, time = "2021-2040",path="./data/CMI/")
names(cmidata)<-renames


##################

bc<-bc_bound()
crs(bc)
bbox <- st_bbox(bc) %>% st_as_sfc() %>% st_transform(crs = 4326)
bbox

bbox_coords <- st_bbox(bbox)
extent_bbox <- extent(bbox_coords[c("xmin", "xmax", "ymin", "ymax")])
cmidata_cropped <- crop(cmidata, extent_bbox)
plot(cmidata_cropped$Annual_Mean_Temperature)

plot(st_geometry(ws))

ggplot()+
  geom_sf(data=ws)

coordinates <- xyFromCell(cmidata_cropped, cell = 1:ncell(cmidata_cropped))
temperature_values <- values(cmidata_cropped)
df <- data.frame(x = coordinates[,1], y = coordinates[,2], z = temperature_values)
ggplot(df, aes(x = x, y = y, fill = z.Annual_Mean_Temperature)) +
  geom_raster()

xmin <- -125;xmax <- -115;ymin <- 48;ymax <- 52


p1<-ggplot() +
  geom_sf(data = st_transform(ws, crs=4326)) +
  geom_raster(data = df, aes(x = x, y = y, fill = z.Annual_Mean_Temperature), alpha = 0.5)
#p1
p2<-p1+coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax))
#("./images/climate.png", width = 10, height = 8, dpi = 300)
p2