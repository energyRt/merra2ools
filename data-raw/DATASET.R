## code to prepare `DATASET` dataset goes here
# usethis::use_data(DATASET, overwrite = TRUE)

rebuild_locid_grids <- T

# rebuild_locid_grids ####
if (rebuild_locid_grids) {
  # points
  locid_p_sf <- st_as_sf(as.data.table(select(merra2ools::locid, locid, lon, lat)), 
                         coords = c("lon", "lat"), remove = F) %>%
    st_set_crs(st_crs(4326))
  class(locid_p_sf)
  
  # polygons
  locid_o_sf <- st_make_grid(locid_p_sf,
                             cellsize = c(0.625, .5),
                             offset = c(-180 - 0.625/2, -90 - .5/2)) %>%
    st_as_sf()
  class(locid_o_sf)
  
  try({
    locid_o_sf <- locid_o_sf %>% rename(geometry = x) %>% as.data.table() %>% st_as_sf()
    locid_o_sf <- locid_o_sf %>% as.data.table()
  })
  class(locid_o_sf)
  
  # library(data.table)
  # locid_o_sf[, .(geometry = x)]
  # colnames(locid_o_sf)[colnames(locid_o_sf) == "x"] <- "geometry" 
  
  # replace invalid polygons (on poles) with points
  ii <- locid$lat == -90 | locid$lat == 90
  summary(ii)
  locid_o_sf <- cbind(locid_o_sf, st_drop_geometry(locid_p_sf)) %>% 
    as.data.table() %>% st_as_sf()
  
  locid_op_sf <- locid_o_sf[!ii,] %>% rbind(locid_p_sf[ii,]) %>% arrange(locid)
  locid_op_sf
  class(locid_op_sf)
  locid_op_sf %>% st_bbox()
  
  cc <- sapply(locid_op_sf$geometry, class)
  class(cc)
  dim(cc)
  cc[, 1:5]
  summary(as.factor(cc[2,]))  
  locid_points_sf <- locid_p_sf # %>% select(-any_of(c("lon", "lat")))
  locid_poly_sf <- locid_op_sf # %>% select(-any_of(c("lon", "lat")))
  class(locid_points_sf); class(locid_poly_sf)
  save(locid_points_sf, locid_poly_sf, file = "data-raw/locid_grids.RData")    
} else {
  load("data-raw/locid_grids.RData")
}

usethis::use_data(locid_points_sf, locid_poly_sf, overwrite = T, internal = T)
# usethis::use_data(, overwrite = TRUE, internal = T)

####