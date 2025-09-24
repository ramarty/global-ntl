# World File Union

world_sf <- read_sf(file.path(wb_bound_dir, "World Bank Official Boundaries - Admin 0.gpkg"))
world_sf <- world_sf %>% st_make_valid() %>% st_union() %>% st_make_valid() %>% st_as_sf()

write_sf(world_sf,
         file.path(wb_bound_dir, "adm0_union", "World Bank Official Boundaries - Admin 0 - Union.gpkg"))

