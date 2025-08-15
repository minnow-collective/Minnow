library(dplyr)
library(tidyverse)
library(data.table)
library(sf)
library(rnaturalearth)
library(terra)
library(mapview)
library(gert)

options(scipen = 999)

source("ref_scripts/watersheds_functions.R")
source("ref_scripts/general_functions.R")


# Call the function to process the shapefiles and combine them
hybas_glob_comb_l1 <- read_level_shapefiles("/Volumes/OWC Elite Pro Dual/Data + Other/Minnow/Watersheds/HydroBASINS", "hybas_lake_.*_lev01-12_v1c$", "lev01_v1c.shp$")


wmo_basins <- st_read("/Volumes/OWC Elite Pro Dual/Data + Other/Minnow/Watersheds/wmobb_json/wmobb_basins.json") %>%
  mutate(across(all_of(c("WMOBB_NAME", "WMOBB_BASIN", "WMOBB_SUBBASIN")), ~ title_case(na_if(str_remove(., " \\(.*$"), "---"))),
         WMOBB_DESCRIPTION = paste0(toupper(substr(WMOBB_DESCRIPTION, 1, 1)), substr(WMOBB_DESCRIPTION, 2, nchar(WMOBB_DESCRIPTION)))) %>%
  st_make_valid() %>%
  st_wrap_dateline(options = c("WRAPDATELINE=YES", "DATELINEOFFSET=180"))



# Step 1: Intersect WMO with hybas_glob_comb_l1 to assign Region (subdivide as needed)
wmo_ovl1 <- st_intersection(wmo_basins, select(hybas_glob_comb_l1, Region)) %>% 
  st_make_valid()

# Step 2: Find WMO features that do NOT intersect any region (assign "Other")
wmo_int_matrix <- st_intersects(wmo_basins, hybas_glob_comb_l1, sparse = FALSE)
wmo_unm1 <- wmo_basins[rowSums(wmo_int_matrix) == 0, ] %>%
  mutate(Region = "Other")

# Step 3: Combine WMO intersecting and non-intersecting
wmo_comb_l1 <- bind_rows(wmo_ovl1, wmo_unm1) %>%
  filter(!st_is_empty(geometry), st_geometry_type(geometry) %in% c("POLYGON", "MULTIPOLYGON")) %>%
  mutate(geometry = st_cast(geometry, "MULTIPOLYGON")) %>%
  group_by(WMOBB_BASIN, REGNAME, Region) %>%
  summarise(geometry = st_union(geometry), .groups = "drop") %>%
  st_make_valid()


# --- Now do the same for hybas_glob_comb_l1 ---

# Step 4: Intersect hybas_glob_comb_l1 with wmo_ovl1 to assign subdivisions
hybas_ovl_wmo1 <- st_intersection(select(wmo_ovl1, WMOBB_BASIN, REGNAME), hybas_glob_comb_l1)

# Step 5: Find basin features that do NOT intersect any WMO feature
hybas_unm_wmo1 <- hybas_glob_comb_l1 %>%
  st_make_valid() %>%
  filter(!lengths(st_intersects(., wmo_basins)) > 0)  # fix: ensure logical vector

# Step 6: Combine basin intersecting and non-intersecting parts
hybas_comb_wmo1 <- bind_rows(hybas_ovl_wmo1, hybas_unm_wmo1) %>%
  filter(!st_is_empty(geometry), st_geometry_type(geometry) %in% c("POLYGON", "MULTIPOLYGON")) %>%
  mutate(geometry = st_cast(geometry, "MULTIPOLYGON")) %>%
  group_by(Region, WMOBB_BASIN, REGNAME) %>%
  summarise(geometry = st_union(geometry), .groups = "drop") %>%
  st_make_valid()

# Step 7: Combine the two layers and drop duplicates based on identifier combo
basins_comb_l1 <- bind_rows(wmo_comb_l1, hybas_comb_wmo1) %>%
  distinct(Region, WMOBB_BASIN, REGNAME, .keep_all = TRUE) %>%
  filter(st_area(.) >= units::set_units(1000000, "m^2"))





# WMO Level 2 version — keep finest granularity
wmo_comb_l2 <- bind_rows(wmo_ovl1, wmo_unm1) %>%
  filter(!st_is_empty(geometry), st_geometry_type(geometry) %in% c("POLYGON", "MULTIPOLYGON")) %>%
  mutate(geometry = st_cast(geometry, "MULTIPOLYGON")) %>%
  select(WMOBB:REGNAME, WMO306_MoC_REFERENCE, Region) %>%
  st_make_valid()

hybas_unm_wmo2 <- hybas_unm_wmo1 %>%
  mutate(
    WMOBB = NA_integer_,
    WMOBB_NAME = NA_character_,
    WMOBB_BASIN = NA_character_,
    WMOBB_SUBBASIN = NA_character_,
    WMOBB_DESCRIPTION = NA_character_,
    REGNAME = NA_character_,
    WMO306_MoC_REFERENCE = NA_character_
  ) %>%
  select(WMOBB:REGNAME, WMO306_MoC_REFERENCE, Region, geometry)

basins_comb_l2 <- bind_rows(wmo_comb_l2, hybas_unm_wmo2) %>%
  filter(st_area(.) >= units::set_units(1000000, "m^2"))


hybas_glob_comb_l5 <- read_level_shapefiles("/Volumes/OWC Elite Pro Dual/Data + Other/Minnow/Watersheds/HydroBASINS", "hybas_lake_.*_lev01-12_v1c$", "lev05_v1c.shp$")


# Get shared Region values
region_groups <- intersect(
  unique(hybas_glob_comb_l5$Region),
  unique(basins_comb_l2$Region)
)

# Apply st_intersection region by region
hybas_l5_wmo_comp <- purrr::map_dfr(region_groups, function(rg) {
  
  hybas_sub <- hybas_glob_comb_l5 %>%
    filter(Region == rg) %>%
    select(HYBAS_ID)
  
  basins_sub <- basins_comb_l2 %>%
    filter(Region == rg) %>%
    select(WMOBB:Region)
  
  st_intersection(hybas_sub, basins_sub)
}) %>%
  filter(st_area(.) >= units::set_units(1000000, "m^2"))

hybas_glob_comb_l6 <- read_level_shapefiles("/Volumes/OWC Elite Pro Dual/Data + Other/Minnow/Watersheds/HydroBASINS", "hybas_lake_.*_lev01-12_v1c$", "lev06_v1c.shp$")

basins_comb_l1_file <- "Watersheds/global_basins_wmo_hybas1_composite_layer_1.gpkg"
if (file.exists(basins_comb_l1_file)) file.remove(basins_comb_l1_file)
st_write(basins_comb_l1, basins_comb_l1_file)

basins_comb_l2_file <- "Watersheds/global_basins_wmo_hybas1_composite_layer_2.gpkg"
if (file.exists(basins_comb_l2_file)) file.remove(basins_comb_l2_file)
st_write(basins_comb_l2, basins_comb_l2_file)

hybas_l5_wmo_comp_file <- "Watersheds/global_basins_wmo_hybas5_composite_layer_3.gpkg"
if (file.exists(hybas_l5_wmo_comp_file)) file.remove(hybas_l5_wmo_comp_file)
st_write(hybas_l5_wmo_comp, hybas_l5_wmo_comp_file)

hybas_glob_comb_l6_file <- "Watersheds/global_basins_hybas6_layer_4.gpkg"
if (file.exists(hybas_glob_comb_l6_file)) file.remove(hybas_glob_comb_l6_file)
st_write(hybas_glob_comb_l6, hybas_glob_comb_l6_file)




rivers_glob <- st_read("Watersheds/HydroRIVERS/HydroRIVERS_v10_shp/HydroRIVERS_v10_shp/HydroRIVERS_v10.shp")

rivers_low <- filter(rivers_glob, ORD_FLOW <= 3 | ORD_STRA >= 6) %>%
  group_by(ORD_FLOW) %>%
  summarise(geometry = st_union(geometry), .groups = "drop") %>%
  st_cast("MULTILINESTRING") %>%
  st_make_valid() %>%
  st_intersection(basins_comb_l1)

rivers_mod <- filter(rivers_glob, ORD_FLOW <= 4 | ORD_STRA >= 5) %>%
  group_by(ORD_FLOW) %>%
  summarise(geometry = st_union(geometry), .groups = "drop") %>%
  st_cast("MULTILINESTRING") %>%
  st_intersection(basins_comb_l2)

rivers_all <- rivers_glob %>%
  group_by(ORD_FLOW) %>%
  summarise(geometry = st_union(geometry), .groups = "drop") %>%
  st_cast("MULTILINESTRING") %>%
  st_intersection(hybas_l5_wmo_comp) %>%
  st_intersection(select(hybas_glob_comb_l6, HYBAS_ID, geometry))













basins <- st_read("Watersheds/RiverATLAS_Data_v10.gdb/RiverATLAS_v10.gdb")
lakes <- st_read("Watersheds/LakeATLAS_Data_v10.gdb/LakeATLAS_v10.gdb")
rivers <- st_read("Watersheds/RiverATLAS_Data_v10.gdb/RiverATLAS_v10.gdb")

lakes_sub <- lakes[1:1000,]
rivers_sub <- rivers[1:1000,]
rivers_7 <- filter(rivers, MAIN_RIV == 10000007)
mapview(rivers_7)

rivers_ne <- ne_download(scale = 10, type = "rivers_lake_centerlines", category = "physical", returnclass = "sf")


rivers_min <- select(rivers, HYRIV_ID, Shape)

#Create reference bounding box around European French boundaries
bbox_eu_france <- st_bbox(c(xmin = -5.5, xmax = 10, ymin = 41, ymax = 51.5), crs = 4326) %>%
  st_as_sfc()



rivers_sub

countries <- ne_countries(scale = "large", returnclass = "sf") %>%
  st_transform(st_crs(rivers))

countries_min <- ne_countries(scale = "large", returnclass = "sf") %>%
  st_transform(st_crs(rivers)) %>%
  select(admin, geometry)

france_bound <- ne_countries(scale = "large", country = "France", returnclass = "sf") %>%
  st_intersection(bbox_eu_france) %>%
  st_transform(st_crs(rivers))

ireland_bound <- ne_countries(scale = "large", country = "Ireland", returnclass = "sf") %>%
  st_transform(st_crs(rivers))

rivers_france <- st_filter(rivers, france_bound, .predicate = st_intersects)
rivers_ireland <- st_filter(rivers, ireland_bound, .predicate = st_intersects)






basins_na_l1 <- st_read("Watersheds/HydroBASINS/hybas_lake_na_lev01-12_v1c/hybas_lake_na_lev01_v1c.shp")
basins_na_l2 <- st_read("Watersheds/HydroBASINS/hybas_lake_na_lev01-12_v1c/hybas_lake_na_lev02_v1c.shp")
basins_na_l3 <- st_read("Watersheds/HydroBASINS/hybas_lake_na_lev01-12_v1c/hybas_lake_na_lev03_v1c.shp")
basins_na_l4 <- st_read("Watersheds/HydroBASINS/hybas_lake_na_lev01-12_v1c/hybas_lake_na_lev04_v1c.shp")
basins_na_l5 <- st_read("Watersheds/HydroBASINS/hybas_lake_na_lev01-12_v1c/hybas_lake_na_lev05_v1c.shp")

rivers_na <- st_read("Watersheds/HydroRIVERS/HydroRIVERS_v10_na_shp/HydroRIVERS_v10_na_shp/HydroRIVERS_v10_na.shp")
rivers_na_m5 <- filter(rivers_na, ORD_FLOW <= 5)
rivers_na_sub <- filter(rivers_na, row_number() < 100000)
rivers_na_sub_m6 <- filter(rivers_na_sub, ORD_FLOW <= 6)
rivers_na_f6 <- filter(rivers_na, ORD_FLOW <= 6)
rivers_na_f5 <- filter(rivers_na, ORD_FLOW <= 5)
rivers_na_f4 <- filter(rivers_na, ORD_FLOW <= 4)
rivers_na_s2 <- filter(rivers_na, ORD_STRA > 2)
rivers_na_s4 <- filter(rivers_na, ORD_STRA > 4)
rivers_na_c3 <- filter(rivers_na, ORD_CLAS < 3)

rivers_na_p <- filter(rivers_na, ORD_FLOW <= 6)

mapview(rivers_na_f6, zcol = "ORD_FLOW")
mapview(rivers_na_f5, zcol = "ORD_FLOW")
mapview(rivers_na_f4, zcol = "ORD_FLOW")

mapview(rivers_na_s4, zcol = "ORD_STRA")

mapview(rivers_na_c3, zcol = "ORD_CLAS")










mapview(rivers_sa_f6, zcol = "ORD_FLOW")
mapview(rivers_sa_f5, zcol = "ORD_FLOW")
mapview(rivers_sa_f4, zcol = "ORD_FLOW")

rivers_na_sub_dis <- rivers_na_sub %>%
  group_by(ORD_FLOW) %>%
  summarise(geometry = st_union(geometry), .groups = "drop") %>%
  st_cast("MULTILINESTRING")

rivers_sa_dis <- rivers_sa %>%
  group_by(ORD_FLOW) %>%
  summarise(geometry = st_union(geometry), .groups = "drop") %>%
  st_cast("MULTILINESTRING")

rivers_sa_f4_dis <- rivers_sa_f4 %>%
  group_by(ORD_FLOW) %>%
  summarise(geometry = st_union(geometry), .groups = "drop") %>%
  st_cast("MULTILINESTRING")

rivers_sa_f3_dis <- rivers_sa_f3 %>%
  group_by(ORD_FLOW) %>%
  summarise(geometry = st_union(geometry), .groups = "drop") %>%
  st_cast("MULTILINESTRING")




