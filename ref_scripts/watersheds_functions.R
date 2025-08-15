# Function to convert ALL CAPS to title case while preserving proper casing



# Function to dynamically read shapefiles from multiple region directories
hybas_read_multi <- function(base_dir, pattern, file_pattern) {
  # List all region directories based on the base directory and pattern provided
  region_dirs <- list.files(base_dir, 
                            pattern = pattern, 
                            full.names = TRUE, 
                            ignore.case = TRUE)
  
  # Function to read a shapefile from a region directory and track the region
  read_shapefile <- function(region_dir, file_pattern) {
    # Extract the region name from the folder name (e.g., "af", "na", "eu")
    region_name <- basename(region_dir)  # Get the last part of the path, assuming it's the region
    
    # Find the path of the shapefile based on the file pattern provided
    shapefile <- list.files(region_dir, pattern = file_pattern, full.names = TRUE)
    
    if (length(shapefile) == 1) {
      # Read the shapefile, select columns, make valid, and transform CRS
      st_read(shapefile) %>%
        select(HYBAS_ID, NEXT_SINK, MAIN_BAS, SUB_AREA, PFAF_ID, geometry) %>%
        st_make_valid() %>%
        st_transform(crs = 4326) %>%
        mutate(Region = region_name)  # Add a new column with the region name
    } else {
      message("No shapefile found matching the pattern in directory: ", region_dir)
      return(NULL)  # Return NULL if no shapefile is found
    }
  }
  
  # Read all shapefiles for each region and combine them, tracking regions
  basins_sa_combined <- map(region_dirs, ~ read_shapefile(.x, file_pattern)) %>%
    compact() %>%  # Remove NULL values if any regions didn't have a matching shapefile
    bind_rows()    # Combine all the shapefiles into one object
  
  return(basins_sa_combined)  # Return the combined data frame
}