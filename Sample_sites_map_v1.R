### Map of diatom and macroinvertebrate sampling locations
###   Use to scope additional sampling sites


### Note: Watersheds shapefile includes subbasins (so, 11 basins total)

### select packages
packages <- c("tidyverse", "readxl",
              "here", "knitr", "kableExtra", 
              "scales", "quarto", "tinytex",
              "sf", "ggspatial")

### quietly load packages
invisible(lapply(packages, require, character.only = TRUE, quietly = TRUE))

file_path <- here::here("Data") 

### Pull in macroinvertebrate data
Macro_ref <- read.csv(here(path = file_path, "CDPHE MMI v5 Reference.csv"))
Macro_ref <- Macro_ref|>
              mutate(Site_type = "Reference")|>
              select(-c(23, 24))|> ### remove blank columns that were imported 
              rename(ORIG = ORIG_REF,
                     NEW = NEW_REF)

Macro_stress <- read.csv(here(path = file_path, "CDPHE MMI v5 Stressed.csv"))
Macro_stress <- Macro_stress|>
                mutate(Site_type = "Stressed")|>
                rename(ORIG = ORIG_STR,
                       NEW = NEW_STR)

Macro_dat <- rbind(Macro_ref, Macro_stress)

### use macroinvertebrate data frame to create periphyton data frame
### column "PERI" if not NA, mark as X



### read in shapefiles
ecotype_sf <- st_read(here(path = file_path, "co_eco_l4", "co_eco_l4.shp"))
#st_crs(ecotype_sf)
watersheds_sf <- st_read(here(path = file_path, "All_River_Basins", "All_River_Basins.shp"))
#st_crs(watersheds_sf)
### transform watershed sf CRS
watersheds_sf_trans <- st_transform(watersheds_sf, crs = st_crs(ecotype_sf))

### create Colorado boundary by compressing ecoregion boundaries
co_boundary_sf <- st_union(ecotype_sf)|>
                  st_transform(crs = 26913)



### Convert site data frames to sf objects
Macro_dat_sf <- st_as_sf(
  Macro_dat, 
  coords = c("Long_Dec", "Lat_Dec"),
  crs = 4326, ### NAD83/UTM zone 13N for Colorado
  remove = F)|>
  ### reproject to match shapefile CRS
  st_transform(crs = st_crs(ecotype_sf))
### check crs just to be sure
#st_crs(Macro_dat_sf)

### check bounding box to ensure points match watershed and ecotype CRS/plotting area
#st_bbox(Macro_dat_sf)
#Macro_dat_sf|>
#    select(Long_Dec, Lat_Dec)|>
#  summary()

############################## start mapping ###################################

### incorporate CDPHE style
source("cdphe_ggplot_theme-v4.R")

map1 <- ggplot()+
  ### Colorado bold outline
  geom_sf(
    data = co_boundary_sf,
    fill = NA,
    color = "black",
    linewidth = 1.2)+
  ### Basin watersheds outlines
  geom_sf(
    data = watersheds_sf,
    #fill = CDPHE_PALETTES$teal[5], # Lightest Teal tint (#D7E0E5) 
    #color = CDPHE_PALETTES$gray[["dark"]], 
    linewidth = 0.9, 
    color = "black")+
  ### Ecoregions
  geom_sf(
    data = ecotype_sf,
    aes(fill = L3_KEY),
    color = "white",
    alpha = 0.2)+
  ### sample points
  geom_sf(
    data = Macro_dat_sf,
    aes(shape = Site_type, size = MMI), 
    #size = 3.5, 
    alpha = 0.65)+
  ### scale bar and north arrow
  annotation_scale(
    location = "bl",  ### bottom left
    width_hint = 0.2)+
  annotation_north_arrow(
    location = "tr",
    which_north = "true",
    style = north_arrow_minimal(text_size = 8))+
  ### labels and styling
  scale_fill_cdphe(palette = "categorical", name = "Level III Ecoregion")+
  scale_color_cdphe(palette = "navy", name = "Site Type")+
  ### continuous scale for MMI point size
  scale_size_area(
    name = "MMI score", 
    max_size = 6, 
    breaks = c(10, 25, 50, 80) ### MMI min = 4, max = 94
  )+
  ### ensure CRS
  coord_sf(crs = 26913, datum = NA)+
theme_cdphe(type = "web", 
            legend_position = "top") + 
  theme(axis.line = element_blank(), # Remove standard chart axis lines 
        axis.title = element_blank(), # Hide Lat/Long axis title text 
        panel.grid.major = element_line(color = "#E5E5E5", linewidth = 0.2)) # Subtle gridlines 

map1
  
  
  
  
  
