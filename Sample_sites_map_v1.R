### Map of diatom and macroinvertebrate sampling locations
###   Use to scope additional sampling sites

### Note: This map only plots stressed or reference sites; many other sites
###   have been sampled that are between those categories! Those could/should
###   be brought in to this map or another map that shows all sample points

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
Macro_dat <- Macro_dat|>
              mutate(Diatom_dat = if_else(
                !is.na(PERI) & str_trim(PERI) != "", "X", NA_character_))


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

### Set up typeface plotting export options
library(showtext)
showtext_auto()
showtext_opts(dpi = 600)
font_add(family = "Trebuchet MS", regular = "trebuc.ttf", bold = "trebucbd.ttf")


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
  ### macroinvertebrate sample points
  geom_sf(
    data = Macro_dat_sf,
    aes(shape = Site_type, size = MMI, color = as.factor(BIOREGION)), 
    #size = 3.5, 
    alpha = 0.8)+
  ### periphyton data points
  geom_sf(
    data = Macro_dat_sf|>filter(Diatom_dat == "X"),
      shape = 23,
      fill = CDPHE_PALETTES$yellow[1],
      size = 2.5,
      color = "black",
      alpha = 0.65, 
      stroke = 0.4)+
  ### scale bar and north arrow
  annotation_scale(
    location = "bl",  ### bottom left
    width_hint = 0.2)+
  annotation_north_arrow(
    location = "tr",
    which_north = "true",
    style = north_arrow_fancy_orienteering(text_size = 12))+
  ### labels and styling
  scale_fill_cdphe(palette = "categorical", name = "Level III Ecoregion")+
  #scale_color_cdphe(palette = "navy", name = "Site Type")+
  ### colors for macroinvertebrate/bioregion points 
  scale_color_manual(
    name = "Bioregion",
    values = c(
      CDPHE_PALETTES$forest[1],
      CDPHE_PALETTES$olive[1],
      CDPHE_PALETTES$purple[1]))+
  ### continuous scale for MMI point size
  scale_size_area(
    name = "MMI score", 
    max_size = 6, 
    breaks = c(10, 25, 50, 80) ### MMI min = 4, max = 94
  )+
  ### shape format
  scale_shape_manual(
    name = "",
    values = c(16, 17))+
  ### ensure CRS
  coord_sf(crs = 26913)+
theme_cdphe(type = "web", 
            legend_position = "top") + 
  theme(axis.line = element_blank(), # Remove standard chart axis lines 
        axis.title = element_blank(), # Hide Lat/Long axis title text 
        panel.grid.major = element_line(color = "grey45", linewidth = 0.2),
        ### adjust legend sizes
        legend.key.size = unit(0.35, "cm"),
        legend.key.height = unit(0.35, "cm"),
        legend.key.width = unit(0.35, "cm"),
        legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 12),
        legend.spacing.y = unit(0.15, "cm"),
        legend.box.margin = margin(t = 2, r = 2, b = 2, l = 2, unit = "pt"))+
  ### try to get legend to fit better
  guides(
    #color = guide_legend(
    #  ncol = 1, 
    #  byrow = T, 
    #  title.position = "top"
    #),
    fill = guide_legend(
      nrow = 3),
    shape = guide_legend(
      nrow = 3),
    color = guide_legend(
      nrow = 3),
    size = guide_legend(
      nrow = 4))
    
    
map1

#ggsave("20261009_RefStress_Biohab_map.jpeg", 
#       plot = map1, 
#       dpi = 600, width = 16, height = 10, units = "in",
#       device = ragg::agg_jpeg)  
  
  
  
  
