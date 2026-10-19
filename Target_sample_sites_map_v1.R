### Evaluate macroinvertebrate and periphyton data to plan 2027 sampling 
###   Use to scope additional sampling sites

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

### Evaluate Macro_dat for summaries of sampling resolution over 
###   different biotypes, etc. 

Macro_dat |>
      group_by(BIOREGION, Site_type)|>
      summarize(
        Diatoms_count = sum(!is.na(Diatom_dat)))

### data may be missing from this data pull (does not include 2026 sampling)
### count = 187 periphyton samples
###     BIOREGION Site_type Diatoms_count
###     <chr>     <chr>             <int>
###     Mountains Reference            81
###     Mountains Stressed             31
###     Plains    Reference            25
###     Plains    Stressed             17
###     Xeric     Reference            20
###     Xeric     Stressed             13

### based on this data set, additional xeric reference and stressed sites would be useful
###  Additional Plains reference and stressed sites would also be useful, although
###   several sites were sampled in 2026 and are not included in the data set

### identify sites with macroinvertebrate data, but no periphyton data
Macros_no_peri <- Macro_dat|>
                  filter(is.na(Diatom_dat))
### 105 sites
Macro_dat|>
        group_by(BIOREGION, Site_type)|>
        summarize(
          Macros_no_peri = sum(is.na(Diatom_dat))
        )

### 8 reference Xeric sites, 15 stressed Xeric sites, 10 stressed plains sites 
###   (may have gotten some of these in 2026!) that have macroinvertebrate data
###   but no periphyton data


################################################################################
### generate map with potential target sites


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
  Macros_no_peri, 
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


target_sites1 <- ggplot()+
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
    max_size = 8, 
    breaks = c(10, 30, 60, 80) ### MMI min = 4, max = 94
  )+
  ### shape format
  scale_shape_manual(
    name = "",
    values = c(16, 17))+
  ### ensure CRS
  coord_sf(crs = 26913)+
  theme_cdphe(type = "web", 
              legend_position = "top",
              ) + 
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


target_sites1

#ggsave("20261019_Biohab_potential_sites_map.jpeg", 
#       plot = target_sites1, 
#       dpi = 600, width = 16, height = 10, units = "in",
#       device = ragg::agg_jpeg)   
