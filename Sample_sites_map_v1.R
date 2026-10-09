### Map of diatom and macroinvertebrate sampling locations
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

### read in shapefiles
watersheds_sf <- st_read(here(path = file_path, "All_River_Basins", "All_River_Basins.shp"))
ecotype_sf <- st_read(here(path = file_path, "co_eco_l4", "co_eco_l4.shp"))

#st_crs(watersheds_sf)

### Convert site data frames to sf objects
Macro_dat_sf <- 
  
  
  
  
