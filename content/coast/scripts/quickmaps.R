library(sf)
library(terra)
library(nbaR)
library(here)
library(ggplot2)
library(tidyverse)
#devtools::install_github("SANBI-NBA/nbaR", force = T)

### Accessing .Renviron variables from within R
# Use the R function `Sys.getenv` to call .Renviron variables from within your R code:
data_file_path <- Sys.getenv("NBA_DATA")

### Reading data from the NBA data repository with list.files

cst_cond_path <- list.files(path = data_file_path, 
                           pattern = "^cst_nba2025_cst_cond_map\\.shp$",
                           recursive = TRUE,
                           full.names = TRUE)

nba2025cst_path <- list.files(path = data_file_path, 
                           pattern = "^cst_nba2025_edcz-map\\.shp$",
                           recursive = TRUE,
                           full.names = TRUE)

# read-in for the condition map and results map
cst_cond_map <- st_read(cst_cond_path) 
nba2025cst <- st_read(nba2025cst_path)

# Simplify these data to make things faster
nba2025cst <- st_simplify(nba2025cst, dTolerance = 50)
cst_cond_map <- st_simplify(cst_cond_map, dTolerance = 50)

############################### Clean up data for mapping

# Condition map
cst_cond_map <- cst_cond_map %>%
  rename(
    `Ecological condition` = Eclgclc,
    area_km2 = are_km2
  )

# Recode to NBA 2025 condition categories
cst_cond_map <- cst_cond_map %>% 
  mutate(`Ecological condition` = recode(
    `Ecological condition`,
    "Natural / near natural" = "Natural / near-natural ",
    "Heavily / intensively modified" = "Heavily modified",
    "Severely/critically modified" = "Severely / critically modified")) %>% 
  mutate(`Ecological condition` = fct_relevel(`Ecological condition`, 
                                              "Natural / near-natural ", 
                                              "Moderately modified", 
                                              "Heavily modified",
                                              "Severely / critically modified"))

############################ clean up cols for RLE and EPL

nba2025cst <- nba2025cst %>% 
  rename(`Threat status` = "Thrtstt", `Protection level` = "Prtctnl") %>%
  filter('Threat status' != "Data Deficient", `Protection level` != "Data Deficient") %>%
  mutate(`Threat status` = fct_relevel(`Threat status`, "Critically Endangered",
                                                              "Endangered",
                                                              "Vulnerable",
                                                              "Near Threatened",
                                                              "Least Concern"),
         `Protection level` = fct_relevel(`Protection level`,"Well Protected",
                                                                    "Moderately Protected",
                                                                    "Poorly Protected",
                                                                    "Not Protected"))

##################MAPS#####################################################

# COndition 
cst_condition_map <- nba_map(DF = cst_cond_map,
                             GEOM = 'vector',
                             FILL = `Ecological condition`,
                             LEGEND = TRUE,
                             MOE = FALSE,
                             SCALE_TEXT = 1) 

ggsave(
  filename = here("quarto/imgs/cst_cond_map_new2.png"),
  plot = cst_condition_map,
  width = 30,   # inches = pixels / dpi
  height = 15,  # adjust aspect ratio
  units = "cm",
  dpi = 150
)


# Create and save coastal ETS map ----------------------------------------------
cst_ets_map <- nba_map(DF = nba2025cst,
                       GEOM = 'vector',
                       FILL = `Threat status`,
                       LEGEND = TRUE,
                       #MOE = FALSE,
                       SCALE_TEXT = 1)

ggsave(
  filename = "quarto/imgs/cst_ets_map_new2.png",
  plot = cst_ets_map,
  width = 30,   # inches = pixels / dpi
  height = 15,  # adjust aspect ratio
  units = "cm",
  dpi = 150
)

# Create and save EPL map ------------------------------------------------------
cst_epl_map <- nba_map(DF = nba2025cst,
                       GEOM = 'vector',
                       FILL = `Protection level`,
                       LEGEND = TRUE,
                       MOE = FALSE,
                       SCALE_TEXT = 1)
ggsave(
  filename = here("quarto/imgs/cst_epl_map_new2.png"),
  plot = cst_epl_map,
  width = 30,   # inches = pixels / dpi
  height = 15,
  units = "cm",
  dpi = 150
)

# Create & save map of highly threatened & under-protected ecosystem types -----
#coast_unluckies <- st_read(here("outputs/cst_nba2025_edcz-map1.shp")) %>%
# make table first 
unlucky_tb <- st_drop_geometry(nba2025cst)
write_csv(unlucky_tb, "quarto/outputs/unlucky_table.csv")

coast_unluckies <- nba2025cst %>% 
  filter(`Threat status` %in% c("Critically Endangered", "Endangered") & 
           `Protection level` %in% c("Not Protected", "Poorly Protected")) %>%
  mutate(`Threat status & protection level` = paste0(`Threat status`,
                                                     " & ", 
                                                     `Protection level`))

map_unlky <- nba_map(DF = coast_unluckies,
                     GEOM = "vector",
                     FILL = `Threat status & protection level`,
                     LEGEND = TRUE,
                     MOE = FALSE,
                     SCALE_TEXT = 1)
ggsave(
  filename = here("quarto/imgs/cst_unluckies_map_new2.png"),
  plot = map_unlky,
  width = 30,   # inches = pixels / dpi
  height = 15,  # adjust aspect ratio
  dpi = 150,
  units = "cm")

