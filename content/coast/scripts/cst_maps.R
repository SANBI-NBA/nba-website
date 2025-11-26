library(sf)
library(terra)
library(nbaR)
library(here)
library(ggplot2)
library(tidyverse)

######################### EXTRACT THE EDCZ FROM THE IEM ########################
# Set up file names ------------------------------------------------------------
iem_filename <-"IEM_5_13_3_010825.shp"
iem_filepath <-"spatial_data/link2spatial_data/IEM2025_August/Shapefile/"

# Read in the Integrated Ecosystem Map -----------------------------------------
iem <- st_read(here(paste0("data/", iem_filepath, iem_filename))) 

# Extract the coast from the IEM -----------------------------------------------
edcz <- iem %>% 
  filter(I_Map_Coas=="Coast") %>% 
  select(I_Rlm_Prim, I_EcoType1,  I_Color_1, Shape_Area, geometry) %>% 
  rename(mapcode=I_Color_1)

# Read in the NBA 2025 coast results -------------------------------------------
cst <- read_csv(here("outputs/cst_results_for_integration.csv"))

# Read in the mapcodes for the microestuaries and add columns to match cst -----
# Estuaries weren't assessed so we need to add some data for them to append to
# the shapefile of the coast
microest <- read_csv(here("data/ecotype_mapcode.csv")) %>% 
  filter(str_detect(type, "Micro-estuary")) %>% 
  mutate(zone="Microestuary", 
         extent ="NA",
         RLE = "NA",
         EPL = "NA",
         realm = "est")

# Add the micro-estuaries (not assessed) to the NBA 2025 Coast results----------
edcz_codes <- rbind(cst, microest)

# Drop the 'shores' qualifier in the mapcodes for the estuaries ----------------
## In the NBA 2025 Results
edcz_codes2 <- edcz_codes %>% 
  mutate(mapcode = if_else(realm == "est",
                         str_replace(mapcode, "s$", ""),  # remove trailing s
                         mapcode))

## ...and in the EDCZ shapefile
edcz2 <- edcz %>% 
  mutate(mapcode = if_else(I_Rlm_Prim == "Estuary",
                           str_replace(mapcode, "s$", ""),  # remove trailing s
                           mapcode))

# Join the NBA 2025 results to the EDCZ shapefile ------------------------------
nba2025cst <- edcz2 %>% 
  left_join(edcz_codes2, by = "mapcode")

# Do some cross checks ---------------------------------------------------------
# namecheck <- nba2025cst %>%
#   mutate(match = I_EcoType1 == type) %>% 
#   filter(match=="FALSE")
# table(namecheck$match)
# 
# rlmcheck <- nba2025cst %>%
#   mutate(concat_realm = str_c(I_Rlm_Prim, realm, sep = "_")) %>% 
#   filter(concat_realm %in% c("Estuary_marine", "Marine_ter"))
# table(rlmcheck$concat_realm)

# Some mismatches in realm / ecosystem type / mapcode because of trumping rules
# for estuaries and kelp, and islands and veg types. Sticking with joining by 
# mapcodes - they feel more correct. So, next step is to dissolve the map by 
# mapcode. More efficient to dissolve first and then add the NBA 2025 results.

# Option for dissolving the map (not run - was actually worse to do this way) ----
## Dissolve EDCZ by mapcode (ecosystem type) (not run) ----
# edcz_dissolved <- edcz2 %>% 
#   group_by(mapcode) %>%
#   summarise(geometry = st_union(geometry))

## Join the NBA 2025 Coast data to the EDCZ shapefile (not run) ----
# nba2025cst <- edcz_dissolved %>% 
#   inner_join(edcz_codes2, by = "mapcode") %>% 
#   mutate(RLE = recode(RLE,
#                       "CR" = "Critically Endangered",
#                       "EN" = "Endangered",
#                       "VU" = "Vulnerable",
#                       "NT" = "Near Threatened",
#                       "LC" = "Least Concern",
#                       "NA" = "Data Deficient"),
#          EPL = recode(EPL,
#                       "WP" = "Well Protected",
#                       "MP" = "Moderately Protected",
#                       "PP" = "Poorly Protected",
#                       "NP" = "Not Protected",
#                       "NA" = "Data Deficient"))
# Save the shapefile (not run) ----
# st_write(nba2025cst, here("outputs/cst_nba2025_edcz-map.shp"), append = FALSE)

# Option without dissolving first ----------------------------------------------
## Join the NBA 2025 Coast data to the EDCZ shapefile ----
nba2025cst <- edcz2 %>% 
  inner_join(edcz_codes2, by = "mapcode") %>% 
  mutate(RLE = recode(RLE,
                      "CR" = "Critically Endangered",
                      "EN" = "Endangered",
                      "VU" = "Vulnerable",
                      "NT" = "Near Threatened",
                      "LC" = "Least Concern",
                      "NA" = "Data Deficient"),
         EPL = recode(EPL,
                      "WP" = "Well Protected",
                      "MP" = "Moderately Protected",
                      "PP" = "Poorly Protected",
                      "NP" = "Not Protected",
                      "NA" = "Data Deficient"))
# Mangrove Forests are trumped by Estuaries. So, if use left_join, it introduces 
# a null row. Better to inner join to avoid that. It does mean, though, that 
# the map then displays 192 ecosystem types instead of 193.

# Order the factor levels ------------------------------------------------------
nba2025cst <- nba2025cst %>% 
  mutate(RLE = factor(RLE, levels = c("Critically Endangered",
                                      "Endangered",
                                      "Vulnerable",
                                      "Near Threatened",
                                      "Least Concern",
                                      "Data Deficient"),
                      ordered = TRUE),
         EPL = factor(EPL, levels = c("Well Protected",
                                      "Moderately Protected",
                                      "Poorly Protected",
                                      "Not Protected",
                                      "Data Deficient"))) %>% 
  rename(`Threat status` = RLE,
         `Protection level` = EPL)


# Save the EDCZ shapefile (nba2025cst) -----------------------------------------
st_write(nba2025cst, here("data/spatial_data/link2spatial_data/cst_nba2025_edcz-map.shp"), append = FALSE)

#nba2025cst <- st_read(here("data/spatial_data/link2spatial_data/cst_nba2025_edcz-map.shp"))

####################### CREATE AND EXPORT MAPS OF RESULTS ######################

# Create and save coastal condition map ----------------------------------------

###############################################################################.
############ Be warned, this condition section takes LONG to run! #############
#################### Rerun only if you absolutely have to! ####################.
########## Click run, then go have a nap... it's honestly that long! ##########.
###############################################################################.

## Prep the coastal (EDCZ) data ------------------------------------------------

## Dissolve EDCZ by realm
edcz_realm_d <- nba2025cst %>%
  group_by(realm) %>%
  summarise(geometry = st_union(geometry))
#st_write(edcz_realm_d, here("data/spatial_data/link2spatial_data/cst_nba2025_edcz-realm-map.shp"), append = FALSE)

## Dissolve to whole EDCZ, then add a 1km buffer and convert to a format terra
## can work with
edcz_d <- st_union(edcz_realm_d)
edcz_d_1kmbfr <- st_buffer(edcz_d, dist=1000)
cst_1kmbfr_sv <- vect(edcz_d_1kmbfr)


## Terrestrial data ------------------------------------------------------------

## Read in the data
terr_cond_filepath <- "data/spatial_data/link2spatial_data/terr_cond/terr_cond_100m.tif"
t_cond <- rast(here(terr_cond_filepath))

# Extract a lookup table that includes a conversion from landcover classes to
# condition categories 
ct <- cats(t_cond)[[1]] %>% 
  mutate(ecolcond = case_when(
    Condition == 90 ~ "Natural / near natural",
    Condition == 30 ~ "Heavily / intensively modified",
    Condition %in% c(0, 5, 10) ~ "Severely/critically modified")) %>% 
  select(Desc_, ecolcond)

## Check CRS between t-cond and the buffered EDCZ - it's the same, so continue
crs(cst_1kmbfr_sv)
crs(t_cond)

## Mask the t_cond data to the buffered EDCZ (to reduce to the area of interest)
t_cond_cst <- mask(t_cond, cst_1kmbfr_sv)

## Convert that masked out t_cond to polygon format, and clip to the terrestrial 
## portion of the EDCZ
# Dissolve contiguous cells with the same value
t_cond_cst_p <- as.polygons(t_cond_cst, dissolve = TRUE)

# Convert from SV to sf feature
t_cond_cst_sf <- st_as_sf(t_cond_cst_p) 

# Pull out the terrestrial component of the EDCZ, and clip the terrestrial
# condition map by that shape
edcz_t <- edcz_realm_d %>% filter(realm == "ter")
t_cond_edcz <- st_intersection(t_cond_cst_sf, edcz_t)

#st_write(t_cond_edcz, here("data/spatial_data/link2spatial_data/cst_nba2025_t_cond-map.shp"), append = FALSE)
#t_cond_edcz <- st_read(here("data/spatial_data/link2spatial_data/cst_nba2025_t_cond-map.shp"))

## Pull the terrestrial condition dataset together
# Calculate the area per polygon
t_cond_edcz1 <- t_cond_edcz %>%
  mutate(Area_m2 = st_area(geometry))

# Summarise that by the landcover description and clean up attributes for 
# integrated map
t_cond_ext <- t_cond_edcz1 %>%
  group_by(Desc_) %>%
  summarise(Total_area_m2 = sum(Area_m2))

# convert to a tibble
t_cond_ext_df <- st_set_geometry(t_cond_ext, NULL)

# Join the actual condition classes to the table. Then summarise area by the 
# actual condition classes
ter_cond_tbl <- t_cond_ext_df %>% 
  left_join(ct, by = "Desc_") %>% 
  mutate(extent_m2 = as.numeric(Total_area_m2)) %>% 
  group_by(ecolcond) %>%
  summarise(area_m2 = sum(extent_m2, na.rm = TRUE)) %>% 
  mutate(area_km2 = area_m2/1e6,
    percent = 100 * area_m2 / sum(area_m2),
    realm = "Coastal Terrestrial")

## Clean up terrestrial condition map for integration and export
ter_cond_edcz <-  t_cond_ext %>% 
  mutate(area_km2 = Total_area_m2/1000000,
         realm = "ter") %>% 
  left_join(ct, by = "Desc_") %>% 
  select(realm, ecolcond, area_km2, geometry)


## Non-coastal (here we can afford to do this at a raster level because there's 
## no shore/coast crack issues to deal with)

noncst_t_cond <- mask(t_cond, nba2025cst, inverse = TRUE)

tcell_area_m2 <- prod(res(noncst_t_cond))

noncst_t_cond_tbl <- freq(noncst_t_cond) %>%
  as_tibble() %>%
  mutate(area_km2 = count * tcell_area_m2 / 1e6)


cats_lc <- as_tibble(cats(t_cond)[[1]]) %>% 
  rename(value=Value)

noncst_t_cond_tbl1 <- noncst_t_cond_tbl %>% 
  right_join(cats_lc, by="value") %>% 
  left_join(ct, by="Desc_") %>% 
  group_by(ecolcond) %>% 
  summarise(area_km2 = sum(area_km2)) %>% 
  mutate(Realm="Terrestrial") %>%
  select(Realm, ecolcond, area_km2)
#write_csv(noncst_t_cond_tbl1, here("data/spatial_data/link2spatial_data/noncst_nba2025_ter_cond-tbl.csv"), append = FALSE)
#noncst_t_cond_tbl1 <- read_csv(here("data/spatial_data/link2spatial_data/noncst_nba2025_ter_cond-tbl.csv"))

noncst_ter_cond_tbl <- noncst_t_cond_tbl1 %>% 
  mutate(area_m2 = area_km2*1e6,
         percent = 100 * area_m2 / sum(area_m2),
         realm="Non-coastal Terrestrial") %>% 
  select(!Realm)

## Estuary data ----------------------------------------------------------------

#nba2025cst <- st_read(here("data/spatial_data/link2spatial_data/cst_nba2025_edcz-map.shp"))

# Read in the data and project the data 
est_cond_filepath <- "data/spatial_data/link2spatial_data/est_results_poly_dd/est_results_poly_dd.gpkg"
st_layers(est_cond_filepath)
est_cond_dd <- st_read(here(est_cond_filepath), layer = "est_results_poly_dd")

st_crs(est_cond_dd)
st_crs(nba2025cst)
est_cond <- st_transform(est_cond_dd, st_crs(nba2025cst))

## Calculate estuary areas and export
# forlara <- est_cond %>% 
#   mutate(area_m2 = as.numeric(st_area(geom))) %>% 
#   select(I_EcoType1, estID, est_name, area_m2, geom) %>% 
#   st_set_geometry(., NULL) %>% 
#   mutate(area_ha = as.numeric(area_m2/10000),
#          area_km2 = as.numeric(area_m2/1000000))
# write_csv(forlara, "data/spatial_data/link2spatial_data/estuaryareas.csv")

est_cond_edcz <- est_cond %>% 
  mutate(area_m2 = st_area(geom)) %>% 
  select(realm, mod24, area_m2, extent, geom) %>%
  mutate(ecolcond = case_when(
    mod24 %in% c("NearNatural", "Natural") ~ "Natural / near natural",
    mod24 == "Moderate" ~ "Moderately modified",
    mod24 == "Heavily" ~ "Heavily / intensively modified",
    mod24 == "Severely" ~ "Severely/critically modified"),
    area_km2=area_m2/1000000)%>% 
  select(realm, ecolcond, area_km2, geom) %>% 
  rename(geometry=geom)
#st_write(est_cond_edcz, here("data/spatial_data/link2spatial_data/cst_nba2025_e_cond-map.shp"), append = FALSE)
#est_cond_edcz <- st_read(here("data/spatial_data/link2spatial_data/cst_nba2025_e_cond-map.shp"))

# Calculate areas
est_cond_tbl <- st_set_geometry(est_cond_edcz, NULL) %>% 
  group_by(ecolcond) %>%
  summarise(area_km2 = as.numeric(sum(area_km2))) %>% 
  mutate(area_m2 = as.numeric(area_km2*1e6),
         percent = 100 * area_m2 / sum(area_m2),
         realm="Estuarine")


## Marine data -----------------------------------------------------------------

# Read in the data
mar_cond_filepath <- "data/spatial_data/link2spatial_data/mar_cond/condition20181114.shp"
mar_cond <- st_read(here(mar_cond_filepath))
#st_crs(mar_cond)

# Pull out the marine component of the EDCZ, and clip the marine
# condition map by that shape
edcz_m <- edcz_realm_d %>% filter(realm == "marine")
m_cond_edcz <- st_intersection(mar_cond, edcz_m)

#st_write(m_cond_edcz, here("data/spatial_data/link2spatial_data/cst_nba2025_m_cond-map.shp"), append = FALSE)
#m_cond_edcz <- st_read(here("data/spatial_data/link2spatial_data/cst_nba2025_m_cond-map.shp"))

marine_cond_edcz <- m_cond_edcz %>% 
  group_by(Condition) %>% 
  summarise() %>% 
  ungroup()

mar_cond_edcz <- marine_cond_edcz %>%
  mutate(area_m2 = st_area(geometry),
         area_km2 = area_m2/1000000,
         realm = "mar",
         ecolcond = case_when(
           Condition == "Natural / Near Natural   (Good)" ~ "Natural / near natural",
           Condition == "Moderately Modified   (Fair)" ~ "Moderately modified",
           Condition == "Severely Modified  (Poor)" ~ "Heavily / intensively modified",
           Condition == "Very Severely Modified  (Very Poor)" ~ "Severely/critically modified")) %>% 
  select(realm, ecolcond, area_km2, geometry)
#st_write(mar_cond_edcz, here("data/spatial_data/link2spatial_data/cst_nba2025_mar_cond-map.shp"), append = FALSE)
#mar_cond_edcz <- st_read(here("data/spatial_data/link2spatial_data/cst_nba2025_mar_cond-map.shp"))

## compile marine dataset
mar_cond_tbl <- st_set_geometry(mar_cond_edcz, NULL) %>% 
  mutate(area_m2 = area_km2*1e6,
         percent = 100 * area_m2 / sum(area_m2),
         realm="Coastal Marine")


## marine non-coastal
mar_condc_filepath <- "data/spatial_data/link2spatial_data/mar_cond/condition20181114clipped.shp"
mar_condc <- st_read(here(mar_condc_filepath))
#noncst_m_cond <- st_difference(mar_condc, nba2025cst) #ran out of memory

#switch to terra objects - more memory efficient
v1 <- vect(mar_condc)
v2 <- vect(nba2025cst)
out <- erase(v1, v2)   # equivalent of st_difference
out_sf <- st_as_sf(out)
#st_write(out_sf, here("data/spatial_data/link2spatial_data/noncst_nba2025_mar_cond-map.shp"), append = FALSE)
#out_sf <- st_read(here("data/spatial_data/link2spatial_data/noncst_nba2025_mar_cond-map.shp"))

out_sf1 <- out_sf %>% 
  group_by(Condition) %>% 
  summarise() %>% 
  ungroup()

out_sf2 <- out_sf1 %>%
  mutate(area_m2 = st_area(geometry),
         area_km2 = area_m2/1000000,
         realm = "mar",
         ecolcond = case_when(
           Condition == "Natural / Near Natural   (Good)" ~ "Natural / near natural",
           Condition == "Moderately Modified   (Fair)" ~ "Moderately modified",
           Condition == "Severely Modified  (Poor)" ~ "Heavily / intensively modified",
           Condition == "Very Severely Modified  (Very Poor)" ~ "Severely/critically modified")) %>% 
  select(realm, ecolcond, area_km2, geometry)
#st_write(mar_cond_edcz, here("data/spatial_data/link2spatial_data/cst_nba2025_mar_cond-map.shp"), append = FALSE)
#mar_cond_edcz <- st_read(here("data/spatial_data/link2spatial_data/cst_nba2025_mar_cond-map.shp"))

## compile marine dataset
noncst_mar_cond_tbl <- st_set_geometry(out_sf2, NULL) %>% 
  mutate(area_m2 = as.numeric(area_km2*1e6),
         percent = 100 * area_m2 / sum(area_m2),
         realm="Non-coastal Marine",
         area_km2 = as.numeric(area_km2))


## Combined realm and coast/non-coast into a single dataset -------------------
# Dataset for plotting graph -----

# coastal data 
cst_cond_dat <- rbind(ter_cond_tbl, est_cond_tbl, mar_cond_tbl)
#write_csv(cst_cond_dat, here("data/spatial_data/link2spatial_data/cst_cond_dat.csv"))
#cst_cond_dat <- read_csv(here("data/spatial_data/link2spatial_data/cst_cond_dat.csv"))

# non-coastal data
noncst_cond_dat <- rbind(noncst_mar_cond_tbl, noncst_ter_cond_tbl) %>% 
  group_by(ecolcond) %>%
  summarise(area_km2 = sum(area_km2)) %>%
  mutate(realm = "Non-coastal",
         percent = area_km2/sum(area_km2)*100) %>% 
  select(!area_km2) %>% 
  rename(`Ecological condition` = ecolcond)

# calculate statistics for coast, add in non-coastal, and format dataset 
cst_cond <- cst_cond_dat %>%
  group_by(ecolcond) %>%
  summarise(area_km2 = sum(area_km2)) %>%
  mutate(realm = "Coastal (overall)",
         area_m2 = area_km2*1000000,
         percent = area_km2/sum(area_km2)*100) %>%
  bind_rows(cst_cond_dat) %>% 
  rename(`Ecological condition` = ecolcond) %>% 
  select(realm, `Ecological condition`, percent) %>% 
  rbind(noncst_cond_dat) %>% 
  mutate(`Ecological condition` = fct_relevel(`Ecological condition`, 
                                              "Natural / near natural", 
                                              "Moderately modified", 
                                              "Heavily / intensively modified",
                                              "Severely/critically modified"),
         realm = fct_relevel(realm,
                             "Coastal Terrestrial",
                             "Estuarine",
                             "Coastal Marine",
                             "Coastal (overall)",
                             "Non-coastal"))
  
# reformat for plotting
cst_cond_wd <-  cst_cond %>% 
  pivot_wider(id_cols = realm,
              names_from = `Ecological condition`,
              values_from = percent) %>% 
  rename(`Natural` = `Natural / near natural`,
         `Heavily modified` = `Heavily / intensively modified`) %>% 
  arrange(desc(realm)) %>% 
  select(realm,
         `Natural`, 
         `Moderately modified`, 
         `Heavily modified`, 
         `Severely/critically modified`) %>% 
  mutate(across(where(is.numeric), ~ round(.x, 2))) 

# Replace the NA with 0 (reverted to skipping this step because it is NA, not 0)
# cst_cond_wd$`Moderately modified`[cst_cond_wd$realm == "Coastal Terrestrial"] <- 0

# Rename to condition categories we settled on for NBA 2025
#cst_cond_wd<-read_csv(here("outputs/cst_condition.csv"))
cst_cond_wd <- cst_cond_wd %>% 
  rename(
    `Natural / near-natural`= "Natural",
    `Severely / critically modified` = `Severely/critically modified`
  )

write_csv(cst_cond_wd, here("outputs/cst_condition.csv"))


# nba_plot(cst_cond_wd,
#                 GROUPS=realm,
#                 COLS = 2:5,
#                 CHRT = "bar",
#                 NUM = FALSE,
#                 LAB = "Percentage of ecosystem types",
#                 SAVE=NULL)


# Map -----
cst_cond_map <- rbind(ter_cond_edcz, est_cond_edcz, mar_cond_edcz) %>% 
  rename(`Ecological condition` = ecolcond) %>% 
  mutate(`Ecological condition` = fct_relevel(`Ecological condition`, 
                                              "Natural / near natural", 
                                              "Moderately modified", 
                                              "Heavily / intensively modified",
                                              "Severely/critically modified"))
st_write(cst_cond_map, here("data/spatial_data/link2spatial_data/cst_nba2025_cst_cond_map.shp"), append = FALSE)

#fast read-in for the condition map to recode below:
# cst_cond_map <- st_read(here("data/spatial_data/link2spatial_data/cst_nba2025_cst_cond_map.shp"))
# cst_cond_map <- cst_cond_map %>%
#   rename(
#     `Ecological condition` = Eclgclc,
#     area_km2 = are_km2
#   )

# recode to NBA 2025 condition categories
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

cst_condition_map <- nba_map(DF = cst_cond_map,
                       GEOM = 'vector',
                       FILL = `Ecological condition`,
                       LEGEND = TRUE,
                       MOE = FALSE,
                       SCALE_TEXT = 0.55)

ggsave(
  filename = here("imgs/cst_cond-map_new.png"),
  plot = cst_condition_map,
  width = 800/120,   # inches = pixels / dpi
  height = (800/120) * 0.51,  # adjust aspect ratio
  dpi = 120
)


## Fast read in to edit and update maps below
# nba2025cst <- st_read(here("data/spatial_data/link2spatial_data/cst_nba2025_edcz-map.shp"))
# nba2025cst <- nba2025cst %>% 
#   rename(`Threat status` = "Thrtstt",
#          `Protection level` = "Prtctnl")


# Create and save coastal ETS map ----------------------------------------------
cst_ets_map <- nba_map(DF = nba2025cst,
                       GEOM = 'vector',
                       FILL = `Threat status`,
                       LEGEND = TRUE,
                       MOE = FALSE,
                       SCALE_TEXT = 0.55)

ggsave(
  filename = here("imgs/cst_ets-map_new.png"),
  plot = cst_ets_map,
  width = 800/120,   # inches = pixels / dpi
  height = (800/120) * 0.51,  # adjust aspect ratio
  dpi = 120
)

# Create and save EPL map ------------------------------------------------------
cst_epl_map <- nba_map(DF = nba2025cst,
                       GEOM = 'vector',
                       FILL = `Protection level`,
                       LEGEND = TRUE,
                       MOE = FALSE)
ggsave(
  filename = here("imgs/cst_epl-map.png"),
  plot = cst_epl_map,
  width = 800/100,   # inches = pixels / dpi
  height = (800/100) * 0.51,  # adjust aspect ratio
  dpi = 100
)

# Create & save map of highly threatened & under-protected ecosystem types -----
#coast_unluckies <- st_read(here("outputs/cst_nba2025_edcz-map1.shp")) %>%

coast_unluckies <- nba2025cst %>% 
  mutate(`Threat status` = recode(`Threat status`,
                      "Critically Endangered" = "CR",
                      "Endangered" = "EN"),
         `Protection level` = recode(`Protection level`,
                      "Not Protected" = "NP",
                      "Poorly Protected" = "PP")) %>%
  filter(`Threat status` %in% c("CR", "EN") & `Protection level` %in% c("NP", "PP")) %>%
  mutate(`Threat status & protection level` = paste0(`Threat status`,
                                                          "_", 
                                                          `Protection level`))

map_unlky <- nba_map(DF = coast_unluckies,
                     GEOM = "vector",
                     FILL = `Threat status & protection level`,
                     LEGEND = TRUE,
                     MOE = FALSE)
ggsave(
  filename = here("imgs/cst_unluckies-map.png"),
  plot = map_unlky,
  width = 800/100,   # inches = pixels / dpi
  height = (800/100) * 0.51,  # adjust aspect ratio
  dpi = 100
)
