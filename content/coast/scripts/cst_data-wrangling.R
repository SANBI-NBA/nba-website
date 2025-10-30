#### SCRIPT TO COMPILE AND CREATE THE COAST ASSESSMENT DATASETS FOR THE NBA ####
#### By Linda R. Harris                                                     

## 1. LOAD LIBRARIES ##############################################################
library(ggpubr) # must load before nbaR
library(nbaR)   # devtools::install_github("SANBI-NBA/nbaR")
library(here)
library(tidyverse)

# If need to edit later statistics only, read in this table and skip to the
# relevant section
# coast_data <- read_csv(here("outputs/cst_coast-data.csv"))

## 2. READ IN FUNCTIONS ###########################################################
# Functions to summarise data for overall coastal and non-coastal stats
source(here("scripts/cst_data-wrangling-functions.R"))

## 3. SET UP THE DATA #############################################################
### (1) Read in realm files and standardize columns --------------------------------
# Read in look-up table for the map codes per ecosystem type 
mapcode_lookup <- read_csv(here("data/ecotype_mapcode.csv"))

# For terrestrial, add ecosystem type names based on their map codes
ter <- read_csv(here("data/ter_results_for_integration.csv")) %>%
  rename(mapcode=type) %>% 
  left_join(., mapcode_lookup, by="mapcode") %>%
  select(realm, type, mapcode, extent, RLE, EPL)

# For estuaries, clean up the names of the ecosystem types and add map codes
est <- read_csv(here("data/est_results_for_integration.csv")) %>%
  mutate(type = str_sub(type, 3),
         type = str_remove(type, " -")) %>% 
  left_join(., mapcode_lookup, by="type") %>%
  select(realm, type, mapcode, extent, RLE, EPL)

# For marine, add the map codes
mar <- read_csv(here("data/mar_results_for_integration.csv")) %>% 
  left_join(., mapcode_lookup, by="type") %>%
  select(realm, type, mapcode, extent, RLE, EPL)

# Terrestrial considers mangrove and swamp forests to be transitional systems,
# so they are excluded from that analysis but need to be included for the 
# coastal summary; included as terrestrial types. Extents from the Harris et.
# al. 2025 Coastal biodiversity paper in AJMS.
extras <- read_csv(here("data/ter_extratypes.csv"))


### (2) Combine terrestrial, estuarine and marine results into a single object -----
tem <- rbind(ter, extras, est, mar) 

rm(ter, est, mar, extras, mapcode_lookup)

# 650 ecosystem types


### (3) Create a dataset of results for coastal  ecosystem types -------------------
cst <- read_csv(here("data/coast-e-ecosystem_types.csv")) %>% 
  rename(type = `Ecosystem Type`,
         zone = Subrealm) %>% 
  left_join(., tem, by = "type") %>%
  filter(!str_detect(type, "Micro-estuary")) %>% 
  select(mapcode, type, zone, extent, RLE, EPL, realm)

# 190 coastal ecosystem types that were assessed (3 micro-estuary types are not
# assessed)

# > save output: cst_results_for_integration.csv ----
write_csv(cst, here("outputs/cst_results_for_integration.csv"))


### (4) Create a dataset of results for non-coastal  ecosystem types ---------------
non_cst <- tem %>%
  filter(!type %in% cst$type) %>% 
  mutate(zone = "Non-coastal") %>% 
  select(mapcode, type, zone, extent, RLE, EPL, realm)

# 460 non-coastal ecosystem types

# check for NAs in the dataset
anyNA(cst)

# > save output: noncst_results_for_integration.csv ----
write_csv(non_cst, here("outputs/noncst_results_for_integration.csv"))


### (5) Reformat the coast dataset for other coastal scripts -----------------------
coast_data <- cst %>% 
  rename(ecotype = type,
         ets = RLE,
         epl = EPL) %>% 
  mutate(realm = recode(realm,
                       "marine" = "Coastal Marine",
                       "ter" = "Coastal Terrestrial",
                       "est" = "Estuarine")) %>% 
  mutate(threatened=if_else(ets %in% c("VU", "EN", "CR"), 1, 0),
         ets = fct_relevel(ets, 
                           "CR", 
                           "EN", 
                           "VU",
                           "NT",
                           "LC"),
         epl = fct_relevel(epl,
                           "WP",
                           "MP",
                           "PP",
                           "NP"),
         rlm = case_when(
           realm == "Coastal Terrestrial" ~ "T",
           realm == "Estuarine" ~ "E",
           realm == "Coastal Marine" ~ "M",
           TRUE ~ NA_character_  # Keeps NA for unmatched values
         )
  )

# check dataset is complete
anyNA(coast_data)

# > save output: cst_coast-data.csv ----
write_csv(coast_data, here("outputs/cst_coast-data.csv"))


## 4. HIGH LEVEL SUMMARY STATISTICS FOR COASTAL & NON-COASTAL #####################
### (1) Implement functions --------------------------------------------------------
# RLE: coast
cst_rle_count <- type_stats(data=cst, metric = RLE, rlm = "Coastal")
cst_rle_ext <- extent_stats(data=cst, metric = RLE, rlm = "Coastal")

# EPL: coast
cst_epl_count <- type_stats(data=cst, metric = EPL, rlm = "Coastal")
cst_epl_ext <- extent_stats(data=cst, metric = EPL, rlm = "Coastal")

# RLE: non-coastal
ncst_rle_count <- type_stats(data=non_cst, metric = RLE, rlm = "Non-coastal")
ncst_rle_ext <- extent_stats(data=non_cst, metric = RLE, rlm = "Non-coastal")

# EPL: non-coastal
ncst_epl_count <- type_stats(data=non_cst, metric = EPL, rlm = "Non-coastal")
ncst_epl_ext <- extent_stats(data=non_cst, metric = EPL, rlm = "Non-coastal")


### (2) Join coastal and non-coastal tables of RLE count together ------------------
rle_count_cnc <- bind_rows(cst_rle_count, ncst_rle_count) %>%
  rename(`Critically Endangered` = CR, 
          Endangered = EN, 
          Vulnerable = VU, 
          `Near Threatened` = NT,
          `Least Concern` = LC ) %>%
  select(`Critically Endangered`,
         Endangered,
         Vulnerable,
         `Near Threatened`,
         `Least Concern`, realm) %>%
  mutate(realm = factor(realm, levels = c("Non-coastal", "Coastal"))) %>%
  arrange(realm)

# > save output: cst-noncst_rle_n.csv ----
write_csv(rle_count_cnc, here("outputs/cst-noncst_rle-n.csv"))


### (3) Join coastal and non-coastal tables of RLE extent together -----------------
rle_ext_cnc <- bind_rows(cst_rle_ext, ncst_rle_ext) %>%
  rename(`Critically Endangered` = CR, 
         Endangered = EN, 
         Vulnerable = VU, 
         `Near Threatened` = NT,
         `Least Concern` = LC ) %>%
  select(`Critically Endangered`,
         Endangered,
         Vulnerable,
         `Near Threatened`,
         `Least Concern`, realm) %>%
  mutate(realm = factor(realm, levels = c("Non-coastal", "Coastal"))) %>%
  arrange(realm)

# > save output: cst-noncst_rle-e.csv ----
write_csv(rle_ext_cnc, here("outputs/cst-noncst_rle-e.csv"))


### (4) Join coastal and non-coastal tables of EPL count together ------------------
epl_count_cnc <- bind_rows(cst_epl_count, ncst_epl_count) %>%
  rename(`Well Protected` = WP, 
         `Moderately Protected` = MP, 
         `Poorly Protected` = PP, 
         `Not Protected` = NP) %>%
  select(`Well Protected`,
         `Moderately Protected`,
         `Poorly Protected`,
         `Not Protected`, realm) %>%
  mutate(realm = factor(realm, levels = c("Non-coastal", "Coastal"))) %>%
  arrange(realm)

# > save output: cst-noncst_epl-n.csv ----
write_csv(epl_count_cnc, here("outputs/cst-noncst_epl-n.csv"))


### (5) Join coastal and non-coastal tables of EPL extent together -----------------
epl_ext_cnc <- bind_rows(cst_epl_ext, ncst_epl_ext) %>%
  rename(`Well Protected` = WP, 
         `Moderately Protected` = MP, 
         `Poorly Protected` = PP, 
         `Not Protected` = NP) %>%
  select(`Well Protected`,
         `Moderately Protected`,
         `Poorly Protected`,
         `Not Protected`, realm) %>%
  mutate(realm = factor(realm, levels = c("Non-coastal", "Coastal"))) %>%
  arrange(realm)

# > save output: cst-noncst_epl-e.csv ----
write_csv(epl_ext_cnc, here("outputs/cst-noncst_epl-e.csv"))

rm(cst, cst_rle_count, cst_rle_ext, cst_epl_count, cst_epl_ext)

## 5. CREATE DATASETS OF ECOSYSTEM TYPES AT RISK ##################################
### (1) Coastal unluckies (CR/EN and NP/PP) -------------------------------------------
# coastal ecosystem types that are CR or EN and NP
unlucky_tbl <- coast_data %>% 
  filter(epl %in% c("NP", "PP") & 
         ets %in% c("CR", "EN")) %>% 
  select(realm, ets, epl, ecotype) %>%
  mutate(realm = str_remove(realm, "Coastal ")) %>% 
  rename(Realm = realm,
         `Threat status` = ets,
         `Protection level` = epl,
         `Ecosystem type` = ecotype) %>% 
  mutate(Realm = fct_relevel(Realm, "Terrestrial",
                             "Estuarine",
                             "Marine"),
         `Threat status` = fct_relevel(`Threat status`, "CR", "EN"),
         `Protection level` = fct_relevel(`Protection level`, "NP", "PP")) %>% 
  arrange(Realm, `Threat status`, `Protection level`)

# > save output: cst_unlucky-CR-EN-NP.csv ----
write_csv(unlucky_tbl, here("outputs/cst_unlucky-CR-EN-NP-PP.csv"))


### (2) Coastal riskies (ETS x EPL) ------------------------------------------------
# cross-tabulation of ets and epls ... didn't work in the nba_tbl_comb
# "riskies" are threatened and underprotected, so CR,EN,VU & NP,PP,MP

# create pivot table of ets by epl
ets_x_epl <- coast_data %>%
  count(ets, epl) %>%                     # count combinations of ets and epl
  mutate(ets = recode(ets,
                      "CR" = "Critically Endangered",
                      "EN" = "Endangered",
                      "VU" = "Vulnerable",
                      "NT" = "Near Threatened",
                      "LC" = "Least Concern")) %>% 
  pivot_wider(names_from = epl,                    # epl values become columns
              values_from = n,                     # counts go in the cells
              values_fill = 0) %>%                 # fill missing combos with 0
  rename(THR = ets) %>% 
  select(all_of("THR"), rev(setdiff(names(.), "THR"))) %>% #order cols from NP-WP
  mutate(`Total (n)` = rowSums(across(c(WP, MP, PP, NP)), na.rm = TRUE)) %>% 
  rename(`Well Protected`= WP,
         `Moderately Protected`= MP,
         `Poorly Protected`=PP,
         `Not Protected`=NP)

# create a total row for the end
total_row <- ets_x_epl %>%
  summarise(across(-all_of("THR"), ~ sum(.x, na.rm = TRUE))) %>%
  mutate(!!"THR" := "Total (n)") %>%
  select(all_of(names(ets_x_epl)))  # keep column order

# combine pivot table with total row 
ets_x_epl <- bind_rows(ets_x_epl, total_row) 

# > save output: cst_ets-x-epl.csv ----
write_csv(ets_x_epl, here("outputs/cst_ets-x-epl.csv"))

rm(total_row)


## 6. SUMMARY ETS STATS PER REALM AND SUBREALM ###################################
### (1) Number of types -----------------------------------------------------------
# First create a pivot table to summarise the data by zone (subrealm)
pivot_tbl_ets <- coast_data %>% 
  select(realm, 
         zone, 
         ecotype, 
         ets)%>%
  mutate(realm = fct_relevel(realm, 
                             "Coastal Terrestrial", 
                             "Estuarine", 
                             "Coastal Marine")) %>% 
  mutate(ets = fct_relevel(ets, 
                           "CR", 
                           "EN", 
                           "VU",
                           "NT",
                           "LC")) %>%
  mutate(zone = fct_relevel(zone, 
                           "Semi-coastal vegetation", 
                           "Coastal vegetation", 
                           "Estuaries",
                           "Shore",
                           "Inner shelf and river influenced")) %>%
  group_by(realm, 
           zone, 
           ets) %>%
  summarise(count = n(), .groups = "drop") %>%
  pivot_wider(names_from = ets,
              values_from = count,
              values_fill = 0)

# Calculate subtotals per realm
subtotals_ets <- pivot_tbl_ets %>%
  group_by(realm) %>%
  summarise(across(where(is.numeric), sum), .groups = "drop") %>%
  mutate(zone = "Total")

# Combine and order (subtotals at top)
combined_ets <- bind_rows(pivot_tbl_ets, subtotals_ets) %>%
  arrange(realm, desc(zone == "Total"))

# Add grand total row
grand_total_row_ets <- subtotals_ets %>%
  summarise(across(where(is.numeric), sum)) %>%
  mutate(realm = "Coastal (overall)",
         zone = "Total",
         Total = rowSums(across(c(CR, EN, LC, VU, NT))))

# Add grand total column
combined_ets <- combined_ets %>%
  mutate(Total = rowSums(across(where(is.numeric))))

# Add grand total row to final table
final_tbl_ets <- bind_rows(combined_ets, grand_total_row_ets) %>% 
 # select(!`NA`) %>% 
  rename(Realm=realm,
         `Sub-realm` = zone) 

# Prep non-coastal row for table
ncst_ets_subr_tbl <- ncst_rle_count %>% 
  mutate(Realm = "Non-coastal",
         `Sub-realm` = "Total",
         Thr = rowSums(across(c(CR, EN, VU)), na.rm = TRUE),
         Total = rowSums(across(c(CR, EN, VU, NT, LC)), na.rm = TRUE)) %>% 
  select(Realm, `Sub-realm`, Thr, CR, EN, VU, NT, LC, Total)

# Add that in and add a threatened column
subrealm_ets_n <- final_tbl_ets %>% 
  mutate(Thr = rowSums(across(c(CR, EN, VU)), na.rm = TRUE))%>%
  select(Realm, `Sub-realm`, Thr, CR, EN, VU, NT, LC, Total) %>% 
  bind_rows(., ncst_ets_subr_tbl) %>%
  filter(`Sub-realm` != "Estuaries")

# > save output: cst_subrealm-ets-n.csv ----
write_csv(subrealm_ets_n, here("outputs/cst_subrealm-ets-n.csv"))

rm(pivot_tbl_ets, final_tbl_ets, subtotals_ets, ncst_ets_subr_tbl, 
   combined_ets, grand_total_row_ets)


### (2) Proportion of types -------------------------------------------------------
# Update the subrealm counts table by dividing by the number of ecosystem types
# per realm or subrealm
subrealm_ets_p <- subrealm_ets_n %>%
  mutate(across(c(Thr, CR, EN, VU, NT, LC, Total), ~ round((. / Total) * 100, 1)))

# > save output: cst_subrealm-ets-p.csv ----
write_csv(subrealm_ets_p, here("outputs/cst_subrealm-ets-p.csv"))


### (3) Proportion of extent ------------------------------------------------------
# Pivot table the ets data by subrealm
pivot_tbl_ets_e <- coast_data %>% 
  select(realm, 
         zone, 
         ecotype, 
         ets,
         extent) %>%
  mutate(realm = fct_relevel(realm, 
                             "Coastal Terrestrial", 
                             "Estuarine", 
                             "Coastal Marine")) %>% 
  mutate(ets = fct_relevel(ets, 
                           "CR", 
                           "EN", 
                           "VU",
                           "NT",
                           "LC")) %>%
  mutate(zone = fct_relevel(zone, 
                            "Semi-coastal vegetation", 
                            "Coastal vegetation", 
                            "Estuaries",
                            "Shore",
                            "Inner shelf and river influenced")) %>%
  group_by(realm, zone, ets) %>%
  summarise(extent = sum(extent, na.rm = TRUE), .groups = "drop") %>%   # sum extents
  pivot_wider(names_from = ets,
              values_from = extent,
              values_fill = 0)

# Calculate subtotals per realm
subtotals_ets_e <- pivot_tbl_ets_e %>%
  group_by(realm) %>%
  summarise(across(where(is.numeric), sum), .groups = "drop") %>%
  mutate(zone = "Total")

# Combine and order (subtotals at top)
combined_ets_e <- bind_rows(pivot_tbl_ets_e, subtotals_ets_e) %>%
  arrange(realm, desc(zone == "Total"))

# Add grand total row
grand_total_row_ets_e <- subtotals_ets_e %>%
  summarise(across(where(is.numeric), sum)) %>%
  mutate(realm = "Coastal (overall)",
         zone = "Total",
         Total = rowSums(across(c(CR, EN, LC, VU, NT))))

# Add grand total column
combined_ets_e <- combined_ets_e %>%
  mutate(Total = rowSums(across(where(is.numeric))))

# Add grand total row to final table
final_tbl_ets_e <- bind_rows(combined_ets_e, grand_total_row_ets_e) %>% 
  rename(Realm = realm,
         `Sub-realm` = zone) 

# Prep non-coastal row for table
ncst_ets_subr_tbl_e <- ncst_rle_ext %>% 
  mutate(Realm = "Non-coastal",
         `Sub-realm` = "Total",
         Thr = rowSums(across(c(CR, EN, VU)), na.rm = TRUE),
         Total = rowSums(across(c(CR, EN, VU, NT, LC)), na.rm = TRUE)) %>% 
  select(Realm, `Sub-realm`, Thr, CR, EN, VU, NT, LC, Total)

# Add that in and add a threatened column
subrealm_ets_e_a <- final_tbl_ets_e %>% 
  mutate(Thr = rowSums(across(c(CR, EN, VU)), na.rm = TRUE)) %>%
  select(Realm, `Sub-realm`, Thr, CR, EN, VU, NT, LC, Total) %>% 
  bind_rows(., ncst_ets_subr_tbl_e) %>%
  filter(`Sub-realm` != "Estuaries")

# > save output: cst_subrealm-ets-e_areas.csv ----
write_csv(subrealm_ets_e_a, here("outputs/cst_subrealm-ets-e_areas.csv"))

# Work the actual areas out as a proportion of the realm or subrealm
subrealm_ets_e_p <- subrealm_ets_e_a %>%
  mutate(across(c(Thr, CR, EN, VU, NT, LC, Total), ~ round((. / Total) * 100, 1)))

# > save output: cst_subrealm-ets-e_proportions.csv ----
write_csv(subrealm_ets_e_p, here("outputs/cst_subrealm-ets-e_proportions.csv"))

rm(pivot_tbl_ets_e, grand_total_row_ets_e, final_tbl_ets_e, 
   ncst_ets_subr_tbl_e, combined_ets_e, subtotals_ets_e)


## 7. SUMMARY EPL STATS PER REALM AND SUBREALM ###################################
### (1) Number of types -----------------------------------------------------------
# First create a pivot table to summarise the data by zone (subrealm)
pivot_tbl_epl <- coast_data %>% 
  select(realm, 
         zone, 
         ecotype, 
         epl)%>%
  mutate(realm = fct_relevel(realm, 
                             "Coastal Terrestrial", 
                             "Estuarine", 
                             "Coastal Marine")) %>% 
  mutate(ets = fct_relevel(epl, 
                           "WP", 
                           "MP", 
                           "PP",
                           "NP")) %>%
  mutate(zone = fct_relevel(zone, 
                            "Semi-coastal vegetation", 
                            "Coastal vegetation", 
                            "Estuaries",
                            "Shore",
                            "Inner shelf and river influenced")) %>%
  group_by(realm, 
           zone, 
           epl) %>%
  summarise(count = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = epl,
    values_from = count,
    values_fill = 0
  )

# Calculate subtotals per realm
subtotals_epl <- pivot_tbl_epl %>%
  group_by(realm) %>%
  summarise(across(where(is.numeric), sum), .groups = "drop") %>%
  mutate(zone = "Total")

# Combine and order (realm totals at top, then subrealms, per realm)
combined_epl <- bind_rows(pivot_tbl_epl, subtotals_epl) %>%
  arrange(realm, desc(zone == "Total"))

# Add grand total row
grand_total_row_epl <- subtotals_epl %>%
  summarise(across(where(is.numeric), sum)) %>%
  mutate(realm = "Coastal (overall)",
         zone = "Total",
         Total = rowSums(across(c(WP, MP, PP, NP))))

# Add grand total column
combined_epl <- combined_epl %>%
  mutate(Total = rowSums(across(where(is.numeric))))

# Add grand total row to final table
final_tbl_epl <- bind_rows(combined_epl, grand_total_row_epl) %>%
  rename(Realm=realm,
         `Sub-realm` = zone) 

# Prep non-coastal row for table
ncst_epl_subr_tbl <- ncst_epl_count %>% 
  mutate(Realm = "Non-coastal",
         `Sub-realm` = "Total",
         Total = rowSums(across(c(WP, MP, PP, NP)))) %>% 
  select(Realm, `Sub-realm`, WP, MP, PP, NP, Total)

# Add that in and add a threatened column
subrealm_epl_n <- final_tbl_epl %>% 
  select(Realm, `Sub-realm`, WP, MP, PP, NP, Total) %>% 
  bind_rows(., ncst_epl_subr_tbl) %>%
  filter(`Sub-realm` != "Estuaries")

# > save output: cst_subrealm-epl-n.csv ----
write_csv(subrealm_epl_n, here("outputs/cst_subrealm-epl-n.csv"))

rm(pivot_tbl_epl, combined_epl, final_tbl_epl, ncst_epl_subr_tbl,
   grand_total_row_epl, subtotals_epl)


### (2) Proportion of types -------------------------------------------------------
# Update the subrealm counts table by dividing by the number of ecosystem types
# per realm or subrealm
subrealm_epl_p <- subrealm_epl_n %>%
  mutate(across(c(WP, MP, PP, NP, Total), ~ round((. / Total) * 100, 1)))

# > save output: cst_subrealm-epl-p.csv ----
write_csv(subrealm_epl_p, here("outputs/cst_subrealm-epl-p.csv"))


### (3) Proportion of extent ------------------------------------------------------
# Pivot table the ets data by subrealm
pivot_tbl_epl_e <- coast_data %>% 
  select(realm, 
         zone, 
         ecotype, 
         epl,
         extent) %>%
  mutate(realm = fct_relevel(realm, 
                             "Coastal Terrestrial", 
                             "Estuarine", 
                             "Coastal Marine")) %>% 
  mutate(epl = fct_relevel(epl, 
                           "WP", 
                           "MP", 
                           "PP",
                           "NP")) %>%
  mutate(zone = fct_relevel(zone, 
                            "Semi-coastal vegetation", 
                            "Coastal vegetation", 
                            "Estuaries",
                            "Shore",
                            "Inner shelf and river influenced")) %>%
  group_by(realm, zone, epl) %>%
  summarise(extent = sum(extent, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = epl,
              values_from = extent,
              values_fill = 0)

# Calculate subtotals per realm
subtotals_epl_e <- pivot_tbl_epl_e %>%
  group_by(realm) %>%
  summarise(across(where(is.numeric), sum), .groups = "drop") %>%
  mutate(zone = "Total")

# Combine and order (subtotals at top)
combined_epl_e <- bind_rows(pivot_tbl_epl_e, subtotals_epl_e) %>%
  arrange(realm, desc(zone == "Total"))

# Add grand total row
grand_total_row_epl_e <- subtotals_epl_e %>%
  summarise(across(where(is.numeric), sum)) %>%
  mutate(realm = "Coastal (overall)",
         zone = "Total",
         Total = rowSums(across(c(WP, MP, PP, NP))))

# Add grand total column
combined_epl_e <- combined_epl_e %>%
  mutate(Total = rowSums(across(where(is.numeric))))

# Add grand total row to final table
final_tbl_epl_e <- bind_rows(combined_epl_e, grand_total_row_epl_e) %>% 
  rename(Realm = realm,
         `Sub-realm` = zone) 

# Prep non-coastal row for table
ncst_epl_subr_tbl_e <- ncst_epl_ext %>% 
  mutate(Realm = "Non-coastal",
         `Sub-realm` = "Total",
         Total = rowSums(across(c(WP, MP, PP, NP)))) %>% 
  select(Realm, `Sub-realm`, WP, MP, PP, NP, Total)

# Add that in and add a threatened column
subrealm_epl_e_a <- final_tbl_epl_e %>% 
  select(Realm, `Sub-realm`, WP, MP, PP, NP, Total) %>% 
  bind_rows(., ncst_epl_subr_tbl_e) %>%
  filter(`Sub-realm` != "Estuaries")

# > save output: cst_subrealm-epl-e_areas.csv ----
write_csv(subrealm_epl_e_a, here("outputs/cst_subrealm-epl-e_areas.csv"))

# Work the actual areas out as a proportion of the realm or subrealm
subrealm_epl_e_p <- subrealm_epl_e_a %>%
  mutate(across(c(WP, MP, PP, NP, Total), ~ round((. / Total) * 100, 1)))

# > save output: cst_subrealm-epl-e_proportions.csv ----
write_csv(subrealm_epl_e_p, here("outputs/cst_subrealm-epl-e_proportions.csv"))

rm(pivot_tbl_epl_e, subtotals_epl_e, grand_total_row_epl_e, combined_epl_e,
   ncst_epl_subr_tbl_e, final_tbl_epl_e)

# remove functions
rm(extent_stats, type_stats)


#save.image(here("outputs/cst_data-wrangling.RData"))









# Delete this - just keeping in case need the plot text
# ### PLOT DATA ------------------------------------------------------------------
# ## Coastal vs non-coastal
# 
# epl_t<-nba_plot(epl_count_cnc,
#                 GROUPS=`realm`,
#                 COLS = 1:4,
#                 CHRT = "bar",
#                 NUM = FALSE,
#                 LAB = "Percentage of ecosystem types",
#                 SAVE=NULL)
# epl_e<-nba_plot(epl_ext_cnc,
#                 GROUPS=realm,
#                 COLS = 1:4,
#                 CHRT = "bar",
#                 NUM = FALSE,
#                 LAB = "Percentage of ecosystem extent",
#                 SAVE=NULL)
# ggarrange(epl_t, epl_e, ncol = 2, labels = c("a","b"), common.legend = T, legend = "bottom")
# 
# ets_t<-nba_plot(rle_count_cnc,
#                 GROUPS=`realm`,
#                 COLS = 1:5,
#                 CHRT = "bar",
#                 NUM = FALSE,
#                 LAB = "Percentage of ecosystem types",
#                 SAVE=NULL)
# ets_e<-nba_plot(rle_ext_cnc,
#                 GROUPS=realm,
#                 COLS = 1:5,
#                 CHRT = "bar",
#                 NUM = FALSE,
#                 LAB = "Percentage of ecosystem extent",
#                 SAVE=NULL)
# ggarrange(ets_t, ets_e, ncol = 2, labels = c("a","b"), common.legend = T, legend = "bottom")
