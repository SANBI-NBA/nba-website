######### SCRIPT TO CALCULATE COAST ASSESSMENT STATISTICS FOR THE NBA ##########
######### By Linda R. Harris

## 1. LOAD LIBRARIES ###########################################################
#devtools::install_github("SANBI-NBA/nbaR")
library(nbaR)
library(english)
library(here)
library(readr)
library(dplyr)

## 2. SET UP THE DATA ##########################################################
### (1) Read in the coast data -------------------------------------------------

#### To recalculate all the values
source(here("scripts/cst_data-wrangling.R"))

#### To load the values already calculated
#load(here("outputs/cst_data-wrangling.RData"))

## 3. SET VALUES FOR COAST ECOSYSTEM STATISTICS ################################
### (1) Number of types ---------------------------------------------------------
#### (a) Number of coastal ecosystem types ----
cet_n <-  as.numeric(nrow(coast_data))

#### (b) Number of non-coastal ecosystem types ----
ncet_n <-  as.numeric(nrow(non_cst))

#### (c) Number of ecosystem types per realm ----
# r (realm) = "T", "E", "M"
ret_n <- function(r){
  sum(coast_data$rlm == r, na.rm = TRUE)
}

#### (d) Number of ecosystem types per subrealm ----
# sr (subrealm) = "Semi-coastal vegetation", "Coastal vegetation", "Estuaries", 
#                 "Shore", "Inner shelf and river influenced"
sret_n <- function(sr){
  sum(coast_data$zone == sr, na.rm = TRUE)
}

#### (e) Number of coastal ecosystem types that are threatened ----
cet_thr_n <-  sum(coast_data$threatened)

#### (f) Number of non-coastal ecosystem types that are threatened ----
ncet_thr_n <- ncst_rle_count %>%
  select(-LC, -NT, -realm) %>%
  rowSums()

#### (g) Number of coastal ecosystem types per realm that are threatened ----
# r (realm) = "T", "E", "M"
ret_thr_n <- function(r){
  n = coast_data %>% 
    filter(rlm == r) 
    sum(n$threatened)
}

#### (h) Number of coast ecosystem types in an ets category ----
# ets_cat = "CR", "EN", "VU", "NT", "LC"
c_ets_n <- function(ets_cat){
  sum(coast_data$ets == ets_cat, na.rm = TRUE)
}

#### (i) Number of ecosystem types in an ets category per realm ----
# ets_cat = "CR", "EN", "VU", "NT", "LC"
# r (realm) = "T", "E", "M"
r_ets_n <- function(r, ets_cat){
  coast_data %>% 
    filter(rlm == r, ets == ets_cat) %>% 
    count() %>% 
    as.numeric()
}

#### (j) Number of ecosystem types in an ets category per subrealm ----
# ets_cat = "CR", "EN", "VU", "NT", "LC"
# sr (subrealm) = "Semi-coastal vegetation", "Coastal vegetation", "Estuaries", 
#                 "Shore", "Inner shelf and river influenced"
sr_ets_n <- function(sr, ets_cat){
  coast_data %>% 
    filter(zone == sr, ets == ets_cat) %>% 
    count() %>% 
    as.numeric()
}

#### (k) Number of coast ecosystem types in an epl category ----
# epl_cat = "WP", "MP", "PP", "NP"
c_epl_n <- function(epl_cat){
  sum(coast_data$epl == epl_cat, na.rm = TRUE)
}

#### (l) Number of coast ecosystem types that are protected ----
c_prot_n <- as.numeric(sum(coast_data$epl %in% 
                             c("WP", "MP", "PP"), na.rm = TRUE))

#### (m) Number of non-coastal ecosystem types that are protected ----
nc_prot_n <- ncst_epl_count %>%
  select(-NP, -realm) %>%
  rowSums()

#### (n) Number of ecosystem types in an epl category per realm ----
# epl_cat = "WP", "MP", "PP", "NP"
# r (realm) = "T", "E", "M"
r_epl_n <- function(r, epl_cat){
  coast_data %>% 
    filter(rlm == r, epl == epl_cat) %>% 
    count() %>% 
    as.numeric()
}

#### (o) Number of ecosystem types that are CR/EN and NP (unluckies) ----
unlucky_n <- coast_data %>% 
  filter(ets %in% c("EN", "CR") & 
           epl %in% c("NP", "PP")) %>% 
  count() %>% 
  as.numeric()

#### (p) Number of ecosystem types that are CR/EN and NP (unluckies) per realm ----
# r (realm) = "T", "E", "M"
r_unlucky_n <- function(r){
  coast_data %>% 
    filter(ets %in% c("EN", "CR") & 
             epl %in% c("NP", "PP") &
             rlm == r) %>% 
    count() %>% 
    as.numeric()
}

#### (q) Number of ecosystem types that are threatened and under-protected ----
risky_n <- coast_data %>% 
  filter(ets %in% c("VU", "EN", "CR") & 
           epl %in% c("MP", "PP", "NP")) %>% 
  count() %>% 
  as.numeric()

#### (r) Number of ecosystem types total (Terrestrial, Estuarine, Marine) ----
# Values taken from the IEM - excluding microestuaries, and including pelagic types
all_et <- 463+ #terrestrial
          22+ #estuaries
          163+ #marine
          222+ #rivers
          82   #wetland

### (2) Percentage of types -----------------------------------------------------
#### (a) Percentage of ecosystem types ----
# types = any of the above number of types
# tot_types = either total number of coastal types or realm types
percent_types <- function(types, tot_types){
  round(types/tot_types*100, digits = 0)
}

### (b) Percentage extent ----------------------------------------------------------
# indicator = ets or epl (not in quotation marks)
# cat = "CR", "EN", VU, "NT", "LC", "thr", "WP", "MP", "NP", "prot"
# grp = "T", "E", "M", "coast"
percent_extent <- function(indicator, cat, grp){
  if (grp == "coast") {
    # Code A
    coast_data2 <- coast_data
    
    tot_ext <- coast_data2 %>%
      summarise(total_extent = sum(extent, na.rm = TRUE)) %>% 
      as.numeric()
  } else {
    coast_data2 <- coast_data %>%
      filter(rlm == grp)
    
    tot_ext <- coast_data2 %>% 
      summarise(total_extent = sum(extent, na.rm = TRUE)) %>% 
      as.numeric()
  }
  
  if (cat == "thr") {
    ext <- coast_data2 %>%
      filter(ets %in% c("CR", "EN", "VU")) %>%
      summarise(total_extent = sum(extent, na.rm = TRUE)) %>% 
      as.numeric()
  } else if (cat == "prot") {
    ext <- coast_data2 %>%
      filter(epl %in% c("WP", "MP", "PP")) %>%
      summarise(total_extent = sum(extent, na.rm = TRUE)) %>% 
      as.numeric()
  } else {
    ext <- coast_data2 %>%
      filter({{indicator}} == cat) %>%
      summarise(total_extent = sum(extent, na.rm = TRUE)) %>% 
      as.numeric()
  }
  
  round(ext/tot_ext*100, digits = 0)
}

### (3) Comparisons with non-coastal -------------------------------------------
#### (a) Times proportionately more threatened ecosystem types ----
c_nc_prop_thr_n <- round((cet_thr_n/cet_n)/(ncet_thr_n/ncet_n))

#### (b) Times proportionately more threatened area ----
# extent of non-coastal area
nc_e <- non_cst %>%
  summarise(total_extent = sum(extent, na.rm = TRUE)) %>% 
  as.numeric()

# extent of non-coastal area that's threatened
nc_thr_e <- non_cst %>%
  filter(RLE %in% c("CR", "EN", "VU")) %>%
  summarise(total_extent = sum(extent, na.rm = TRUE)) %>% 
  as.numeric()

# times proportionately more threated area in the coastal zone
c_nc_prop_thr_e <- round(percent_extent(ets, "thr", "coast")/(nc_thr_e/nc_e*100))


## 4. IN TEXT ##################################################################
### (1) Rewrite numbers as words in title case ---------------------------------

Astext <- function (number){
  tools::toTitleCase(as.character(english(number)))
  }


### (2) Rewrite numbers as words in lower case ---------------------------------
astext <- function (number){
  as.character(english(number))
}

## 5. INDIVIDUAL VALUES ########################################################
# Proportion of the extent of semi-coastal vegetation that's CR
SCV_CR_E_P <- subrealm_ets_e_p %>%
  filter(`Sub-realm` == "Semi-coastal vegetation") %>%
  pull(CR)

CV_CR_E_P <- subrealm_ets_e_p %>%
  filter(`Sub-realm` == "Coastal vegetation") %>%
  pull(CR)

