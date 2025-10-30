########### BACKGROUND FUNCTIONS FOR SCRIPTS IN THE COAST ASSESSMENT ###########
########### By Linda R. Harris

## 1. FUNCTIONS ################################################################
### (1) Function to summarise counts of ecosystem types per RLE/EPL category ----

# data = dataset to summarise
# metric = RLE or EPL
# rlm = the realm name to be coded (e.g., "Coastal")

type_stats <- function(data, metric, rlm) {
  data %>%
    count({{ metric }}) %>%
    pivot_wider(names_from = {{ metric }}, values_from = n, values_fill = 0) %>%
    mutate(realm = rlm)
}


### (2) Function to summarise extent of ecosystem types per RLE/EPL category ----

# data = dataset to summarise
# metric = RLE or EPL
# rlm = the realm name to be coded (e.g., "Coastal")

extent_stats <- function(data, metric, rlm) {
  data %>%
    group_by({{ metric }}) %>%
    summarise(tot_extent = sum(extent, na.rm = TRUE)) %>%
    pivot_wider(names_from = {{ metric }}, values_from = tot_extent, 
                values_fill = 0) %>%
    mutate(realm = rlm)
}