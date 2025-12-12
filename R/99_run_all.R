############################################################
# REGIONAL UK CLUSTERING (BUSINESS DEMOGRAPHY + EMPLOYMENT)
# Adjusted version for:
#  - businessdemographyexceltables2023.xlsx (ONS)
#  - nomis_2025_12_04_201028.xlsx (BRES, sheet "Data" in wide layout)
############################################################

# ---------------------------
# 1. Packages
# ---------------------------
packages <- c("readxl", "dplyr", "tidyr", "ggplot2",
              "cluster", "factoextra", "stringr", "readr", "tibble")

installed <- rownames(installed.packages())
to_install <- packages[!packages %in% installed]
if (length(to_install) > 0) {
  install.packages(to_install)
}

suppressPackageStartupMessages({
  lapply(packages, library, character.only = TRUE)
})

# ---------------------------
# 2. File paths (adjust if needed)
# ---------------------------
biz_file  <- "C:/Users/user/Documents/MSC Data Science Hub/IJC437 Introduction to Data Science/Report/businessdemographyexceltables2023.xlsx"
bres_file <- "C:/Users/user/Documents/MSC Data Science Hub/IJC437 Introduction to Data Science/Report/nomis_2025_12_04_201028.xlsx"

# ---------------------------
# 3. General helpers
# ---------------------------

# Normalize region names (uppercase, without extra spaces)
clean_region <- function(x) {
  stringr::str_trim(stringr::str_to_upper(as.character(x)))
}

# List of NUTS1 regions to keep (we use "EAST" as in ONS/BRES)
nuts1_regions <- clean_region(c(
  "North East",
  "North West",
  "Yorkshire and The Humber",
  "East Midlands",
  "West Midlands",
  "East",
  "London",
  "South East",
  "South West",
  "Wales",
  "Scotland",
  "Northern Ireland"
))

# ---------------------------
# 4. Tables type 1.1a / 2.1a / 3.1a (one year per sheet)
# ---------------------------
# Expected structure (e.g. Table 1.1a):
#  - Row with: [code, NA, year] (e.g. NA in col2, 2018 in col3)
#  - Following rows: [code, region/local authority name, value]

read_ons_single_year_table <- function(file,
                                       sheet,
                                       region_col = 2,
                                       value_col  = 3,
                                       year_min   = 2018,
                                       year_max   = 2023) {
  message("Reading sheet: ", sheet)
  
  # Read without names to avoid getting confused with multirow headers
  df_raw <- readxl::read_excel(file, sheet = sheet, col_names = FALSE)
  
  # Remove completely empty rows
  df_raw <- df_raw %>%
    dplyr::filter(dplyr::if_any(dplyr::everything(), ~ !is.na(.)))
  
  n <- nrow(df_raw)
  
  # Detect row with the year in column value_col and region_col == NA
  year_candidates <- tibble::tibble(
    row_id     = seq_len(n),
    region_val = df_raw[[region_col]],
    value_val  = df_raw[[value_col]]
  ) %>%
    dplyr::mutate(
      year_num = suppressWarnings(as.numeric(value_val))
    ) %>%
    dplyr::filter(
      is.na(region_val),
      !is.na(year_num),
      year_num >= year_min,
      year_num <= year_max
    )
  
  if (nrow(year_candidates) == 0) {
    stop(
      "Could not detect a row with the year (", year_min, "-", year_max,
      ") in sheet '", sheet,
      "'. Check with read_excel('", file,
      "', sheet = '", sheet, "', col_names = FALSE, n_max = 20)."
    )
  }
  
  header_row <- year_candidates$row_id[1]
  year_val   <- year_candidates$year_num[1]
  
  if (header_row >= n) {
    stop("The row with the year is the last one; there are no data below in '", sheet, "'.")
  }
  
  # Data rows below the year row
  body <- df_raw[(header_row + 1):n, , drop = FALSE]
  
  region_vec <- clean_region(body[[region_col]])
  value_vec  <- suppressWarnings(as.numeric(body[[value_col]]))
  
  df <- tibble::tibble(
    region = region_vec,
    year   = year_val,
    value  = value_vec
  ) %>%
    dplyr::filter(
      !is.na(region),
      region %in% nuts1_regions,
      !is.na(value)
    )
  
  return(df)
}

# ---------------------------
# 5. 3-year survival (Table 4.1)
# ---------------------------

read_survival_3y <- function(file,
                             sheet = "Table 4.1",
                             skip = 4) {
  message("Reading survival (", sheet, ")")
  
  surv_raw <- readxl::read_excel(file, sheet = sheet, skip = skip)
  
  # Safe column names
  cn <- names(surv_raw)
  cn[is.na(cn) | cn == ""] <- paste0("V", which(is.na(cn) | cn == ""))
  names(surv_raw) <- cn
  
  # First column = region name (even if it is called "2018")
  names(surv_raw)[1] <- "region_raw"
  
  if (ncol(surv_raw) < 8) {
    stop("The sheet '", sheet, "' has fewer than 8 columns; check the structure.")
  }
  
  # Column 8 = % 3-year survival of the 2018 cohort in the first block
  surv_raw$region      <- clean_region(surv_raw$region_raw)
  surv_raw$surv_3y_raw <- surv_raw[[8]]
  
  surv <- surv_raw %>%
    dplyr::filter(region %in% nuts1_regions) %>%
    dplyr::mutate(
      survival_3y_reg = suppressWarnings(
        readr::parse_number(as.character(surv_3y_raw))
      )
    ) %>%
    dplyr::filter(!is.na(survival_3y_reg)) %>%
    dplyr::group_by(region) %>%
    dplyr::slice(1) %>%   # first occurrence = 2018 cohort
    dplyr::ungroup() %>%
    dplyr::select(region, survival_3y_reg)
  
  # Convert from % to proportion if it comes as percentage
  surv$survival_3y_reg[surv$survival_3y_reg > 1] <-
    surv$survival_3y_reg[surv$survival_3y_reg > 1] / 100
  
  return(surv)
}

# ---------------------------
# 6. Active employer enterprises 2023 (Table 6.1)
# ---------------------------

read_employer_active_2023 <- function(file,
                                      sheet = "Table 6.1",
                                      skip = 4,
                                      region_col = 2) {  # region in the 2nd column according to tmp6
  message("Reading employer enterprises (", sheet, 
          "), region_col = ", region_col)
  
  tab6_raw <- readxl::read_excel(file, sheet = sheet, skip = skip)
  
  cn <- names(tab6_raw)
  cn[is.na(cn) | cn == ""] <- paste0("V", which(is.na(cn) | cn == ""))
  names(tab6_raw) <- cn
  
  if (region_col > ncol(tab6_raw)) {
    stop("region_col out of range in ", sheet,
         ". It has ", ncol(tab6_raw), " columns.")
  }
  names(tab6_raw)[region_col] <- "region"
  
  tab6 <- tab6_raw %>%
    dplyr::filter(!is.na(region)) %>%
    dplyr::mutate(region = clean_region(region)) %>%
    dplyr::filter(region %in% nuts1_regions)
  
  # Column "Active" = active employer enterprises 2023
  active_col <- which(names(tab6) == "Active")
  if (length(active_col) == 0) {
    stop("Could not find a column called 'Active' in ", sheet,
         ". Current names: ", paste(names(tab6), collapse = ", "))
  }
  
  tab6$employer_active_2023_reg <- suppressWarnings(
    readr::parse_number(as.character(tab6[[active_col]]))
  )
  
  tab6 <- tab6 %>%
    dplyr::filter(!is.na(employer_active_2023_reg)) %>%
    dplyr::select(region, employer_active_2023_reg)
  
  return(tab6)
}

# ---------------------------
# 7. BRES employment 2018–2022 (Nomis wide layout)
# ---------------------------

# Normalize BRES region -> same convention as nuts1_regions
normalize_bres_region <- function(x) {
  clean_region(x)
}

read_bres_employment_wide <- function(file,
                                      sheet    = "Data",
                                      year_min = 2018,
                                      year_max = 2022) {
  message("Reading BRES employment (wide layout) from sheet: ", sheet)
  
  # Read without names to handle "weird" headers
  df_raw <- readxl::read_excel(file, sheet = sheet, col_names = FALSE)
  n <- nrow(df_raw)
  p <- ncol(df_raw)
  
  colnames(df_raw) <- paste0("V", seq_len(p))
  first_col <- as.character(df_raw$V1)
  
  # Rows that mark the start of a year block: V1 == "date"
  date_rows <- which(tolower(first_col) == "date")
  if (length(date_rows) == 0) {
    stop("No rows with 'date' were found in the first column of the BRES sheet.")
  }
  
  all_res <- list()
  
  for (i in seq_along(date_rows)) {
    start_row <- date_rows[i]
    end_row   <- if (i < length(date_rows)) date_rows[i + 1] - 1 else n
    
    # Year in column 2 of the 'date' row
    year_val <- suppressWarnings(as.integer(df_raw[start_row, 2, drop = TRUE]))
    if (is.na(year_val) || year_val < year_min || year_val > year_max) {
      next
    }
    
    # Search within the block for the "Industry" row
    block_first_col <- as.character(df_raw$V1)
    header_rel <- which(block_first_col[(start_row + 1):end_row] == "Industry")[1]
    if (is.na(header_rel)) {
      next
    }
    header_row <- start_row + header_rel
    header     <- df_raw[header_row, , drop = FALSE]
    
    # Region columns: names not NA, different from "Flags" and >1
    header_vals <- as.character(unlist(header))
    region_cols <- which(!is.na(header_vals) &
                           header_vals != "Flags" &
                           seq_len(p) > 1)
    if (length(region_cols) == 0) next
    
    # Data rows: from header_row + 2 (skip "number/Flags" row) to end_row
    data_rows <- seq(header_row + 2, end_row)
    sub <- df_raw[data_rows, , drop = FALSE]
    industry_names <- as.character(sub$V1)
    valid_rows <- which(!is.na(industry_names) & industry_names != "NA")
    sub <- sub[valid_rows, , drop = FALSE]
    
    # Accumulate total employment by region
    for (j in region_cols) {
      reg_name_raw <- header_vals[j]
      reg_std <- normalize_bres_region(reg_name_raw)
      
      # Only NUTS1 regions of interest
      if (is.na(reg_std) || !(reg_std %in% nuts1_regions)) next
      
      vals <- suppressWarnings(as.numeric(as.character(sub[[j]])))
      total_val <- sum(vals, na.rm = TRUE)
      
      all_res[[length(all_res) + 1]] <- tibble::tibble(
        region = reg_std,
        year   = year_val,
        value  = total_val
      )
    }
  }
  
  if (length(all_res) == 0) {
    stop("Could not construct any BRES observation. Check the sheet structure.")
  }
  
  emp_long <- dplyr::bind_rows(all_res) %>%
    dplyr::group_by(region, year) %>%
    dplyr::summarise(value = sum(value, na.rm = TRUE), .groups = "drop")
  
  return(emp_long)
}

# ---------------------------
# 8. Construction of the regional dataframe
# ---------------------------

use_example <- FALSE  # TRUE to test only with simulated data

if (!use_example) {
  # 8.1 Relevant business demography sheets
  birth_sheets  <- "Table 1.1a"
  death_sheets  <- "Table 2.1a"
  active_sheets <- "Table 3.1a"
  
  # 8.2 Births, deaths and active 2018–2023
  births_list <- lapply(birth_sheets, function(sh) {
    read_ons_single_year_table(
      file       = biz_file,
      sheet      = sh,
      region_col = 2,
      value_col  = 3,
      year_min   = 2018,
      year_max   = 2023
    )
  })
  births_long <- dplyr::bind_rows(births_list)
  
  deaths_list <- lapply(death_sheets, function(sh) {
    read_ons_single_year_table(
      file       = biz_file,
      sheet      = sh,
      region_col = 2,
      value_col  = 3,
      year_min   = 2018,
      year_max   = 2023
    )
  })
  deaths_long <- dplyr::bind_rows(deaths_list)
  
  active_list <- lapply(active_sheets, function(sh) {
    read_ons_single_year_table(
      file       = biz_file,
      sheet      = sh,
      region_col = 2,
      value_col  = 3,
      year_min   = 2018,
      year_max   = 2023
    )
  })
  active_long <- dplyr::bind_rows(active_list)
  
  # 8.3 3-year survival and active employer enterprises 2023
  survival_df <- read_survival_3y(
    file  = biz_file,
    sheet = "Table 4.1",
    skip  = 4
  )
  
  employer_active_2023 <- read_employer_active_2023(
    file       = biz_file,
    sheet      = "Table 6.1",
    skip       = 4,
    region_col = 2
  )
  
  # 8.4 BRES employment 2018–2022
  emp_long <- read_bres_employment_wide(
    file      = bres_file,
    sheet     = "Data",
    year_min  = 2018,
    year_max  = 2022
  )
  
  # 8.5 Birth rate 2018–2022 (births / active)
  births_active <- births_long %>%
    dplyr::inner_join(active_long,
                      by = c("region", "year"),
                      suffix = c("_births", "_active")) %>%
    dplyr::filter(year >= 2018, year <= 2022,
                  value_active > 0)
  
  births_rates <- births_active %>%
    dplyr::mutate(birth_rate = value_births / value_active) %>%
    dplyr::group_by(region) %>%
    dplyr::summarise(
      birth_rate_reg = mean(birth_rate, na.rm = TRUE),
      .groups = "drop"
    )
  
  # 8.6 Death rate 2018–2022 (deaths / active)
  deaths_active <- deaths_long %>%
    dplyr::inner_join(active_long,
                      by = c("region", "year"),
                      suffix = c("_deaths", "_active")) %>%
    dplyr::filter(year >= 2018, year <= 2022,
                  value_active > 0)
  
  deaths_rates <- deaths_active %>%
    dplyr::mutate(death_rate = value_deaths / value_active) %>%
    dplyr::group_by(region) %>%
    dplyr::summarise(
      death_rate_reg = mean(death_rate, na.rm = TRUE),
      .groups = "drop"
    )
  
  # 8.7 Employment growth 2018–2022
  emp_18_22 <- emp_long %>%
    dplyr::filter(year %in% c(2018, 2022)) %>%
    dplyr::select(region, year, value) %>%
    tidyr::pivot_wider(
      names_from   = year,
      values_from  = value,
      names_prefix = "emp_"
    ) %>%
    dplyr::filter(!is.na(emp_2018), !is.na(emp_2022), emp_2018 > 0) %>%
    dplyr::mutate(
      emp_growth_18_22_reg = (emp_2022 - emp_2018) / emp_2018
    ) %>%
    dplyr::select(region, emp_growth_18_22_reg)
  
  # 8.8 Final union of indicators for clustering
  reg_cluster_df <- births_rates %>%
    dplyr::inner_join(deaths_rates,         by = "region") %>%
    dplyr::inner_join(survival_df,          by = "region") %>%
    dplyr::inner_join(emp_18_22,            by = "region") %>%
    dplyr::inner_join(employer_active_2023, by = "region")
  
} else {
  # Example simulated data
  reg_cluster_df <- tibble::tibble(
    region = nuts1_regions[1:4],
    birth_rate_reg           = c(0.12, 0.19, 0.14, 0.11),
    death_rate_reg           = c(0.10, 0.16, 0.11, 0.09),
    survival_3y_reg          = c(0.55, 0.60, 0.57, 0.56),
    emp_growth_18_22_reg     = c(0.02, 0.08, 0.03, 0.04),
    employer_active_2023_reg = c(40000, 250000, 80000, 130000)
  )
}

print(reg_cluster_df)

# ---------------------------
# 9. Clustering (k-means + hierarchical)
# ---------------------------

# Matrix of numeric variables
vars_for_clust <- reg_cluster_df %>%
  dplyr::select(
    birth_rate_reg,
    death_rate_reg,
    survival_3y_reg,
    emp_growth_18_22_reg,
    employer_active_2023_reg
  )

# Standardization
reg_scaled <- scale(vars_for_clust)
row.names(reg_scaled) <- reg_cluster_df$region

# k-means (k = 3)
set.seed(123)
km3 <- kmeans(reg_scaled, centers = 3, nstart = 25)
reg_cluster_df$cluster_kmeans <- factor(km3$cluster)

# Hierarchical clustering (Ward)
dist_reg <- dist(reg_scaled, method = "euclidean")
hc_ward  <- hclust(dist_reg, method = "ward.D2")

# Dendrogram
factoextra::fviz_dend(
  hc_ward,
  k                   = 3,
  rect                = TRUE,
  rect_fill           = TRUE,          
  color_labels_by_k   = TRUE,          
  k_colors            = c("#2E9FDF", "#E7B800", "#FC4E07"),
  horiz               = TRUE,         
  cex                 = 0.4,
  main                = "Hierarchical clustering of regions",
  xlab                = "",          
  ylab                = "Height",
  ggtheme             = theme_minimal()
)




# Clusters projected in PCA space
# Rename cluster levels in the k-means object
km3$cluster <- factor(
  km3$cluster,
  levels = c(1, 2, 3),
  labels = c("Cluster1", "Cluster 2", "Cluster3")
)

factoextra::fviz_cluster(
  km3,
  data            = reg_scaled,
  geom            = "point",
  ellipse.type    = "norm",
  ellipse.alpha   = 0.15,      # semi-transparent ellipses
  show.clust.cent = TRUE,      # shows centroids
  alpha           = 0.7,       # point transparency
  shape           = 16,
  repel           = TRUE,
  labelsize       = 3,
  palette         = "Dark2",   # color palette
  ggtheme         = theme_minimal(),
  main            = "Regional clusters (k-means, PCA space)"
) +
  theme(
    plot.title       = element_text(face = "bold", hjust = 0.5),
    legend.position  = "bottom",
    legend.title     = element_blank(),
    panel.background = element_rect(fill = "grey96", colour = NA),
    panel.grid.major = element_line(
      linetype = "dotted",
      linewidth = 0.6,
      colour   = "grey70"
    ),
    panel.grid.minor = element_line(
      linetype = "dotted",
      linewidth = 0.3,
      colour   = "grey85"
    )
  )


# ---------------------------
# 10. Descriptives by cluster and interpretation
# ---------------------------

cluster_summary <- reg_cluster_df %>%
  dplyr::group_by(cluster_kmeans) %>%
  dplyr::summarise(
    n_regiones              = dplyr::n(),
    mean_birth_rate         = mean(birth_rate_reg, na.rm = TRUE),
    mean_death_rate         = mean(death_rate_reg, na.rm = TRUE),
    mean_survival_3y        = mean(survival_3y_reg, na.rm = TRUE),
    mean_emp_growth_18_22   = mean(emp_growth_18_22_reg, na.rm = TRUE),
    mean_employer_active_23 = mean(employer_active_2023_reg, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    tipo_cluster = dplyr::case_when(
      mean_birth_rate >= median(mean_birth_rate) &
        mean_emp_growth_18_22 >= median(mean_emp_growth_18_22) ~ "HOT / dynamic regions",
      mean_death_rate <= median(mean_death_rate) &
        mean_survival_3y >= median(mean_survival_3y) ~ "Mature and stable regions",
      TRUE ~ "Lagging / struggling regions"
    )
  )

print(cluster_summary)

# Add qualitative label to each region
reg_cluster_df <- reg_cluster_df %>%
  dplyr::left_join(
    cluster_summary %>% dplyr::select(cluster_kmeans, tipo_cluster),
    by = "cluster_kmeans"
  )

# ---------------------------
# 11. Yorkshire & Humber, London and northern regions
# ---------------------------

# Yorkshire and The Humber
yorkshire_row <- reg_cluster_df %>%
  dplyr::filter(stringr::str_detect(region, "YORKSHIRE"))

# London
london_row <- reg_cluster_df %>%
  dplyr::filter(region == "LONDON")

# Northern regions (North East, North West, Yorkshire)
northern_regions <- reg_cluster_df %>%
  dplyr::filter(stringr::str_detect(region, "NORTH EAST|NORTH WEST|YORKSHIRE")) %>%
  dplyr::arrange(region)





























