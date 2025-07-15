# ---- CODE for Academic Freedom and International Research Collaboration ----
# ---- Network Visualization ----

library(tibble)
library(tidyr)
library(dplyr)
library(readr)
library(network)
library(sna)
library(RColorBrewer)
library(GGally)
library(ggplot2)
library(ggpubr)
library(vdemdata)
library(backbone)
library(abind)

# read in academic freedom data which will set the constraint on country N

acadvar <- vdemdata::vdem %>%
  select(country_text_id, year, v2xca_academ) %>%
  filter(year >= 1993 & year <= 2022) %>%
  arrange(country_text_id) %>%
  pivot_wider(names_from = year, values_from = v2xca_academ) %>%
  column_to_rownames("country_text_id") %>%
  drop_na()
all_codes <- row.names(acadvar)

create_matrix_template <- function(all_codes) {
  size <- length(all_codes)
  mat <- matrix(0, nrow = size, ncol = size)
  colnames(mat) <- all_codes
  rownames(mat) <- all_codes
  return(mat)
}

# function to process matrices

process_file_to_dataframe <- function(file, matrix_template) {
  data <- suppressMessages(
    read_csv(file, col_names = TRUE)
  ) %>% as.data.frame()
  
  standardized_matrix <- matrix_template
  
  for (i in 1:nrow(data)) {
    row_code <- data[i, 1]
    if (row_code %in% rownames(standardized_matrix)) {
      for (col_name in names(data)[-1]) {
        if (col_name %in% colnames(standardized_matrix)) {
          standardized_matrix[row_code, col_name] <- data[i, col_name]
        }
      }
    }
  }
  return(as.data.frame(standardized_matrix))
}

# ID file path
file_paths <- list.files(path = "C:/Users/twhetsell3/OneDrive - Georgia Institute of Technology/Priority 1/academic_freedom/WOS_HEADING_CAT_PY/FINAL_PY_20253006/PY_ST", pattern = "*science_technology_dis.csv", full.names = TRUE)

matrix_template <- create_matrix_template(all_codes)

# bring in data

for (file in file_paths) {
  file_name <- gsub(".csv$", "", basename(file)) 
  variable_name <- paste("df", file_name, sep = "_") 
  assign(variable_name, process_file_to_dataframe(file, matrix_template))
}

# Identify if any countries have zero values across all years

sums_by_country <- list()

for (year in 1993:2022) {
  matrix_name <- paste("df_country_matrix", year, "science_technology_dis", sep = "_")
  current_matrix <- get(matrix_name)
  column_sums <- colSums(current_matrix)
  for (country in names(column_sums)) {
    if (!country %in% names(sums_by_country)) {
      sums_by_country[[country]] <- 0
    }
    sums_by_country[[country]] <- sums_by_country[[country]] + column_sums[country]
  }
}

total_sums_by_country <- unlist(sums_by_country)
print(total_sums_by_country)

# SML has zero values, Sml causes problems later when loading gdp data, HKG was apparently merged into chn by wos around 2000

for (year in 1993:2022) {
  matrix_name <- paste("df_country_matrix", year, "science_technology_dis", sep = "_")
  matrix <- get(matrix_name)
  matrix <- matrix[!(rownames(matrix) %in% c("SML", "HKG")), !(colnames(matrix) %in% c("SML", "HKG"))]
  assign(matrix_name, matrix, envir = .GlobalEnv)
}

# remove HKG and SML from acadvar

acadvar <- acadvar[!(rownames(acadvar) %in% c("SML", "HKG")), ]

# use academic freedom as basis for country N

country_names <-rownames(acadvar)

# create network list

binary_matrices_list_ST <- list()

for (year in 1993:2022) {
  df_name <- paste0("df_country_matrix_", year, "_science_technology_dis")
  if (exists(df_name, where = .GlobalEnv)) {
    net_df <- get(df_name)
    net_matrix <- as.matrix(net_df)
    try({
      binary_matrix_ST <- disparity(net_matrix, alpha=0.05, narrative = FALSE)
      binary_matrices_list_ST[[as.character(year)]] <- binary_matrix_ST
    }, silent = FALSE)
  } else {
    print(paste(df_name, "does not exist in the environment"))
  }
}

matrices_array_ST <- do.call("abind", c(binary_matrices_list_ST, along = 3))

# create network list

network_list <- lapply(binary_matrices_list_ST, function(mat) {
  network::network(mat, directed = FALSE)
})

# names by year

years <- names(binary_matrices_list_ST)

# function to delete isolates, specify years, add colors, layout, other fig specs

ggnet2_call <- function(graph, year) {
  # identify isolates (nodes with zero degree) using sna::degree
  isolates <- which(sna::degree(graph, gmode = "graph") == 0)
  if (length(isolates) > 0) {
    graph <- network::delete.vertices(graph, isolates)
  }
  
  node_names <- network.vertex.names(graph)
  acadvar_values <- acadvar[node_names, as.character(year)]
  acadvar_bins <- cut(acadvar_values, breaks = 9)
  color_palette <- RColorBrewer::brewer.pal(9, "RdYlBu")
  acadvar_colors <- color_palette[as.numeric(acadvar_bins)]
  
  ggnet2(graph, mode = "fruchtermanreingold",
         layout.par = list(vcount = network.size(graph)), 
         size = "degree", alpha = 0.5, color = acadvar_colors,
         label = FALSE, edge.alpha = 0.5, edge.color = "lightgrey",
         edge.size = 0.25) +
    theme_void() +
    labs(title = paste("", year)) +
    theme(plot.title = element_text(size = 10)) +
    guides(color = FALSE, size = FALSE)
}

# create plot list and plot array

plot_list <- mapply(ggnet2_call, network_list, years, SIMPLIFY = FALSE)
combined_plot <- do.call("ggarrange", c(plot_list, list(ncol = 5, nrow = 6)))
print(combined_plot)