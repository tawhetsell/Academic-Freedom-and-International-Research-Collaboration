# ---- CODE for Academic Freedom and International Research Collaboration ----
# ---- Table 4 Temporal ----

setwd("C:/Users/twhetsell3/OneDrive - Georgia Institute of Technology/Priority 1/academic_freedom")

rm(list = ls())         

library(tibble)
library(readr)
library(dplyr)
library(tidyr)
library(Matrix)
library(abind)
library(backbone)
library(data.table)
library(countrycode)
library(WDI)
library(RSiena)
library(vdemdata)
library(sna)
library(network)
library(reshape2)
library(GGally)
library(ggplot2)
library(lme4)
library(plm)
library(car)

#read in academic freedom data which will set the constraint on country N

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

# identify if any countries have zero values across all years

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

# import distance data as coDyadCovar

gravity_data <- fread("C:/Users/twhetsell3/OneDrive - Georgia Institute of Technology/Priority 1/academic_freedom/Gravity_V202211.csv",
                      select = c("year", "iso3_o", "iso3_d",
                                 "dist", "distcap", "distw_harmonic",
                                 "comlang_off", "comlang_ethno"))

gravity_data_2020 <- na.omit(
  gravity_data[year == 2020 & 
                 iso3_o %in% country_names & 
                 iso3_d %in% country_names, ]
)

dist_matrix <- gravity_data_2020 %>%
  select(origin = iso3_o, destination = iso3_d, dist) %>%
  pivot_wider(names_from = destination, values_from = dist, values_fill = NA) %>%
  column_to_rownames("origin") %>%
  as.matrix()

missing_rows <- setdiff(country_names, rownames(dist_matrix))
missing_cols <- setdiff(country_names, colnames(dist_matrix))

if (length(missing_rows) > 0) {
  new_rows <- matrix(NA, nrow = length(missing_rows), ncol = ncol(dist_matrix),
                     dimnames = list(missing_rows, colnames(dist_matrix)))
  dist_matrix <- rbind(dist_matrix, new_rows)
}

if (length(missing_cols) > 0) {
  new_cols <- matrix(NA, nrow = nrow(dist_matrix), ncol = length(missing_cols),
                     dimnames = list(rownames(dist_matrix), missing_cols))
  dist_matrix <- cbind(dist_matrix, new_cols)
}

distmatrix1 <- dist_matrix[country_names, country_names]
diag(distmatrix1) <- NA

# add language variable - 

comlang_off_matrix <- gravity_data_2020 %>%
  select(origin = iso3_o, destination = iso3_d, comlang_off) %>%
  pivot_wider(names_from = destination, values_from = comlang_off, values_fill = NA) %>%
  column_to_rownames("origin") %>%
  as.matrix()

missing_rows <- setdiff(country_names, rownames(comlang_off_matrix))
missing_cols <- setdiff(country_names, colnames(comlang_off_matrix))

if (length(missing_rows) > 0) {
  new_rows <- matrix(NA, nrow = length(missing_rows), ncol = ncol(comlang_off_matrix),
                     dimnames = list(missing_rows, colnames(comlang_off_matrix)))
  comlang_off_matrix <- rbind(comlang_off_matrix, new_rows)
}

if (length(missing_cols) > 0) {
  new_cols <- matrix(NA, nrow = nrow(comlang_off_matrix), ncol = length(missing_cols),
                     dimnames = list(rownames(comlang_off_matrix), missing_cols))
  comlang_off_matrix <- cbind(comlang_off_matrix, new_cols)
}

lang_matrix <- comlang_off_matrix[country_names, country_names]
diag(lang_matrix) <- NA

# import gdp per cap data 

gdp_data_ppp <- WDI(country=country_names , indicator="NY.GDP.PCAP.CD", start=1993, end=2022, extra=TRUE)

gdp_data_ppp_wide <- gdp_data_ppp %>%
  select(iso3c, year, NY.GDP.PCAP.CD) %>%  
  spread(key = year, value = NY.GDP.PCAP.CD)
gdp_data_ppp_wide <- gdp_data_ppp_wide %>% add_row(iso3c = "TWN")
gdp_data_ppp_wide <- gdp_data_ppp_wide %>% arrange(iso3c)
row.names(gdp_data_ppp_wide) <- gdp_data_ppp_wide$iso3c
gdp_data_ppp_wide <- gdp_data_ppp_wide[, -which(names(gdp_data_ppp_wide) == "iso3c")]

total_values <- prod(dim(gdp_data_ppp_wide))
missing_values <- sum(is.na(gdp_data_ppp_wide)) 
present_values <- total_values - missing_values 
ratio_missing_to_present <- missing_values / present_values
ratio_missing_to_present

# fetch Population data with ISO3 codes

population_data <- WDI(country=country_names , indicator="SP.POP.TOTL", start=1993, end=2022, extra=TRUE)
population_data_wide <- population_data %>%
  select(iso3c, year, SP.POP.TOTL) %>%
  spread(key = year, value = SP.POP.TOTL)
population_data_wide <- population_data_wide %>% add_row(iso3c = "TWN")
population_data_wide <- population_data_wide %>% arrange(iso3c)
row.names(population_data_wide) <- population_data_wide$iso3c
population_data_wide <- population_data_wide[, -which(names(population_data_wide) == "iso3c")]

# fetch urbanization data

urbanization_data <- WDI(country=country_names , indicator = "SP.URB.TOTL.IN.ZS", start = 1993, end = 2022, extra = TRUE)
urbanization_data_wide <- urbanization_data %>%
  select(iso3c, year, SP.URB.TOTL.IN.ZS) %>%
  spread(key = year, value = SP.URB.TOTL.IN.ZS)
urbanization_data_wide <- urbanization_data_wide %>% add_row(iso3c = "TWN")
urbanization_data_wide <- urbanization_data_wide %>% arrange(iso3c)
row.names(urbanization_data_wide) <- urbanization_data_wide$iso3c
urbanization_data_wide <- urbanization_data_wide[, -which(names(urbanization_data_wide) == "iso3c")]

# read in article totals by country 

WOS_totpubs <- read_csv("C:/Users/twhetsell3/OneDrive - Georgia Institute of Technology/Priority 1/academic_freedom/country_pubs_frac_counts_disam.csv") %>%
  rename(country_code = Country)

WOS_pubs <- WOS_totpubs %>%
  filter(country_code %in% country_names) %>%
  arrange(country_code) %>%
  column_to_rownames(var = "country_code")

# add globalization data 

econ_glob_data <- read.csv("qog_std_ts_jan25.csv", stringsAsFactors = FALSE)

econ_glob_data_filtered <- econ_glob_data %>%
  filter(year >= 1993, year <= 2022, ccodealp %in% country_names)

econ_glob_data_wide <- econ_glob_data_filtered %>%
  select(ccodealp, year, dr_eg) %>%
  spread(key = year, value = dr_eg)
econ_glob_data_wide <- econ_glob_data_wide %>% arrange(ccodealp)
row.names(econ_glob_data_wide) <- econ_glob_data_wide$ccodealp
econ_glob_data_wide <- econ_glob_data_wide[, -which(names(econ_glob_data_wide) == "ccodealp")]
missing_countries <- setdiff(country_names, rownames(econ_glob_data_wide))
print(missing_countries)
if(length(missing_countries) > 0){
  for(country in missing_countries){
    econ_glob_data_wide <- rbind(econ_glob_data_wide,
                                 setNames(as.data.frame(matrix(NA, nrow = 1, ncol = ncol(econ_glob_data_wide))),
                                          names(econ_glob_data_wide)))
    rownames(econ_glob_data_wide)[nrow(econ_glob_data_wide)] <- country
  }
}

econ_glob_data_wide <- econ_glob_data_wide[sort(rownames(econ_glob_data_wide)), ]

glob_matrix<-as.data.frame(econ_glob_data_wide)

# prepare matrixes for transformation by R backbone using 0.05 sig level

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


# ---- Subset RSiena model to years 2013–2022 ----

# define years to keep subset the network array
years_to_keep <- as.character(2013:2022)
matrices_array_ST_sub <- matrices_array_ST[, , years_to_keep]

# subset time-varying covariates
acadvar_sub <- acadvar[, years_to_keep]
gdp_sub <- log(as.matrix(gdp_data_ppp_wide[, years_to_keep]) + 1)
pop_sub <- log(as.matrix(population_data_wide[, years_to_keep]) + 1)
urb_sub <- urbanization_data_wide[, years_to_keep]
pubs_sub <- log(as.matrix(WOS_pubs[, years_to_keep]) + 1)
glob_sub <- glob_matrix[, years_to_keep]
glob <- varCovar(as.matrix(glob_sub))

# recreate variables with names
IRCnets_ST_sub <- sienaDependent(matrices_array_ST_sub)
dist <- coDyadCovar(log((distmatrix1 / 1000) + 1))
lang <- coDyadCovar(lang_matrix)
free <- varCovar(as.matrix(acadvar_sub))
gdp <- varCovar(gdp_sub)
pop <- varCovar(pop_sub)
urb <- varCovar(as.matrix(urb_sub))
pubs <- varCovar(pubs_sub)

# create siena data object 
IRC_ST_sub <- sienaDataCreate(
  IRCnets_ST_sub = IRCnets_ST_sub,
  dist = dist,
  lang = lang,
  free = free,
  glob = glob,
  gdp = gdp,
  pop = pop,
  urb = urb
)

# specify model effects
effects_ST_sub <- getEffects(IRC_ST_sub)
effects_ST_sub <- includeEffects(effects_ST_sub, egoPlusAltX, simX, interaction1 = 'free')
effects_ST_sub <- includeEffects(effects_ST_sub, egoPlusAltX, simX, interaction1 = 'glob')
effects_ST_sub <- includeEffects(effects_ST_sub, egoPlusAltX, simX, interaction1 = 'gdp')
effects_ST_sub <- includeEffects(effects_ST_sub, egoPlusAltX, simX, interaction1 = 'pop')
effects_ST_sub <- includeEffects(effects_ST_sub, egoPlusAltX, simX, interaction1 = 'urb')
effects_ST_sub <- includeEffects(effects_ST_sub, X, interaction1 = 'lang')
effects_ST_sub <- includeEffects(effects_ST_sub, X, interaction1 = 'dist')
effects_ST_sub <- includeEffects(effects_ST_sub, degPlus, gwesp)

# run RSiena model
IRC_algo_sub <- sienaAlgorithmCreate(n3 = 9000, seed = 101, modelType = c(IRCnets_ST_sub = 3))
Model_T1_M1_sub <- siena07(IRC_algo_sub,data = IRC_ST_sub,effects = effects_ST_sub,useCluster = TRUE,nbrNodes = 8)
Model_T1_M1_sub

save(Model_T1_M1_sub, file = "Model_T1_M1_sub.Rdata")

# ---- Social Sciences Analysis ----

rm(list = ls())         

acadvar <- vdemdata::vdem %>%
  select(country_text_id, year, v2xca_academ) %>%
  filter(year >= 1993 & year <= 2022) %>%
  arrange(country_text_id) %>%
  pivot_wider(names_from = year, values_from = v2xca_academ) %>%
  column_to_rownames("country_text_id") %>%
  drop_na()

all_codes <- row.names(acadvar)

# creates a matrix template with all country codes

create_matrix_template <- function(all_codes) {
  size <- length(all_codes)
  mat <- matrix(0, nrow = size, ncol = size)
  colnames(mat) <- all_codes
  rownames(mat) <- all_codes
  return(mat)
}

# process each file to fit the created matrix template

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


# read in matrixes

file_paths <- list.files(path = "C:/Users/twhetsell3/OneDrive - Georgia Institute of Technology/Priority 1/academic_freedom/WOS_HEADING_CAT_PY/FINAL_PY_20253006/PY_SS", pattern = "*socialsciences_dis.csv", full.names = TRUE)

matrix_template <- create_matrix_template(all_codes)

# bring in data

for (file in file_paths) {
  file_name <- gsub(".csv$", "", basename(file)) 
  variable_name <- paste("df", file_name, sep = "_") 
  assign(variable_name, process_file_to_dataframe(file, matrix_template))
}

# identify if any countries have zero values across all years
sums_by_country <- list()

for (year in 1993:2022) {
  matrix_name <- paste("df_country_matrix", year, "socialsciences_dis", sep = "_")
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

# SML and HKG have zero values

for (year in 1993:2022) {
  matrix_name <- paste("df_country_matrix", year, "socialsciences_dis", sep = "_")
  matrix <- get(matrix_name)
  matrix <- matrix[!(rownames(matrix) %in% c("SML", "HKG")), !(colnames(matrix) %in% c("SML", "HKG"))]
  assign(matrix_name, matrix, envir = .GlobalEnv)
}

# remove HKG and SML from acadvar

acadvar <- acadvar[!(rownames(acadvar) %in% c("SML", "HKG")), ]

country_names <-rownames(acadvar)

# import distance data as coDyadCovar

gravity_data <- fread("C:/Users/twhetsell3/OneDrive - Georgia Institute of Technology/Priority 1/academic_freedom/Gravity_V202211.csv",
                      select = c("year", "iso3_o", "iso3_d",
                                 "dist", "distcap", "distw_harmonic",
                                 "comlang_off", "comlang_ethno"))

gravity_data_2020 <- na.omit(
  gravity_data[year == 2020 & 
                 iso3_o %in% country_names & 
                 iso3_d %in% country_names, ]
)

dist_matrix <- gravity_data_2020 %>%
  select(origin = iso3_o, destination = iso3_d, dist) %>%
  pivot_wider(names_from = destination, values_from = dist, values_fill = NA) %>%
  column_to_rownames("origin") %>%
  as.matrix()

missing_rows <- setdiff(country_names, rownames(dist_matrix))
missing_cols <- setdiff(country_names, colnames(dist_matrix))

if (length(missing_rows) > 0) {
  new_rows <- matrix(NA, nrow = length(missing_rows), ncol = ncol(dist_matrix),
                     dimnames = list(missing_rows, colnames(dist_matrix)))
  dist_matrix <- rbind(dist_matrix, new_rows)
}

if (length(missing_cols) > 0) {
  new_cols <- matrix(NA, nrow = nrow(dist_matrix), ncol = length(missing_cols),
                     dimnames = list(rownames(dist_matrix), missing_cols))
  dist_matrix <- cbind(dist_matrix, new_cols)
}

distmatrix1 <- dist_matrix[country_names, country_names]
diag(distmatrix1) <- NA

# add language variable - 

comlang_off_matrix <- gravity_data_2020 %>%
  select(origin = iso3_o, destination = iso3_d, comlang_off) %>%
  pivot_wider(names_from = destination, values_from = comlang_off, values_fill = NA) %>%
  column_to_rownames("origin") %>%
  as.matrix()

missing_rows <- setdiff(country_names, rownames(comlang_off_matrix))
missing_cols <- setdiff(country_names, colnames(comlang_off_matrix))

if (length(missing_rows) > 0) {
  new_rows <- matrix(NA, nrow = length(missing_rows), ncol = ncol(comlang_off_matrix),
                     dimnames = list(missing_rows, colnames(comlang_off_matrix)))
  comlang_off_matrix <- rbind(comlang_off_matrix, new_rows)
}

if (length(missing_cols) > 0) {
  new_cols <- matrix(NA, nrow = nrow(comlang_off_matrix), ncol = length(missing_cols),
                     dimnames = list(rownames(comlang_off_matrix), missing_cols))
  comlang_off_matrix <- cbind(comlang_off_matrix, new_cols)
}

lang_matrix <- comlang_off_matrix[country_names, country_names]
diag(lang_matrix) <- NA

# bring in variables from WDI, import gdp per cap data - NY.GDP.MKTP.KD , NY.GDP.MKTP.PP.CD https://datatopics.worldbank.org/world-development-indicators/stories/purchasing-power-parities-putting-global-public-good-socioeconomic-analyses.html

gdp_data_ppp <- WDI(country=country_names , indicator="NY.GDP.PCAP.CD", start=1993, end=2022, extra=TRUE)

gdp_data_ppp_wide <- gdp_data_ppp %>%
  select(iso3c, year, NY.GDP.PCAP.CD) %>%  # options NY.GDP.PCAP.CD, NY.GDP.MKTP.PP.CD
  spread(key = year, value = NY.GDP.PCAP.CD)
gdp_data_ppp_wide <- gdp_data_ppp_wide %>% add_row(iso3c = "TWN")
gdp_data_ppp_wide <- gdp_data_ppp_wide %>% arrange(iso3c)
row.names(gdp_data_ppp_wide) <- gdp_data_ppp_wide$iso3c
gdp_data_ppp_wide <- gdp_data_ppp_wide[, -which(names(gdp_data_ppp_wide) == "iso3c")]

total_values <- prod(dim(gdp_data_ppp_wide))
missing_values <- sum(is.na(gdp_data_ppp_wide)) 
present_values <- total_values - missing_values 
ratio_missing_to_present <- missing_values / present_values
ratio_missing_to_present

# fetch Population data with ISO3 codes

population_data <- WDI(country=country_names , indicator="SP.POP.TOTL", start=1993, end=2022, extra=TRUE)
population_data_wide <- population_data %>%
  select(iso3c, year, SP.POP.TOTL) %>%
  spread(key = year, value = SP.POP.TOTL)
population_data_wide <- population_data_wide %>% add_row(iso3c = "TWN")
population_data_wide <- population_data_wide %>% arrange(iso3c)
row.names(population_data_wide) <- population_data_wide$iso3c
population_data_wide <- population_data_wide[, -which(names(population_data_wide) == "iso3c")]

# fetch urbanization data

urbanization_data <- WDI(country=country_names , indicator = "SP.URB.TOTL.IN.ZS", start = 1993, end = 2022, extra = TRUE)
urbanization_data_wide <- urbanization_data %>%
  select(iso3c, year, SP.URB.TOTL.IN.ZS) %>%
  spread(key = year, value = SP.URB.TOTL.IN.ZS)
urbanization_data_wide <- urbanization_data_wide %>% add_row(iso3c = "TWN")
urbanization_data_wide <- urbanization_data_wide %>% arrange(iso3c)
row.names(urbanization_data_wide) <- urbanization_data_wide$iso3c
urbanization_data_wide <- urbanization_data_wide[, -which(names(urbanization_data_wide) == "iso3c")]

# read in article totals by country 

WOS_totpubs <- read_csv("C:/Users/twhetsell3/OneDrive - Georgia Institute of Technology/Priority 1/academic_freedom/country_pubs_frac_counts_disam.csv") %>%
  rename(country_code = Country)

WOS_pubs <- WOS_totpubs %>%
  filter(country_code %in% country_names) %>%
  arrange(country_code) %>%
  column_to_rownames(var = "country_code")

# add globalization data 

econ_glob_data <- read.csv("qog_std_ts_jan25.csv", stringsAsFactors = FALSE)

econ_glob_data_filtered <- econ_glob_data %>%
  filter(year >= 1993, year <= 2022, ccodealp %in% country_names)

econ_glob_data_wide <- econ_glob_data_filtered %>%
  select(ccodealp, year, dr_eg) %>%
  spread(key = year, value = dr_eg)
econ_glob_data_wide <- econ_glob_data_wide %>% arrange(ccodealp)
row.names(econ_glob_data_wide) <- econ_glob_data_wide$ccodealp
econ_glob_data_wide <- econ_glob_data_wide[, -which(names(econ_glob_data_wide) == "ccodealp")]
missing_countries <- setdiff(country_names, rownames(econ_glob_data_wide))
print(missing_countries)
if(length(missing_countries) > 0){
  for(country in missing_countries){
    econ_glob_data_wide <- rbind(econ_glob_data_wide,
                                 setNames(as.data.frame(matrix(NA, nrow = 1, ncol = ncol(econ_glob_data_wide))),
                                          names(econ_glob_data_wide)))
    rownames(econ_glob_data_wide)[nrow(econ_glob_data_wide)] <- country
  }
}

econ_glob_data_wide <- econ_glob_data_wide[sort(rownames(econ_glob_data_wide)), ]

glob_matrix<-as.data.frame(econ_glob_data_wide)

### prepare matrixes for transformation by backbone

binary_matrices_list_SS <- list()

for (year in 1993:2022) {
  df_name <- paste0("df_country_matrix_", year, "_socialsciences_dis")
  
  if (exists(df_name, where = .GlobalEnv)) {
    net_df <- get(df_name)
    net_matrix <- as.matrix(net_df)
    try({
      binary_matrix_SS <- disparity(net_matrix, alpha=0.05, narrative = FALSE)
      binary_matrices_list_SS[[as.character(year)]] <- binary_matrix_SS
    }, silent = FALSE)
  } else {
    print(paste(df_name, "does not exist in the environment"))
  }
}

matrices_array_SS <- do.call("abind", c(binary_matrices_list_SS, along = 3))

# define the years to keep and subset the network array
years_to_keep <- as.character(2013:2022)
matrices_array_SS_sub <- matrices_array_SS[, , years_to_keep]

# subset all time-varying covariates
acadvar_sub <- acadvar[, years_to_keep]
gdp_sub <- log(as.matrix(gdp_data_ppp_wide[, years_to_keep]) + 1)
pop_sub <- log(as.matrix(population_data_wide[, years_to_keep]) + 1)
urb_sub <- urbanization_data_wide[, years_to_keep]
pubs_sub <- log(as.matrix(WOS_pubs[, years_to_keep]) + 1)
glob_sub <- glob_matrix[, years_to_keep]
glob <- varCovar(as.matrix(glob_sub))

# recreate Siena variables with proper names
IRCnets_SS_sub <- sienaDependent(matrices_array_SS_sub)
dist <- coDyadCovar(log((distmatrix1 / 1000) + 1))
lang <- coDyadCovar(lang_matrix)
free <- varCovar(as.matrix(acadvar_sub))
gdp <- varCovar(gdp_sub)
pop <- varCovar(pop_sub)
urb <- varCovar(as.matrix(urb_sub))
pubs <- varCovar(pubs_sub)

# create Siena data object with named arguments
IRC_SS_sub <- sienaDataCreate(
  IRCnets_SS_sub = IRCnets_SS_sub,
  dist = dist,
  lang = lang,
  free = free,
  glob = glob,
  gdp = gdp,
  pop = pop,
  urb = urb
)

# specify model effects
effects_SS_sub <- getEffects(IRC_SS_sub)
effects_SS_sub <- includeEffects(effects_SS_sub, egoPlusAltX, simX, interaction1 = 'free')
effects_SS_sub <- includeEffects(effects_SS_sub, egoPlusAltX, simX, interaction1 = 'glob')
effects_SS_sub <- includeEffects(effects_SS_sub, egoPlusAltX, simX, interaction1 = 'gdp')
effects_SS_sub <- includeEffects(effects_SS_sub, egoPlusAltX, simX, interaction1 = 'pop')
effects_SS_sub <- includeEffects(effects_SS_sub, egoPlusAltX, simX, interaction1 = 'urb')
effects_SS_sub <- includeEffects(effects_SS_sub, X, interaction1 = 'lang')
effects_SS_sub <- includeEffects(effects_SS_sub, X, interaction1 = 'dist')
effects_SS_sub <- includeEffects(effects_SS_sub, degPlus, gwesp)

# run RSiena model
IRC_algo_sub <- sienaAlgorithmCreate(n3 = 9000, seed = 101, modelType = c(IRCnets_SS_sub = 3))
Model_T1_M2_sub <- siena07(IRC_algo_sub,data = IRC_SS_sub,effects = effects_SS_sub,useCluster = TRUE,nbrNodes = 8)
Model_T1_M2_sub

save(Model_T1_M2_sub, file = "Model_T1_M2_sub.Rdata")

# ---- Arts and Humanities Analysis ----

rm(list = ls())         

acadvar <- vdemdata::vdem %>%
  select(country_text_id, year, v2xca_academ) %>%
  filter(year >= 1993 & year <= 2022) %>%
  arrange(country_text_id) %>%
  pivot_wider(names_from = year, values_from = v2xca_academ) %>%
  column_to_rownames("country_text_id") %>%
  drop_na()

all_codes <- row.names(acadvar)

# Creates a matrix template with all country codes

create_matrix_template <- function(all_codes) {
  size <- length(all_codes)
  mat <- matrix(0, nrow = size, ncol = size)
  colnames(mat) <- all_codes
  rownames(mat) <- all_codes
  return(mat)
}

# Process each file to fit the created matrix template
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

# read in matrixes

file_paths <- list.files(path = "C:/Users/twhetsell3/OneDrive - Georgia Institute of Technology/Priority 1/academic_freedom/WOS_HEADING_CAT_PY/FINAL_PY_20253006/PY_AH", pattern = "*arts_humanities_dis.csv", full.names = TRUE)

matrix_template <- create_matrix_template(all_codes)

for (file in file_paths) {
  file_name <- gsub(".csv$", "", basename(file)) 
  variable_name <- paste("df", file_name, sep = "_") 
  assign(variable_name, process_file_to_dataframe(file, matrix_template))
}


# Identify if any countries have zero values across all years
sums_by_country <- list()

for (year in 1993:2022) {
  matrix_name <- paste("df_country_matrix", year, "arts_humanities_dis", sep = "_")
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

# now remove HKG and SML

for (year in 1993:2022) {
  matrix_name <- paste("df_country_matrix", year, "arts_humanities_dis", sep = "_")
  matrix <- get(matrix_name)
  matrix <- matrix[!(rownames(matrix) %in% c("SML", "HKG")), !(colnames(matrix) %in% c("SML", "HKG"))]
  assign(matrix_name, matrix, envir = .GlobalEnv)
}

# remove HKG and SML from acadvar

acadvar <- acadvar[!(rownames(acadvar) %in% c("SML", "HKG")), ]

country_names <-rownames(acadvar)

# import distance data as coDyadCovar

gravity_data <- fread("C:/Users/twhetsell3/OneDrive - Georgia Institute of Technology/Priority 1/academic_freedom/Gravity_V202211.csv",
                      select = c("year", "iso3_o", "iso3_d",
                                 "dist", "distcap", "distw_harmonic",
                                 "comlang_off", "comlang_ethno"))

gravity_data_2020 <- na.omit(
  gravity_data[year == 2020 & 
                 iso3_o %in% country_names & 
                 iso3_d %in% country_names, ]
)

dist_matrix <- gravity_data_2020 %>%
  select(origin = iso3_o, destination = iso3_d, dist) %>%
  pivot_wider(names_from = destination, values_from = dist, values_fill = NA) %>%
  column_to_rownames("origin") %>%
  as.matrix()

missing_rows <- setdiff(country_names, rownames(dist_matrix))
missing_cols <- setdiff(country_names, colnames(dist_matrix))

if (length(missing_rows) > 0) {
  new_rows <- matrix(NA, nrow = length(missing_rows), ncol = ncol(dist_matrix),
                     dimnames = list(missing_rows, colnames(dist_matrix)))
  dist_matrix <- rbind(dist_matrix, new_rows)
}

if (length(missing_cols) > 0) {
  new_cols <- matrix(NA, nrow = nrow(dist_matrix), ncol = length(missing_cols),
                     dimnames = list(rownames(dist_matrix), missing_cols))
  dist_matrix <- cbind(dist_matrix, new_cols)
}

distmatrix1 <- dist_matrix[country_names, country_names]
diag(distmatrix1) <- NA

# add language variable - 

comlang_off_matrix <- gravity_data_2020 %>%
  select(origin = iso3_o, destination = iso3_d, comlang_off) %>%
  pivot_wider(names_from = destination, values_from = comlang_off, values_fill = NA) %>%
  column_to_rownames("origin") %>%
  as.matrix()

missing_rows <- setdiff(country_names, rownames(comlang_off_matrix))
missing_cols <- setdiff(country_names, colnames(comlang_off_matrix))

if (length(missing_rows) > 0) {
  new_rows <- matrix(NA, nrow = length(missing_rows), ncol = ncol(comlang_off_matrix),
                     dimnames = list(missing_rows, colnames(comlang_off_matrix)))
  comlang_off_matrix <- rbind(comlang_off_matrix, new_rows)
}

if (length(missing_cols) > 0) {
  new_cols <- matrix(NA, nrow = nrow(comlang_off_matrix), ncol = length(missing_cols),
                     dimnames = list(rownames(comlang_off_matrix), missing_cols))
  comlang_off_matrix <- cbind(comlang_off_matrix, new_cols)
}

lang_matrix <- comlang_off_matrix[country_names, country_names]
diag(lang_matrix) <- NA

# bring in variables from WDI, import gdp per cap data - NY.GDP.MKTP.KD , NY.GDP.MKTP.PP.CD https://datatopics.worldbank.org/world-development-indicators/stories/purchasing-power-parities-putting-global-public-good-socioeconomic-analyses.html

gdp_data_ppp <- WDI(country=country_names , indicator="NY.GDP.PCAP.CD", start=1993, end=2022, extra=TRUE)

gdp_data_ppp_wide <- gdp_data_ppp %>%
  select(iso3c, year, NY.GDP.PCAP.CD) %>%  ### NY.GDP.PCAP.CD    NY.GDP.MKTP.PP.CD
  spread(key = year, value = NY.GDP.PCAP.CD)
gdp_data_ppp_wide <- gdp_data_ppp_wide %>% add_row(iso3c = "TWN")
gdp_data_ppp_wide <- gdp_data_ppp_wide %>% arrange(iso3c)
row.names(gdp_data_ppp_wide) <- gdp_data_ppp_wide$iso3c
gdp_data_ppp_wide <- gdp_data_ppp_wide[, -which(names(gdp_data_ppp_wide) == "iso3c")]

total_values <- prod(dim(gdp_data_ppp_wide))
missing_values <- sum(is.na(gdp_data_ppp_wide)) 
present_values <- total_values - missing_values 
ratio_missing_to_present <- missing_values / present_values
ratio_missing_to_present

# fetch Population data with ISO3 codes

population_data <- WDI(country=country_names , indicator="SP.POP.TOTL", start=1993, end=2022, extra=TRUE)
population_data_wide <- population_data %>%
  select(iso3c, year, SP.POP.TOTL) %>%
  spread(key = year, value = SP.POP.TOTL)
population_data_wide <- population_data_wide %>% add_row(iso3c = "TWN")
population_data_wide <- population_data_wide %>% arrange(iso3c)
row.names(population_data_wide) <- population_data_wide$iso3c
population_data_wide <- population_data_wide[, -which(names(population_data_wide) == "iso3c")]

# fetch urbanization data

urbanization_data <- WDI(country=country_names , indicator = "SP.URB.TOTL.IN.ZS", start = 1993, end = 2022, extra = TRUE)
urbanization_data_wide <- urbanization_data %>%
  select(iso3c, year, SP.URB.TOTL.IN.ZS) %>%
  spread(key = year, value = SP.URB.TOTL.IN.ZS)
urbanization_data_wide <- urbanization_data_wide %>% add_row(iso3c = "TWN")
urbanization_data_wide <- urbanization_data_wide %>% arrange(iso3c)
row.names(urbanization_data_wide) <- urbanization_data_wide$iso3c
urbanization_data_wide <- urbanization_data_wide[, -which(names(urbanization_data_wide) == "iso3c")]

# read in article totals by country 

WOS_totpubs <- read_csv("C:/Users/twhetsell3/OneDrive - Georgia Institute of Technology/Priority 1/academic_freedom/country_pubs_frac_counts_disam.csv") %>%
  rename(country_code = Country)

WOS_pubs <- WOS_totpubs %>%
  filter(country_code %in% country_names) %>%
  arrange(country_code) %>%
  column_to_rownames(var = "country_code")

# add globalization data 

econ_glob_data <- read.csv("qog_std_ts_jan25.csv", stringsAsFactors = FALSE)

econ_glob_data_filtered <- econ_glob_data %>%
  filter(year >= 1993, year <= 2022, ccodealp %in% country_names)

econ_glob_data_wide <- econ_glob_data_filtered %>%
  select(ccodealp, year, dr_eg) %>%
  spread(key = year, value = dr_eg)
econ_glob_data_wide <- econ_glob_data_wide %>% arrange(ccodealp)
row.names(econ_glob_data_wide) <- econ_glob_data_wide$ccodealp
econ_glob_data_wide <- econ_glob_data_wide[, -which(names(econ_glob_data_wide) == "ccodealp")]
missing_countries <- setdiff(country_names, rownames(econ_glob_data_wide))
print(missing_countries)
if(length(missing_countries) > 0){
  for(country in missing_countries){
    econ_glob_data_wide <- rbind(econ_glob_data_wide,
                                 setNames(as.data.frame(matrix(NA, nrow = 1, ncol = ncol(econ_glob_data_wide))),
                                          names(econ_glob_data_wide)))
    rownames(econ_glob_data_wide)[nrow(econ_glob_data_wide)] <- country
  }
}

econ_glob_data_wide <- econ_glob_data_wide[sort(rownames(econ_glob_data_wide)), ]

glob_matrix<-as.data.frame(econ_glob_data_wide)

### prepare matrixes for transformation by backbone

binary_matrices_list_AH <- list()

for (year in 1993:2022) {
  df_name <- paste0("df_country_matrix_", year, "_arts_humanities_dis")
  
  if (exists(df_name, where = .GlobalEnv)) {
    net_df <- get(df_name)
    net_matrix <- as.matrix(net_df)
    try({
      binary_matrix_AH <- disparity(net_matrix, alpha=0.05, narrative = FALSE)
      binary_matrices_list_AH[[as.character(year)]] <- binary_matrix_AH
    }, silent = FALSE)
  } else {
    print(paste(df_name, "does not exist in the environment"))
  }
}

matrices_array_AH <- do.call("abind", c(binary_matrices_list_AH, along = 3))

# ---- subset ----

# define the years to keep and subset the network array
years_to_keep <- as.character(2013:2022)
matrices_array_AH_sub <- matrices_array_AH[, , years_to_keep]

# subset all time-varying covariates
acadvar_sub <- acadvar[, years_to_keep]
gdp_sub <- log(as.matrix(gdp_data_ppp_wide[, years_to_keep]) + 1)
pop_sub <- log(as.matrix(population_data_wide[, years_to_keep]) + 1)
urb_sub <- urbanization_data_wide[, years_to_keep]
pubs_sub <- log(as.matrix(WOS_pubs[, years_to_keep]) + 1)
glob_sub <- glob_matrix[, years_to_keep]
glob <- varCovar(as.matrix(glob_sub))
                 
# recreate siena variables with proper names
IRCnets_AH_sub <- sienaDependent(matrices_array_AH_sub)
dist <- coDyadCovar(log((distmatrix1 / 1000) + 1))
lang <- coDyadCovar(lang_matrix)
free <- varCovar(as.matrix(acadvar_sub))
gdp <- varCovar(gdp_sub)
pop <- varCovar(pop_sub)
urb <- varCovar(as.matrix(urb_sub))
pubs <- varCovar(pubs_sub)

# create Siena data object with named arguments
IRC_AH_sub <- sienaDataCreate(
  IRCnets_AH_sub = IRCnets_AH_sub,
  dist = dist,
  lang = lang,
  free = free,
  glob = glob,
  gdp = gdp,
  pop = pop,
  urb = urb
)

# specify model effects
effects_AH_sub <- getEffects(IRC_AH_sub)
effects_AH_sub <- includeEffects(effects_AH_sub, egoPlusAltX, simX, interaction1 = 'free')
effects_AH_sub <- includeEffects(effects_AH_sub, egoPlusAltX, simX, interaction1 = 'glob')
effects_AH_sub <- includeEffects(effects_AH_sub, egoPlusAltX, simX, interaction1 = 'gdp')
effects_AH_sub <- includeEffects(effects_AH_sub, egoPlusAltX, simX, interaction1 = 'pop')
effects_AH_sub <- includeEffects(effects_AH_sub, egoPlusAltX, simX, interaction1 = 'urb')
effects_AH_sub <- includeEffects(effects_AH_sub, X, interaction1 = 'lang')
effects_AH_sub <- includeEffects(effects_AH_sub, X, interaction1 = 'dist')
effects_AH_sub <- includeEffects(effects_AH_sub, degPlus, gwesp)

# run RSiena model
IRC_algo_sub <- sienaAlgorithmCreate(n3 = 9000, seed = 101, modelType = c(IRCnets_AH_sub = 3))
Model_T1_M3_sub <- siena07(IRC_algo_sub,data = IRC_AH_sub,effects = effects_AH_sub,useCluster = TRUE,nbrNodes = 8)
Model_T1_M3_sub

save(Model_T1_M3_sub, file ="Model_T1_M3_sub.Rdata")

# ---- Main SIENA Table ----

rm(list = ls())     

setwd("C:/Users/twhetsell3/OneDrive - Georgia Institute of Technology/Priority 1/academic_freedom/Github_Files_20250207/models")

# load necessary RData files for T1
load("Model_T1_M1_sub.Rdata")
load("Model_T1_M2_sub.Rdata")
load("Model_T1_M3_sub.Rdata")

# create model group for T1
model_group_T1 <- list(Model_T1_M1_sub, Model_T1_M2_sub, Model_T1_M3_sub)

# define model names for T1
model_names_T1 <- c("ST", "SS", "AH")

# define custom variable names
custom_variable_names <- c(
  "Dens (degree)", 
  "Tran (gwesp)", 
  "Pref (degPlus)", 
  "Dist. (coDyadCovar)", 
  "Lang. (coDyadCovar)", 
  "Free (egoPlusAtlX)", 
  "Free (simX)", 
  "Glob (egoPlusAtlX)", 
  "Glob (simX)",
  "GDP (egoPlusAtlX)", 
  "GDP (simX)",
  "Pop (egoPlusAtlX)", 
  "Pop (simX)", 
  "Urb (egoPlusAtlX)", 
  "Urb (simX)", 
  "Convergence Ratio",
  "Iteration"
)

# define effect names (from T1)
effect_names <- Model_T1_M1_sub$effects$effectName

# calculate p-values and categories for significance (p_cat)
p_values_T1 <- vector("list", length(model_group_T1))
p_cat_T1 <- vector("list", length(model_group_T1))

for (i in 1:length(model_group_T1)) {
  model_summary <- summary(model_group_T1[[i]]) 
  
  z_scores <- model_summary$theta / model_summary$se
  p_values_T1[[i]] <- 2 * (1 - pnorm(abs(z_scores)))
  
  p_cat_T1[[i]] <- ifelse(p_values_T1[[i]] < 0.001, "***",
                          ifelse(p_values_T1[[i]] < 0.01, "**",
                                 ifelse(p_values_T1[[i]] < 0.05, "*", "ns")))
}

# function to generate the LaTeX table for model group T1
generate_latex_table <- function(model_group, model_names, effect_names, custom_names, p_cat) {
  table_latex <- "\\begin{table}[ht]\n\\centering\n\\caption{Model Results}\n\\begin{tabular}{lccc}\n"
  table_latex <- paste(table_latex, "Variable & ", paste(model_names, collapse = " & "), " \\\\\n\\hline\n", sep = "")
  
  # loop through each effect
  for (i in 1:length(effect_names)) {
    custom_name <- custom_names[i]
    
    # first row estimates with p-value significance (asterisks)
    row_latex <- paste(custom_name, " & ")
    for (j in 1:length(model_group)) {
      estimate <- round(model_group[[j]]$theta[i], 3)
      p_cat_value <- p_cat[[j]][i]
      estimate_with_significance <- paste0(estimate, " ", p_cat_value)
      row_latex <- paste(row_latex, estimate_with_significance, " & ", sep = "")
    }
    row_latex <- substring(row_latex, 1, nchar(row_latex)-2)  # Remove last "&"
    row_latex <- paste(row_latex, " \\\\\n", sep = "")
    
    # second row standard errors in parentheses
    se_row_latex <- "& "
    for (j in 1:length(model_group)) {
      se <- round(model_group[[j]]$se[i], 3)
      se_row_latex <- paste(se_row_latex, "(", se, ")", " & ", sep = "")
    }
    se_row_latex <- substring(se_row_latex, 1, nchar(se_row_latex)-2)  # Remove last "&"
    se_row_latex <- paste(se_row_latex, " \\\\\n", sep = "")
    
    # add both rows to the table
    table_latex <- paste(table_latex, row_latex, se_row_latex, sep = "")
  }
  
  # add rows for convergence ratio and iterations at the bottom
  conv_row_latex <- "Convergence Ratio & "
  iter_row_latex <- "Iterations & "
  
  for (j in 1:length(model_group)) {
    conv_ratio <- round(model_group[[j]]$tconv.max, 3)
    iterations <- model_group[[j]]$n
    conv_row_latex <- paste(conv_row_latex, conv_ratio, " & ", sep = "")
    iter_row_latex <- paste(iter_row_latex, iterations, " & ", sep = "")
  }
  
  # remove the trailing "&" from both rows
  conv_row_latex <- substring(conv_row_latex, 1, nchar(conv_row_latex)-2)  
  iter_row_latex <- substring(iter_row_latex, 1, nchar(iter_row_latex)-2)
  
  # append the two rows to the table
  conv_row_latex <- paste(conv_row_latex, " \\\\\n", sep = "")
  iter_row_latex <- paste(iter_row_latex, " \\\\\n", sep = "")
  
  table_latex <- paste(table_latex, conv_row_latex, iter_row_latex, sep = "")
  
  # end table
  table_latex <- paste(table_latex, "\\hline\n\\end{tabular}\n", sep = "")
  
  # add table notes row, before the end of the table
  table_latex <- paste(table_latex, "\\vspace{0.2cm}\\begin{center} \\small{\\textit{Table Notes: The estimates are rounded to 3 decimals. Standard errors in parentheses. ns=p-value \\textgreater{} 0.5, *=p-value \\textless{} 0.5, **=p-value \\textless{} 0.01, ***=p-value \\textless{} 0.001. Convergence ratio \\textless{} 0.25 indicates good convergence.}} \\end{center}", sep = "")
  
  # close table environment
  table_latex <- paste(table_latex, "\\end{table}\n", sep = "")
  
  return(table_latex)
}

# generate LaTeX table for model group T1
latex_table_T3 <- generate_latex_table(model_group_T1, model_names_T1, effect_names, custom_variable_names, p_cat_T1)

# save LaTeX table to a file
cat(latex_table_T3, file = "latex_table_T3.tex")

rm(list = ls())      

# ---- END ----

