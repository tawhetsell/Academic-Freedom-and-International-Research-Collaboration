# ---- CODE for Academic Freedom and International Research Collaboration  ----
# ---- Appendix Table 1, Robustness Checks ----

setwd("C:/Users/twhetsell3/OneDrive - Georgia Institute of Technology/Priority 1/academic_freedom")

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

# import gdp per cap data - NY.GDP.MKTP.KD , NY.GDP.MKTP.PP.CD https://datatopics.worldbank.org/world-development-indicators/stories/purchasing-power-parities-putting-global-public-good-socioeconomic-analyses.html

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

# create siena variables

dist <- coDyadCovar(log((distmatrix1/1000)+1))
free <- varCovar(as.matrix(acadvar))
gdp <- varCovar(log(as.matrix(gdp_data_ppp_wide + 1)))
pop <- varCovar(log(as.matrix(population_data_wide + 1)))
urb <- varCovar(as.matrix(urbanization_data_wide))
pubs <- varCovar(log(as.matrix(WOS_pubs +1 )))

lang <- coDyadCovar(lang_matrix)
glob <- varCovar(as.matrix(glob_matrix))

# ---- pubs test ----

# lag model 

IRCnets_ST  <- sienaDependent(matrices_array_ST)
IRC_ST  <- sienaDataCreate(IRCnets_ST, dist, lang, free, glob, gdp, pop, urb, pubs)
effects_ST <- getEffects(IRC_ST)

# model spec

effects_ST <- includeEffects(effects_ST, egoPlusAltX, simX, interaction1='free')
effects_ST <- includeEffects(effects_ST, egoPlusAltX, simX, interaction1='glob')
effects_ST <- includeEffects(effects_ST, egoPlusAltX, simX, interaction1='gdp')
effects_ST <- includeEffects(effects_ST, egoPlusAltX, simX, interaction1='pop')
effects_ST <- includeEffects(effects_ST, egoPlusAltX, simX, interaction1='urb')
effects_ST <- includeEffects(effects_ST, egoPlusAltX, simX, interaction1='pubs')
effects_ST <- includeEffects(effects_ST, X, interaction1='lang')
effects_ST <- includeEffects(effects_ST, X, interaction1='dist')
effects_ST <- includeEffects(effects_ST, degPlus, gwesp)

# run siena model

IRC_algo <- sienaAlgorithmCreate(n3=9000, seed=101, modelType = c(IRCnets_ST=3))
results_T1_M1_pubs <- siena07(IRC_algo, data=IRC_ST , effects = effects_ST, useCluster = TRUE, nbrNodes = 8)
results_T1_M1_pubs

save(results_T1_M1_pubs, file ="results_T1_M1_pubs.Rdata")

# ---- lag test ----

free_df <- as.data.frame(as.matrix(acadvar))
free_df_lag  <- cbind(New = NA, free_df) 
free_df_lag <- free_df_lag[, -ncol(free_df_lag)]
colnames(free_df_lag) <- 1993:2022 
free_lag <- varCovar(as.matrix(free_df_lag))

# lag model 

rm(IRCnets_ST )
rm(IRC_ST )
rm(effects_ST)
rm(Model_T1_M1)

IRCnets_ST  <- sienaDependent(matrices_array_ST)
IRC_ST  <- sienaDataCreate(IRCnets_ST, dist, lang, free_lag, glob, gdp, pop, urb)
effects_ST <- getEffects(IRC_ST)

# model spec

effects_ST <- includeEffects(effects_ST, egoPlusAltX, simX, interaction1='free_lag')
effects_ST <- includeEffects(effects_ST, egoPlusAltX, simX, interaction1='glob')
effects_ST <- includeEffects(effects_ST, egoPlusAltX, simX, interaction1='gdp')
effects_ST <- includeEffects(effects_ST, egoPlusAltX, simX, interaction1='pop')
effects_ST <- includeEffects(effects_ST, egoPlusAltX, simX, interaction1='urb')
effects_ST <- includeEffects(effects_ST, X, interaction1='lang')
effects_ST <- includeEffects(effects_ST, X, interaction1='dist')
effects_ST <- includeEffects(effects_ST, degPlus, gwesp)

# run siena model

IRC_algo <- sienaAlgorithmCreate(n3=9000, seed=101, modelType = c(IRCnets_ST=3))
results_T1_M1_Lag <- siena07(IRC_algo, data=IRC_ST , effects = effects_ST, useCluster = TRUE, nbrNodes = 8)
results_T1_M1_Lag

save(results_T1_M1_Lag, file ="results_T1_M1_Lag.Rdata")

# load("results_T1_M1_Lag")

# ---- coev test ----

# discretize free 

acadvar_m  <- as.matrix(acadvar)

acadvar_d <- acadvar_m
acadvar_d[is.na(acadvar_d)] <- 0
all_values <- as.vector(acadvar_d)
quantiles <- quantile(all_values, probs = seq(0, 1, length.out = 11), na.rm = TRUE)
cat("Quantile breaks for the entire data set:\n")
print(quantiles)
cat("\n")
acadvar_d_d <- data.frame(matrix(ncol = ncol(acadvar_d), nrow = nrow(acadvar_d)))
colnames(acadvar_d_d) <- colnames(acadvar_d)
rownames(acadvar_d_d) <- rownames(acadvar_d)

for (i in 1:ncol(acadvar_d)) {
  column_values <- acadvar_d[, i]
  acadvar_d_d[, i] <- as.numeric(cut(column_values, 
                                     breaks = quantiles, 
                                     labels = 1:10, 
                                     include.lowest = TRUE, 
                                     right = FALSE))}

free_d <- as.matrix(acadvar_d_d)
free_d <- sienaDependent(free_d, type="behavior")

#

rm(IRCnets_ST )
rm(IRC_ST )
rm(effects_ST)

IRCnets_ST  <- sienaDependent(matrices_array_ST)
IRC_ST  <- sienaDataCreate(IRCnets_ST, dist, lang, free_d, free, glob, gdp, pop, urb)
effects_ST <- getEffects(IRC_ST)

# model spec

effects_ST <- includeEffects(effects_ST, egoPlusAltX, simX, interaction1='free')
effects_ST <- includeEffects(effects_ST, egoPlusAltX, simX, interaction1='glob')
effects_ST <- includeEffects(effects_ST, egoPlusAltX, simX, interaction1='gdp')
effects_ST <- includeEffects(effects_ST, egoPlusAltX, simX, interaction1='pop')
effects_ST <- includeEffects(effects_ST, egoPlusAltX, simX, interaction1='urb')
effects_ST <- includeEffects(effects_ST, X, interaction1='lang')
effects_ST <- includeEffects(effects_ST, X, interaction1='dist')
effects_ST <- includeEffects(effects_ST, degPlus, gwesp)

# effects on behavior

effects_ST <- includeEffects(effects_ST, name="free_d", outdeg, interaction1="IRCnets_ST")
effects_ST <- includeEffects(effects_ST, name="free_d", avAlt, interaction1="IRCnets_ST")

# run siena model

IRC_algo_coev <- sienaAlgorithmCreate(n3=9000, seed=101, modelType = c(IRCnets_ST=3))
results_T1_M1_Coev <- siena07(IRC_algo_coev, data=IRC_ST , effects = effects_ST, useCluster = TRUE, nbrNodes = 8)
results_T1_M1_Coev

save(results_T1_M1_Coev, file ="results_T1_M1_Coev.Rdata")

# ---- with more covariates ----

diag(dist) <- NA  
mean_dist <- rowMeans(dist, na.rm = TRUE)
m_dist <- coCovar(mean_dist)

diag(lang) <- NA  
mean_lang <- rowMeans(lang, na.rm = TRUE)
m_lang <- coCovar(mean_lang)

IRCnets_ST  <- sienaDependent(matrices_array_ST)
IRC_ST  <- sienaDataCreate(IRCnets_ST, dist, lang, m_dist, free_d, free, glob, gdp, pop, urb)
effects_ST <- getEffects(IRC_ST)

# model spec

effects_ST <- includeEffects(effects_ST, egoPlusAltX, simX, interaction1='free')
effects_ST <- includeEffects(effects_ST, egoPlusAltX, simX, interaction1='glob')
effects_ST <- includeEffects(effects_ST, egoPlusAltX, simX, interaction1='gdp')
effects_ST <- includeEffects(effects_ST, egoPlusAltX, simX, interaction1='pop')
effects_ST <- includeEffects(effects_ST, egoPlusAltX, simX, interaction1='urb')
effects_ST <- includeEffects(effects_ST, X, interaction1='lang')
effects_ST <- includeEffects(effects_ST, X, interaction1='dist')
effects_ST <- includeEffects(effects_ST, degPlus, gwesp)

# effects on behavior

effects_ST <- includeEffects(effects_ST, name="free_d", outdeg, interaction1="IRCnets_ST")
effects_ST <- includeEffects(effects_ST, name="free_d", avAlt, interaction1="IRCnets_ST")
effects_ST <- includeEffects(effects_ST, name="free_d", effFrom, interaction1="m_lang")
effects_ST <- includeEffects(effects_ST, name="free_d", effFrom, interaction1="m_dist")
effects_ST <- includeEffects(effects_ST, name="free_d", effFrom, interaction1="glob")
effects_ST <- includeEffects(effects_ST, name="free_d", effFrom, interaction1="gdp")
effects_ST <- includeEffects(effects_ST, name="free_d", effFrom, interaction1="pop")
effects_ST <- includeEffects(effects_ST, name="free_d", effFrom, interaction1="urb")

# run siena model

IRC_algo_coev <- sienaAlgorithmCreate(n3=9000, seed=101, modelType = c(IRCnets_ST=3))
results_T1_M1_Coev_a <- siena07(IRC_algo_coev, data=IRC_ST , effects = effects_ST, useCluster = TRUE, nbrNodes = 8)
results_T1_M1_Coev_a

save(results_T1_M1_Coev_a, file ="results_T1_M1_Coev_a.Rdata")

# ---- no usa or chn ----

rm(list = ls())     

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

# ID file path to matrices generated without USA or CHN ties, excluded in the parsing script

file_paths <- list.files(path = "C:/Users/twhetsell3/OneDrive - Georgia Institute of Technology/Priority 1/academic_freedom/WOS_HEADING_CAT_PY/FINAL_PY_20253006/NO_ST", pattern = "*science_technology_dis.csv", full.names = TRUE)

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
  matrix <- matrix[!(rownames(matrix) %in% c("SML", "HKG", "CHN", "USA")), !(colnames(matrix) %in% c("SML", "HKG", "CHN", "USA"))]
  assign(matrix_name, matrix, envir = .GlobalEnv)
}

# remove HKG SML USA CHN from acadvar

acadvar <- acadvar[!(rownames(acadvar) %in% c("SML", "HKG", "USA", "CHN")), ]

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
  select(iso3c, year, NY.GDP.PCAP.CD) %>%  # option NY.GDP.PCAP.CD or NY.GDP.MKTP.PP.CD
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

# create siena variables

dist <- coDyadCovar(log((distmatrix1/1000)+1))
free <- varCovar(as.matrix(acadvar))
gdp <- varCovar(log(as.matrix(gdp_data_ppp_wide + 1)))
pop <- varCovar(log(as.matrix(population_data_wide + 1)))
urb <- varCovar(as.matrix(urbanization_data_wide))
pubs <- varCovar(log(as.matrix(WOS_pubs +1 )))

lang <- coDyadCovar(lang_matrix)
glob <- varCovar(as.matrix(glob_matrix))

IRCnets_ST  <- sienaDependent(matrices_array_ST)
IRC_ST  <- sienaDataCreate(IRCnets_ST, dist, lang, free, glob, gdp, pop, urb)
effects_ST <- getEffects(IRC_ST)

# model spec

effects_ST <- includeEffects(effects_ST, egoPlusAltX, simX, interaction1='free')
effects_ST <- includeEffects(effects_ST, egoPlusAltX, simX, interaction1='glob')
effects_ST <- includeEffects(effects_ST, egoPlusAltX, simX, interaction1='gdp')
effects_ST <- includeEffects(effects_ST, egoPlusAltX, simX, interaction1='pop')
effects_ST <- includeEffects(effects_ST, egoPlusAltX, simX, interaction1='urb')
effects_ST <- includeEffects(effects_ST, X, interaction1='lang')
effects_ST <- includeEffects(effects_ST, X, interaction1='dist')
effects_ST <- includeEffects(effects_ST, degPlus, gwesp)

# run siena model

IRC_algo <- sienaAlgorithmCreate(n3=9000, seed=101, modelType = c(IRCnets_ST=3))
Model_T1_M1_noUSA <- siena07(IRC_algo, data=IRC_ST , effects = effects_ST, useCluster = TRUE, nbrNodes = 8)
Model_T1_M1_noUSA

# siena.table(Model_T1_M1, sig=TRUE)
save(Model_T1_M1_noUSA, file="Model_T1_M1_noUSA.Rdata")

# ---- latex tables ---- 

setwd("C:/Users/twhetsell3/OneDrive - Georgia Institute of Technology/Priority 1/academic_freedom/Github_Files_20250207/models")

# load the models
load("Model_T1_M1_noUSA.RData")
load("results_T1_M1_Lag.RData")
load("results_T1_M1_pubs.RData")
load("results_T1_M1_Coev.RData")

# define custom variable names, had to fix these manually afterwards in overleaf
custom_variable_names <- c(
  "degree (density)", 
  "GWESP (69)", 
  "degree act+pop", 
  "dist", 
  "lang", 
  "free ego and alt", 
  "free similarity", 
  "glob ego and alt", 
  "glob similarity", 
  "gdp ego and alt", 
  "gdp similarity", 
  "pop ego and alt", 
  "pop similarity", 
  "urb ego and alt", 
  "urb similarity", 
  "pubs ego and alt", 
  "pubs similarity", 
  "free_lag ego and alt", 
  "free_lag similarity", 
  "free_d linear shape", 
  "free_d quadratic shape", 
  "free_d degree", 
  "free_d average alter"
)

# define four models and names 
model_group_T4 <- list(Model_T1_M1_noUSA, results_T1_M1_Lag, results_T1_M1_pubs, results_T1_M1_Coev)
model_names_T4 <- c("NoUSAorCHN", "LaggedFree", "w/Pubs", "FreeCoevolution")

cat("Number of models in model_group_T4:", length(model_group_T4), "\n")
cat("Model names:", model_names_T4, "\n")

# alignment function for each model 
align_model_results <- function(model, custom_names) {
  model_effects <- model$effects$effectName
  aligned_est <- rep(NA, length(custom_names))
  aligned_se  <- rep(NA, length(custom_names))
  
  idx <- match(custom_names, model_effects)
  if (any(!is.na(idx))) {
    aligned_est[!is.na(idx)] <- model$theta[idx[!is.na(idx)]]
    aligned_se[!is.na(idx)]  <- model$se[idx[!is.na(idx)]]
  }
  
  return(list(est = aligned_est, se = aligned_se))
}

# align results for each model 
aligned_results <- lapply(model_group_T4, align_model_results, custom_names = custom_variable_names)

# compute p-values and significance markers 
p_cat_aligned <- vector("list", length(model_group_T4))
for (i in 1:length(model_group_T4)) {
  model_summary <- summary(model_group_T4[[i]])
  model_effects <- model_group_T4[[i]]$effects$effectName
  idx <- match(custom_variable_names, model_effects)
  
  z_scores <- model_summary$theta / model_summary$se
  p_vals <- 2 * (1 - pnorm(abs(z_scores)))
  aligned_p <- rep(NA, length(custom_variable_names))
  aligned_p[!is.na(idx)] <- p_vals[idx[!is.na(idx)]]
  
  p_cat <- ifelse(aligned_p < 0.001, "***",
                  ifelse(aligned_p < 0.01, "**",
                         ifelse(aligned_p < 0.05, "*", ifelse(is.na(aligned_p), "", "ns"))))
  p_cat_aligned[[i]] <- p_cat
}

# function to generate the LaTeX table
generate_latex_table <- function(aligned_results, model_names, custom_names, p_cat_aligned, conv_ratios, iterations) {
  n_models <- length(aligned_results)
  
  # build column specification (one for the variable names + one for each model)
  col_spec <- paste0("l", paste(rep("c", n_models), collapse=""))
  
  table_latex <- "\\begin{table}[H]\n\\centering\n\\caption{Sensitivity Tests}\n"
  table_latex <- paste0(table_latex, "\\begin{tabular}{", col_spec, "}\n")
  table_latex <- paste0(table_latex, "Variable & ", paste(model_names, collapse=" & "), " \\\\\n\\hline\n")
  
  for (i in 1:length(custom_names)) {
    row_line <- custom_names[i]
    row_line2 <- ""
    
    for (j in 1:n_models) {
      est <- aligned_results[[j]]$est[i]
      se <- aligned_results[[j]]$se[i]
      p_cat <- p_cat_aligned[[j]][i]
      
      if (is.na(est)) {
        row_line <- paste0(row_line, " & ")
        row_line2 <- paste0(row_line2, " & ")
      } else {
        row_line <- paste0(row_line, " & ", round(est, 3), " ", p_cat)
        row_line2 <- paste0(row_line2, " & (", round(se, 3), ")")
      }
    }
    table_latex <- paste0(table_latex, row_line, " \\\\\n", row_line2, " \\\\\n")
  }
  
  conv_line <- "Convergence Ratio"
  iter_line <- "Iterations"
  for (j in 1:n_models) {
    conv_line <- paste0(conv_line, " & ", round(conv_ratios[j], 3))
    iter_line <- paste0(iter_line, " & ", iterations[j])
  }
  table_latex <- paste0(table_latex, conv_line, " \\\\\n", iter_line, " \\\\\n")
  
  table_latex <- paste0(table_latex, "\\hline\n\\end{tabular}\n")
  table_latex <- paste0(table_latex, 
                        "\\vspace{0.2cm}\\begin{center} \\small{\\textit{Table Notes: The estimates are rounded to 3 decimals. Standard errors in parentheses. ns=p-value \\textgreater{} 0.5, *=p-value \\textless{} 0.5, **=p-value \\textless{} 0.01, ***=p-value \\textless{} 0.001. Convergence ratio \\textless{} 0.25 indicates good convergence.}} \\end{center}\n")
  table_latex <- paste0(table_latex, "\\end{table}\n")
  
  return(table_latex)
}

# extract convergence ratios and iterations
conv_ratios <- sapply(model_group_T4, function(model) model$tconv.max)
iterations <- sapply(model_group_T4, function(model) model$n)

# Generate the LaTeX table code for the four models 
latex_table <- generate_latex_table(aligned_results, model_names_T4, custom_variable_names, p_cat_aligned, conv_ratios, iterations)

# save the table to a file 
cat(latex_table, file = "latex_table_app.tex")
