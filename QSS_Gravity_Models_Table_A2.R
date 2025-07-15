# ---- CODE for Academic Freedom and International Research Collaboration ----
# ---- Appendix Table 2, Gravity Models ----

library(dplyr)
library(tibble)
library(reshape2)
library(abind)
library(tidyr)
library(fixest)

setwd("C:/Users/twhetsell3/OneDrive - Georgia Institute of Technology/Priority 1/academic_freedom/Github_Files_20250207/images")

load("Model_T1_M1_image.Rdata")

# ---- ST Weight ----

binary_matrices_list_ST_w <- list()

for (year in 1993:2022) {
  df_name <- paste0("df_country_matrix_", year, "_science_technology_dis")
  if (exists(df_name, where = .GlobalEnv)) {
    net_df <- get(df_name)
    net_matrix <- as.matrix(net_df)
    # Store the matrix in the correct list variable.
    binary_matrices_list_ST_w[[as.character(year)]] <- net_matrix
  } else {
    print(paste(df_name, "does not exist in the environment"))
  }
}


matrices_array_ST_w <- do.call("abind", c(binary_matrices_list_ST_w, along = 3))

long_data_ST <- melt(matrices_array_ST_w, 
                     varnames = c("o", "d", "time"), 
                     value.name = "dv")

# ---- SS Weight ----

load("Model_T1_M2_image.Rdata")

binary_matrices_list_SS_w <- list()

for (year in 1993:2022) {
  df_name <- paste0("df_country_matrix_", year, "_socialsciences_dis")
  if (exists(df_name, where = .GlobalEnv)) {
    net_df <- get(df_name)
    net_matrix <- as.matrix(net_df)
    # Store the matrix in the correct list variable.
    binary_matrices_list_SS_w[[as.character(year)]] <- net_matrix
  } else {
    print(paste(df_name, "does not exist in the environment"))
  }
}


matrices_array_SS_w <- do.call("abind", c(binary_matrices_list_SS_w, along = 3))

long_data_SS <- melt(matrices_array_SS_w, 
                     varnames = c("o", "d", "time"), 
                     value.name = "dv")

# ---- AH Weight ----

load("Model_T1_M3_image.Rdata")

binary_matrices_list_AH_w <- list()

for (year in 1993:2022) {
  df_name <- paste0("df_country_matrix_", year, "_arts_humanities_dis")
  if (exists(df_name, where = .GlobalEnv)) {
    net_df <- get(df_name)
    net_matrix <- as.matrix(net_df)
    # Store the matrix in the correct list variable.
    binary_matrices_list_AH_w[[as.character(year)]] <- net_matrix
  } else {
    print(paste(df_name, "does not exist in the environment"))
  }
}


matrices_array_AH_w <- do.call("abind", c(binary_matrices_list_AH_w, along = 3))

long_data_AH <- melt(matrices_array_AH_w, 
                     varnames = c("o", "d", "time"), 
                     value.name = "dv")

long_data_ST <- long_data_ST %>% mutate(dom = "ST")
long_data_SS <- long_data_SS %>% mutate(dom = "SS")
long_data_AH <- long_data_AH %>% mutate(dom = "AH")

gravity_data <- bind_rows(long_data_ST, long_data_SS, long_data_AH)

#

dist <-as.matrix((distmatrix1/1000)+1)
lang <-as.matrix(lang_matrix)
free <-as.data.frame(acadvar)
gdp <-as.data.frame(log(gdp_data_ppp_wide + 1))
pop <-as.data.frame(log(population_data_wide + 1))
urb <-as.data.frame(urbanization_data_wide)
pubs <-as.data.frame(log(WOS_pubs +1 ))
glob <-as.data.frame(glob_matrix)

# 

free_long <- free %>%
  rownames_to_column(var = "country_code") %>%  
  pivot_longer(
    cols = -country_code,     
    names_to = "time",         
    values_to = "free_value"   
  ) %>%
  mutate(time = as.integer(time)) 

gdp_long <- gdp %>%
  rownames_to_column(var = "country_code") %>% 
  pivot_longer(
    cols = -country_code,           
    names_to = "time",              
    values_to = "gdp_value"         
  ) %>%
  mutate(time = as.integer(time))   

pop_long <- pop %>%
  rownames_to_column(var = "country_code") %>%
  pivot_longer(
    cols = -country_code,
    names_to = "time",
    values_to = "pop_value"
  ) %>%
  mutate(time = as.integer(time))

urb_long <- urb %>%
  rownames_to_column(var = "country_code") %>%
  pivot_longer(
    cols = -country_code,
    names_to = "time",
    values_to = "urb_value"
  ) %>%
  mutate(time = as.integer(time))

pubs_long <- pubs %>%
  rownames_to_column(var = "country_code") %>%
  pivot_longer(
    cols = -country_code,
    names_to = "time",
    values_to = "pubs_value"
  ) %>%
  mutate(time = as.integer(time))

glob_long <- glob %>%
  rownames_to_column(var = "country_code") %>%  
  pivot_longer(
    cols = -country_code,     
    names_to = "time",         
    values_to = "glob_value"   
  ) %>%
  mutate(time = as.integer(time)) 


#

gravity_data <- gravity_data %>%
  left_join(free_long, by = c("o" = "country_code", "time")) %>%
  rename(o_free = free_value) %>%
  left_join(free_long, by = c("d" = "country_code", "time")) %>%
  rename(d_free = free_value) %>%
  
  left_join(gdp_long, by = c("o" = "country_code", "time")) %>%
  rename(o_gdp = gdp_value) %>%
  left_join(gdp_long, by = c("d" = "country_code", "time")) %>%
  rename(d_gdp = gdp_value) %>%
  
  left_join(pop_long, by = c("o" = "country_code", "time")) %>%
  rename(o_pop = pop_value) %>%
  left_join(pop_long, by = c("d" = "country_code", "time")) %>%
  rename(d_pop = pop_value) %>%
  
  left_join(urb_long, by = c("o" = "country_code", "time")) %>%
  rename(o_urb = urb_value) %>%
  left_join(urb_long, by = c("d" = "country_code", "time")) %>%
  rename(d_urb = urb_value) %>%
  
  left_join(pubs_long, by = c("o" = "country_code", "time")) %>%
  rename(o_pubs = pubs_value) %>%
  left_join(pubs_long, by = c("d" = "country_code", "time")) %>%
  rename(d_pubs = pubs_value) %>%
  
  left_join(glob_long, by = c("o" = "country_code", "time")) %>%
  rename(o_glob = glob_value) %>%
  left_join(glob_long, by = c("d" = "country_code", "time")) %>%
  rename(d_glob = glob_value)

# convert distance matrix 'dist' into a long-format data frame

distance_long <- data.frame(
  o = rep(rownames(dist), times = ncol(dist)),
  d = rep(colnames(dist), each = nrow(dist)),
  dist = as.vector(dist)
) %>%
  mutate(
    o = as.character(o),
    d = as.character(d),
# standardize dyad ordering so that the lower identifier comes first
    temp_o = pmin(o, d),
    temp_d = pmax(o, d),
    o = temp_o,
    d = temp_d
  ) %>%
  select(-temp_o, -temp_d) %>%
  distinct(o, d, .keep_all = TRUE)

# convert language matrix 'lang' into a long-format data frame
lang_long <- data.frame(
  o = rep(rownames(lang), times = ncol(lang)),
  d = rep(colnames(lang), each = nrow(lang)),
  lang = as.vector(lang)
) %>%
  mutate(
    o = as.character(o),
    d = as.character(d),
# standardize dyad ordering
    temp_o = pmin(o, d),
    temp_d = pmax(o, d),
    o = temp_o,
    d = temp_d
  ) %>%
  select(-temp_o, -temp_d) %>%
  distinct(o, d, .keep_all = TRUE)

# more data wrangling

gravity_data_1 <- gravity_data %>%
  mutate(
    o = as.character(o),
    d = as.character(d),
    dyad = paste0(pmin(o, d), "-", pmax(o, d))
  )

distance_long <- distance_long %>%
  mutate(
    o = as.character(o),
    d = as.character(d),
    dyad = paste0(pmin(o, d), "-", pmax(o, d))
  )

lang_long <- lang_long %>%
  mutate(
    o = as.character(o),
    d = as.character(d),
    dyad = paste0(pmin(o, d), "-", pmax(o, d))
  )

# data merge

gravity_data_1 <- gravity_data_1 %>%
  left_join(distance_long %>% select(dyad, dist), by = "dyad")

gravity_data_1 <- gravity_data_1 %>%
  left_join(lang_long %>% select(dyad, lang), by = "dyad")

gravity_data_1 <- gravity_data %>%
  left_join(distance_long, by = c("o" = "d", "d" = "o")) %>%
  left_join(lang_long, by = c("o" = "d", "d" = "o"))

gravity_data_1 <- gravity_data_1 %>%
  mutate(
    free_avg = (o_free + d_free) / 2,
    gdp_avg = (o_gdp + d_gdp) / 2,
    pop_avg = (o_pop + d_pop) / 2,
    urb_avg = (o_urb + d_urb) / 2,
    pubs_avg = (o_pubs + d_pubs) / 2,
    glob_avg = (o_glob + d_glob) / 2
    
  )

gravity_data_2<-gravity_data_1

gravity_data_2 <- gravity_data_2 %>%
  filter(o != d)

gravity_data_2 <- gravity_data_2 %>%
  arrange(o,d, time)

summary(gravity_data_2$dv)
hist(gravity_data_2$dv, main = "Distribution of DV", xlab = "dv")

gravity_data_2 <- gravity_data_2 %>%
  mutate(log_dv = log(dv + 1))

hist(gravity_data_2$log_dv, main = "Distribution of DV", xlab = "log_dv")

# more data wrangling

gravity_data_2 <- gravity_data_2 %>%
  mutate(
    o = toupper(trimws(o)),
    d = toupper(trimws(d)),
    dyad = paste0(pmin(o, d), "-", pmax(o, d))
  )

gravity_data_2 <- gravity_data_2 %>%
  mutate(
    free_avg_c = free_avg - mean(free_avg, na.rm = TRUE),
    time_c     = time - mean(time, na.rm = TRUE),
    glob_avg_c = glob_avg - mean(glob_avg, na.rm = TRUE),
    gdp_avg_c  = gdp_avg - mean(gdp_avg, na.rm = TRUE),
    pop_avg_c  = pop_avg - mean(pop_avg, na.rm = TRUE),
    urb_avg_c  = urb_avg - mean(urb_avg, na.rm = TRUE)
  )

# filter out nas

gravity_data_3 <- gravity_data_2 %>%
  distinct(dyad, time, dom, .keep_all = TRUE)

gravity_data_3 <- gravity_data_2 %>%
  filter(!is.na(dist), !is.na(lang))

# ---- gravity models ----

gravity1 <- fepois(
  dv ~ dist + lang + free_avg_c + glob_avg_c + gdp_avg_c + pop_avg_c + urb_avg_c | o + d + dom + time_c ,
  data = gravity_data_3, 
  cluster = ~ o + d + dom + time_c
)
summary(gravity1)

gravity2 <- fepois(
  dv ~ dist + lang + free_avg_c + dom + time_c + glob_avg_c + gdp_avg_c + pop_avg_c + urb_avg_c | o + d  ,
  data = gravity_data_3, 
  cluster = ~ o + d + dom + time_c
)
summary(gravity2)

gravity3 <- fepois(
  dv ~ dist + lang + free_avg_c * dom + time_c + glob_avg_c + gdp_avg_c + pop_avg_c + urb_avg_c | o + d  ,
  data = gravity_data_3, 
  cluster = ~ o + d + dom + time_c
)
summary(gravity3)

gravity4 <- fepois(
  dv ~ dist + lang + free_avg_c * dom * time_c + glob_avg_c + gdp_avg_c + pop_avg_c + urb_avg_c | o + d  ,
  data = gravity_data_3, 
  cluster = ~ o + d + dom + time_c
)
summary(gravity4)


# ---- tables ----

# store models list
model_group <- list(gravity1, gravity2, gravity3, gravity4)
model_names <- c("M1", "M2", "M3", "M4")

# define custom variable names exactly matching coefficient names in the models
custom_variable_names <- c(
  "dist",
  "lang",
  "free_avg_c",
  "domSS",
  "domST",
  "time_c",
  "glob_avg_c",
  "gdp_avg_c",
  "pop_avg_c",
  "urb_avg_c",
  "free_avg_c:domSS",
  "free_avg_c:domST",
  "free_avg_c:time_c",
  "domSS:time_c",
  "domST:time_c",
  "free_avg_c:domSS:time_c",
  "free_avg_c:domST:time_c"
)

# function to extract coefficients and standard errors
extract_coefs <- function(model) {
  est <- summary(model)$coeftable
  coef_df <- data.frame(
    Variable = rownames(est),
    Estimate = est[, "Estimate"],
    StdError = est[, "Std. Error"],
    p_value = est[, "Pr(>|z|)"]
  )
  rownames(coef_df) <- NULL
  return(coef_df)
}

# extract coefficients for each model
model_coefs <- lapply(model_group, extract_coefs)

# function to generate significance stars
get_stars <- function(pval) {
  ifelse(pval < 0.001, "***",
         ifelse(pval < 0.01, "**",
                ifelse(pval < 0.05, "*", "ns")))
}

generate_gravity_latex <- function(model_coefs, model_names, custom_names) {
  latex_table <- "\\begin{table}[H]\n\\centering\n\\caption{Gravity Model Results}\n\\begin{tabular}{lcccc}\n"
  latex_table <- paste(latex_table, "Variable & ", paste(model_names, collapse = " & "), " \\\\\n\\hline\n", sep = "")
  
  # loop through each custom variable name for coefficient rows
  for (var in custom_names) {
    row_latex <- paste(var, " & ")
    se_row_latex <- " & "
    for (model in model_coefs) {
      if (var %in% model$Variable) {
        idx <- which(model$Variable == var)
        est <- round(model$Estimate[idx], 3)
        se <- round(model$StdError[idx], 3)
        stars <- get_stars(model$p_value[idx])
        est_with_sig <- paste0(est, " ", stars)
      } else {
        est_with_sig <- ""
        se <- ""
      }
      row_latex <- paste(row_latex, est_with_sig, " & ", sep = "")
      se_row_latex <- paste(se_row_latex, ifelse(se == "", "", paste0("(", se, ")")), " & ", sep = "")
    }
    row_latex <- substr(row_latex, 1, nchar(row_latex)-2)
    se_row_latex <- substr(se_row_latex, 1, nchar(se_row_latex)-2)
    row_latex <- paste(row_latex, " \\\\\n", sep = "")
    se_row_latex <- paste(se_row_latex, " \\\\\n", sep = "")
    latex_table <- paste(latex_table, row_latex, se_row_latex, sep = "")
  }
  
  # append horizontal line before table closing
  latex_table <- paste(latex_table, "\\hline\n", sep = "")
  
  latex_table <- paste(latex_table, "\\end{tabular}\n", sep = "")
  
  latex_table <- paste(latex_table, "\\vspace{0.2cm}\\begin{center} \\small{\\textit{Table Notes: Estimates rounded to 3 decimals. Standard errors in parentheses. ns=p-value $>$ 0.05, *=p-value $<$ 0.05, **=p-value $<$ 0.01, ***=p-value $<$ 0.001.}} \\end{center}", sep = "")
  
  latex_table <- paste(latex_table, "\\end{table}\n", sep = "")
  
  return(latex_table)
}

# generate LaTeX table code without model statistics
latex_table_gravity <- generate_gravity_latex(model_coefs, model_names, custom_variable_names)

# write LaTeX table code to a file
cat(latex_table_gravity, file = "gravity_models_table.tex")




