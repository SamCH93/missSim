# Packages ----------------------------------------------------------------
library(tidyverse)    # data manipulation
library(here)         # reproducible file handling
library(lubridate)    # dealing with dates
library(readxl)       # excel files
# library(validate)     # validate dataset
library(janitor)
library(writexl)

# Load Data ---------------------------------------------------------------
dat_fb <- readxl::read_excel(here::here("data/lit_review_data_FB.xlsx"))
dat_bs <- readxl::read_excel(here::here("data/lit_review_data_BS.xlsx"))
dat_sp <- readxl::read_excel(here::here("data/lit_review_data_SP.xlsx"))
dat_al <- readxl::read_excel(here::here("data/lit_review_data_AL.xlsx"))

# Merge Data --------------------------------------------------------------
# Check compatibility
janitor::compare_df_cols_same(dat_fb, dat_bs, dat_sp, dat_al)

# Combine dataframes
all_res <- rbind(dat_fb, dat_bs, dat_sp, dat_al)

# Clean names
all_res <- janitor::clean_names(all_res)

# correct typo
all_res <- all_res |> 
  rename(q1_is_sim_study = q1_is_sim_stufy)



# Fix data structure -----------------------------------------------------
# Remove columns not used in the analyses
# only the general comments column for now
sim_res <- all_res |> 
  select(!c(q6_comments))

# Issue: Excel converted "2-3" to 44987
# sim_res <- sim_res |> 
#   mutate(issue = as.character(issue)) |> 
#   mutate(issue = ifelse(issue == 44987, "2-3", issue))

# Check for NAs in columns that should be filled
sim_res |> 
  dplyr::filter(q1_is_sim_study == "yes") |> 
  dplyr::filter(is.na(q2_mentions_missingness) | is.na(q7_coding_confidence))

# TODO add more here for the different q2 and q3 columns




# Reformat most cols to factor
non_factor_vars <- c("year", "issue", "doi")

sim_res_fac <- sim_res |> 
  mutate(across(!c(contains(non_factor_vars), contains("comment")),
         ~as.factor(.)))



# Save data
writexl::write_xlsx(sim_res_fac, path = here("data/sim_res_fac.xlsx"))
saveRDS(sim_res_fac, file = here("data/sim_res_fac.RDS"))

# LEGACY CODE
# # Alternative: delete strings from some columns, convert these to numeric
# numeric_vars <- c("nsimstudies_q2", "nconds_q6", "q7", "q8", "q11", "q14")
# 
# # Remove everything but digits
# sim_res_num <- sim_res_fac |> 
#   mutate(across(contains(numeric_vars),
#          ~ as.numeric(stringr::str_extract(., "\\d+"))))
# 
# # Save data
# writexl::write_xlsx(sim_res_num, path = here("data/sim_res_num.xlsx"))
# saveRDS(sim_res_num, file = here("data/sim_res_num.RDS"))


