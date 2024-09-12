# Packages ----------------------------------------------------------------
library(lubridate)    # dealing with dates
library(readxl)       # excel files
# library(validate)     # validate dataset
library(janitor)
library(writexl)
library(tidyverse)    # data manipulation

# Load Data ---------------------------------------------------------------
dat_fb <- readxl::read_excel(here("data/lit_review_data_FB.xlsx"))
dat_bs <- readxl::read_excel(here("data/lit_review_data_BS.xlsx"))
dat_sp <- readxl::read_excel(here("data/lit_review_data_SP.xlsx"))
dat_al <- readxl::read_excel(here("data/lit_review_data_AL.xlsx"))

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

# remove irrelevant rows
all_res <- all_res |> 
  filter(!is.na(doi))

# remove old econ papers
all_res <- all_res |> 
  filter(journal != "EM")

# remove pilot studies
all_res <- all_res |> 
  filter(!grepl("*p", id)) |> 
  filter(q6_comments != "pilot" | is.na(q6_comments))

# Filter erroneous duplicate (sim study was not in that issue)
all_res <- subset(all_res, !c(doi == "https://doi.org/10.1037/met0000420" & reviewer == "SP"))

# Filter out duplicates (one reviewer accidentally coded the wrong issue)
all_res <- subset(all_res, !c(journal == "PM" & reviewer == "AL" & year == "2021" & issue == "5"))

# Fix data structure -----------------------------------------------------
# Remove columns not used in the analyses
# only the general comments column for now
sim_res <- all_res |> 
  select(!c(q6_comments))


# Check for NA ------------------------------------------------------------
# Check for NAs in columns that should be filled
# Q1: sim study should be coded for Q2 and Q7
sim_res |> 
  dplyr::filter(q1_is_sim_study == "yes") |> 
  dplyr::filter(is.na(q2_mentions_missingness) | is.na(q7_coding_confidence))

# Q2: studies that mention missingness should have one place where it is mentioned
sim_res |> 
  dplyr::filter(q1_is_sim_study == "yes") |> 
  dplyr::filter(q2_mentions_missingness == "yes") |> 
  # select columns that show different places of missingness mentioning
  select(c(reviewer, doi, all_of(contains("mentions_missingness_")))) |> 
  rowwise() |> 
  mutate(missingness_mentioned = sum(c_across(contains("mentions_missingness_")) == TRUE)) |> 
  filter(missingness_mentioned == 0)


# Q2: studies that mention missingness should mention how it is summarized
sim_res |> 
  dplyr::filter(q1_is_sim_study == "yes") |> 
  dplyr::filter(q2_mentions_missingness == "yes") |> 
  # select columns that show different places of missingness mentioning
  select(reviewer, doi, q2_2_missingness_summarized) |> 
  filter(is.na(q2_2_missingness_summarized))


# Q3: studies that mention missingness should mention how it handled
# if NA, we code as not reported
sim_res |> 
  dplyr::filter(q1_is_sim_study == "yes") |> 
  dplyr::filter(q2_mentions_missingness == "yes") |> 
  # select columns that show different places of missingness mentioning
  select(reviewer, doi, q3_report_dealing_with_missingness) |> 
  filter(is.na(q3_report_dealing_with_missingness))

# change to "no"
sim_res <- sim_res |> 
  mutate(q3_report_dealing_with_missingness = ifelse(is.na(q3_report_dealing_with_missingness) & 
                                                       q1_is_sim_study == "yes" &
                                                       q2_mentions_missingness == "yes", 
                                                     "no", 
                                                     q3_report_dealing_with_missingness))



# Q3: If authors reported how they dealt with it
# should report which method used and if it is justified
sim_res |> 
  dplyr::filter(q1_is_sim_study == "yes") |> 
  dplyr::filter(q2_mentions_missingness == "yes") |> 
  dplyr::filter(q3_report_dealing_with_missingness == "yes") |> 
  # which method used
  select(reviewer, doi, q3_1_method_dealing_with_missingness, q3_2_method_justification) |> 
  filter(is.na(q3_1_method_dealing_with_missingness) | is.na(q3_2_method_justification))

# Q3: if it was justified, should state which justification was give
sim_res |> 
  dplyr::filter(q1_is_sim_study == "yes") |> 
  dplyr::filter(q2_mentions_missingness == "yes") |> 
  dplyr::filter(q3_report_dealing_with_missingness == "yes") |> 
  dplyr::filter(q3_2_method_justification == "yes") |> 
  # which justification was given
  select(reviewer, doi, q3_3_type_justification) |> 
  filter(is.na(q3_3_type_justification))

# Q4: studies that are a sim study should state if code was available
sim_res |> 
  dplyr::filter(q1_is_sim_study == "yes") |> 
  # select columns that show different places of missingness mentioning
  select(reviewer, doi, q4_code_available) |> 
  filter(is.na(q4_code_available))


# Reformat and save -------------------------------------------------------
# Reformat most cols to factor
# non_factor_vars <- c("year", "issue", "doi")

# sim_res_fac <- sim_res |> 
#   mutate(across(!c(contains(non_factor_vars), contains("comment")),
#          ~as.factor(.)))


# convert to logical
sim_res <- sim_res |> 
  dplyr::mutate(across(c(
    contains("mentions_missingness_"),
    q2_1a_missingness_sumarized_text,
    q2_1b_missingness_sumarized_table, 
    q2_1c_missingness_sumarized_visualization
  ),
         ~as.logical(.)))
  
# convert other columns to factor
sim_res <- sim_res |> 
  dplyr::mutate(across(c(
    q1_is_sim_study,
    q2_mentions_missingness,
    q3_report_dealing_with_missingness,
    q3_1_method_dealing_with_missingness,
    q3_2_method_justification,
    q3_3_type_justification,
    q4_code_available,
    q7_coding_confidence
  ),
         ~as.factor(.)))


# Save data
writexl::write_xlsx(sim_res, path = here("data/sim_res.xlsx"))
saveRDS(sim_res, file = here("data/sim_res.RDS"))

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


