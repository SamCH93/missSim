# Packages ----------------------------------------------------------------
library(tidyverse)    # data manipulation
library(here)         # reproducible file handling
library(lubridate)    # dealing with dates
library(readxl)       # excel files
# library(validate)     # validate dataset
library(janitor)
library(writexl)

# Load Data ---------------------------------------------------------------
dat_fb <- readxl::read_excel(here::here("data/lit_review_data - Frantisek.xlsx"))
dat_bs <- readxl::read_excel(here::here("data/lit_review_data_bjoern.xlsx"))
dat_sp <- readxl::read_excel(here::here("data/lit_review_data_SP.xlsx"))


# Merge Data --------------------------------------------------------------
# Check for non-overlapping cols
diff_cols <- setdiff(colnames(dat_sp), union(colnames(dat_fb), dat_bs))

# Delete redundant cols
dat_sp <- dat_sp[,!colnames(dat_sp) %in% diff_cols]
dat_bs <- dat_bs[,!colnames(dat_bs) %in% diff_cols]

# Find non-compatible cols
non_comp_cols <- janitor::compare_df_cols(dat_fb, 
                                          dat_bs, 
                                          dat_sp, 
                                          return = "mismatch")

# Convert numeric to character for safe merging
dat_bs <- dat_bs %>% 
  mutate(across(all_of(non_comp_cols$column_name),
                ~as.character(.)))
dat_sp <- dat_sp %>% 
  mutate(across(all_of(non_comp_cols$column_name),
                ~as.character(.)))

# Check compatibility again
janitor::compare_df_cols_same(dat_fb, dat_bs, dat_sp)

# Combine dataframes
all_res <- rbind(dat_fb, dat_bs, dat_sp)

# Clean names
all_res <- janitor::clean_names(all_res)

# Add categorization
sim_res <- all_res
sim_res$coding_type <- sapply(1:nrow(sim_res), function(i){
 if(sim_res$simstudy_q1[i] != "yes"){
   return(NA)
 }else if(tolower(sim_res$x1[i]) == "poor/medium"){
   return("agreement")
 }else{
   return("assessment")
 }
})


# Fix data structure -----------------------------------------------------
# Remove columns not used in the analyses
sim_res<- sim_res %>% 
  select(!c(comments, reproducibilitynote, dateofreview, userwrittenquote_q19))


# Journal: It is called MBR, not MBRM
sim_res$journal <- gsub("MBRM", "MBR", sim_res$journal)

# Issue: Excel converted "2-3" to 44987
sim_res <- sim_res %>% 
  mutate(issue = as.character(issue)) %>% 
  mutate(issue = ifelse(issue == 44987, "2-3", issue))

# Factors varied: recode 0 to 1 after discussion for consistency
# also, treat these as fully-factorial and not one-at-a-time
sim_res <- sim_res %>% 
  mutate(dgmfactorial_q7 = ifelse(factorsvaried_q7 == 0,
                                  "fully-factorial", dgmfactorial_q7)) %>% 
  mutate(factorsvaried_q7 = ifelse(factorsvaried_q7 == 0, 
                                 1, factorsvaried_q7))



# Software: Draw multiple mentions apart
sim_res <- sim_res %>% 
  tidyr::separate_wider_delim(cols = software_q18, 
                              delim = ", ",
                              names = c("software_1_q18", 
                                        "software_2_q18", 
                                        "software_3_q18"),
                              too_few = "align_start") 

# Seed: Fill in NAs with not found
sim_res <- sim_res %>% 
  mutate(seedprovided_q21 = tidyr::replace_na(seedprovided_q21, "not found"))


# Reformat most cols to factor
non_factor_vars <- c("year", "issue", "doi", "quoteaims_q3",
                     "note", "quote", "link", "comments", "dateofreview")

sim_res_fac <- sim_res %>% 
  mutate(across(!contains(non_factor_vars),
         ~as.factor(.)))

# If one factor was varied, convert dgmfactorial to "partially-factorial"
# decided to count these as partially/one-at-a-time for consistency
sim_res_fac <- sim_res_fac %>% 
  mutate(dgmfactorial_q7 = if_else(factorsvaried_q7 == 1 & !is.na(dgmfactorial_q7),
                                  "partially-factorial", dgmfactorial_q7))



# Save data
writexl::write_xlsx(sim_res_fac, path = here("data/sim_res_fac.xlsx"))
saveRDS(sim_res_fac, file = here("data/sim_res_fac.RDS"))


# Alternative: delete strings from some columns, convert these to numeric
numeric_vars <- c("nsimstudies_q2", "nconds_q6", "q7", "q8", "q11", "q14")

# Remove everything but digits
sim_res_num <- sim_res_fac %>% 
  mutate(across(contains(numeric_vars),
         ~ as.numeric(stringr::str_extract(., "\\d+"))))

# Save data
writexl::write_xlsx(sim_res_num, path = here("data/sim_res_num.xlsx"))
saveRDS(sim_res_num, file = here("data/sim_res_num.RDS"))


