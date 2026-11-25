# Burden of disease calculation
library(tidyverse)

# Required data:
# 1. (a) - (h): Influenza counts per site, per age group, per week
# 2. (i) - (l): Denominator & BoD calculation

# 1. (a) - (h): Influenza counts per site, per age group, per week #############
# SOURCE: WHO Manual, appendix WS7 (p113), part 3.7 (p49)
df <- read.csv("inputs/influenza_cleaned_data.csv",
               sep = ",",
               check.names = TRUE)

df_posneg <- df %>% 
  dplyr::filter(influ_pos %in% c("Positive", "Negative"))

# (a) Number of SARI cases positive for Influenza A(H1N1)pdm09
# (b) Number of SARI cases positive for Influenza A(H3N2)
# (c) Number of SARI cases positive for unsubtyped Influenza A
# (d) Number of SARI cases positive for Influenza B
# (e) Number of SARI cases positive for other human influenza viruses
influ_cases_per_virus_type <- df_posneg %>% 
  filter(
    influ_pos == "Positive",
    !is.na(influ_pos),
    !is.na(yearWeek),
    !is.na(age_group),
    !is.na(hospital)
  ) %>%
  group_by(
    yearWeek,
    age_group,
    hospital,
    influ_subtype
  ) %>%
  summarise(
    n = n(),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = influ_subtype,
    values_from = n,
    # names_prefix = "count_"
  ) %>% 
  rowwise() %>%
  dplyr::transmute(
    yearWeek = as.Date(yearWeek),
    age_group = age_group,
    hospital = hospital,
    n_A_H1N1_pdm09 = coalesce(`A (H1N1) pdm09`, 0),
    n_A_H3N2 = coalesce(`A (H3N2)`, 0),
    n_A_Nontypeable = coalesce(`A (Nontypeable)`, 0),
    n_B = coalesce(B, 0),
    
    # (f) Total number of SARI cases positive for all human influenza viruses = (a)+(b)+(c)+(d)+(e)
    n_positive_influ = n_A_H1N1_pdm09 + n_A_H3N2 + n_A_Nontypeable + n_B
  ) %>%
  # view() %>% 
  glimpse()
  

# (f) Total number of SARI cases positive for all human influenza viruses = (a)+(b)+(c)+(d)+(e)
# (from positive-negative data)
influ_prev_week_age_hospital <- df_posneg %>%
  filter(
    !is.na(influ_pos),
    !is.na(yearWeek),
    !is.na(age_group),
    !is.na(hospital)
  ) %>%
  group_by(
    yearWeek,
    age_group,
    hospital
  ) %>%
  summarise(
    n_positive = sum(influ_pos == "Positive", na.rm = TRUE),
    n_negative = sum(influ_pos == "Negative", na.rm = TRUE),
    n_tested = n_positive + n_negative,
    .groups = "drop"
  ) %>%
  rowwise() %>%
  mutate(
    # Positivity proportion
    proportion = n_positive/n_tested*100,
    
    # Exact Clopper-Pearson 95% CI
    ci_low_cp = binom.test(
      n_positive,
      n_tested
    )$conf.int[1]*100,
    
    ci_high_cp = binom.test(
      n_positive,
      n_tested
    )$conf.int[2]*100
  ) %>%
  ungroup() %>% 
  mutate(
    yearWeek = as.Date(yearWeek),
    age_group = factor(age_group,
                       levels = c("0 to <6 months", "6m to <1 year",
                                  "1 to <2 years", "2 to <5 years",
                                  "5 to <15 years", "15 to <50 years",
                                  "50 to <65 years", "65+ years"
                       )
    )
  ) %>% 
  # extract sites name from df
  dplyr::left_join(
    df %>% 
      dplyr::select(hospital, sites) %>% 
      distinct()
    ,
    by = "hospital"
  ) %>% 
  glimpse()


# (g) Total number of SARI cases in who clinical specimens were collected for laboratory confirmation of influenza
sari_prev_week_age_hospital <- df %>%
  filter(
    !is.na(influ_pos),
    !is.na(yearWeek),
    !is.na(age_group),
    !is.na(hospital)
  ) %>%
  mutate(
    # sari_pos = "SARI positive",
    sari_lab = ifelse(influ_pos != "No specimen collected", "Positive", "Negative")
  ) %>% 
  group_by(
    yearWeek,
    age_group,
    hospital
  ) %>%
  summarise(
    n_positive = sum(sari_lab == "Positive", na.rm = TRUE),
    n_negative = sum(sari_lab == "Negative", na.rm = TRUE),
    n_tested = n_positive + n_negative,
    .groups = "drop"
  ) %>%
  rowwise() %>%
  mutate(
    # (h) Proportion of influenza positive SARI (%) = [(f)/(g)] x 100
    # Positivity proportion
    proportion = n_positive/n_tested*100,
    
    # Exact Clopper-Pearson 95% CI
    ci_low_cp = binom.test(
      n_positive,
      n_tested
    )$conf.int[1]*100,
    
    ci_high_cp = binom.test(
      n_positive,
      n_tested
    )$conf.int[2]*100
  ) %>%
  ungroup() %>% 
  mutate(
    yearWeek = as.Date(yearWeek),
    age_group = factor(age_group,
                       levels = c("0 to <6 months", "6m to <1 year",
                                  "1 to <2 years", "2 to <5 years",
                                  "5 to <15 years", "15 to <50 years",
                                  "50 to <65 years", "65+ years"
                       )
    )
  ) %>% 
  # extract sites name from df
  dplyr::left_join(
    df %>% 
      dplyr::select(hospital, sites) %>% 
      distinct()
    ,
    by = "hospital"
  ) %>% 
  glimpse()

influ_sari_estimation_weekly <- dplyr::full_join(
  influ_prev_week_age_hospital %>% 
    dplyr::transmute(
      yearWeek = yearWeek,
      age_group = age_group,
      hospital = hospital,
      
      # (h) Proportion of influenza positive SARI (%) = [(f)/(g)] x 100
      n_positive_influ = n_positive,
      n_tested_influ = n_tested,
      prop_influ = proportion,
      ci_influ_low = ci_low_cp,
      ci_influ_high = ci_high_cp
    )
  ,
  sari_prev_week_age_hospital %>% 
    dplyr::transmute(
      yearWeek = yearWeek,
      age_group = age_group,
      hospital = hospital,
      
      total_sari = n_tested
    )
  ,
  by = c("yearWeek", "age_group", "hospital")
) %>% 
  dplyr::mutate(
    estimation = prop_influ/100*total_sari,
    ci_low_cp = ci_influ_low/100*total_sari,
    ci_high_cp = ci_influ_high/100*total_sari,
    
  ) %>% 
  dplyr::left_join(
    df %>% 
      dplyr::select(hospital, sites) %>% 
      distinct()
    ,
    by = "hospital"
  ) %>% 
  glimpse()

# combine data for reporting process
final_influ_df <- influ_cases_per_virus_type %>% 
  dplyr::full_join(
    influ_sari_estimation_weekly
    ,
    by = c("yearWeek", "age_group", "hospital", "n_positive_influ")
  ) %>% 
  glimpse()


# 2. (i) - (l): Denominator & BoD calculation ##################################
# (i) Total number of SARI cases (from all hospital wards likely to admit SARI cases and includes those cases not tested for influenza or negative for influenza on laboratory tests) 
# (j) Estimated annual number of influenza-associated SARI cases in the hospital = (i) x (h)
# (k) Total annual number of hospital admissions (all-cause) 
# (l) Proportional contribution of influenza-associated SARI to all hospital admissions in year _________(%) = [(j)/(k)] x100

sari_all <- read.csv("inputs/all_sari_cleaned_data.csv",
                     sep = ",",
                     check.names = TRUE)

# test patient data per RS, per week
n_patient <- sari_all %>% 
  dplyr::rename(
    hospital_long = nama_rs
  ) %>% 
  dplyr::group_by(hospital_long, age_group) %>% 
  dplyr::summarise(n = sum(n), .groups = "drop") %>% 
  dplyr::mutate(
    sentinel_hospital = ifelse(hospital_long %in% df$hospital_long, "sentinel", "others"),
    # yearWeek = as.Date(yearWeek)
  ) %>% 
  
  # test full join data to calculate estimate per week, per hospital
  dplyr::full_join(
    final_influ_df %>% 
      # adjust age_group based on all SARI availability data
      dplyr::mutate(
        age_group = ifelse(age_group == "1 to <2 years" |
                             age_group == "2 to <5 years", "1 to <5 years",
                           age_group
                           ),
      ) %>% 
      dplyr::group_by(
        hospital, age_group
      ) %>% 
      dplyr::summarise(
        estimation = sum(estimation),
        .groups = "drop"
      ) %>% 
      dplyr::left_join(
        df %>% 
          dplyr::select(hospital, hospital_long) %>% 
          distinct()
        ,
        by = "hospital"
      )
    ,
    by = c("age_group", "hospital_long")
  ) %>% 
  # calculate Influenza (need Influenza case counts per week, per RS, per age group)
  # dplyr::mutate(
  #   
  # ) %>% 
  dplyr::filter(
    sentinel_hospital != "others"
  ) %>% 
  dplyr::mutate(
    # test difference
    diff = n - estimation
  ) %>% 
  dplyr::filter(
    diff <= 0 
  ) %>% 
  glimpse()


