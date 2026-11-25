library(tidyverse)
source_all <- function(path = "global"){
  files <- list.files(path,
                      pattern = "\\.R$",
                      full.names = TRUE
  )
  invisible(lapply(files, source))
}
source_all()







# read.csv("inputs/sari_rme_cleaned_compiled_data.csv") %>% 
#   group_by(match_patient_dttm_in) %>% 
#   summarise(n = n()) %>% 
#   filter(n > 1) %>% 
#   view()




################################################################################
# temporary script for comparing Kabupaten's most common people came from each sentinel hospital
# and other market share hospital

# sentinel hospital
df_epi_chosen_catchment <- read.csv("inputs/influenza_cleaned_data.csv") %>% 
  # filter(influ_pos == "Positive") %>% 
  group_by(hospital_long, validated_patient_regency) %>%
  summarise(n_patients = n(), .groups = "drop") %>%
  group_by(hospital_long) %>%
  mutate(
    n_hospital = sum(n_patients),
    prop = n_patients / n_hospital
  ) %>%
  arrange(desc(n_patients), .by_group = TRUE) %>%
  mutate(
    cumulative_prop = cumsum(prop),
    cumulative_prop_before = lag(cumulative_prop, default = 0),
    catchment = cumulative_prop_before < 0.80
  ) %>%
  ungroup() %>% 
  # view() %>% 
  glimpse()

influ_sari_catchment_regencies <- df_epi_chosen_catchment %>%
  filter(catchment) %>% 
  glimpse()

# other market share hospital
# 1. Extract the vector of Catchment Regencies from your Sentinel Hospital
sentinel_catchment_list <- influ_sari_catchment_regencies %>%
  pull(validated_patient_regency) %>%
  unique() %>% 
  glimpse()

write.csv(influ_sari_catchment_regencies,
          "inputs/temporary_sentinel_catchment_list.csv",
          row.names = FALSE)

# 2. Filter ALL hospitals (Sentinel + Other Hospitals) for residents of THAT catchment area
has_data_filtered <- read.csv("inputs/sari_rme_cleaned_compiled_data.csv") %>%
  filter(rme_data_patient_regency %in% sentinel_catchment_list) %>%
  group_by(rme_data_hospital_long, rme_data_patient_regency) %>%
  summarise(n_sari_patients = n(), .groups = "drop") %>% 
  glimpse()

# 3. Calculate the Sentinel Hospital's Market Share (Pss) for that Catchment Area
pss_summary <- has_data_filtered %>%
  group_by(rme_data_patient_regency) %>%
  mutate(total_regency_sari = sum(n_sari_patients)) %>%
  filter(rme_data_hospital_long %in% unique(influ_sari_catchment_regencies$hospital_long)) %>%
  mutate(pss_share = n_sari_patients / total_regency_sari) %>% 
  glimpse()


# Join Pss share with Regency Census Population to get Effective Catchment Population
final_denominator <- pss_summary %>%
  left_join(
    regency_census_table
    ,
    by = c("rme_data_patient_regency" = "regency_name")
    
    ) %>%
  mutate(
    effective_catchment_pop = pss_share * census_population
  )









