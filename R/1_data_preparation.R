library(tidyverse)
source_all <- function(path = "global"){
  files <- list.files(path,
                      pattern = "\\.R$",
                      full.names = TRUE
  )
  invisible(lapply(files, source))
}
source_all()

df <- readxl::read_excel("raw_data/DATA REVIEW for BoD.xlsx",
                         sheet = "DATA SARI") %>% 
  janitor::clean_names() %>%
  dplyr::mutate(
    # nik = stringr::str_replace_all(nik, "'", ""),
    ttl = as.Date(ttl),
    tgl_masuk = as.Date(tgl_masuk),
    tglwwcr = as.Date(tglwwcr),
    
  ) %>%
  glimpse()

# I clean up date inconsistencies in ttl, tgl_masuk and tglwwcr:
# Warning message:
#   There were 2 warnings in `dplyr::mutate()`.
# The first warning was:
#   ℹ In argument: `ttl = as.Date(as.numeric(ttl), origin = "1899-12-30")`.
# Caused by warning in `as.Date()`:
#   ! NAs introduced by coercion
# ℹ Run dplyr::last_dplyr_warnings() to
# see the 1 remaining warning.
# 
# Warning message:
#   There was 1 warning in `dplyr::mutate()`.
# ℹ In argument: `tgl_masuk = as.Date(as.numeric(tgl_masuk), origin = "1899-12-30")`.
# Caused by warning in `as.Date()`:
#   ! NAs introduced by coercion 
# 
# Warning message:
#   Expecting date in V2497 / R2497C22: got '21-08-0202'


# Detect duplicated IDs (NIK may not be reliable coz children used their parent's NIK);
# 87 ID in which 3 have no ID,
# 2 ID represent 2 different people each,
# 82 represent repeated infection
report_duplicated_id1 <- detoxdats::report_duplicate(df = df,
                                                     column_id = nama_pasien,
                                                     other_iden = tglwwcr
                                                     ) %>% 
  # view() %>% 
  glimpse()

report_duplicated_id2 <- detoxdats::report_duplicate(df = df,
                                                     column_id = nik,
                                                     other_iden = tgl_masuk) %>% 
  # view() %>% 
  glimpse()

# test duplicated ID with multiple identifier
# only 1 duplicated ID found
report_duplicated_id_grouped <- df %>% 
  dplyr::group_by(#nik,
                  nama_pasien,
                  ttl,
                  tgl_masuk,
                  # tglwwcr,
                  nama_rs
                  ) %>% 
  dplyr::summarise(n = n()) %>% 
  ungroup() %>% 
  dplyr::filter(n > 1) %>% 
  # view() %>%
  glimpse()


################################################################################
# reasonable columns:
# 1. sentinel site based on alamat, kecamatan, kelurahan or province (province based on RS's province kah???) (?)
# 2. epiWeek based on tgl_masuk (preferrable) or tglwwcr/mggu_epd/minggu_epid (?)
# 3. jenis_kelamin
# 4. age based on ttl (prefferable) or umur_bln

# Influenza positivity based on lab_influenza or subtype_influenza or covid_19
all_week <- data.frame(week_date = seq.Date(from = min(as.Date(df$tglwwcr)),
                                            to = max(as.Date(df$tglwwcr)), 
                                            by = "week")) %>% 
  dplyr::mutate(yearWeek = ISOweek::ISOweek2date(
    paste0(ISOweek::ISOweek(week_date), "-1")),
  ) %>% 
  distinct(yearWeek) %>% 
  glimpse()

df_epi_chosen <- df %>% 
  dplyr::transmute(
    id_pasien = id_pasien, # id to merge address data; duplicated ID need to be checked again once we get more data
    # # sentinel related data
    province = stringr::str_to_title(stringr::str_squish(provinsi)),
    province = dplyr::recode(
      province,
      "Dki Jakarta" = "DKI Jakarta",
      "Daerah Istimewa Yogyakarta" = "DI Yogyakarta",
      "Bangka Belitung" = "Kepulauan Bangka Belitung"
    ),
    
    hospital = nama_rs,
    hospital_long = dplyr::recode(hospital, !!!switch_hosName),
    sites = case_when(
      province %in% c("Aceh", "Sumatera Utara", "Sumatera Barat", "Riau",
                      "Kepulauan Riau", "Jambi", "Sumatera Selatan",
                      "Bangka Belitung", "Kepulauan Bangka Belitung",
                      "Bengkulu", "Lampung") ~ "Sumatra",
      
      province %in% c("DKI Jakarta", "Jawa Barat", "Jawa Tengah",
                      "DI Yogyakarta", "Jawa Timur", "Banten") ~ "Java",
      
      province %in% c("Bali", "Nusa Tenggara Barat", "Nusa Tenggara Timur") ~ "Bali & Nusa Tenggara",
      
      province %in% c("Kalimantan Barat", "Kalimantan Tengah", "Kalimantan Selatan",
                      "Kalimantan Timur", "Kalimantan Utara") ~ "Kalimantan",
      
      # province %in% c("Sulawesi Utara", "Sulawesi Tengah", "Sulawesi Selatan",
      #                 "Sulawesi Tenggara", "Sulawesi Barat", "Gorontalo",
      #                 "Maluku", "Maluku Utara") ~ "Sulawesi & Maluku",
      
      province %in% c("Sulawesi Utara", "Sulawesi Tengah", "Sulawesi Selatan",
                      "Sulawesi Tenggara", "Sulawesi Barat", "Gorontalo") ~ "Sulawesi",
      
      province %in% c("Maluku", "Maluku Utara") ~ "Maluku",
      
      province %in% c("Papua", "Papua Barat", "Papua Selatan",
                      "Papua Tengah", "Papua Pegunungan") ~ "Papua",
      
      TRUE ~ NA_character_
    ),
    
    # sentinel classification based on province and/or hospital (tobecontinued)
    sentinel_province = province %in% sentinel_province
    ,
    sentinel_hospital = case_when(
      hospital %in% sentinel_hospital_priority1 ~ "Priority 1",
      hospital %in% sentinel_hospital_priority2 ~ "Priority 2",
      TRUE ~ "No"
    ),
    
    date = tglwwcr,
    week = as.numeric(stringr::str_extract(ISOweek::ISOweek(date), "(?<=W)\\d+")),
    yearWeek = ISOweek::ISOweek2date(paste0(ISOweek::ISOweek(date), "-1")),
    
    # patient related data
    sex = ifelse(jenis_kelamin == "Perempuan", "Female", "Male"),
    date_of_birth = ttl,
    age_month = round(lubridate::time_length(interval(date_of_birth, tgl_masuk),
                                             unit = "months"), 1),
    age_year = round(lubridate::time_length(interval(date_of_birth, tgl_masuk),
                                            unit = "years"), 1),
    
    # age group based on WHO
    age_group = case_when(
      age_month < 6 ~ "0 to <6 months",
      age_month >= 6 & age_year < 1 ~ "6m to <1 year",
      age_year >= 1 & age_year < 2 ~ "1 to <2 years",
      age_year >= 2 & age_year < 5 ~ "2 to <5 years",
      age_year >= 5 & age_year < 15 ~ "5 to <15 years",
      age_year >= 15 & age_year < 50 ~ "15 to <50 years",
      age_year >= 50 & age_year < 65 ~ "50 to <65 years",
      age_year >= 65 ~ "65+ years",
      TRUE ~ NA_character_
    ),
    
    # influenza related data
    influ_pos = stringr::str_to_title(lab_influenza), # standardise casing
    influ_pos = case_when(
      influ_pos == "Positif" ~ "Positive",
      influ_pos == "Negatif" ~ "Negative",
      
      influ_pos == "Tidak Diambil Spesimen" ~ "No specimen collected", # I think we omit this column
      # influ_pos == "Invalid" ~ "Invalid",
      TRUE ~ influ_pos
    ),
    influ_subtype = stringr::str_to_title(subtype_influenza),
    # Standardise influ_subtype based on WHO classification Nontypeable
    influ_subtype = case_when(
      influ_subtype == "A (H1n1pdm09)" ~ "A (H1N1) pdm09",
      influ_subtype == "A (H3n2)" ~ "A (H3N2)",
      influ_subtype == "A (Not Subtype)" ~ "A (Nontypeable)",
      
      stringr::str_detect(influ_subtype, "B \\(") ~ "B",
      influ_subtype == "Tidak Diambil Spesimen" ~ "No specimen collected",
      TRUE ~ "Negative/Invalid"
    ),
    
    # update status
    status = dplyr::case_when(
      status_keluar %in% c("Sembuh", "SEMBUH") ~ "Recovered",
      status_keluar %in% c("Pulang Paksa", "Pulang paksa") ~ "Discharged against medical advice",
      status_keluar == "Meninggal" ~ "Deceased",
      status_keluar == "Dirujuk" ~ "Referred",
      status_keluar == "Belum di entry" ~ "Not yet entered",
      TRUE ~ status_keluar
    ),
    
    # clinical diagnosis
    diag_admission_diagnosis = diagnosis_masuk,
    diag_other_admission_diagnosis = diagnosis_masuk_lainnya,
    # diag_private_clinic = klinik_swasta,
    # diag_community_health_center = pkm,
    # diag_hospital = rs,
    # diag_other_location = lainnya,
    
    diag_fever_history = convert_bool(riwayat_panas),
    # diag_fever_onset_date = tgl_panas,
    # diag_symptom_onset_date = tgl_gejala,
    diag_cough = convert_bool(batuk),
    diag_sore_throat = convert_bool(sakit_tenggorokan),
    diag_shortness_of_breath = convert_bool(sesak_napas),
    diag_pleuritic_pain = convert_bool(pleuritik),
    diag_vomiting = convert_bool(muntah),
    diag_diarrhea = convert_bool(diare),
    diag_runny_nose = convert_bool(pilek),
    
    diag_respiratory_rate = frek_nafas, # numeric form
    diag_rhonchi = convert_bool(ronki),
    diag_systolic_blood_pressure = convert_bool(tdd),
    diag_malaise = convert_bool(malaise),
    diag_chills = convert_bool(menggigil),
    diag_wheezing = convert_bool(wheezing),
    diag_abdominal_pain = convert_bool(nyeri_abdomen),
    
    # co-morbidities & complications
    com_smoking_status = convert_bool(perokok),
    com_pregnancy_status = convert_bool(hamil),
    com_diabetes = convert_bool(diabetes),
    com_chronic_kidney_disease = convert_bool(ginjal_kronis),
    com_tuberculosis = convert_bool(tb),
    com_cardiovascular_disease = convert_bool(peny_kardio),
    com_copd = convert_bool(ppok),
    com_immunosuppression = convert_bool(imunosupresif),
    com_chronic_liver_disease = convert_bool(hati_kronis),
    com_hematological_disease = convert_bool(hematologis),
    com_cancer = convert_bool(kanker),
    com_asthma = convert_bool(asma),
    com_obesity = convert_bool(obesitas),
    com_neurological_disease = convert_bool(neurologis),
    
    # other info
    otr_influenza_vaccination = convert_bool(vaksin_flu),
    otr_covid_vaccination = convert_bool(vaksin_covid),
    otr_international_travel = convert_bool(ppln),
    
    otr_sick_family_travel_contact = convert_bool(keluarga_ppln_dan_sakit),
    
    otr_travel_country_1 = negara1,
    otr_travel_country_2 = negara2,
    otr_influenza_contact = convert_bool(kontak_flu),
    otr_dead_poultry_contact = convert_bool(kontak_unggas_mati),
    otr_livestock = convert_bool(lokasi_ternak),
    
  ) %>% 
  
  # join with standardised address
  dplyr::left_join(
    read.csv("inputs/influenza_cleaned_address_data.csv") %>% 
      dplyr::select(-hospital)
    ,
    by = c("id_pasien", "hospital_long")
  ) %>% 
  dplyr::mutate(
    # # temporary adjustment due to data missing in all SARI data
    # hospital_province = case_when(
    #   hospital == "RSUP Prof. Dr. R.D. Kandou" ~ "Sulawesi Utara",
    #   hospital == "RSUP dr. Johannes Leimena" ~ "Maluku",
    #   TRUE ~ hospital_province
    # ),
    # 
    # hospital_regency = case_when(
    #   hospital == "RSUP Prof. Dr. R.D. Kandou" ~ "Kota Manado",
    #   hospital == "RSUP dr. Johannes Leimena" ~ "Kota Ambon",
    #   TRUE ~ hospital_province
    # ),
    
    province_check = ifelse(province == hospital_province, 1L, 0L),
    catchment_regency = ifelse(validated_patient_regency == hospital_regency, 1L, 0L),
    catchment_province = ifelse(validated_patient_province == hospital_province, 1L, 0L),
  ) %>% 
  
  # combine with GPS coordinate
  dplyr::left_join(
    read.csv("raw_data/master.sarana/master.sarana_rs.csv") %>% 
      janitor::clean_names() %>% 
      dplyr::transmute(
        hospital_code = as.numeric(kode_sarana),
        hospital_latitude = geo_latitude,
        hospital_longitude = geo_longitude
      )
    ,
    by = "hospital_code"
  ) %>% 
  glimpse()


# test unique values
report_unique_values <- detoxdats::report_uniqval(df = df_epi_chosen) %>%
  # view() %>% 
  glimpse()

# test positivity double standard
pos <- df_epi_chosen %>% 
  dplyr::group_by(influ_pos, influ_subtype) %>% 
  dplyr::summarise(n = n(),
                   .groups = "drop") %>% 
  # view() %>% 
  glimpse()

write.csv(df_epi_chosen, "inputs/influenza_cleaned_data.csv",
          row.names = FALSE)


################################################################################
# load hospital admission for denominator calc
sheet_names <- readxl::excel_sheets("raw_data/berikut datanya.xlsx") %>% 
  glimpse()
# hos_rj <- readxl::read_excel("raw_data/berikut datanya.xlsx",
#                           sheet = "Rawat Jalan") %>% 
#   glimpse()
# only inpatients data needed; pull records from "all the wards where SARI cases are likely to be admitted"

# list unique name of 35 hospitals
list_hos <- df_epi_chosen %>% 
  dplyr::transmute(
    province = province,
    hospital = hospital,
    hospital_long = hospital_long
  ) %>%
  glimpse()


# length(unique(hos_ri$nama_rs)) all available hospital in Indonesia
# [1] 2753
hos_ri <- readxl::read_excel("raw_data/berikut datanya.xlsx",
                             sheet = "Rawat Inap") %>% 
  janitor::clean_names() %>% 
  dplyr::rename(
    date = periode,
  ) %>% 
  dplyr::select(
    -contains("jmlh_pas_hidup_mati_gen_"),
    -contains("jmlh_pas_keluar_mati_gen_"),
    -contains("total_pas")
  ) %>% 
  # dplyr::filter(nama_rs %in% df_epi_chosen$hospital_long) %>% 
  
  # long-type data based on age group and sex; can be regrouped again during BoD calculation
  tidyr::pivot_longer(
    cols = dplyr::starts_with("jmlh_pas_hidup_mati_umur_gen_"),
    names_to = "col_name",
    values_to = "n"
  ) %>%
  dplyr::mutate(
    date = as.Date(date),
    week = as.numeric(stringr::str_extract(ISOweek::ISOweek(date), "(?<=W)\\d+")),
    yearWeek = ISOweek::ISOweek2date(paste0(ISOweek::ISOweek(date), "-1")),
    
    sex = dplyr::case_when(
      stringr::str_ends(col_name, "_l") ~ "Male",
      stringr::str_ends(col_name, "_p") ~ "Female",
      TRUE  ~ NA_character_
    ),
    
    age_raw = col_name %>%
      stringr::str_remove("^jmlh_pas_hidup_mati_umur_gen_") %>%
      stringr::str_remove("_l$|_p$"),
    
    age_group = dplyr::case_when(
      age_raw %in% c("0_1jam", "1_23jam",
                     "1_7hr", "8_28hr",
                     "29hr_3bln", "3_6bln") ~ "0 to <6 months",
      
      age_raw == "6_11bln" ~ "6m to <1 year",
      age_raw == "1_4th" ~ "1 to <5 years", # WHO-based classification separates (1 to <5) to (1 to <2) AND (2 to <5)
      
      age_raw %in% c("5_9th", "10_14th") ~ "5 to <15 years",
      age_raw %in% c("15_19th", "20_24th",
                     "25_29th", "30_34th",
                     "35_39th", "40_44th",
                     "45_49th") ~ "15 to <50 years",
      age_raw %in% c("50_54th", "55_59th",
                     "60_64th") ~ "50 to <65 years",
      age_raw %in% c("65_69th", "70_74th",
                     "75_79th", "80_84th",
                     "maleebih85th") ~ "65+ years",
      TRUE  ~ NA_character_
    )
  ) %>%
  dplyr::select(-col_name, -age_raw) %>% 
  dplyr::filter(!is.na(age_group),
                !is.na(sex)) %>% 
  
  # combine with GPS coordinate
  dplyr::left_join(
    read.csv("raw_data/master.sarana/master.sarana_rs.csv") %>% 
      janitor::clean_names() %>% 
      dplyr::transmute(
        kode_rs = as.numeric(kode_sarana),
        latitude = geo_latitude,
        longitude = geo_longitude
      )
    ,
    by = "kode_rs"
  ) %>% 
  glimpse()

# write.csv(hos_ri %>% 
#             select(nama_rs) %>% 
#             distinct(),
#           "nama_semua.csv")

write.csv(hos_ri, "inputs/all_sari_cleaned_data.csv",
          row.names = FALSE)


hospital_coordinate <- read.csv("raw_data/master.sarana/master.sarana_rs.csv") %>% 
    janitor::clean_names() %>% 
    dplyr::transmute(
      kode_sat_set,
      kode_rs = as.numeric(kode_sarana),
      nama,
      provinsi,
      kabupaten_kota,
      latitude = geo_latitude,
      longitude = geo_longitude
    ) %>%
  glimpse()

write.csv(hospital_coordinate
          ,
          "inputs/all_hospital_coordinate_data.csv",
          row.names = FALSE)
