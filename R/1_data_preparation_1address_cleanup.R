library(tidyverse)
source_all <- function(path = "global"){
  files <- list.files(path,
                      pattern = "\\.R$",
                      full.names = TRUE
  )
  invisible(lapply(files, source))
}
source_all()

# prepare kode wilayah (38 provinces)
# https://github.com/yonatanyl/KODE-WILAYAH-KEPMENDAGRI-2025/tree/main
# https://pelita.kemendagri.go.id/katalog/kode-referensi
# KEPMEN NOMOR 300.2.2 -2138 TAHUN 2025 TENTANG PEMBERIAN DAN PEMUTAKHIRAN KODE, DATA WILAYAH ADMINISTRASI PEMERINTAHAN, DAN PULAU
area_df <- readxl::read_excel("raw_data/KODE-WILAYAH-KEPMENDAGRI-2025.xlsx") %>%
  janitor::clean_names() %>%
  dplyr::transmute(
    standard_village = nama_kelurahan_desa_desa_adat,
    standard_district = stringr::str_to_title(nama_kecamatan),
    standard_regency = nama_kabupaten %>%
      stringr::str_to_title() %>%
      stringr::str_remove("^(Kabupaten|Kab\\.?)\\s+") %>% # "Kabupaten ", "Kab. " or "Kab "
      stringr::str_squish(),
    
    standard_province = case_when(
      nama_provinsi == "Daerah Khusus Ibukota Jakarta" ~ "DKI Jakarta",
      nama_provinsi == "Daerah Istimewa Yogyakarta" ~ "DI Yogyakarta",
      # nama_provinsi == "Kepulauan Bangka Belitung" ~ "Bangka Belitung",
      TRUE ~ nama_provinsi
    ),
    # nama_kecamatan_provinsi_combined = paste0(nama_kecamatan, ", ", nama_provinsi)
  ) %>%
  dplyr::mutate(across(everything(), stringr::str_squish)) %>% 
  dplyr::select(
    -contains("kode_")
  ) %>%
  # test filter
  # dplyr::filter(nama_kecamatan %in% c("Limboto", "Tibawa")) %>%
  glimpse()


# KESLAN data check hospital address ###########################################
# overall SARI 1 (SIRS/KESLAN data) doesn't include patient's address
hos_sari_sirs <- readxl::read_excel("raw_data/berikut datanya.xlsx",
                                    sheet = "Rawat Inap") %>%
  janitor::clean_names() %>%
  dplyr::transmute(
    hospital_province = dplyr::recode(provinsi,
                                      "Yogyakarta" = "DI Yogyakarta",
                                      "Dki Jakarta" = "DKI Jakarta"),
    hospital_regency = dplyr::recode(kab_kota, !!!switch_regencyStandard),
    
    hospital_code = kode_rs,
    hospital_long = stringr::str_squish(nama_rs)
  ) %>%
  # dplyr::summarise(n = n(), .groups = "drop") %>%
  distinct() %>%
  dplyr::mutate(across(everything(), stringr::str_squish)) %>% 
  dplyr::left_join(
    area_df %>% 
      dplyr::select(
        -standard_village,
        -standard_district
      ) %>% 
      distinct()
    ,
    by = c("hospital_province" = "standard_province",
           "hospital_regency" = "standard_regency")
  ) %>% 
  
  # temporary adjustment due to data missing in all SARI data
  dplyr::add_row(hospital_province = "Maluku",
                 hospital_regency = "Kota Ambon",
                 hospital_code = "8171123",
                 hospital_long = "RSUP dr. Johannes Leimena"
                 ) %>%
  
  dplyr::add_row(hospital_province = "Sulawesi Utara",
                 hospital_regency = "Kota Manado",
                 hospital_code = "7171013",
                 hospital_long = "RSUP Prof. Dr. R.D. Kandou"
  ) %>%
  # test unique value
  # dplyr::group_by(hospital_code) %>% 
  # dplyr::summarise(n = n(), .groups = "drop") %>% 
  # dplyr::filter(n > 1) %>% 
  glimpse()

# check what still doesn't match
dplyr::anti_join(hos_sari_sirs
                 ,
                 area_df %>% 
                   distinct(standard_province, standard_regency)
                 ,
                 by = c("hospital_province" = "standard_province",
                        "hospital_regency" = "standard_regency")
)


# RME data check hospital address ##############################################
# overall SARI 2 (RME data) include patient's address

# prepare sentinel list
sentinel <- read.csv("raw_data/list_all_sentinel_hospital.csv") %>% 
  glimpse()

# check all hospital data from our preferred hospital
combine <- read.csv("raw_data/catchment area RS dari RS sentinel SARI 2025_with_RS_code_coded_validated_with_sentinel.csv") %>% 
  glimpse()

# data updated in 02 November 2026
hos_sari_rme <- readxl::read_excel("raw_data/KAB-760 ISPA Jan 2025 - Dec 2025 results-20261102-002732.xlsx") %>%
  janitor::clean_names() %>% 
  # dplyr::filter(pasien_kabupaten_kota_domisili != "TIDAK DIKETAHUI") %>% # filter unknown address
  dplyr::transmute(
    match_patient_dttm_in = paste0(tanggal_lahir, ",", tanggal_masuk, ",",
                                   kode_sat_set, ",",
                                   diagnosis_utama,  ",",
                                   diagnosis_sekunder
                                   ),
    
    hospital_province = dplyr::recode(stringr::str_to_title(faskes_provinsi),
                                      "Yogyakarta" = "DI Yogyakarta",
                                      "Dki Jakarta" = "DKI Jakarta"),
    hospital_regency = faskes_kabupaten_kota %>%
      stringr::str_to_title() %>%
      stringr::str_remove("^(Kabupaten|Kab\\.?)\\s+") %>% # "Kabupaten ", "Kab. " or "Kab "
      stringr::str_squish(),
    # hospital_district = stringr::str_to_title(kecamatan_domisili),
    hospital_regency  = dplyr::recode(hospital_regency, !!!switch_regencyStandard),
    hospital_code = kode_sarana,
    hospital_satset = kode_sat_set,
    hospital_long = stringr::str_squish(matched_hospital_name),
    hospital_chosen_sentinel = ifelse(hospital_satset %in% sentinel$kode_sat_set, "chosen_sentinel",
                                      "other")
    
    # diag_1 = diagnosis_utama,
    # diag_2 = diagnosis_sekunder,
    # diag_exclude = stringr::str_detect(diag_1, "^J0[0-8]") &
    #   stringr::str_detect(diag_2, "^J0[0-8]"),
    
  ) %>%
  # distinct() %>%
  dplyr::mutate(
    across(everything(), stringr::str_squish),
    
    # 2 Papua province to 5, correct some regency inconsistency
    hospital_regency = ifelse(hospital_regency == "Toba Samosir", "Toba",
                              hospital_regency),
    
    hospital_province = case_when(
      hospital_regency == "Seluma" ~ "Bengkulu",
      hospital_regency == "Flores Timur" ~ "Nusa Tenggara Timur",
      hospital_regency == "Kota Sorong"~ "Papua Barat Daya",
      hospital_regency == "Mimika" ~ "Papua Tengah",
      hospital_regency %in% c("Merauke", "Asmat", "Boven Digoel") ~ "Papua Selatan",
      hospital_regency %in% c("Pegunungan Bintang", "Tolikara") ~ "Papua Pegunungan",
      # hospital_regency == "Toba Samosir" ~ "Sumatera Utara",
      TRUE~ hospital_province
    ),
    
    
  ) %>% 
  dplyr::left_join(
    area_df %>% 
      dplyr::select(
        -standard_village,
        -standard_district
      ) %>% 
      distinct()
    ,
    by = c("hospital_province" = "standard_province",
           "hospital_regency" = "standard_regency")
  ) %>% 
  # test unique value
  # dplyr::group_by(hospital_code) %>% 
  # dplyr::summarise(n = n(), .groups = "drop") %>% 
  # dplyr::filter(n > 1) %>% 
  glimpse()

# check hospital availability status from RME data
dplyr::anti_join(readxl::read_excel("raw_data/KAB-760 ISPA Jan 2025 - Dec 2025 results-20261102-002732.xlsx") %>%
                   janitor::clean_names() %>% 
                   dplyr::mutate(kode_sat_set = as.character(kode_sat_set))
                 ,
                 combine
                 ,
                 by = "kode_sat_set"
)

dplyr::anti_join(sentinel %>% 
                   dplyr::mutate(kode_sat_set = as.character(kode_sat_set))
                 ,
                 # readxl::read_excel("raw_data/KAB-760 ISPA Jan 2025 - Dec 2025 results-20261102-002732.xlsx") %>%
                 #   janitor::clean_names() %>% 
                 #   dplyr::mutate(kode_sat_set = as.character(kode_sat_set))
                 hos_sari_rme %>% 
                   dplyr::filter(hospital_chosen_sentinel == "chosen_sentinel") %>% 
                   distinct(hospital_satset, .keep_all = T)
                 ,
                 by = c("kode_sat_set" = "hospital_satset")
)

# check what still doesn't match
dplyr::anti_join(hos_sari_rme
                 ,
                 area_df %>% 
                   distinct(standard_province, standard_regency)
                 ,
                 by = c("hospital_province" = "standard_province",
                        "hospital_regency" = "standard_regency")
)

# check patient address
patient_sari_rme <- readxl::read_excel("raw_data/KAB-760 ISPA Jan 2025 - Dec 2025 results-20261102-002732.xlsx") %>%
  janitor::clean_names() %>% 
  dplyr::filter(pasien_kabupaten_kota_domisili != "TIDAK DIKETAHUI") %>% 
  dplyr::transmute(
    match_patient_dttm_in = paste0(tanggal_lahir, ",", tanggal_masuk, ",",
                                   kode_sat_set, ",",
                                   diagnosis_utama,  ",",
                                   diagnosis_sekunder
    ),
    
    date = tanggal_masuk,
    week = as.numeric(stringr::str_extract(ISOweek::ISOweek(date), "(?<=W)\\d+")),
    yearWeek = ISOweek::ISOweek2date(paste0(ISOweek::ISOweek(date), "-1")),
    
    sex = ifelse(stringr::str_to_lower(jenis_kelamin) == "perempuan" | 
                   stringr::str_to_lower(jenis_kelamin) == "p", "Female",
                 "Male"
                 ),
    date_of_birth = tanggal_lahir,
    age_month = round(lubridate::time_length(interval(tanggal_lahir, tanggal_masuk),
                                             unit = "months"), 1),
    age_year = round(lubridate::time_length(interval(tanggal_lahir, tanggal_masuk),
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
    
    
    # patient_province = dplyr::recode(stringr::str_to_title(provinsi),
    #                                   "Yogyakarta" = "DI Yogyakarta",
    #                                   "Dki Jakarta" = "DKI Jakarta"),
    patient_regency = pasien_kabupaten_kota_domisili %>%
      stringr::str_to_title() %>%
      stringr::str_remove("^(Kabupaten|Kab\\.?)\\s+") %>% # "Kabupaten ", "Kab. " or "Kab "
      stringr::str_squish(),
    # patient_district = stringr::str_to_title(kecamatan_domisili),
    patient_regency  = dplyr::recode(patient_regency, !!!switch_regencyStandard),
    # patient_code = kode_rs,
    # patient_long = stringr::str_squish(nama_rumah_sakit)
    
    diag_1 = diagnosis_utama,
    diag_2 = diagnosis_sekunder,
    diag_exclude = stringr::str_detect(diag_1, "^J0[0-8]") &
      stringr::str_detect(diag_2, "^J0[0-8]"),
    
    
  ) %>%
  distinct() %>%
  dplyr::mutate(
    across(everything(), stringr::str_squish),
    
    # 2 Papua province to 5, correct some regency inconsistency
    patient_regency = ifelse(patient_regency == "Toba Samosir", "Toba",
                             patient_regency),
    
    # patient_province = case_when(
    #   patient_regency == "Seluma" ~ "Bengkulu",
    #   patient_regency == "Flores Timur" ~ "Nusa Tenggara Timur",
    #   patient_regency == "Kota Sorong"~ "Papua Barat Daya",
    #   patient_regency == "Mimika" ~ "Papua Tengah",
    #   patient_regency %in% c("Merauke", "Asmat", "Boven Digoel") ~ "Papua Selatan",
    #   patient_regency %in% c("Pegunungan Bintang", "Tolikara") ~ "Papua Pegunungan",
    #   # patient_regency == "Toba Samosir" ~ "Sumatera Utara",
    #   TRUE~ patient_province
    # ),
    
    
  ) %>% 
  dplyr::left_join(
    area_df %>% 
      dplyr::select(
        -standard_village,
        -standard_district
      ) %>% 
      distinct()
    ,
    by = c(#"patient_province" = "standard_province",
      "patient_regency" = "standard_regency")
  ) %>% 
  # test unique value
  # dplyr::group_by(patient_code) %>% 
  # dplyr::summarise(n = n(), .groups = "drop") %>% 
  # dplyr::filter(n > 1) %>% 
  glimpse()

# check what still doesn't match
dplyr::anti_join(patient_sari_rme
                 ,
                 area_df %>% 
                   distinct(standard_province, standard_regency)
                 ,
                 by = c(#"patient_province" = "standard_province",
                   "patient_regency" = "standard_regency")
)

write.csv(patient_sari_rme, "inputs/sari_rme_cleaned_address_data.csv",
          row.names = FALSE)


# combined RME data
# test duplicated data
dup_test <- readxl::read_excel("raw_data/KAB-760 ISPA Jan 2025 - Dec 2025 results-20261102-002732.xlsx") %>%
  janitor::clean_names() %>% 
  dplyr::mutate(
    match_patient_dttm_in = paste0(tanggal_lahir, ",", tanggal_masuk, ",",
                                   kode_sat_set, ",",
                                   diagnosis_utama,  ",",
                                   diagnosis_sekunder
    ),
    
  ) %>% 
  dplyr::group_by(across(everything())) %>%
  dplyr::mutate(duplicate_count = n()) %>%
  ungroup() %>%
  dplyr::filter(duplicate_count > 1) %>% 
  # view() %>% 
  glimpse()


test <- readxl::read_excel("raw_data/KAB-760 ISPA Jan 2025 - Dec 2025 results-20261102-002732.xlsx") %>%
  janitor::clean_names() %>% 
  dplyr::transmute(
    match_patient_dttm_in = paste0(tanggal_lahir, ",", tanggal_masuk, ",",
                                   kode_sat_set, ",",
                                   diagnosis_utama,  ",",
                                   diagnosis_sekunder
    ),
  ) %>% 
  dplyr::left_join(
    hos_sari_rme
    ,
    by = "match_patient_dttm_in"
  ) %>% 
  dplyr::left_join(
    patient_sari_rme
    ,
    by = "match_patient_dttm_in"
  ) %>% 
  # distinct(match_patient_dttm_in, .keep_all = TRUE) %>% 
  
  # match with hospital sentinel's code
  dplyr::left_join(
    read.csv("raw_data/catchment area RS dari RS sentinel SARI 2025_with_RS_code_coded_validated_with_sentinel_link.csv") %>% 
      dplyr::select(kode_sat_set, contains("sentinel_"),
                    -sentinel_data_dist,
                    -sentinel_data_gap_to_next,
                    -sentinel_data_validated
      )
    ,
    by = c("hospital_satset" = "kode_sat_set")
  ) %>% 
  dplyr::rename_all(~ paste0("rme_data_", .)) %>% 
  
  # filter duplicate values (ranging from n = 2 to n = 9) & exclude J00-J08
  # distinct(rme_data_match_patient_dttm_in, .keep_all = TRUE) %>%
  dplyr::filter(rme_data_diag_exclude == "FALSE") %>% # exclude J00-J08
  glimpse()

write.csv(test, "inputs/sari_rme_cleaned_compiled_data.csv",
          row.names = FALSE)


# Cleanup patient's address in verified Influenza-associated SARI data #########
regency_size <- count(area_df,
                      standard_province,
                      standard_regency,
                      name = "n_villages")

std <- area_df %>%
  dplyr::distinct(standard_province,
                  standard_regency,
                  standard_district
                  ) %>%
  dplyr::left_join(
    regency_size
    ,
    by = c("standard_province", "standard_regency")
    ) %>%
  dplyr::mutate(
    dk = keyword(standard_district),
    rk = standard_regency %>%
      stringr::str_remove("^(Kota Administrasi|Kota Adm\\.|Kabupaten|Kab\\.?)\\s+") %>% 
      keyword()
  )

pairs_dk <- std %>% dplyr::distinct(dk,
                                    standard_province, standard_regency,
                                    rk,
                                    n_villages
                                    )

pairs_rk <- std %>% dplyr::distinct(rk,
                                    standard_province, standard_regency,
                                    n_villages
                                    )
reg_keys <- unique(std$rk)
std_dk <- unique(std$dk)

dk_by_prov <- std %>% 
  dplyr::distinct(standard_province, dk) %>%
  dplyr::group_by(standard_province) %>% 
  dplyr::summarise(dk = list(dk),
                   .groups = "drop")
dk_by_prov <- stats::setNames(dk_by_prov$dk,
                              dk_by_prov$standard_province)

# patient's data
patt <- readxl::read_excel("raw_data/DATA REVIEW for BoD.xlsx",
                           sheet = "DATA SARI") %>% 
  janitor::clean_names() %>%
  # dplyr::mutate(
  #   # nik = stringr::str_replace_all(nik, "'", ""),
  #   ttl = as.Date(ttl),
  #   tgl_masuk = as.Date(tgl_masuk),
  #   tglwwcr = as.Date(tglwwcr),
  #   
  # ) %>%
  dplyr::transmute(
    row = row_number(),
    id_pasien = id_pasien,
    hospital = nama_rs,
    hospital_long = dplyr::recode(hospital, !!!switch_hosName),
    hospital_province = provinsi,
    
    # clean up province column
    hospital_province = stringr::str_squish(hospital_province), # destroy extra whitespace
    hospital_province = stringr::str_to_title(hospital_province), # standardise casing
    hospital_province = case_when(
      hospital_province == "Dki Jakarta" ~ "DKI Jakarta",
      hospital_province == "Daerah Istimewa Yogyakarta" ~ "DI Yogyakarta",
      hospital_province == "Bangka Belitung" ~ "Kepulauan Bangka Belitung",
      TRUE ~ hospital_province
    ),

    patient_district_ori = stringr::str_to_title(kecamatan),
    patient_district_ori = case_when(
      patient_district_ori == "Ilir Barat I" |
        patient_district_ori == "Ilir Barat 1" ~ "Ilir Barat Satu",
      patient_district_ori == "Ilir Barat Ii" |
        patient_district_ori == "Ilir Barat 2" ~ "Ilir Barat Dua",
      patient_district_ori == "Ilir Barat Iii" |
        patient_district_ori == "Ilir Barat 3" ~ "Ilir Barat Tiga",
      
      patient_district_ori == "Ilir Timur I" |
        patient_district_ori == "Ilir Timur 1" ~ "Ilir Timur Satu",
      patient_district_ori == "Ilir Timur Ii" |
        patient_district_ori == "Ilir Timur 2" ~ "Ilir Timur Dua",
      patient_district_ori == "Ilir Timur Iii" |
        patient_district_ori == "Ilir Timur 3" ~ "Ilir Timur Tiga",
      
      patient_district_ori == "Seberang Ulu I" |
        patient_district_ori == "Seberang Ulu 1" ~ "Seberang Ulu Satu",
      patient_district_ori == "Seberang Ulu Ii" |
        patient_district_ori == "Seberang Ulu 2" ~ "Seberang Ulu Dua",
      patient_district_ori == "Seberang Ulu Iii" |
        patient_district_ori == "Seberang Ulu 3" ~ "Seberang Ulu Tiga",
      
      # patient_district_ori == "Banyuasin I" |
      #   patient_district_ori == "Banyuasin 1" ~ "Banyuasin Satu",
      # patient_district_ori == "Banyuasin Ii" |
      #   patient_district_ori == "Banyuasin 2" ~ "Banyuasin Dua",
      # patient_district_ori == "Banyuasin Iii" |
      #   patient_district_ori == "Banyuasin 3" ~ "Banyuasin Tiga",
      
      patient_district_ori == "Astana Anyar" ~ "Astanaanyar",
      patient_district_ori == "Batu Ceper" ~ "Batuceper",
      patient_district_ori == "Stm Hilir" ~ "Sinembah Tanjung Muda Hilir",
      patient_district_ori == "Teluk Naga" ~ "Teluknaga",
      
      patient_district_ori == "Tidak Ada" ~ NA_character_,
      TRUE ~ patient_district_ori
    ),
    
    patient_district = stringr::str_remove(patient_district_ori,
                                           regex("\\b(?:Kab)\\b.*$",
                                                 ignore_case = TRUE)
                                           ) %>% 
      stringr::str_remove("[[:punct:][:space:]]+$"),
    patient_district = stringr::str_squish(patient_district),
    
    patient_subdistrict = stringr::str_to_title(kelurahan),
  ) %>% 
  glimpse()

# Strict check: province + district must match exactly (as before)
lookup_pd <- std %>%
  dplyr::group_by(standard_province, standard_district) %>%
  dplyr::summarise(n_regency = dplyr::n_distinct(standard_regency),
                   strict_regency = if (n_regency == 1) first(standard_regency)
                   else
                     NA_character_,
                   .groups = "drop")

patt <- patt %>%
  dplyr::left_join(
    lookup_pd
    ,
    by = c("hospital_province" = "standard_province",
           "patient_district" = "standard_district"),
    relationship = "many-to-one"
  ) %>%
  dplyr::mutate(
    address_patient_validation_status = case_when(
      is.na(patient_district)~ "blank_district",
      !hospital_province %in% std$standard_province ~ "province_not_in_standard",
      is.na(n_regency) ~ "district_not_in_province",
      n_regency > 1 ~ "ambiguous_regency",
      TRUE~ "matched"
    )
  )

# Normalised district key + regency hint from the original text
parsed <- t(vapply(patt$patient_district_ori, split_hint,
                   c(dist = NA_character_, hint = NA_character_),
                   USE.NAMES = FALSE))
patt$dk <- parsed[, 1]
patt$hint <- parsed[, 2]
cleaned_key <- keyword(patt$patient_district)
patt$dk <- dplyr::if_else(patt$dk %in% std_dk, patt$dk,
                          dplyr::if_else(cleaned_key %in% std_dk, cleaned_key, patt$dk))
patt$dk <- dplyr::coalesce(patt$dk, cleaned_key)

todo <- dplyr::filter(patt,
                      !is.na(patient_district),
                      !is.na(dk)
                      )

est <- lapply(seq_len(nrow(todo)),
              function(i){
                r <- todo[i, ]
                find_candidates(r$dk, r$hint, r$hospital_province) %>%
                  dplyr::mutate(
                    row = r$row,
                    hint = r$hint,
                    hospital_province = r$hospital_province) %>%
                  pick()
              }
) %>% 
  dplyr::bind_rows()

# compile everything
final <- patt %>%
  dplyr::left_join(est, by = "row") %>%
  dplyr::mutate(
    validated_patient_province = dplyr::if_else(address_patient_validation_status == "matched",
                                            hospital_province, est_province),
    validated_patient_regency = dplyr::if_else(address_patient_validation_status == "matched",
                                           strict_regency, est_regency),
    address_patient_fill_method = case_when(
      address_patient_validation_status == "matched" ~ "exact_province_district",
      is.na(est_province) ~ NA_character_,
      TRUE~ paste(name_match, fill_step, sep = " > ")
      ),
    address_patient_fill_confidence = case_when(
      address_patient_validation_status == "matched"~ "high",
      is.na(est_province)~ NA_character_,
      str_detect(name_match, "_far$")~ "very_low",
      str_starts(name_match, "fuzzy") |
        name_match == "regency_hint_only" |
        fill_step == "size_tiebreak"~ "low",
      name_match == "regency_name" |
        fill_step %in% c("regency_hint", "hospital_province") ~ "medium",
      TRUE ~ "high"
      )
  ) %>%
  dplyr::select(-row,
                # -hospital_province,
                -dk, -hint, -n_regency, -strict_regency,
                -est_province, -est_regency, -name_match, -fill_step,
                -patient_district_ori, -patient_district, -patient_subdistrict
         ) %>% 
  # combined with validated hospital province and regency
  dplyr::left_join(
    hos_sari_sirs #%>% 
      # dplyr::select(-hospital_code)
    ,
    by = c("hospital_long", "hospital_province")
  )

count(final, address_patient_fill_confidence)
count(final, address_patient_validation_status, address_patient_fill_confidence)

# rows to check by hand: low/very_low confidence or still NA
# filter(final, is.na(address_patient_fill_confidence) | address_patient_fill_confidence %in% c("low", "very_low")) %>%
#   readr::write_csv("patient_to_review.csv")

write.csv(final, "inputs/influenza_cleaned_address_data.csv",
          row.names = FALSE)