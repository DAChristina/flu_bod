library(tidyverse)
a <- readxl::read_excel("raw_data/catchment area RS dari RS sentinel SARI 2025.xlsx",
                        sheet = "RS catchment area SARI") %>%
  janitor::clean_names() %>% 
  dplyr::mutate(
    kabupaten = stringr::str_to_title(kabupaten),
    catchment_area_rs = stringr::str_to_upper(catchment_area_rs) %>%
      stringr::str_replace_all(fixed("."), " ") %>% 
      stringr::str_squish()
  ) %>% 
  glimpse()

b <- read.csv("raw_data/Monitoring_Integrasi_Data_SATUSEHAT_2024_2026_RS_puskesmas_lab.csv",
              fileEncoding = "UTF-16",
              sep = "\t") %>% 
  janitor::clean_names() %>% 
  dplyr::transmute(
    kode_sat_set = as.character(kode_satusehat),
    kode_sarana = NA_character_,
    nama_rs = stringr::str_to_upper(nama_fasyankes),
    nama_rs = stringr::str_replace(nama_rs, "RS UMUM DAERAH ", "RSUD "),
    
    provinsi = stringr::str_remove(provinsi,
                                   "^\\d+\\s*-\\s*(Kab\\.?|Kabupaten)?\\s*") %>%
      stringr::str_squish(),
    kabupaten = stringr::str_remove(kabkota,
                                    "^\\d+\\s*-\\s*(Kab\\.?|Kabupaten)?\\s*") %>%
      stringr::str_squish()
  ) %>% 
  dplyr::bind_rows(
    read.csv("raw_data/master.sarana/master.sarana_rs.csv") %>% 
      janitor::clean_names() %>% 
      dplyr::transmute(
        kode_sat_set = kode_sat_set,
        kode_sarana = kode_sarana,
        nama_rs = stringr::str_to_upper(nama),
        nama_rs = stringr::str_replace(nama_rs, "RS UMUM DAERAH ", "RSUD "),
        provinsi = stringr::str_remove(provinsi,
                                       "^\\d+\\s*-\\s*(Kab\\.?|Kabupaten)?\\s*") %>%
          stringr::str_squish(),
        kabupaten = stringr::str_remove(kabupaten_kota,
                                        "^\\d+\\s*-\\s*(Kab\\.?|Kabupaten)?\\s*") %>%
          stringr::str_squish()
      )
    # ,
    # by = "nama_rs"
  ) %>% 
  
  # adjust name
  dplyr::mutate(
    kabupaten = dplyr::recode(kabupaten,
                              "Banda Aceh" = "Kota Banda Aceh")
  ) %>% 
  
  distinct() %>% 
  glimpse()

write.csv(b, "inputs/all_kode_sarana_RS.csv",
          row.names = FALSE)


cand <- a %>%
  dplyr::inner_join(b
                    ,
                    by = "kabupaten",
                    suffix = c("_a", "_b"),
                    relationship = "many-to-many") %>%
  dplyr::mutate(
    dist = stringdist::stringdist(catchment_area_rs,
                                  nama_rs_b,
                                  method = "cosine", 
                                  q = 2)
  ) %>% 
  glimpse()

# Best candidate per hospital
best <- cand %>%
  dplyr::group_by(catchment_area_rs) %>%
  dplyr::arrange(dist, .by_group = TRUE) %>%
  dplyr::mutate(
    gap_to_next = lead(dist) - dist
  ) %>%
  slice(1) %>%
  ungroup() %>%
  dplyr::select(catchment_area_rs,
                matched_hospital_name = nama_rs_b,
                kabupaten_b = kabupaten,
                provinsi_b,
                kode_sat_set,
                kode_sarana,
                dist,
                gap_to_next) %>%
  glimpse()

# Put it back
result <- a %>%
  dplyr::left_join(
    best
    ,
    # by = c("kabupaten" = "kabupaten_b", "catchment_area_rs")
    by = "catchment_area_rs"
  ) %>%
  dplyr::mutate(
    no = dplyr::row_number()
  ) %>% 
  # recheck missing kode_sarana
  dplyr::left_join(
    b %>%
      select(contains("kode_")) %>%
      filter(!is.na(kode_sarana))
    ,
    by = "kode_sat_set"
  ) %>%
  dplyr::mutate(
    kode_sarana.x = ifelse(is.na(kode_sarana.x) | kode_sarana.x == "NA",
                           kode_sarana.y,
                           kode_sarana.x)
  ) %>%
  distinct(no, .keep_all = TRUE) %>%
  glimpse()

count(result, matched_hospital_name)


write.csv(result,
          "raw_data/catchment area RS dari RS sentinel SARI 2025_with_RS_code.csv",
          row.names = FALSE)
# I check high fuzzy distance manually, including RS mismatch (with Andra)

post_mannual <- readxl::read_excel("raw_data/catchment area RS dari RS sentinel SARI 2025_with_RS_code_coded_validated.xlsx"
) %>%
  janitor::clean_names() %>% 
  glimpse()


# prepare sentinel hospital list
sentinel <- read.csv("inputs/influenza_cleaned_address_data.csv") %>% 
  dplyr::select(
    hospital,
    hospital_long,
    hospital_province,
    hospital_regency,
    hospital_code
  ) %>% 
  distinct(hospital_long, .keep_all = TRUE) %>% 
  dplyr::left_join(
    b %>% 
      transmute(
        kode_sat_set = kode_sat_set,
        kode_sarana = as.numeric(kode_sarana)
      ) %>% 
      filter(!is.na(kode_sarana))
    ,
    by = c("hospital_code"= "kode_sarana")
  ) %>% 
  # correct colum name
  dplyr::transmute(
    no = dplyr::row_number() + (max(result$no)-1),
    nama_rs = hospital,
    kabupaten = hospital_regency,
    provinsi = hospital_province,
    catchment_area_rs = hospital,
    matched_hospital_name = hospital_long,
    kode_sat_set = kode_sat_set,
    kode_sarana = as.character(hospital_code),
    validated = "sentinel_site"
  ) %>% 
  glimpse()


# combine all hospital list into 1 df
combine <- dplyr::bind_rows(
  readxl::read_excel("raw_data/catchment area RS dari RS sentinel SARI 2025_with_RS_code_coded_validated.xlsx")
  ,
  sentinel
) %>% 
  glimpse()

# create 1 df that link sentinel hospital to other hospitals
hospital_link <- combine %>% 
  dplyr::left_join(
    combine %>% 
      dplyr::filter(validated == "sentinel_site") %>% 
      dplyr::rename_all(~ paste0("sentinel_data_", .))
    ,
    by = c("provinsi" = "sentinel_data_provinsi")
  ) %>% 
  glimpse()

write.csv(sentinel,
          "raw_data/list_all_sentinel_hospital.csv",
          row.names = FALSE)

write.csv(combine,
          "raw_data/catchment area RS dari RS sentinel SARI 2025_with_RS_code_coded_validated_with_sentinel.csv",
          row.names = FALSE)

write.csv(hospital_link,
          "raw_data/catchment area RS dari RS sentinel SARI 2025_with_RS_code_coded_validated_with_sentinel_link.csv",
          row.names = FALSE)
