



# SOURCE:
# https://www.bps.go.id/id/statistics-table/2/Mjc5MSMy/jumlah-penduduk-laki-laki-menurut-kabupaten-kota-dan-kelompok-umur--.html
# https://www.bps.go.id/id/statistics-table/2/Mjc5MiMy/jumlah-penduduk-perempuan-menurut-kabupaten-kota-dan-kelompok-umur.html

test_lakik <- read.csv("raw_data/Jumlah Penduduk Laki-Laki menurut Kabupaten_Kota dan Kelompok umur__, 2025.csv",
                       skip = 2) %>% 
  dplyr::slice(-1) %>%
  dplyr::mutate(
    X = stringr::str_trim(as.character(X)),
    adm = case_when(
      stringr::str_detect(X, "[A-Z]") &
        !stringr::str_detect(X, "[a-z]") ~ "province",
      TRUE ~ "regency"
    )
  ) %>%
  dplyr::filter(adm == "regency") %>% 
  tidyr::pivot_longer(
    cols = -c(X, adm),
    names_to = "age_interval",
    values_to = "population"
  ) %>%
  
  # Assign WHO-compatible age groups
  dplyr::mutate(
    age_group = case_when(
      
      # Cannot disaggregate this interval further
      age_interval == "X0.4" ~ "0 to <5 years",
      age_interval %in% c("X5.9", "X10.14") ~
        "5 to <15 years",
      age_interval %in% c(
        "X15.19", "X20.24", "X25.29",
        "X30.34", "X35.39", "X40.44",
        "X45.49"
      ) ~ "15 to <50 years",
      age_interval %in% c(
        "X50.54", "X55.59", "X60.64"
      ) ~ "50 to <65 years",
      age_interval %in% c(
        "X65.69", "X70.74", "X75."
      ) ~ "65+ years",
      TRUE ~ NA_character_
    ),
  ) %>%
  dplyr::filter(!is.na(age_group)) %>%
  dplyr::group_by(
    X,
    adm,
    age_group
  ) %>%
  dplyr::summarise(population = sum(population, na.rm = TRUE),
                   .groups = "drop"
  ) %>% 
  dplyr::mutate(
    sex = "Male"
  ) %>% 
  glimpse()


test_cewek <- read.csv("raw_data/Jumlah Penduduk Perempuan menurut Kabupaten Kota dan Kelompok Umur, 2025.csv",
                       skip = 2) %>% 
  dplyr::slice(-1) %>%
  dplyr::mutate(
    X = stringr::str_trim(as.character(X)),
    adm = case_when(
      stringr::str_detect(X, "[A-Z]") &
        !stringr::str_detect(X, "[a-z]") ~ "province",
      TRUE ~ "regency"
    )
  ) %>%
  dplyr::filter(adm == "regency") %>% 
  tidyr::pivot_longer(
    cols = -c(X, adm),
    names_to = "age_interval",
    values_to = "population"
  ) %>%
  
  # Assign WHO-compatible age groups
  dplyr::mutate(
    age_group = case_when(
      
      # Cannot disaggregate this interval further
      age_interval == "X0.4" ~ "0 to <5 years",
      age_interval %in% c("X5.9", "X10.14") ~
        "5 to <15 years",
      age_interval %in% c(
        "X15.19", "X20.24", "X25.29",
        "X30.34", "X35.39", "X40.44",
        "X45.49"
      ) ~ "15 to <50 years",
      age_interval %in% c(
        "X50.54", "X55.59", "X60.64"
      ) ~ "50 to <65 years",
      age_interval %in% c(
        "X65.69", "X70.74", "X75."
      ) ~ "65+ years",
      TRUE ~ NA_character_
    ),
  ) %>%
  dplyr::filter(!is.na(age_group)) %>%
  dplyr::group_by(
    X,
    adm,
    age_group
  ) %>%
  dplyr::summarise(population = sum(population, na.rm = TRUE),
                   .groups = "drop"
  ) %>% 
  dplyr::mutate(
    sex = "Female"
  ) %>% 
  glimpse()





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


# bind rows and standardise regency ############################################
pop_all <- dplyr::bind_rows(
  test_lakik,
  test_cewek
) %>% 
  dplyr::mutate(
    X = dplyr::recode(X, !!!switch_regencyStandard),
  ) %>% 
  dplyr::left_join(
    area_df %>% 
      dplyr::select(
        -standard_village,
        -standard_district
      ) %>% 
      distinct()
    ,
    by = c(#"province" = "standard_province",
           "X" = "standard_regency")
  ) %>% 
  # test unique value
  # dplyr::group_by(hospital_code) %>% 
  # dplyr::summarise(n = n(), .groups = "drop") %>% 
  # dplyr::filter(n > 1) %>% 
  glimpse()

# check what still doesn't match
dplyr::anti_join(pop_all
                 ,
                 area_df %>% 
                   distinct(standard_province, standard_regency)
                 ,
                 by = c(#"hospital_province" = "standard_province",
                        "X" = "standard_regency")
) %>% 
  distinct(X) %>% #, .keep_all = TRUE) %>% 
  # view() %>% 
  glimpse()

write.csv(pop_all, "inputs/population_bps_data.csv",
          row.names = FALSE)











