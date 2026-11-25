col_map <- c(
  # basic yes/no
  "yes" = "steelblue",
  "no" = "gray80",
  
  # sample availability
  "Positive" = "indianred2",
  "Negative" = "seagreen4",
  "No specimen collected" = "gray50",
  "Invalid" = "orange",
  
  # sex
  "Male"   = "royalblue4",
  "Female" = "tomato3",
  
  # Influenza subtypes
  "A (H1N1) pdm09" = "tomato3",
  "A (H3N2)" = "orange",
  "A (Nontypeable)" = "gold2",
  "B" = "royalblue4",
  "Flu B" = "royalblue4"
  
)

sentinel_province <- c(
  # province based on "Concept Note - BoD.docx"
  "Sumatera Utara",
  "Sumatera Selatan",
  "Lampung",
  "Banten",
  "DKI Jakarta",
  "Jawa Barat",
  "DI Yogyakarta",
  "Jawa Tengah",
  "Bali",
  "Maluku Utara",
  "Kalimantan Utara"
)

sentinel_plus_puskesmas <- c(
  # hospital according to "Diskusi Konsep.pptx"
  # Sumatera Utara:
  "RSUD H. Amri Tambunan", # Deli Serdang
  "Puskesmas Teladan", # Medan
  # Sumatera Selatan:
  "RSUP dr. M. Hoesin", # Palembang
  "Puskesmas Tujuh Ulu", # Palembang
  # Lampung:
  "RSUD dr. Abdul Moeloek", # Bandar Lampung
  "Puskesmas Sumur Batu", # Bandar Lampung
  
  # Banten:
  "RSUP dr. Sitanala", # Tangerang
  "Puskesmas Serpong I", # Tangerang Selatan
  # Jakarta:
  "RSPI Prof. Dr. Sulianti Saroso",
  # Jawa Barat:
  "RSUP dr. Hasan Sadikin", # Bandung
  "Puskesmas Padasuka", # Bandung
  # Jawa Tengah:
  "RSUP dr. Kariadi", # Semarang
  "Puskesmas Pandanaran", # Semarang
  
  # Kalimantan Timur:
  "RSUD dr. Kanujoso Djatiwibowo", # Balikpapan
  "Puskesmas Klandasan Ilir", # Balikpapan
  # DI Yogyakarta:
  "RSUP dr. Sardjito",
  "Puskesmas Danurejan I",
  
  # Maluku Utara:
  "RSUD Dr. H. Chasan Boesoirie", # Ternate
  "Puskesmas Kalumata", # Ternate
  
  # Bali:
  "RSUD Wangaya", # Denpasar
  "Puskesmas Denpasar I" # Denpasar
)

sentinel_hospital_priority1 <- c(
  # hospital according to "DATA REVIEW for BoD.xlsx"
  # orange marker
  "RSUP dr. Hasan Sadikin", # Java
  "RSUP dr. Sardjito", # Java
  "RSUD dr. Saiful Anwar", # Java
  "RSUD dr. Jusuf K", # Kalimantan
  "RSUD dr. Kanujoso Djatiwibowo", # Kalimantan
  "RSUD Dr. H. Chasan Boesoirie", # Maluku utara
  "RSUD Prov. NTB", # Lesser Sundas
  "RSUD Jayapura", # Papua
  "RSUD dr. Hasri Ainun Habibie", # Sulawesi
  "RSUD Undata Palu", # Sulawesi
  "RSUD H. Amri Tambunan", # Sumatera
  "RSUD dr. Abdul Moeloek" # Sumatera
)

sentinel_hospital_priority2 <- c(
  # hospital according to "DATA REVIEW for BoD.xlsx"
  # yellow marker
  "RSUD Wangaya", # Bali
  "RSUP dr. Sitanala", # Java
  "RSPI Prof. Dr. Sulianti Saroso", # Java
  "RSUP dr. Kariadi", # Java
  "RSUP dr. M. Hoesin" # Sumatera
)

# switch hospital name epi <-> hospital list
switch_hosName <- c(
  "RSUD dr. Hasri Ainun Habibie" = "RS Umum Daerah dr. Hasri Ainun Habibie", # or "RS Dr. Hasri Ainun Habibie Parepare"
  "RSUD Raden Mattaher" = "RS Umum Daerah Raden Mattaher Jambi",
  "RSUP dr. M. Hoesin" = "RS Umum Pusat Dr. Mohammad Hoesin Palembang",
  "RSUD Zainoel Abidin" = "RS Umum Daerah Dr. Zainoel Abidin",
  "RSUP dr. M. Djamil" = "RSUP Dr. M. Djamil",
  "RSUP dr. Sardjito" = "RSUP Dr. Sardjito",
  "RSUD dr. Abdul Moeloek" = "RS Umum Daerah Dr H Abdul Moeloek",
  "RSPI Prof. Dr. Sulianti Saroso" = "RS Penyakit Infeksi Prof. Dr. Sulianti Saroso",
  "RSUP dr. Hasan Sadikin" = "RS Umum Pusat Dr. Hasan Sadikin",
  "RSUP dr. Sitanala" = "RSUP Dr. Sitanala Tangerang",
  "RSUD dr. Jusuf K" = "RS Umum Daerah dr.H. Jusuf SK", 
  "RSUD Kota Kendari" = "RS Umum Daerah Kota Kendari",
  "RSUD Embung Fatimah" = "RS Umum Daerah Embung Fatimah Kota Batam",
  "RSUP dr. Kariadi" = "RS Umum Pusat Dr. Kariadi",
  "RSUP dr. Johannes Leimena" = "RSUP dr. Johannes Leimena", # NA_character_, # Not found in the CSV
  "RSUP Prof. Dr. R.D. Kandou" = "RSUP Prof. Dr. R.D. Kandou", # NA_character_, # Not found in the CSV
  "RSUD Merauke" = "RS Umum Merauke",
  "RSUD H. Amri Tambunan" = "RS Umum Daerah Drs. H. Amri Tambunan",
  "RSUD dr. Saiful Anwar" = "RS Umum Daerah Dr. Saiful Anwar",
  "RSUD Prov. NTB" = "RS Umum Daerah Provinsi NTB",
  "RSUD Undata Palu" = "RS Umum Daerah Undata Palu",
  "RSUD dr. Soedarso" = "RS Umum Daerah Dr. Soedarso Pontianak",
  "RSUD Depati Bahrin" = "RS Umum Daerah Depati Bahrin",
  "RSUD Provinsi Papua Barat" = "RS Umum Daerah Provinsi Papua Barat",
  "RSUD Dr. H. Chasan Boesoirie" = "RS Umum Daerah Dr. H. Chasan Boesoirie Ternate",
  "RSUD dr. Kanujoso Djatiwibowo" = "RS Umum Daerah Dr. Kanujoso Djatiwibowo",
  "RSUD Ulin Banjarmasin" = "RS Umum Daerah Ulin Banjarmasin",
  "RSUD Wangaya" = "RS Umum Daerah Wangaya",
  "RSUD Arifin Achmad" = "RS Umum Daerah Arifin Achmad",
  "RSUP dr.Wahidin Sudirohusodo" = "RSUP Dr. Wahidin Sudirohusodo",
  "RSUD Prof. Dr. W Z Johannes" = "RS Umum Prof. Dr. WZ Johanes", 
  "RSUD dr. Doris Slyvanus" = "RS Umum Daerah Dr. Doris Sylvanus Palangka Raya", 
  "RSUD Dr. M. Yunus" = "RS Umum Daerah Dr. M. Yunus Bengkulu",
  "RSUD Jayapura"  = "RS Umum Jayapura", # or "RS Umum Pusat Jayapura"
  "RSUD Prov. Sulawesi Barat" = "RS Umum Daerah Provinsi Sulawesi Barat"
)

switch_regencyStandard <- c(
  "Banda Aceh" = "Kota Banda Aceh",
  
  "Banyu Asin" = "Banyuasin",
  "Batang Hari" = "Batanghari",
  "Fakfak" = "Fak Fak",
  "Gunung Kidul" = "Gunungkidul",
  "Kep. Seribu" = "Administrasi Kepulauan Seribu",
  "Kota Banjar Baru" = "Kota Banjarbaru",
  "Kota Baubau" = "Kota Bau Bau",
  
  
  "Muko Muko" = "Mukomuko",
  "Pahuwato" = "Pohuwato",
  "Kota Pare Pare" = "Kota Parepare",
  "Pangkajene Kepulauan" = "Pangkajene Dan Kepulauan",
  "Toli Toli" = "Toli-Toli",
  "Kota Padang Sidempuan" = "Kota Padangsidimpuan",
  "Kota Tanjung Balai" = "Kota Tanjungbalai",
  "Adm. Kep. Seribu" = "Administrasi Kepulauan Seribu",
  "Kota Adm. Jakarta Barat" = "Kota Administrasi Jakarta Barat",
  "Kota Jakarta Barat" = "Kota Administrasi Jakarta Barat",
  "Kota Adm. Jakarta Pusat" = "Kota Administrasi Jakarta Pusat",
  "Kota Jakarta Pusat" = "Kota Administrasi Jakarta Pusat",
  "Kota Adm. Jakarta Selatan" = "Kota Administrasi Jakarta Selatan",
  "Kota Jakarta Selatan" = "Kota Administrasi Jakarta Selatan",
  "Kota Adm. Jakarta Timur" = "Kota Administrasi Jakarta Timur",
  "Kota Jakarta Timur" = "Kota Administrasi Jakarta Timur",
  "Kota Adm. Jakarta Utara" = "Kota Administrasi Jakarta Utara",
  "Kota Jakarta Utara" = "Kota Administrasi Jakarta Utara",
  
  "Kota Lubuklinggau" = "Kota Lubuk Linggau",
  "Kota Makasar" = "Kota Makassar",
  "Kota Palangka Raya" = "Kota Palangkaraya",
  "Kota Pematang Siantar" = "Kota Pematangsiantar",
  "Kota Sawah Lunto" = "Kota Sawahlunto",
  "Labuhan Batu" = "Labuhanbatu",
  "Labuhan Batu Selatan" = "Labuhanbatu Selatan",
  "Labuhan Batu Utara" = "Labuhanbatu Utara",
  
  "Maluku Tenggara Barat / Kepulauan Tanimbar" = "Kepulauan Tanimbar",
  "Mamuju Utara / Pasangkayu" = "Pasangkayu",
  "Pangkajene dan Kepulauan" = "Pangkajene Dan Kepulauan",
  "Siau Tagulandang Biaro" = "Kep. Siau Tagulandang Biaro",
  "Toba Samosir / Toba" = "Toba",
  "Tojo Una-Una" = "Tojo Una Una",
  "Tulangbawang" = "Tulang Bawang"
)

