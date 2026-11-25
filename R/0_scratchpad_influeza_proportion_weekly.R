library(dplyr)
library(ggplot2)
library(binom)
library(scales)

df <- read.csv("inputs/influenza_cleaned_data.csv",
               sep = ",",
               check.names = TRUE)

df_posneg <- df %>% 
  dplyr::filter(influ_pos %in% c("Positive", "Negative"))

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


# analyse SARI proportion per epidemiological week (per age group, per hospital)
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

# combine influ_pos and sari_tested
influ_sari_estimation_weekly <- dplyr::full_join(
  influ_prev_week_age_hospital %>% 
    dplyr::transmute(
      yearWeek = yearWeek,
      age_group = age_group,
      hospital = hospital,
      
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



################################################################################
# DATA VIZ prevalence weekly per hospital per age group
influ_prev_week_age_hospital_viz <-
  influ_prev_week_age_hospital %>%
  dplyr::filter(sites == "Papua") %>% 
  
  ggplot(
    aes(
      x = yearWeek,
      y = proportion,
      group = 1
    )
  ) +
  
  # 95% CI
  geom_ribbon(
    aes(
      ymin = ci_low_cp,
      ymax = ci_high_cp
    ),
    alpha = 0.2
  ) +
  
  # Positivity proportion
  geom_line(
    linewidth = 0.7
  ) +
  
  geom_point(
    size = 1.5
  ) +
  
  # Facet:
  # hospital = rows
  # age group = columns
  facet_grid(
    hospital ~ age_group
  ) +
  scale_x_date(
    date_breaks= "1 months",
    date_labels= "%b %Y"
  ) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    limits = c(0, 100),
    expand = expansion(mult = c(0.02, 0.05))
  ) +
  
  labs(
    title = "Influenza positivity by epidemiological week",
    subtitle = "(Shaded areas represent 95% confidence intervals)",
    x = "Epidemiological week",
    y = ""
  ) +
  
  theme_bw(base_size = 10) +
  
  theme(
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(
      angle = 60,
      hjust = 1,
      size = 7
    ),
    strip.text = element_text(
      face = "bold"
    )
  )

print(influ_prev_week_age_hospital_viz)

################################################################################
# DATA VIZ influenza associated SARI estimation weekly per hospital per age group
influ_sari_estimation_weekly_viz <-
  influ_sari_estimation_weekly %>%
  dplyr::filter(sites == "Sumatra") %>%
  
  ggplot(
    aes(
      x = yearWeek,
      y = estimation,
      group = 1
    )
  ) +
  
  # 95% CI
  geom_ribbon(
    aes(
      ymin = ci_low_cp,
      ymax = ci_high_cp
    ),
    alpha = 0.2
  ) +
  
  # Positivity proportion
  geom_line(
    linewidth = 0.7
  ) +
  
  geom_point(
    size = 1.5
  ) +
  
  # Facet:
  # hospital = rows
  # age group = columns
  facet_grid(
    hospital ~ age_group
  ) +
  scale_x_date(
    date_breaks= "1 months",
    date_labels= "%b %Y"
  ) +
  # scale_y_continuous(
  # labels = function(x) paste0(x, "%"),
  # limits = c(0, 100),
  # expand = expansion(mult = c(0.02, 0.05))
  # ) +
  
  labs(
    title = "Influenza-associated SARI estimation by epidemiological week",
    subtitle = "(Shaded areas represent 95% confidence intervals)",
    x = "Epidemiological week",
    y = ""
  ) +
  
  theme_bw(base_size = 10) +
  
  theme(
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(
      angle = 60,
      hjust = 1,
      size = 7
    ),
    strip.text = element_text(
      face = "bold"
    )
  )

print(influ_sari_estimation_weekly_viz)


################################################################################
# ANNUAL
# Median annual proportion of Influenza-associated SARI
# load influ_sari_estimation_weekly first!

influ_sari_estimation_annual <- influ_sari_estimation_weekly %>%
  mutate(
    year = 2025
  ) %>%
  # filter(total_ward > 0) %>%
  group_by(year, age_group, hospital) %>%
  summarise(
    
    # influenza associated SARI estimation ANNUAL per hospital per age group
    estimation_annual = sum(estimation, na.rm = TRUE), # estimation is based on weekly sum
    total_sari_annual = sum(total_sari, na.rm = TRUE),
    n_positive = sum(n_positive_influ, na.rm = TRUE),
    n_tested = sum(n_tested_influ, na.rm = TRUE),
    
    n_positive = round(n_positive),
    n_tested = round(n_tested),
    n_positive = pmin(n_positive, n_tested),
    n_positive = pmax(n_positive, 0), 
    n_tested = pmax(n_tested, 1),
    
    
    prop_influ_annual = n_positive/n_tested*100, # while proportion is based on annual aggregated positive & negative
    
    # Exact Clopper-Pearson 95% CI
    ci_low_cp_prop = binom.test(
      n_positive,
      n_tested
    )$conf.int[1]*100,
    
    ci_high_cp_prop = binom.test(
      n_positive,
      n_tested
    )$conf.int[2]*100,
    
    ci_low_cp_estimation = sum(ci_low_cp, na.rm = TRUE),
    ci_high_cp_estimation = sum(ci_high_cp, na.rm = TRUE),
    
    n_weeks = n(),
    .groups = "drop"
    
  ) %>% 
  dplyr::left_join(
    df %>% 
      dplyr::select(hospital, sites) %>% 
      distinct()
    ,
    by = "hospital"
  ) %>% 
  
  glimpse()


# DataViz proportion per age group
influ_prop_annual_sites_agegroup_viz <- influ_sari_estimation_annual %>%
  dplyr::filter(sites == "Papua") %>% 
  ggplot(aes(x = age_group,
             y = prop_influ_annual)) +
  geom_point(size = 3) +
  geom_errorbar(
    aes(ymin = ci_low_cp_prop,
        ymax = ci_high_cp_prop),
    width = 0.3
  ) +
  geom_text(
    aes(y = ci_high_cp_prop,
        label = paste0(round(prop_influ_annual, 1), 
                       "%\n(", n_positive, "/", n_tested, ")")),
    vjust = -0.3,
    size = 2.5,
    colour = "grey20"
  ) +
  
  facet_wrap(~hospital,
             scales = "free_x",
             ncol = 4) +
  
  # facet_grid(
  # sites ~ age_group
  # ) +
  
  scale_y_continuous(
    labels = scales::percent_format(scale = 1),
    limits = c(0, 100),
    expand = expansion(mult = c(0.05, 0.15))
  ) +
  
  scale_colour_brewer(palette = "Set1", guide = "none") +
  
  labs(
    title = "Annual Influenza proportion by hospital per age group per island(s)",
    subtitle = "proportion = (Influenza+)/(Influenza+ + Influenza-); bars = 95% CI",
    x = "",
    y = ""
  ) +
  
  theme_bw(base_size = 10) +
  theme(
    axis.text.x = element_text(angle = 60, hjust = 1, size = 8),
    panel.grid.major.x = element_blank()
  )

print(influ_prop_annual_sites_agegroup_viz)


# DataViz estimation count
influ_estimation_annual_sites_agegroup_viz <- influ_sari_estimation_annual %>%
  dplyr::filter(sites == "Sumatra") %>% 
  ggplot(aes(x = age_group,
             y = estimation_annual)) +
  geom_point(size = 3) +
  geom_errorbar(
    aes(ymin = ci_low_cp_estimation,
        ymax = ci_high_cp_estimation),
    width = 0.3
  ) +
  geom_text(
    aes(y = ci_high_cp_estimation,
        label = paste0(round(prop_influ_annual, 1), 
                       "%\n(", n_positive, "/", n_tested, ")")),
    vjust = -0.3,
    size = 2.5,
    colour = "grey20"
  ) +
  
  facet_wrap(~hospital,
             scales = "free_x",
             ncol = 4) +
  
  # facet_grid(
  # sites ~ age_group
  # ) +
  
  scale_y_continuous(
    # labels = scales::percent_format(scale = 1),
    # limits = c(0, 100),
    expand = expansion(mult = c(0.05, 0.3))
  ) +
  
  scale_colour_brewer(palette = "Set1", guide = "none") +
  
  labs(
    title = "Annual Influenza-associated SARI estimation by hospital per age group per island(s)",
    subtitle = "proportion = (Influenza+)/(Influenza+ + Influenza-); bars = 95% CI",
    x = "",
    y = ""
  ) +
  
  theme_bw(base_size = 10) +
  theme(
    axis.text.x = element_text(angle = 60, hjust = 1, size = 8),
    panel.grid.major.x = element_blank()
  )

print(influ_estimation_annual_sites_agegroup_viz)


################################################################################
# ANNUAL SITES (change hospital to sites)
# Median annual proportion of Influenza-associated SARI per-kelompok umur per-sites only
# load influ_sari_estimation_sites_weekly first!

influ_sari_estimation_sites_annual <- influ_sari_estimation_weekly %>%
  mutate(
    year = 2025
  ) %>%
  # filter(total_ward > 0) %>%
  group_by(year, age_group, sites) %>%
  summarise(
    
    # influenza associated SARI estimation ANNUAL per hospital per age group
    estimation_annual = sum(estimation, na.rm = TRUE), # estimation is based on weekly sum
    total_sari_annual = sum(total_sari, na.rm = TRUE),
    n_positive = sum(n_positive_influ, na.rm = TRUE),
    n_tested = sum(n_tested_influ, na.rm = TRUE),
    
    n_positive = round(n_positive),
    n_tested = round(n_tested),
    n_positive = pmin(n_positive, n_tested),
    n_positive = pmax(n_positive, 0), 
    n_tested = pmax(n_tested, 1),
    
    
    prop_influ_annual = n_positive/n_tested*100, # while proportion is based on annual aggregated positive & negative
    
    # Exact Clopper-Pearson 95% CI
    ci_low_cp_prop = binom.test(
      n_positive,
      n_tested
    )$conf.int[1]*100,
    
    ci_high_cp_prop = binom.test(
      n_positive,
      n_tested
    )$conf.int[2]*100,
    
    ci_low_cp_estimation = sum(ci_low_cp, na.rm = TRUE),
    ci_high_cp_estimation = sum(ci_high_cp, na.rm = TRUE),
    
    n_weeks = n(),
    .groups = "drop"
    
  ) %>% 
  dplyr::mutate(
    sites = factor(sites,
                   levels = c("Sumatra", "Jawa", "Kalimantan", "Bali & Nusa Tenggara",
                              "Sulawesi", "Maluku", "Papua")
    ),
  ) %>% 
  
  glimpse()


# DataViz proportion per age group
influ_prop_annual_sites_agegroup_viz <- influ_sari_estimation_sites_annual %>%
  # dplyr::filter(sites == "Papua") %>% 
  ggplot(aes(x = age_group,
             y = prop_influ_annual)) +
  geom_point(size = 3) +
  geom_errorbar(
    aes(ymin = ci_low_cp_prop,
        ymax = ci_high_cp_prop),
    width = 0.3
  ) +
  geom_text(
    aes(y = ci_high_cp_prop,
        label = paste0(round(prop_influ_annual, 1), 
                       "%\n(", n_positive, "/", n_tested, ")")),
    vjust = -0.3,
    size = 2.5,
    colour = "grey20"
  ) +
  
  facet_wrap(~sites,
             scales = "free_x",
             ncol = 4) +
  
  # facet_grid(
  # sites ~ age_group
  # ) +
  
  scale_y_continuous(
    labels = scales::percent_format(scale = 1),
    limits = c(0, 100),
    expand = expansion(mult = c(0.05, 0.15))
  ) +
  
  scale_colour_brewer(palette = "Set1", guide = "none") +
  
  labs(
    title = "Annual Influenza proportion by island(s) per age group",
    subtitle = "proportion = (Influenza+)/(Influenza+ + Influenza-); bars = 95% CI",
    x = "",
    y = ""
  ) +
  
  theme_bw(base_size = 10) +
  theme(
    axis.text.x = element_text(angle = 60, hjust = 1, size = 8),
    panel.grid.major.x = element_blank()
  )

print(influ_prop_annual_sites_agegroup_viz)


# DataViz estimation count
influ_estimation_annual_sites_agegroup_viz <- influ_sari_estimation_sites_annual %>%
  # dplyr::filter(sites == "Papua") %>% 
  ggplot(aes(x = age_group,
             y = estimation_annual)) +
  geom_point(size = 3) +
  geom_errorbar(
    aes(ymin = ci_low_cp_estimation,
        ymax = ci_high_cp_estimation),
    width = 0.3
  ) +
  geom_text(
    aes(y = ci_high_cp_estimation,
        label = paste0(round(prop_influ_annual, 1), 
                       "%\n(", n_positive, "/", n_tested, ")")),
    vjust = -0.3,
    size = 2.5,
    colour = "grey20"
  ) +
  
  facet_wrap(~ sites,
             scales = "free_x",
             ncol = 4) +
  
  # facet_grid(
  # sites ~ age_group
  # ) +
  
  scale_y_continuous(
    # labels = scales::percent_format(scale = 1),
    # limits = c(0, 100),
    expand = expansion(mult = c(0.05, 0.15))
  ) +
  
  scale_colour_brewer(palette = "Set1", guide = "none") +
  
  labs(
    title = "Annual Influenza-associated SARI estimation by island(s) per age group",
    subtitle = "proportion = (Influenza+)/(Influenza+ + Influenza-); bars = 95% CI",
    x = "",
    y = ""
  ) +
  
  theme_bw(base_size = 10) +
  theme(
    axis.text.x = element_text(angle = 60, hjust = 1, size = 8),
    panel.grid.major.x = element_blank()
  )

print(influ_estimation_annual_sites_agegroup_viz)


################################################################################
# ANNUAL NASIONAL
# Median annual proportion of Influenza-associated SARI per-kelompok umur only
# load influ_sari_estimation_national_weekly first!

influ_sari_estimation_national_annual <- influ_sari_estimation_annual %>%
  mutate(
    year = 2025
  ) %>%
  # filter(total_ward > 0) %>%
  group_by(year, age_group) %>%
  summarise(
    
    # influenza associated SARI estimation ANNUAL per hospital per age group
    estimation_annual = sum(estimation_annual, na.rm = TRUE), # estimation is based on weekly sum
    total_sari_annual = sum(total_sari_annual, na.rm = TRUE),
    n_positive = sum(n_positive, na.rm = TRUE),
    n_tested = sum(n_tested, na.rm = TRUE),
    
    n_positive = round(n_positive),
    n_tested = round(n_tested),
    n_positive = pmin(n_positive, n_tested),
    n_positive = pmax(n_positive, 0), 
    n_tested = pmax(n_tested, 1),
    
    
    prop_influ_annual = n_positive/n_tested*100, # while proportion is based on annual aggregated positive & negative
    
    # Exact Clopper-Pearson 95% CI
    ci_low_cp_prop = binom.test(
      n_positive,
      n_tested
    )$conf.int[1]*100,
    
    ci_high_cp_prop = binom.test(
      n_positive,
      n_tested
    )$conf.int[2]*100,
    
    ci_low_cp_estimation = sum(ci_low_cp_estimation, na.rm = TRUE),
    ci_high_cp_estimation = sum(ci_high_cp_estimation, na.rm = TRUE),
    
    n_weeks = n(),
    .groups = "drop"
    
  ) %>% 
  
  glimpse()


# DataViz proportion per age group
influ_prop_annual_national_agegroup_viz <- influ_sari_estimation_national_annual %>%
  # dplyr::filter(sites == "Papua") %>% 
  ggplot(aes(x = age_group,
             y = prop_influ_annual)) +
  geom_point(size = 3) +
  geom_errorbar(
    aes(ymin = ci_low_cp_prop,
        ymax = ci_high_cp_prop),
    width = 0.3
  ) +
  geom_text(
    aes(y = ci_high_cp_prop,
        label = paste0(round(prop_influ_annual, 1), 
                       "%\n(", n_positive, "/", n_tested, ")")),
    vjust = -0.3,
    size = 2.5,
    colour = "grey20"
  ) +
  
  # facet_wrap(~sites,
  #scales = "free_x",
  #ncol = 4) +
  
  # facet_grid(
  # sites ~ age_group
  # ) +
  
  scale_y_continuous(
    labels = scales::percent_format(scale = 1),
    limits = c(0, 40),
    expand = expansion(mult = c(0.05, 0.15))
  ) +
  
  scale_colour_brewer(palette = "Set1", guide = "none") +
  
  labs(
    title = "Annual Influenza proportion per age group",
    subtitle = "proportion = (Influenza+)/(Influenza+ + Influenza-); bars = 95% CI",
    x = "",
    y = ""
  ) +
  
  theme_bw(base_size = 10) +
  theme(
    # axis.text.x = element_text(angle = 0, hjust = 1, size = 8),
    panel.grid.major.x = element_blank()
  )

# print(influ_prop_annual_national_agegroup_viz)


# DataViz estimation count
influ_estimation_annual_national_agegroup_viz <- influ_sari_estimation_national_annual %>%
  # dplyr::filter(sites == "Papua") %>% 
  ggplot(aes(x = age_group,
             y = estimation_annual)) +
  geom_point(size = 3) +
  geom_errorbar(
    aes(ymin = ci_low_cp_estimation,
        ymax = ci_high_cp_estimation),
    width = 0.3
  ) +
  geom_text(
    aes(y = ci_high_cp_estimation,
        label = paste0(round(prop_influ_annual, 1), 
                       "%\n(", n_positive, "/", n_tested, ")")),
    vjust = -0.3,
    size = 2.5,
    colour = "grey20"
  ) +
  
  # facet_wrap(~ sites,
  #scales = "free_x",
  #ncol = 4) +
  
  # facet_grid(
  # sites ~ age_group
  # ) +
  
  scale_y_continuous(
    # labels = scales::percent_format(scale = 1),
    # limits = c(0, 100),
    expand = expansion(mult = c(0.05, 0.15))
  ) +
  
  scale_colour_brewer(palette = "Set1", guide = "none") +
  
  labs(
    title = "Annual Influenza-associated SARI estimation per age group",
    subtitle = "proportion = (Influenza+)/(Influenza+ + Influenza-); bars = 95% CI",
    x = "",
    y = ""
  ) +
  
  theme_bw(base_size = 10) +
  theme(
    # axis.text.x = element_text(angle = 0, hjust = 1, size = 8),
    panel.grid.major.x = element_blank()
  )

# print(influ_estimation_annual_national_agegroup_viz)

print_national <- cowplot::plot_grid(influ_prop_annual_national_agegroup_viz,
                                     influ_estimation_annual_national_agegroup_viz,
                                     nrow = 2,
                                     labels = c("A", "B"))

print(print_national)

