# test visualise status besed on age group
library(tidyverse)

source_all <- function(path = "global"){
  files <- list.files(path,
                      pattern = "\\.R$",
                      full.names = TRUE
  )
  invisible(lapply(files, source))
}
source_all()

df_epi_chosen <- read.csv("inputs/influenza_cleaned_data.csv") %>% 
  dplyr::mutate(
    yearWeek = as.Date(yearWeek)
  ) %>% 
  glimpse()

# all data
df_epi_chosen %>% 
  dplyr::filter(influ_pos == "Positive") %>% 
  # dplyr::filter(sentinel_hospital == "Priority 1") %>% 
  dplyr::mutate(
    yearWeek = as.Date(yearWeek)
  ) %>%
  dplyr::group_by(yearWeek,
                  # sex,
                  ) %>%
  dplyr::summarise(
    n = dplyr::n(),
    .groups = "drop"
  ) %>%
  
  ggplot(aes(x = yearWeek,
             y = n,
             # colour = sex,
             # group = sex
             )) +
  
  geom_line(linewidth = 0.7, alpha = 0.8) +
  geom_point(size = 1.2, alpha = 0.6) +
  
  # facet_wrap(~ national,
  #            scales = "free_y",
  #            ncol   = 2) +
  # 
  scale_x_date(
    date_breaks  = "1 months",
    date_labels  = "%b"
  ) +
  
  scale_colour_brewer(
    palette = "Set1",
    name    = "Sex"
  ) +
  
  labs(
    title    = "Weekly case count of Influenza in 2025 (laboratory clarified positive only)",
    x        = "Epidemiological week (labelled by month)",
    y        = "Number of cases"
  ) +
  
  theme_bw(base_size = 11) +
  theme(
    # axis.text.x      = element_text(angle = 60, hjust = 1, size = 7),
    legend.position  = "bottom",
    panel.grid.minor = element_blank()
  ) +
  
  guides(colour = guide_legend(nrow = 2))


# grouped by sex
df_epi_chosen %>% 
  dplyr::filter(influ_pos == "Positive") %>% 
  # dplyr::filter(sentinel_hospital == "Priority 1") %>% 
  dplyr::mutate(
    yearWeek = as.Date(yearWeek)
  ) %>%
  dplyr::group_by(yearWeek,
                  sex,
  ) %>%
  dplyr::summarise(
    n = dplyr::n(),
    .groups = "drop"
  ) %>%
  
  ggplot(aes(x = yearWeek,
             y = n,
             colour = sex,
             group = sex
  )) +
  
  geom_line(linewidth = 0.7, alpha = 0.8) +
  geom_point(size = 1.2, alpha = 0.6) +
  
  # facet_wrap(~ national,
  #            scales = "free_y",
  #            ncol   = 2) +
  # 
  scale_x_date(
    date_breaks  = "1 months",
    date_labels  = "%b"
  ) +
  scale_y_continuous(
    breaks = seq(0, 30, by = 5),
    # labels = scales::percent_format(scale = 1),
    expand = expansion(mult = c(0.05, 0.25))
  ) +
  scale_colour_brewer(
    palette = "Set1",
    name    = "Sex"
  ) +
  
  labs(
    title    = "Weekly case count of Influenza in 2025 grouped by sex (laboratory clarified positive only)",
    x        = "Epidemiological week (labelled by month)",
    y        = "Number of cases"
  ) +
  
  theme_bw(base_size = 11) +
  theme(
    # axis.text.x      = element_text(angle = 60, hjust = 1, size = 7),
    legend.position  = c(0.1, 0.85), # "bottom",
    panel.grid.minor = element_blank()
  ) +
  
  guides(colour = guide_legend(nrow = 2))



# grouped by age group
df_epi_chosen %>% 
  dplyr::filter(influ_pos == "Positive") %>% 
  # dplyr::filter(sentinel_hospital == "Priority 1") %>% 
  dplyr::mutate(
    yearWeek = as.Date(yearWeek)
  ) %>%
  dplyr::group_by(yearWeek,
                  age_group,
  ) %>%
  dplyr::summarise(
    n = dplyr::n(),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    age_group = factor(age_group,
                       levels = c("0 to <6 months", "6m to <1 year",
                                  "1 to <2 years", "2 to <5 years",
                                  "5 to <15 years", "15 to <50 years",
                                  "50 to <65 years", "65+ years"
                       ))
  ) %>% 
  
  ggplot(aes(x = yearWeek,
             y = n,
             colour = age_group,
             group = age_group
  )) +
  
  geom_line(linewidth = 0.7, alpha = 0.8) +
  geom_point(size = 1.2, alpha = 0.6) +
  
  # facet_wrap(~ national,
  #            scales = "free_y",
  #            ncol   = 2) +
  # 
  scale_x_date(
    date_breaks  = "1 months",
    date_labels  = "%b"
  ) +
  scale_y_continuous(
    breaks = seq(0, 30, by = 5),
    # labels = scales::percent_format(scale = 1),
    expand = expansion(mult = c(0.05, 0.25))
  ) +
  scale_colour_brewer(
    palette = "Set1",
    name    = "Sex"
  ) +
  
  labs(
    title    = "Weekly case count of Influenza in 2025 grouped by age group (laboratory clarified positive only)",
    x        = "Epidemiological week (labelled by month)",
    y        = "Number of cases"
  ) +
  
  theme_bw(base_size = 11) +
  theme(
    # axis.text.x      = element_text(angle = 60, hjust = 1, size = 7),
    legend.position  = c(0.3, 0.85), # "bottom",
    panel.grid.minor = element_blank()
  ) +
  
  guides(colour = guide_legend(nrow = 2))



# grouped by Influenza types and/or subtypes
df_epi_chosen %>% 
  dplyr::filter(influ_pos == "Positive") %>% 
  # dplyr::filter(sentinel_hospital == "Priority 1") %>% 
  dplyr::mutate(
    yearWeek = as.Date(yearWeek)
  ) %>%
  dplyr::group_by(yearWeek,
                  influ_subtype,
  ) %>%
  dplyr::summarise(
    n = dplyr::n(),
    .groups = "drop"
  ) %>%
  
  ggplot(aes(x = yearWeek,
             y = n,
             colour = influ_subtype,
             group = influ_subtype
  )) +
  
  geom_line(linewidth = 0.7, alpha = 0.8) +
  geom_point(size = 1.2, alpha = 0.6) +
  
  # facet_wrap(~ national,
  #            scales = "free_y",
  #            ncol   = 2) +
  # 
  scale_x_date(
    date_breaks  = "1 months",
    date_labels  = "%b"
  ) +
  scale_y_continuous(
    breaks = seq(0, 30, by = 5),
    # labels = scales::percent_format(scale = 1),
    expand = expansion(mult = c(0.05, 0.25))
  ) +
  scale_colour_brewer(
    palette = "Set1",
    name    = "influ_subtype"
  ) +
  
  labs(
    title    = "Weekly case count of Influenza in 2025 grouped by influenza subtype (laboratory clarified positive only)",
    x        = "Epidemiological week (labelled by month)",
    y        = "Number of cases"
  ) +
  
  theme_bw(base_size = 11) +
  theme(
    # axis.text.x      = element_text(angle = 60, hjust = 1, size = 7),
    legend.position  = c(0.2, 0.85), # "bottom",
    panel.grid.minor = element_blank()
  ) +
  
  guides(colour = guide_legend(nrow = 2))


# grouped by Influenza types and/or subtypes per age group
df_epi_chosen %>% 
  dplyr::filter(influ_pos == "Positive") %>% 
  # dplyr::filter(sentinel_hospital == "Priority 1") %>% 
  dplyr::mutate(
    yearWeek = as.Date(yearWeek)
  ) %>%
  dplyr::group_by(yearWeek,
                  influ_subtype,
                  age_group
  ) %>%
  dplyr::summarise(
    n = dplyr::n(),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    age_group = factor(age_group,
                       levels = c("0 to <6 months", "6m to <1 year",
                                  "1 to <2 years", "2 to <5 years",
                                  "5 to <15 years", "15 to <50 years",
                                  "50 to <65 years", "65+ years"
                       ))
  ) %>% 
  
  ggplot(aes(x = yearWeek,
             y = n,
             colour = influ_subtype,
             group = influ_subtype
  )) +
  
  geom_line(linewidth = 0.7, alpha = 0.8) +
  geom_point(size = 1.2, alpha = 0.6) +
  
  facet_wrap(~ age_group,
             # scales = "free_y",
             ncol   = 4) +
  
  scale_x_date(
    date_breaks  = "1 months",
    date_labels  = "%b"
  ) +
  scale_y_continuous(
    breaks = seq(0, 30, by = 5),
    # labels = scales::percent_format(scale = 1),
    expand = expansion(mult = c(0.05, 0.25))
  ) +
  scale_colour_brewer(
    palette = "Set1",
    name    = "influ_subtype"
  ) +
  
  labs(
    title    = "Weekly case count of Influenza in 2025 grouped by influenza subtype (laboratory clarified positive only)",
    x        = "Epidemiological week (labelled by month)",
    y        = "Number of cases"
  ) +
  
  theme_bw(base_size = 11) +
  theme(
    # axis.text.x      = element_text(angle = 60, hjust = 1, size = 7),
    legend.position  = "bottom",
    panel.grid.minor = element_blank()
  ) +
  
  guides(colour = guide_legend(nrow = 2))

