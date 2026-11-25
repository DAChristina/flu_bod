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

df_epi_chosen <- read.csv("inputs/influenza_cleaned_data.csv")

df_epi_chosen %>% 
  dplyr::group_by(age_group, status, influ_pos) %>% 
  dplyr::summarise(
    n = n(),
    .groups = "drop"
  ) %>% 
  dplyr::mutate(
    age_group = factor(age_group,
                       levels = c("0 to <6 months", "6m to <1 year",
                                  "1 to <2 years", "2 to <5 years",
                                  "5 to <15 years", "15 to <50 years",
                                  "50 to <65 years", "65+ years"
                       )
    )
  ) %>% 
  dplyr::filter(!influ_pos %in% c("Positive", "Negative")) %>% 
  ggplot(
    aes(
      x = age_group,
      y = n,
      colour = influ_pos,
      group = influ_pos
    )
  ) +
  geom_line() +
  geom_point() +
  facet_wrap(~ status,
             scales = "free_y") +
  theme_bw() +
  scale_colour_manual(
    values = c(col_map),
    name = "Influenza status"
  ) +
  labs(
    x = "Age group",
    y = "Number of patients"
  ) +
  theme(
    axis.text.x = ggtext::element_markdown(
      angle = 60,
      hjust = 1,
      size  = 8
    ),
    legend.position = "bottom",
    panel.grid.major.x = element_blank()
  )


# Viz use barplot instead ######################################################
df_epi_chosen %>% 
  dplyr::group_by(age_group, status, influ_pos) %>% 
  dplyr::summarise(
    n = n(),
    .groups = "drop"
  ) %>% 
  dplyr::mutate(
    age_group = factor(age_group,
                       levels = c("0 to <6 months", "6m to <1 year",
                                  "1 to <2 years", "2 to <5 years",
                                  "5 to <15 years", "15 to <50 years",
                                  "50 to <65 years", "65+ years"
                       )
    ),
    influ_pos = factor(influ_pos,
                       levels = c("Positive", "Negative",
                                  "Invalid",
                                  "No specimen collected"
                                  )
      
    )
  ) %>% 
  # dplyr::filter(!influ_pos %in% c("Positive", "Negative")) %>%
  dplyr::filter(influ_pos == "Positive") %>%
  ggplot(aes(
    x = age_group,
    y = n,
    fill = influ_pos
  )) +
  geom_col(position = "dodge") +
  facet_wrap(~ status, scales = "free_y") +
  scale_fill_manual(
    values = col_map,
    name = "Influenza status"
  ) +
  labs(
    x = "Age group",
    y = "Number of patients"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )
