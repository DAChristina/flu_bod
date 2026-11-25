Influenza_table <- df %>%
  filter(influ_pos %in% c("Positive", "Negative")) %>%
  mutate(
    influ_pos = factor(
      influ_pos,
      levels = c("Positive", "Negative")
    ),
    
    age_group = factor(age_group,
                       levels = c("0 to <6 months", "6m to <1 year",
                                  "1 to <2 years", "2 to <5 years",
                                  "5 to <15 years", "15 to <50 years",
                                  "50 to <65 years", "65+ years"
                       ))
  ) %>%
  select(age_group, sex, influ_pos) %>%
  tbl_summary(
    by = influ_pos,
    label = list(
      age_group ~ "Age group",
      sex ~ "Sex"
    ),
    statistic = list(
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = list(
      all_categorical() ~ c(0, 1)
    ),
    missing = "no"
  ) %>%
  
  add_overall(
    last = FALSE
  ) %>%
  
  add_p(
    test = list(
      all_categorical() ~ "fisher.test"
    ),
    test.args = list(
      all_categorical() ~ list(
        simulate.p.value = TRUE,
        B = 10000
      )
    )
  ) %>%
  
  modify_header(
    label ~ "**Characteristic**",
    stat_0 ~ "**Overall**\nn = {n}",
    stat_1 ~ "**Influenza Positive**\nn = {n}",
    stat_2 ~ "**Influenza Negative**\nn = {n}",
    p.value ~ "**P-value**"
  ) %>%
  
  modify_caption(
    "**Table: Influenza positivity by age group and sex**"
  ) %>%
  
  bold_labels() %>%
  bold_p(t = 0.05)

Influenza_table

################################################################################
