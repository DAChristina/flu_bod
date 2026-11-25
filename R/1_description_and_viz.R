library(tidyverse)
library(ComplexUpset)
source("global/fun.R")
source("global/obj.R")

df_epi_chosen <- read.csv("inputs/influenza_cleaned_data.csv") %>%
  dplyr::select(
    influ_pos,
    influ_subtype,
    age_group,
    sex,
    status,
    contains(c("diag_",
               "com_",
               "otr_"
    )),
    -diag_admission_diagnosis,
    -diag_other_admission_diagnosis,
    -diag_respiratory_rate,
    -otr_travel_country_1,
    -otr_travel_country_1
  ) %>% 
  
  dplyr::mutate(
    across(6:ncol(.), as.numeric)
    
    # readjust column naming for publication
    
  ) %>%
  as.data.frame() %>% 
  dplyr::mutate(
    across(where(is.integer), as.double),
    Test = influ_subtype
  ) %>% 
  # dplyr::rename(!!!col_pub) %>%
  dplyr::filter(
    influ_pos != "No specimen collected",
    Test != "Negative/Invalid"
    ) %>%
  glimpse()

# png(file = "pictures/epi_combinations_comorbidities.png",
#     width = 29, height = 15, unit = "cm", res = 600)
target <- df_epi_chosen %>% 
  # non-laboratory results
  dplyr::select(
    -influ_pos,
    -status,
    -sex, -age_group,
    -Test,
    
    -contains(c("diag_", "otr_"))
  ) %>% 
  colnames()

ComplexUpset::upset(
  df_epi_chosen,
  intersect = target,
  # encode_sets=FALSE, # for annotate() to select the set by name disable encoding
  base_annotations = list(
    "Test result" = intersection_size(
      counts = FALSE,
      mapping = aes(fill=Test),
      text = list(
        check_overlap=TRUE,
        vjust = -0.1,
        hjust = -0.1
      ))), # +
  # scale_fill_manual(values=col_map)),
  annotations = list(
    "Age\n(%)" = ggplot(mapping=aes(fill=status)) + # test age_group or status
      geom_bar(stat='count', position='fill') +
      # scale_fill_manual(values=col_map) +
      scale_y_continuous(labels=scales::percent_format())
  ),
  # queries=list(
  #   upset_query(
  #     intersect=c('pneumonia', 'pleural_effusion'),
  #     color='red',
  #     fill='red',
  #     only_components=c('intersections_matrix', 'Intersection size')
  #   ),
  # ),
  wrap = F,
  mode = "distinct",
  width_ratio=0.1,
  min_degree=1, # min combination
  set_sizes=upset_set_size(mapping = aes(),
                           geom = geom_bar(aes(fill=influ_pos,
                                               x=group),
                                           width = 0.6),
                           position = "left",
                           filter_intersections = T) +
    # scale_fill_manual(values=col_map) +
    geom_text(aes(label=..count..),
              hjust=1.1, size = 3,
              stat='count') +
    expand_limits(y=200) +
    theme(legend.position = "none",
          axis.text.x=element_text(angle=90))
)
# dev.off()


