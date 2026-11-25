# test piechart
library(tidyverse)

# 1. All sample status #########################################################
df <- read.csv("inputs/influenza_cleaned_data.csv") %>% 
  dplyr::mutate(
    sites = factor(sites,
                   levels = c("Sumatra", "Java", "Bali & Nusa Tenggara",
                              "Kalimantan", "Sulawesi", "Maluku",
                              "Papua"
                              )
                   )
  ) %>% 
  glimpse()

fluall <- df %>% 
  # dplyr::filter(influ_pos == "Positive") %>%
  dplyr::group_by(influ_pos) %>% 
  dplyr::summarise(count = n()) %>%
  dplyr::mutate(prop = round(count/sum(count)*100, 1),
                report = paste0(count, "\n(", prop, "%)")
  ) %>% 
  dplyr::arrange(desc(influ_pos)) %>%
  dplyr::mutate(label_coor = cumsum(prop)-0.5*prop, # arrange first & mutate
  ) %>% 
  ggplot(
    .,
    aes(x = "", y = prop, fill = influ_pos)
  ) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start=0) +
  # ggtitle(h) +
  geom_text(aes(y = label_coor,
                label = report),
            colour = "white", size = 3) +
  guides(fill = guide_legend(nrow = 1)) +
  scale_fill_manual(values=col_map) +
  theme_void() +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        plot.title = element_text(size = 10)
  )

fluall

# 2. bar #######################################################################
# source: https://onlinelibrary.wiley.com/doi/full/10.1111/irv.70061
flu <- df %>%
  dplyr::filter(!is.na(date)) %>%
  dplyr::mutate(
    month_date = floor_date(as.Date(date), "month"),
    influ_subtype = ifelse(influ_subtype == "B", "Flu B", influ_subtype)
    )

all_months <- seq(min(flu$month_date), max(flu$month_date), by = "month")

bars <- flu %>%
  dplyr::filter(
    influ_pos == "Positive",
    !is.na(influ_subtype)) %>% # positives only
  dplyr::count(
    month_date,
    subtype = influ_subtype,
    name = "n"
    ) %>% 
  glimpse()

sub_levels <- intersect(names(col_map), unique(bars$subtype))
bars <- bars %>% 
  dplyr::filter(
    subtype %in% sub_levels
    ) %>%
  dplyr::mutate(
    subtype = factor(subtype, levels = sub_levels)
    ) %>%
  tidyr::complete(
    month_date = all_months,
    subtype,
    fill = list(n = 0)
    ) %>%
  glimpse()

# line
pct <- flu %>%
  dplyr::filter(influ_pos %in% c("Positive", "Negative")) %>%
  dplyr::group_by(month_date) %>%
  dplyr::summarise(
    pct = mean(influ_pos == "Positive")*100,
    .groups = "drop"
    ) %>%
  tidyr::complete(month_date = all_months) %>% 
  glimpse()

lab <- function(d) d %>% 
  dplyr::mutate(
    year = factor(year(month_date)),
    month = factor(month(month_date),
                   levels = 1:12,
                   labels = month.name)
    )
bars <- lab(bars)
pct <- lab(pct)

# secondary axis
y_max <- ceiling(max(tapply(bars$n, bars$month_date, sum))/10)*10
pct_max <- ceiling(max(pct$pct, na.rm = TRUE)/10)*10
k <- y_max/pct_max

# plot
barall <- ggplot() +
  geom_col(data = bars,
           aes(month, n,
               fill = subtype),
           position = position_stack(reverse = TRUE),
           width = 0.85,
           colour = "white",
           linewidth = 0.25) +
  geom_line(data = pct,
            aes(month, pct*k,
                group = year,
                colour = "% positive (Flu)"),
            linewidth = 1.6, na.rm = TRUE) +
  scale_fill_manual(NULL,
                    values = col_map[sub_levels],
                    drop = FALSE) +
  scale_colour_manual(NULL,
                      values = c("% positive (Flu)" = "black")
                      ) +
  scale_y_continuous("Influenza cases",
                     limits = c(0, y_max),
                     expand = expansion(mult = c(0, 0.02)),
                     sec.axis = sec_axis(~ ./k,
                                         name = "Influenza positivity",
                                         labels = function(x) paste0(x, "%")
                                         )
                     ) +
  # facet_grid(. ~ year,
  #            scales = "free_x", space = "free_x",
  #            switch = "x") +
  labs(x = NULL) +
  theme_bw(base_size = 14) +
  theme(strip.placement = "outside",
        strip.background = element_rect(fill = "white"),
        panel.spacing.x = unit(0, "pt"),
        panel.grid = element_blank(),
        axis.text.x = element_text(angle = 45,
                                   hjust = 1, vjust = 1),
        legend.position = "top")

barall


# 3. Piechart ##################################################################
fluTypes_all <- df %>% 
  dplyr::filter(influ_pos == "Positive") %>%
  dplyr::group_by(influ_subtype) %>% 
  dplyr::summarise(count = n()) %>%
  dplyr::mutate(prop = round(count/sum(count)*100, 1),
                report = paste0(count, "\n(", prop, "%)")
  ) %>% 
  dplyr::arrange(desc(influ_subtype)) %>%
  dplyr::mutate(label_coor = cumsum(prop)-0.5*prop, # arrange first & mutate
  ) %>% 
  ggplot(
    .,
    aes(x = "", y = prop, fill = influ_subtype)
  ) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start=0) +
  # ggtitle(h) +
  geom_text(aes(y = label_coor,
                label = report),
            colour = "white", size = 5) +
  guides(fill = guide_legend(nrow = 1)) +
  scale_fill_manual(values=col_map) +
  theme_void() +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        plot.title = element_text(size = 10)
  )

fluTypes_all

# 3. stratify all influenza subtypes per island(s)
islands <- unique(df$sites)
plotStore_islands <- list()

for(h in islands){
  p <- df %>% 
    dplyr::filter(influ_pos == "Positive") %>%
    dplyr::filter(sites == h) %>% 
    dplyr::group_by(influ_subtype) %>% 
    dplyr::summarise(count = n()) %>%
    dplyr::mutate(prop = round(count/sum(count)*100, 1),
                  report = paste0(count, "\n(", prop, "%)")
    ) %>% 
    dplyr::arrange(desc(influ_subtype)) %>%
    dplyr::mutate(label_coor = cumsum(prop)-0.5*prop, # arrange first & mutate
    ) %>% 
    ggplot(
      .,
      aes(x = "", y = prop, fill = influ_subtype)
    ) +
    geom_bar(stat = "identity", width = 1, color = "white") +
    coord_polar("y", start=0) +
    ggtitle(h) +
    geom_text(aes(y = label_coor,
                  label = report),
              colour = "white", size = 2) +
    guides(fill = guide_legend(nrow = 3)) +
    scale_fill_manual(values=col_map) +
    theme_void() +
    theme(#legend.title = element_blank(),
      legend.position = "none",
      plot.title = element_text(size = 10)
    )
  
  # redefine plots
  plotStore_islands[[h]] <- p
}

fluTypes_islands <- cowplot::plot_grid(plotlist = plotStore_islands,
                                       nrow = 3)
fluTypes_islands

fluTypes_total <- cowplot::plot_grid(fluTypes_all, fluTypes_islands,
                                     ncol = 2,
                                     rel_widths = c(2, 1),
                                     labels = c("A", "B"))
fluTypes_total


# Test medication status within positive-confirmed patients ####################
fluTypes_all <- df %>% 
  dplyr::filter(influ_pos == "Positive") %>%
  dplyr::group_by(influ_subtype) %>% 
  dplyr::summarise(count = n()) %>%
  dplyr::mutate(prop = round(count/sum(count)*100, 1),
                report = paste0(count, "\n(", prop, "%)")
  ) %>% 
  dplyr::arrange(desc(influ_subtype)) %>%
  dplyr::mutate(label_coor = cumsum(prop)-0.5*prop, # arrange first & mutate
  ) %>% 
  ggplot(
    .,
    aes(x = "", y = prop, fill = influ_subtype)
  ) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start=0) +
  # ggtitle(h) +
  geom_text(aes(y = label_coor,
                label = report),
            colour = "white", size = 5) +
  guides(fill = guide_legend(nrow = 1)) +
  scale_fill_manual(values=col_map) +
  theme_void() +
  theme(legend.title = element_blank(),
        legend.position = "none",
        plot.title = element_text(size = 10)
  )

fluTypes_all

# 3. stratify all influenza subtypes per island(s)
# islands <- unique(df$sites)
islands <- c("Sumatra", "Java", "Bali & Nusa Tenggara",
             "Kalimantan", "Sulawesi", "Maluku",
             "Papua"
)
plotStore_islands <- list()

for(h in islands){
  p <- df %>% 
    dplyr::filter(influ_pos == "Positive") %>%
    dplyr::filter(sites == h) %>% 
    dplyr::group_by(influ_subtype) %>% 
    dplyr::summarise(count = n()) %>%
    dplyr::mutate(prop = round(count/sum(count)*100, 1),
                  report = paste0(count, "\n(", prop, "%)")
    ) %>% 
    dplyr::arrange(desc(influ_subtype)) %>%
    dplyr::mutate(label_coor = cumsum(prop)-0.5*prop, # arrange first & mutate
    ) %>% 
    ggplot(
      .,
      aes(x = "", y = prop, fill = influ_subtype)
    ) +
    geom_bar(stat = "identity", width = 1, color = "white") +
    coord_polar("y", start=0) +
    ggtitle(h) +
    geom_text(aes(y = label_coor,
                  label = report),
              colour = "white", size = 2) +
    guides(fill = guide_legend(nrow = 3)) +
    scale_fill_manual(values=col_map) +
    theme_void() +
    theme(#legend.title = element_blank(),
      legend.position = "none",
      plot.title = element_text(size = 10, hjust = 0.5)
    )
  
  # redefine plots
  plotStore_islands[[h]] <- p
}

fluTypes_islands <- cowplot::plot_grid(plotlist = plotStore_islands,
                                       nrow = 3)
fluTypes_islands

fluTypes_total <- cowplot::plot_grid(fluTypes_all, fluTypes_islands,
                                     ncol = 2,
                                     rel_widths = c(1, 1),
                                     labels = c("B", "C"))
fluTypes_total


combined_bar_pie <- cowplot::plot_grid(barall, fluTypes_total,
                                       ncol = 1,
                                       # rel_widths = c(2, 1),
                                       labels = c("A", ""))

png(file = "pictures/epi_combinations_barplot_piechart.png",
    width = 23, height = 30, unit = "cm", res = 600)
print(combined_bar_pie)
dev.off()

