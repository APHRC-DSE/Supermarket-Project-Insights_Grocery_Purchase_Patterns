library(dplyr)
library(forcats)
library(ggplot2)

working_directory

ggtheme_heat_plot()

## Socio By Heat Map - class name Rules

heatmap_low = "turquoise4"
heatmap_mid = "cornsilk1"
heatmap_high = "brown3"
heatmap_gap_colour = "white"


### Age gender heat map
age_gender_classname_rules_heatmap <- df_age_gender_classname_rules %>%
  dplyr::select(lhs, count, support, age_group, gender) %>%
  dplyr::filter(support >= 0.02) %>%
  ggplot(aes(x=gender, y=reorder(lhs,count), fill=support)) + 
  geom_raster() +
  scale_fill_gradient2(low=heatmap_low, mid=heatmap_mid, high=heatmap_high, na.value = heatmap_gap_colour,
                       n.breaks = 6
                       ) + 
  scale_x_discrete(expand = c(0, 0)) + 
  scale_y_discrete(expand=c(0,0)
                   ,labels = function(x) stringr::str_wrap(x, width = 80)
                   ) +
  labs(fill = "Support", x=NULL, y=NULL) +
  facet_wrap(~age_group, ncol = 4, scales = "fixed")

print(age_gender_classname_rules_heatmap)

### Age county heat map
age_county_classname_rules_heatmap <- df_age_county_classname_rules %>%
  dplyr::select(lhs, count, support, age_group, county_name) %>%
  dplyr::filter(support >= 0.025) %>%
  ggplot(aes(x=county_name, y=reorder(lhs,count), fill=support)) + 
  geom_raster() +
  scale_fill_gradient2(low=heatmap_low, mid=heatmap_mid, high=heatmap_high, na.value = heatmap_gap_colour,
                       n.breaks = 6
                       ) + 
  scale_x_discrete(expand = c(0, 0)) + 
  scale_y_discrete(expand=c(0,0)
                   ,labels = function(x) stringr::str_wrap(x, width = 80)
  ) +
  labs(fill = "Support", x=NULL, y=NULL) +
  facet_wrap(~age_group, ncol = 4, scales = "fixed") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

print(age_county_classname_rules_heatmap)

### Gender county heat map
gender_county_classname_rules_heatmap <- df_gender_county_classname_rules %>%
  dplyr::select(lhs, count, support, county_name, gender) %>%
  dplyr::filter(support >= 0.025) %>%
  ggplot(aes(x=county_name, y=reorder(lhs,count), fill=support)) + 
  geom_raster() +
  scale_fill_gradient2(low=heatmap_low, mid=heatmap_mid, high=heatmap_high, na.value = heatmap_gap_colour,
                       n.breaks = 6
                       ) + 
  scale_x_discrete(expand = c(0, 0)) + 
  scale_y_discrete(expand=c(0,0)
                   ,labels = function(x) stringr::str_wrap(x, width = 80)
  ) +
  labs(fill = "Support", x=NULL, y=NULL) +
  facet_wrap(~gender, ncol = 5, scales = "fixed")

print(gender_county_classname_rules_heatmap)

## Saving the class name rules heat map plots
ggsave(plot=age_gender_classname_rules_heatmap, height = 7.5, width = 9,
       filename = paste0("age_gender_classname_rules_heatmap_plot",".png"),
       path = subDir_output_plots, bg='white'
       )

ggsave(plot=age_county_classname_rules_heatmap, height = 8.5, width = 10,
       filename = paste0("age_county_classname_rules_heatmap_plot",".png"),
       path = subDir_output_plots, bg='white'
       )

ggsave(plot=gender_county_classname_rules_heatmap, height = 8, width = 10,
       filename = paste0("gender_county_classname_rules_heatmap_plot",".png"),
       path = subDir_output_plots, bg='white'
       )

