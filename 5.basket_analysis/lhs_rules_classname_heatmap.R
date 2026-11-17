library(dplyr)
library(forcats)
library(ggplot2)

working_directory

ggtheme_heat_plot()

## Overall Heat Map - class name LHS Rules 

### Coverage (Alias LHS Support)
heatmap_low = "turquoise4"
heatmap_mid = "cornsilk1"
heatmap_high = "brown3"
heatmap_gap_colour = "white"

classname_lhs_rules_heatmap <- sapply(c("gender", "age_group", "county_name", "year"), function(x){
  nn <- x
  df <- dplyr::bind_rows(df_gender_classname_rules, df_age_classname_rules, df_county_classname_rules,
                             df_year_classname_rules) %>%
    dplyr::select(lhs, count, coverage, level, group) %>%
    dplyr::filter(group == nn)
  
  df_new <- if (nn == "county_name") { 
    df %>%
      dplyr::filter(coverage >= 0.045)
    } else { 
    df %>%
      dplyr::filter(coverage >= 0.03)
      }
  
  out <- if (nn == "county_name") {
    ggplot(df_new, aes(x=level, y=reorder(lhs,count), fill=coverage)) + 
    geom_raster() +
    scale_fill_gradient2(low=heatmap_low, mid=heatmap_mid, high=heatmap_high, na.value = heatmap_gap_colour,
                         n.breaks = 10
                         ) + 
    scale_x_discrete(expand = c(0, 0)) + 
    scale_y_discrete(expand=c(0,0)
                     ,labels = function(x) stringr::str_wrap(x, width = 80)
                     ) +
    labs(fill = "Coverage", x=NULL, y=NULL)
  } else {
    ggplot(df_new, aes(x=level, y=reorder(lhs,count), fill=coverage)) + 
      geom_raster() +
      scale_fill_gradient2(low=heatmap_low, mid=heatmap_mid, high=heatmap_high, na.value = heatmap_gap_colour,
                           n.breaks = 8
                           ) + 
      scale_x_discrete(expand = c(0, 0)) + 
      scale_y_discrete(expand=c(0,0)
                       ,labels = function(x) stringr::str_wrap(x, width = 80)
      ) +
      labs(fill = "Coverage", x=NULL, y=NULL)
  }
    
}, simplify = FALSE
)
  
print(classname_lhs_rules_heatmap)

## Saving the class name lhs rules heat map plots
for (j in seq(length(classname_lhs_rules_heatmap))) {
  ggsave(plot=classname_lhs_rules_heatmap[[j]], height = 7.5, width = 9,
         filename = paste0(names(classname_lhs_rules_heatmap)[j],"_classname_lhs_rules_heatmap_plot",".png"),
         path = subDir_output_plots, bg='white'
  )  
}

