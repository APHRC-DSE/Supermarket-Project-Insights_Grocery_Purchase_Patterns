library(dplyr)
library(forcats)
library(ggplot2)

working_directory

ggtheme_heat_plot()

heatmap_low = "turquoise4"
heatmap_mid = "cornsilk1"
heatmap_high = "brown3"
heatmap_gap_colour = "white"

## Year By Heat Map - class name RHS Rules 

### Coverage (Alias LHS Support)

year_by_classname_rhs_rules_heatmap <- sapply(c("gender", "age_group", "county_name"), function(x){
  nn <- x
  df <- dplyr::bind_rows(df_year_gender_classname_rules, df_year_age_classname_rules,
                         df_year_county_classname_rules) %>%
    dplyr::select(rhs, count, rhsSupport, year, level, group) %>%
    dplyr::filter(group == nn)
  
  df_new <- df
  
  out <- if (nn == "county_name") {
    ggplot(df_new, aes(x=as.character(year), y=reorder(rhs,count), fill=rhsSupport)) + 
      geom_raster() +
      scale_fill_gradient2(low=heatmap_high, mid=heatmap_mid, high=heatmap_low, na.value = heatmap_gap_colour,
                           n.breaks = 8
                           ) + 
      scale_x_discrete(expand = c(0, 0)) + 
      scale_y_discrete(expand=c(0,0)
                       ,labels = function(x) stringr::str_wrap(x, width = 80)
                       ) +
      labs(fill = "RHS Support", x=NULL, y=NULL) +
      facet_wrap(~level, ncol = 5, scales = "fixed")
  } else {
    ggplot(df_new, aes(x=as.character(year), y=reorder(rhs,count), fill=rhsSupport)) + 
      geom_raster() +
      scale_fill_gradient2(low=heatmap_high, mid=heatmap_mid, high=heatmap_low, na.value = heatmap_gap_colour,
                           n.breaks = 6
                           ) + 
      scale_x_discrete(expand = c(0, 0)) + 
      scale_y_discrete(expand=c(0,0)
                       ,labels = function(x) stringr::str_wrap(x, width = 80)
                       ) +
      labs(fill = "RHS Support", x=NULL, y=NULL) +
      facet_wrap(~level, ncol = 5, scales = "fixed")
  }
  
}, simplify = FALSE
)

print(year_by_classname_rhs_rules_heatmap)

## Saving the class name rhs rules heat map plots
for (j in seq(length(year_by_classname_rhs_rules_heatmap))) {
  ggsave(plot=year_by_classname_rhs_rules_heatmap[[j]], height = 7.5, width = 9,
         filename = paste0("year_",names(year_by_classname_rhs_rules_heatmap)[j],"_classname_rhs_rules_heatmap_plot",".png"),
         path = subDir_output_plots, bg='white'
  )  
}

