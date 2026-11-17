library(dplyr)
library(ggplot2)
library(ggpubr)

working_directory

ggtheme_descriptive_plot(angletext_xaxis = 90)

## Monthly sex-county Time series item count proportion plots
### nova by sex-county
nova_sex_county_month_count_prop_plot <- 
  sapply(sort(unique(df_loyalty_analysis$gender[!is.na(df_loyalty_analysis$gender)])), function(x){
  nn <- x
  
  df_max <- df_loyalty_analysis %>%
    tidyr::drop_na(county_name, nova, gender) %>%
    group_by(across(c(gender, month_date, county_name))) %>%
    mutate(total = n()) %>%
    ungroup() %>%
    group_by(across(c(month_date, county_name, total, gender, nova))) %>%
    summarise(count = n(), .groups = "drop") %>%
    mutate(prop = round(count/total, 3))
  
  df_new <- df_loyalty_analysis %>%
    tidyr::drop_na(county_name, nova) %>%
    dplyr::filter(gender == nn) %>%
    group_by(across(c(month_date, county_name))) %>%
    mutate(total = n()) %>%
    ungroup() %>%
    group_by(across(c(month_date, county_name, total, gender, nova))) %>%
    summarise(count = n(), .groups = "drop") %>%
    mutate(prop = round(count/total, 3))
  
  plot1 <- line_sum_plot(df=df_new %>%
                           dplyr::filter(nova == "Processed foods"),
                         x_vars= c("month_date"),
                         y_vars = c("prop"),
                         x_label = NULL,
                         y_label = NULL,
                         colour_vars= c("county_name"),
                         facet_vars = c("nova"),
                         facet_wrap = TRUE,
                         facet_scales = "fixed",
                         legend = FALSE,
                         facet_ncol = 1,
                         y_axis_breaks = 4,
                         y_axis_limits = c(0, max(df_max$prop[df_max$nova == "Processed foods"])),
                         title_label = FALSE,
                         x_axis_date_breaks = "3 months",
                         y_axis_label_percent = TRUE
                         )
  
  plot2 <- line_sum_plot(df=df_new %>%
                           dplyr::filter(nova == "Processed Culinary Ingredients"),
                         x_vars= c("month_date"),
                         y_vars = c("prop"),
                         x_label = NULL,
                         y_label = NULL,
                         colour_vars= c("county_name"),
                         facet_vars = c("nova"),
                         facet_wrap = TRUE,
                         facet_scales = "fixed",
                         legend = FALSE,
                         facet_ncol = 1,
                         y_axis_breaks = 4,
                         y_axis_limits = c(0, max(df_max$prop[df_max$nova == "Processed Culinary Ingredients"])),
                         title_label = FALSE,
                         x_axis_date_breaks = "3 months",
                         y_axis_label_percent = TRUE
                         )
  
  plot3 <- if (nn == "Female") {
    line_sum_plot(df=df_new %>%
                    dplyr::filter(nova == "Ultra-processed foods" | nova == "Unprocessed/Minimally processed foods"
                    ) %>%
                    dplyr::mutate(nova = factor(nova, levels = c("Unprocessed/Minimally processed foods",
                                                                 "Ultra-processed foods"))
                    ) ,
                  x_vars= c("month_date"),
                  y_vars = c("prop"),
                  x_label = NULL,
                  y_label = NULL,
                  colour_vars= c("county_name"),
                  facet_vars = c("nova"),
                  facet_wrap = TRUE,
                  facet_scales = "fixed",
                  nrow_legend = 5,
                  facet_ncol = 2,
                  y_axis_breaks = 4,
                  y_axis_limits = c(0, max(df_max$prop[df_max$nova == "Unprocessed/Minimally processed foods"|
                                                         df_max$nova == "Ultra-processed foods"])),
                  title_label = FALSE,
                  x_axis_date_breaks = "3 months",
                  y_axis_label_percent = TRUE
    )
  } else {
    line_sum_plot(df=df_new %>%
                    dplyr::filter(nova == "Ultra-processed foods" | nova == "Unprocessed/Minimally processed foods"
                    ) %>%
                    dplyr::mutate(nova = factor(nova, levels = c("Unprocessed/Minimally processed foods",
                                                                 "Ultra-processed foods"))
                    ) ,
                  x_vars= c("month_date"),
                  y_vars = c("prop"),
                  x_label = NULL,
                  y_label = NULL,
                  colour_vars= c("county_name"),
                  facet_vars = c("nova"),
                  facet_wrap = TRUE,
                  facet_scales = "fixed",
                  legend = FALSE,
                  facet_ncol = 2,
                  y_axis_breaks = 4,
                  y_axis_limits = c(0, max(df_max$prop[df_max$nova == "Unprocessed/Minimally processed foods"|
                                                         df_max$nova == "Ultra-processed foods"])),
                  title_label = FALSE,
                  x_axis_date_breaks = "3 months",
                  y_axis_label_percent = TRUE
    )
    
  }
  
  plot <- ggpubr::ggarrange(plotlist = c(plot1, plot2, plot3),
                            ncol = NULL,
                            nrow = 1,
                            legend = "right",
                            widths = c(0.3, 0.3, 0.6),
                            common.legend = TRUE)
  
}, simplify = FALSE
)

print(nova_sex_county_month_count_prop_plot)  


## Combined Monthly sex-county time series plots 
### nova by sex-county item count
nova_sex_county_month_count_prop_plot_grid <- ggpubr::annotate_figure(
  ggpubr::ggarrange(plotlist = nova_sex_county_month_count_prop_plot,
                    ncol = NULL,
                    nrow = length(nova_sex_county_month_count_prop_plot),
                    labels = names(nova_sex_county_month_count_prop_plot),
                    hjust = c(-0.5, -0.5),
                    vjust = 2.5,
                    font.label = list(size = 12, color = "black", face = "bold", family = NULL),
                    legend = "right", 
                    common.legend = TRUE
                    ),
  top = "",
  right = NULL,
  left = text_grob("Proportion of Food Purchases", color = "black", face = "bold", size = 12, rot = 90),
  bottom = text_grob("Time(Months)", color = "black", face = "bold", size = 12)
)

nova_sex_county_month_count_prop_plot_grid


## Saving the Monthly sex-county grid plots
ggsave(plot=nova_sex_county_month_count_prop_plot_grid, height = 7, width = 12,
       filename = paste0("nova_sex_county_month_count_prop_plot",".png"),
       path = output_prop_plots_Dir, bg='white')


