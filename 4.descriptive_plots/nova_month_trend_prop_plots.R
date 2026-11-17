library(dplyr)
library(ggplot2)
library(ggpubr)


working_directory

ggtheme_descriptive_plot()

## Monthly Time series item counts proportion plots
### nova 
nova_month_count_prop_plot <- line_sum_plot(df=df_loyalty_analysis %>%
                                         tidyr::drop_na(nova) %>%
                                         group_by(month_date) %>%
                                         mutate(total = n()) %>%
                                         ungroup() %>%
                                         group_by(across(c(month_date, total, nova))) %>%
                                         summarise(count = n(), .groups = "drop") %>%
                                         mutate(prop = round(count/total, 3)),
                                       x_vars= c("month_date"),
                                       y_vars = c("prop"),
                                       x_label = "Time(Months)",
                                       y_label = "Proportion of Food Purchases",
                                       colour_vars= c("nova"),
                                       title_label = TRUE,
                                       x_axis_date_breaks = "3 months",
                                       y_axis_label_percent = TRUE
                                       )

print(nova_month_count_prop_plot)

ggtheme_descriptive_plot(angletext_xaxis = 90, striptext_size_x = 9)
### nova by gender, age_group, county
nova_month_count_by_prop_plot <- sapply(c("gender", "age_group", "county_name"), function(x){ 
  
  nn <- x
  
  df_new <- df_loyalty_analysis %>%
    tidyr::drop_na(nova, any_of(nn)) %>%
    group_by(across(c(month_date, any_of(nn)))) %>%
    mutate(total = n()) %>%
    ungroup() %>%
    group_by(across(c(month_date, any_of(nn), total, nova))) %>%
    summarise(count = n(), .groups = "drop") %>%
    mutate(prop = round(count/total, 3))
  
  plot1 <- line_sum_plot(df=df_new %>%
                           dplyr::filter(nova == "Processed foods"),
                        x_vars= c("month_date"),
                        y_vars = c("prop"),
                        x_label = NULL,
                        y_label = NULL,
                        colour_vars= nn,
                        facet_vars = c("nova"),
                        facet_wrap = TRUE,
                        facet_scales = "fixed",
                        nrow_legend = 5,
                        facet_ncol = 1,
                        y_axis_limits = c(0, NA),
                        y_axis_breaks = 6,
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
                         colour_vars= nn,
                         facet_vars = c("nova"),
                         facet_wrap = TRUE,
                         facet_scales = "fixed",
                         nrow_legend = 5,
                         facet_ncol = 1,
                         y_axis_limits = c(0, NA),
                         y_axis_breaks = 6,
                         title_label = FALSE,
                         x_axis_date_breaks = "3 months",
                         y_axis_label_percent = TRUE
                         )
  
  plot3 <- line_sum_plot(df=df_new %>%
                           dplyr::filter(nova == "Ultra-processed foods" | nova == "Unprocessed/Minimally processed foods"
                                         ) %>%
                           dplyr::mutate(nova = factor(nova, levels = c("Unprocessed/Minimally processed foods",
                                                                         "Ultra-processed foods"))) ,
                         x_vars= c("month_date"),
                         y_vars = c("prop"),
                         x_label = NULL,
                         y_label = NULL,
                         colour_vars= nn,
                         facet_vars = c("nova"),
                         facet_wrap = TRUE,
                         facet_scales = "fixed",
                         nrow_legend = 5,
                         facet_ncol = 2,
                         y_axis_limits = c(0, NA),
                         y_axis_breaks = 6,
                         title_label = FALSE,
                         x_axis_date_breaks = "3 months",
                         y_axis_label_percent = TRUE
                         )
  
  plot <- ggpubr::ggarrange(plotlist = c(plot1, plot2, plot3),
                            ncol = NULL,
                            nrow = 1,
                            legend = "right",
                            widths = c(0.3, 0.3, 0.6),
                            common.legend = TRUE)
  
}, simplify = FALSE
)

print(nova_month_count_by_prop_plot)  


## Combined Monthly time series plots 
### nova proportion item counts by gender, age_group, county

nova_month_count_by_prop_plot_grid <- ggpubr::annotate_figure(
  ggpubr::ggarrange(plotlist = nova_month_count_by_prop_plot,
                    ncol = NULL,
                    nrow = length(nova_month_count_by_prop_plot),
                    labels = c("Gender", "Age group (years)", "Location"), #names(nova_month_count_by_prop_plot)
                    hjust = c(-0.5, -0.3, -0.5),
                    vjust = 2.5,
                    font.label = list(size = 10, color = "black", face = "bold", family = NULL),
                    legend = "right", 
                    common.legend = FALSE),
  top = "",
  right = NULL,
  left = text_grob("Proportion of Food Purchases", color = "black", face = "bold", size = 12, rot = 90),
  bottom = text_grob("Time(Months)", color = "black", face = "bold", size = 12)
  )

nova_month_count_by_prop_plot_grid


ggtheme_descriptive_plot()
## Saving the Monthly grid plots
### nova item counts
for (j in seq(length(nova_month_count_prop_plot))) {
  ggsave(plot=nova_month_count_prop_plot[[j]], height = 5, width = 9,
         filename = paste0(nova_month_count_prop_plot[[j]][["labels"]][["title"]],"_month_count_prop_plot",".png"),
         path = output_prop_plots_Dir, bg='white')  
}

ggtheme_descriptive_plot(angletext_xaxis = 90)
### nova item counts by gender, age_group, county
ggsave(plot=nova_month_count_by_prop_plot_grid, height = 7, width = 12,
       filename = paste0("nova_month_count_by_prop_plot",".png"),
       path = output_prop_plots_Dir, bg='white')

