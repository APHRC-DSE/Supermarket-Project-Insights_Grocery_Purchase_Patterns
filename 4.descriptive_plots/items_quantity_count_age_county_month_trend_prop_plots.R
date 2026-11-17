library(dplyr)
library(tidyr)
library(forcats)
library(ggplot2)
library(ggpubr)

working_directory

ggtheme_descriptive_plot(angletext_xaxis = 90)

## Monthly age_county Time series item counts proportion plots
### class_name 
item_age_county_month_count_prop_plot_1 <- 
  sapply(sort(unique(df_loyalty_analysis$age_group[!is.na(df_loyalty_analysis$age_group)])), function(x){
    nn <- x
    
    df_max <- df_loyalty_analysis %>%
      tidyr::drop_na(county_name, class_name, age_group) %>%
      group_by(class_name) %>%
      mutate(n = n()) %>%
      ungroup() %>%
      group_by(across(c(age_group, month_date, county_name))) %>%
      mutate(total = n()) %>%
      ungroup() %>%
      group_by(across(c(month_date, county_name, total, age_group, class_name, n))) %>%
      summarise(count = n(), .groups = "drop") %>%
      mutate(prop = round(count/total, 3)) %>%
      dplyr::filter(dense_rank(-n) < 5) %>%
      dplyr::arrange(desc(n))
      
    df_new <- df_loyalty_analysis %>%
      tidyr::drop_na(county_name, class_name, age_group) %>% 
      group_by(class_name) %>%
      mutate(n = n()) %>%
      ungroup() %>%
      dplyr::filter(age_group == nn) %>%
      group_by(across(c(month_date, county_name))) %>%
      mutate(total = n()) %>%
      ungroup() %>%
      group_by(across(c(month_date, county_name, total, age_group, class_name, n))) %>%
      summarise(count = n(), .groups = "drop") %>%
      mutate(prop = round(count/total, 3)) %>%
      dplyr::filter(dense_rank(-n) < 5) %>% #Top 1-4 class_name_uom
      dplyr::arrange(desc(n)) %>%
      dplyr::mutate(class_name = forcats::as_factor(class_name))
    
    plot <- line_sum_plot(df= df_new ,
                          x_vars= c("month_date") ,
                          y_vars = c("prop"),
                          x_label = NULL,
                          y_label = NULL,
                          colour_vars= c("county_name"),
                          facet_vars = c("class_name"),
                          facet_wrap = TRUE,
                          facet_scales = "fixed",
                          title_label = FALSE,
                          x_axis_date_breaks = "3 months",
                          y_axis_label_percent = TRUE,
                          y_axis_limits = c(0, max(df_max$prop)),
                          nrow_legend = 5,
                          y_axis_breaks = 4
                          )
  }, simplify = TRUE
  ) 

item_age_county_month_count_prop_plot_2 <- 
  sapply(sort(unique(df_loyalty_analysis$age_group[!is.na(df_loyalty_analysis$age_group)])), function(x){
    nn <- x
    
    df_max <- df_loyalty_analysis %>%
      tidyr::drop_na(county_name, class_name, age_group) %>%
      group_by(class_name) %>%
      mutate(n = n()) %>%
      ungroup() %>%
      group_by(across(c(age_group, month_date, county_name))) %>%
      mutate(total = n()) %>%
      ungroup() %>%
      group_by(across(c(month_date, county_name, total, age_group, class_name, n))) %>%
      summarise(count = n(), .groups = "drop") %>%
      mutate(prop = round(count/total, 3)) %>%
      dplyr::filter(dense_rank(-n) <5) %>%
      dplyr::arrange(desc(n))
    
    df_new <- df_loyalty_analysis %>%
      tidyr::drop_na(county_name, class_name) %>% 
      group_by(class_name) %>%
      mutate(n = n()) %>%
      ungroup() %>%
      dplyr::filter(age_group == nn) %>%
      group_by(across(c(month_date, county_name))) %>%
      mutate(total = n()) %>%
      ungroup() %>%
      group_by(across(c(month_date, county_name, total, age_group, class_name, n))) %>%
      summarise(count = n(), .groups = "drop") %>%
      mutate(prop = round(count/total, 3)) %>%
      dplyr::filter(dense_rank(-n) > 4, dense_rank(-n) < 9) %>% #Top 5-8 class_name_uom
      dplyr::arrange(desc(n)) %>%
      dplyr::mutate(class_name = forcats::as_factor(class_name))
    
    plot <- line_sum_plot(df= df_new,
                          x_vars= c("month_date") ,
                          y_vars = c("prop"),
                          x_label = NULL,
                          y_label = NULL,
                          colour_vars= c("county_name"),
                          facet_vars = c("class_name"),
                          facet_wrap = TRUE,
                          facet_scales = "fixed",
                          title_label = FALSE,
                          x_axis_date_breaks = "3 months",
                          y_axis_label_percent = TRUE,
                          y_axis_limits = c(0, max(df_max$prop)),
                          nrow_legend = 5,
                          y_axis_breaks = 4
    )
  }, simplify = TRUE
  )

item_age_county_month_count_prop_plot_3 <- 
  sapply(sort(unique(df_loyalty_analysis$age_group[!is.na(df_loyalty_analysis$age_group)])), function(x){
    nn <- x
    
    df_max <- df_loyalty_analysis %>%
      tidyr::drop_na(county_name, class_name, age_group) %>%
      group_by(class_name) %>%
      mutate(n = n()) %>%
      ungroup() %>%
      group_by(across(c(age_group, month_date, county_name))) %>%
      mutate(total = n()) %>%
      ungroup() %>%
      group_by(across(c(month_date, county_name, total, age_group, class_name, n))) %>%
      summarise(count = n(), .groups = "drop") %>%
      mutate(prop = round(count/total, 3)) %>%
      dplyr::filter(dense_rank(-n) <5) %>%
      dplyr::arrange(desc(n))
    
    df_new <- df_loyalty_analysis %>%
      tidyr::drop_na(county_name, class_name) %>% 
      group_by(class_name) %>%
      mutate(n = n()) %>%
      ungroup() %>%
      dplyr::filter(age_group == nn) %>%
      group_by(across(c(month_date, county_name))) %>%
      mutate(total = n()) %>%
      ungroup() %>%
      group_by(across(c(month_date, county_name, total, age_group, class_name, n))) %>%
      summarise(count = n(), .groups = "drop") %>%
      mutate(prop = round(count/total, 3)) %>%
      dplyr::filter(dense_rank(-n) > 8, dense_rank(-n) < 13) %>% #Top 9-12 class_name_uom
      dplyr::arrange(desc(n)) %>%
      dplyr::mutate(class_name = forcats::as_factor(class_name))
    
    plot <- line_sum_plot(df= df_new,
                          x_vars= c("month_date") ,
                          y_vars = c("prop"),
                          x_label = NULL,
                          y_label = NULL,
                          colour_vars= c("county_name"),
                          facet_vars = c("class_name"),
                          facet_wrap = TRUE,
                          facet_scales = "fixed",
                          title_label = FALSE,
                          x_axis_date_breaks = "3 months",
                          y_axis_label_percent = TRUE,
                          y_axis_limits = c(0, max(df_max$prop)),
                          nrow_legend = 5,
                          y_axis_breaks = 4
    )
  }, simplify = TRUE
  ) 

print(item_age_county_month_count_prop_plot_1)
print(item_age_county_month_count_prop_plot_2)
print(item_age_county_month_count_prop_plot_3)

## Combined Monthly age-county time series item counts proportion plots
### class_name 
item_age_county_month_count_prop_plot_1_grid <- ggpubr::annotate_figure(
  ggpubr::ggarrange(plotlist = item_age_county_month_count_prop_plot_1,
                    ncol = NULL,
                    nrow = length(item_age_county_month_count_prop_plot_1),
                    labels = names(item_age_county_month_count_prop_plot_1),
                    hjust = c(-0.5, -0.5, -0.5, -0.3),
                    vjust = c(0.8, 0.8, 0.8, 0.8),
                    font.label = list(size = 12, color = "black", face = "bold", family = NULL),
                    legend = "right", 
                    common.legend = TRUE),
  top = "",
  right = NULL,
  left = text_grob("Proportion of Food Purchases", color = "black", face = "bold", size = 12, rot = 90),
  bottom = text_grob("Time(Months)", color = "black", face = "bold", size = 12)
)

item_age_county_month_count_prop_plot_2_grid <- ggpubr::annotate_figure(
  ggpubr::ggarrange(plotlist = item_age_county_month_count_prop_plot_2,
                    ncol = NULL,
                    nrow = length(item_age_county_month_count_prop_plot_2),
                    labels = names(item_age_county_month_count_prop_plot_2),
                    hjust = c(-0.5, -0.5, -0.5, -0.3),
                    vjust = c(0.8, 0.8, 0.8, 0.8),
                    font.label = list(size = 12, color = "black", face = "bold", family = NULL),
                    legend = "right", 
                    common.legend = TRUE),
  top = "",
  right = NULL,
  left = text_grob("Proportion of Food Purchases", color = "black", face = "bold", size = 12, rot = 90),
  bottom = text_grob("Time(Months)", color = "black", face = "bold", size = 12)
)

item_age_county_month_count_prop_plot_3_grid <- ggpubr::annotate_figure(
  ggpubr::ggarrange(plotlist = item_age_county_month_count_prop_plot_3,
                    ncol = NULL,
                    nrow = length(item_age_county_month_count_prop_plot_3),
                    labels = names(item_age_county_month_count_prop_plot_3),
                    hjust = c(-0.5, -0.5, -0.5, -0.3),
                    vjust = c(0.8, 0.8, 0.8, 0.8),
                    font.label = list(size = 12, color = "black", face = "bold", family = NULL),
                    legend = "right", 
                    common.legend = TRUE),
  top = "",
  right = NULL,
  left = text_grob("Proportion of Food Purchases", color = "black", face = "bold", size = 12, rot = 90),
  bottom = text_grob("Time(Months)", color = "black", face = "bold", size = 12)
)

item_age_county_month_count_prop_plot_1_grid
item_age_county_month_count_prop_plot_2_grid
item_age_county_month_count_prop_plot_3_grid

## Saving the Monthly age-county grid plots
ggsave(plot=item_age_county_month_count_prop_plot_1_grid, height = 7, width = 13.5,
       filename = paste0("class_name_age_county_month_count_prop_plot_1",".png"),
       path = output_prop_plots_Dir, bg='white')

ggsave(plot=item_age_county_month_count_prop_plot_2_grid, height = 7, width = 13.5,
       filename = paste0("class_name_age_county_month_count_prop_plot_2",".png"),
       path = output_prop_plots_Dir, bg='white')

ggsave(plot=item_age_county_month_count_prop_plot_3_grid, height = 7, width = 13.5,
       filename = paste0("class_name_age_county_month_count_prop_plot_3",".png"),
       path = output_prop_plots_Dir, bg='white')
