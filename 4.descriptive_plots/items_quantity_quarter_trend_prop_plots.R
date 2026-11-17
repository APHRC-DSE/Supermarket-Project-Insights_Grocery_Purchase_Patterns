library(dplyr)
library(tidyr)
library(forcats)
library(rlang)
library(ggplot2)

working_directory

ggtheme_descriptive_plot(angletext_xaxis = 90, striptext_size_x = 8)

## Quarter Time series item counts proportion plots
### class_name 
item_quarter_count_prop_plot <- line_sum_plot(df=df_loyalty_analysis %>%
                                                tidyr::drop_na(class_name) %>% 
                                                dplyr::add_count(class_name) %>%
                                                dplyr::group_by(quarter_date) %>%
                                                dplyr::mutate(total = n()) %>%
                                                dplyr::ungroup() %>%
                                                dplyr::group_by(across(c(quarter_date, total, class_name, n))) %>%
                                                dplyr::summarise(count = n(), .groups = "drop") %>%
                                                dplyr::mutate(prop = round(count/total, 3)) %>%
                                                dplyr::filter(dense_rank(-n) < 13) %>% #Top 12 class_name
                                                dplyr::arrange(desc(n)) %>%
                                                dplyr::mutate(class_name = forcats::as_factor(class_name)),
                                            x_vars= c("quarter_date"),
                                            y_vars = c("prop"),
                                            x_label = "Time(Quarters)",
                                            y_label = "Proportion of Food Purchases",
                                            colour_vars= c("class_name"),
                                            facet_vars = c("class_name"),
                                            title_label = TRUE,
                                            facet_wrap = TRUE,
                                            facet_scales = "fixed",
                                            legend = FALSE,
                                            y_axis_limits = c(0, NA),
                                            y_axis_breaks = 6,
                                            x_axis_date_breaks = "3 months",
                                            y_axis_label_percent = TRUE
                                            )

print(item_quarter_count_prop_plot)

### Quarter item counts proportion by gender, age_group, county
item_quarter_count_by_prop_plot <- sapply(c("gender", "age_group", "county_name"), function(x){ 
  
  nn <- x
  
  df_new <- df_loyalty_analysis %>%
    tidyr::drop_na(class_name, any_of(nn)) %>%
    dplyr::add_count(class_name) %>% 
    dplyr::group_by(across(c(quarter_date, any_of(nn)))) %>%
    dplyr::mutate(total = n()) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(across(c(quarter_date, any_of(nn), total, class_name, n))) %>%
    dplyr::summarise(count = n(), .groups = "drop") %>%
    dplyr::mutate(prop = round(count/total, 3)) %>%
    dplyr::filter(dense_rank(-n) < 13) %>% #Top 12 class_name
    dplyr::arrange(desc(n)) %>%
    dplyr::mutate(class_name = forcats::as_factor(class_name))
  
  plot <- line_sum_plot(df=df_new,
                        x_vars= c("quarter_date"),
                        y_vars = c("prop"),
                        x_label = "Time(Quarters)",
                        y_label = "Proportion of Food Purchases",
                        colour_vars= nn,
                        facet_vars = c("class_name"),
                        title_label = TRUE,
                        facet_wrap = TRUE,
                        facet_scales = "fixed",
                        legend = TRUE,
                        y_axis_limits = c(0, NA),
                        y_axis_breaks = 6,
                        x_axis_date_breaks = "3 months",
                        y_axis_label_percent = TRUE
                        )
  
}, simplify = FALSE
)

print(item_quarter_count_by_prop_plot)


## Saving the Quarter plots
### class_name count
for (k in seq(length(item_quarter_count_prop_plot))) {
  ggsave(plot=item_quarter_count_prop_plot[[k]], height = 7, width = 13.5,
         filename = paste0(
           rlang::quo_get_expr(item_quarter_count_prop_plot[[k]][["facet"]][["params"]][["facets"]][[1]]),
           "_quarter_count_prop_plot", ".png"),
         path = output_prop_plots_Dir, bg='white')  
}


### class_name count by gender, age_group, county
for (k in seq(length(item_quarter_count_by_prop_plot))) {
  ggsave(plot=item_quarter_count_by_prop_plot[[k]][[1]], height = 7, width = 13.5,
         filename = paste0(
           rlang::quo_get_expr(item_quarter_count_by_prop_plot[[k]][[1]][["facet"]][["params"]][["facets"]][[1]]),
           "_",names(item_quarter_count_by_prop_plot)[[k]], "_quarter_count_prop_plot", ".png"),
         path = output_prop_plots_Dir, bg='white')  
}

