library(dplyr)
library(ggplot2)
library(ggpubr)

working_directory

ggtheme_descriptive_plot(angletext_xaxis = 90)

## Quarter Age-gender Time series customer plots
### Proportion by Age-gender

customer_age_sex_quarter_prop_plot <- line_sum_plot(
  df= df_loyalty_analysis %>%
    tidyr::drop_na(age_group, gender) %>%
    distinct(customer_id, quarter_date, .keep_all = TRUE) %>%
    group_by(quarter_date, gender) %>%
    mutate(total = n()) %>%
    ungroup() %>%
    group_by(across(c(quarter_date, gender, total, age_group ))) %>%
    summarise(count = n(), .groups = "drop") %>%
    mutate(prop = round(count/total, 3)),
  x_vars= c("quarter_date"),
  y_vars = c("prop"),
  x_label = "Time(Quarters)",
  y_label = "Proportion of Shoppers",
  colour_vars= c("gender"),
  facet_vars = c("age_group"),
  facet_wrap = TRUE,
  facet_ncol = 4,
  title_label = FALSE,
  y_axis_label_percent = TRUE,
  facet_scales = "fixed",
  y_axis_limits = c(0, NA),
  nrow_legend = 1,
  y_axis_breaks = 10
  )

print(customer_age_sex_quarter_prop_plot)  

## Saving the Monthly age-sex customer plots
for (j in seq(length(customer_age_sex_quarter_prop_plot))) {
  ggsave(plot=customer_age_sex_quarter_prop_plot[[j]], height = 5, width = 9,
         filename = paste0("customer_age_sex_quarter_prop_plot_",j,".png"),
         path = output_prop_plots_Dir, bg='white')  
}

