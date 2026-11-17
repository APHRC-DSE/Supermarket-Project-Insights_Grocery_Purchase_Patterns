library(dplyr)
library(ggplot2)
library(ggpubr)

working_directory

ggtheme_descriptive_plot(angletext_xaxis = 90)

## Quarter Sex-County Time series customer plots
### Proportion by Sex-County

customer_sex_county_quarter_prop_plot <- line_sum_plot(
  df= df_loyalty_analysis %>%
    tidyr::drop_na(gender, county_name) %>%
    distinct(customer_id, quarter_date, .keep_all = TRUE) %>%
    group_by(quarter_date, county_name) %>%
    mutate(total = n()) %>%
    ungroup() %>%
    group_by(across(c(quarter_date, county_name, total, gender ))) %>%
    summarise(count = n(), .groups = "drop") %>%
    mutate(prop = round(count/total, 3)),
  x_vars= c("quarter_date"),
  y_vars = c("prop"),
  x_label = "Time(Quarters)",
  y_label = "Proportion of Shoppers",
  colour_vars= c("county_name"),
  facet_vars = c("gender"),
  facet_wrap = TRUE,
  facet_ncol = 2,
  title_label = FALSE,
  y_axis_label_percent = TRUE,
  facet_scales = "fixed",
  y_axis_limits = c(0, NA),
  nrow_legend = 1,
  y_axis_breaks = 10
  )

print(customer_sex_county_quarter_prop_plot)  

## Saving the Monthly sex-county customer plots
for (j in seq(length(customer_sex_county_quarter_prop_plot))) {
  ggsave(plot=customer_sex_county_quarter_prop_plot[[j]], height = 5, width = 9,
         filename = paste0("customer_sex_county_quarter_prop_plot_",j,".png"),
         path = output_prop_plots_Dir, bg='white')  
}

