library(dplyr)
library(ggplot2)
library(ggpubr)

working_directory

ggtheme_descriptive_plot()

## Quarter Time series customer plots
### proportion by gender, age_group, county
customer_quarter_by_prop_plot <- sapply(c("gender", "age_group", "county_name"), function(x){ 
  
  nn <- x
  
  df_new <- df_loyalty_analysis %>%
    tidyr::drop_na(any_of(nn)) %>%
    dplyr::select(customer_id, quarter_date, any_of(nn)) %>%
    distinct(customer_id, quarter_date, .keep_all = TRUE) %>%
    group_by(quarter_date) %>%
    mutate(total = n()) %>%
    ungroup() %>%
    group_by(across(c(quarter_date, total, any_of(nn)))) %>%
    summarise(count = n(), .groups = "drop") %>%
    mutate(prop = round(count/total, 3))
  
  plot <- line_sum_plot(df=df_new,
                        x_vars= c("quarter_date"),
                        y_vars = c("prop"),
                        x_label = NULL,
                        y_label = NULL,
                        colour_vars= nn,
                        nrow_legend = 5,
                        y_axis_limits = c(0, NA),
                        title_label = TRUE,
                        y_axis_label_percent = TRUE
                        )
  
  }, simplify = TRUE
)

print(customer_quarter_by_prop_plot) 

## Combined Quarter time series customer plots
### proportion by gender, age_group, county
customer_quarter_by_prop_plot_grid <- ggpubr::annotate_figure(
  ggpubr::ggarrange(plotlist = customer_quarter_by_prop_plot,
                    ncol = NULL,
                    nrow = length(customer_quarter_by_prop_plot),
                    legend = "right", 
                    common.legend = FALSE),
  top = NULL,
  right = NULL,
  left = text_grob("Proportion of Shoppers", color = "black", face = "bold", size = 12, rot = 90),
  bottom = text_grob("Time(Quarters)", color = "black", face = "bold", size = 12)
)

print(customer_quarter_by_prop_plot_grid)

## Saving the Quarter grid customer plots

### proportion by gender, age_group, county
ggsave(plot=customer_quarter_by_prop_plot_grid, height = 6.5, width = 12,
       filename = paste0("customer_quarter_by_prop_plot",".png"),
       path = output_prop_plots_Dir, bg='white')

