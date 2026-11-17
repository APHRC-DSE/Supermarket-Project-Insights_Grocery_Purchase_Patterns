library(dplyr)
library(ggplot2)
library(ggpubr)
library(ggstats)

working_directory

ggtheme_descriptive_plot()

## Plots
### Simple plots
customer_descriptive_plot <- single_plot(df = df_loyalty_analysis %>%
                                           dplyr::select(customer_id, gender, mean_agegroup, county_name) %>%
                                           distinct(customer_id, .keep_all = TRUE), 
                      variable = c("gender", "mean_agegroup", "county_name"), 
                      rotate_axis = FALSE,
                      percent_yaxis=FALSE,
                      text_label_size = 2.5,
                      x_axis_label_wrap_width = 15
                      )

print(customer_descriptive_plot)

### stacked plots

customer_stack_plot <- sapply(c("gender", "mean_agegroup","county_name"), function(x) {
  
  df_new <- df_loyalty_analysis %>%
    dplyr::select(customer_id, gender, mean_agegroup, county_name) %>%
    distinct(customer_id, .keep_all = TRUE)
  
  out <- if (x == "mean_agegroup" | x == "county_name") {
    stacked_plot(df = df_new, 
                 variable = c("gender", "mean_agegroup", "county_name")[!(c("gender", "mean_agegroup", "county_name") %in% x)],
                 fill_vars = x,
                 title_label = NULL,
                 nrow_legend = 5
                 )
    } else {
  stacked_plot(df = df_new, 
               variable = c("gender", "mean_agegroup", "county_name")[!(c("gender", "mean_agegroup", "county_name") %in% x)],
               fill_vars = x,
               title_label = NULL,
               nrow_legend = 2
               )
    }
  
  out
}, simplify = FALSE
)

print(customer_stack_plot)

### stacked plots - panels                  
customer_stack_year_plot <- sapply(c("gender", "mean_agegroup","county_name"), function(x) {
  
  df_new <- df_loyalty_analysis %>%
    dplyr::select(customer_id, gender, mean_agegroup, county_name, year) %>%
    distinct(customer_id, year, .keep_all = TRUE)
  
  out <- if (x == "mean_agegroup"| x == "county_name") {
    stacked_plot(df = df_new, 
                 variable = c("gender", "mean_agegroup","county_name")[!(c("gender", "mean_agegroup","county_name") %in% x)],
                 fill_vars = x,
                 facet_vars = c("year"),
                 facet_wrap=TRUE,
                 nrow_legend = 5
                 )
    } else {
  stacked_plot(df = df_new, 
               variable = c("gender", "mean_agegroup","county_name")[!(c("gender", "mean_agegroup","county_name") %in% x)],
               fill_vars = x,
               facet_vars = c("year"),
               facet_wrap=TRUE,
               nrow_legend = 2
               )
    }
  
  out
}, simplify = FALSE
)

print(customer_stack_year_plot)                

## Combined plots
### simple plots
customer_descriptive_plot_grid <- ggpubr::ggarrange(plotlist = customer_descriptive_plot, 
                       ncol = length(customer_descriptive_plot),
                       nrow = NULL,
                       legend = "bottom", 
                       common.legend = FALSE)

customer_descriptive_plot_grid

### stacked plots

customer_stack_plot_grid <- sapply(names(customer_stack_plot), function(x) {
  ggpubr::ggarrange(plotlist = customer_stack_plot[[x]], 
                    ncol = length(customer_stack_plot[[x]]),
                    nrow = NULL,
                    legend = "right", 
                    common.legend = TRUE)
  
}, simplify = FALSE
)
  
customer_stack_plot_all_grid <- ggpubr::ggarrange(plotlist = customer_stack_plot_grid, 
                                                  nrow = length(customer_stack_plot_grid),
                                                  ncol = NULL,
                                                  legend = NULL, 
                                                  common.legend = FALSE)
customer_stack_plot_all_grid

### stacked plots - panels
customer_stack_year_plot_grid <- sapply(names(customer_stack_year_plot), function(x) {
  ggpubr::ggarrange(plotlist = customer_stack_year_plot[[x]], 
                    ncol = length(customer_stack_year_plot[[x]]),
                    nrow = NULL,
                    legend = "right", 
                    common.legend = TRUE)
  
}, simplify = FALSE
)

customer_stack_year_plot_all_grid <- ggpubr::ggarrange(plotlist = customer_stack_year_plot_grid, 
                                                  nrow = length(customer_stack_year_plot_grid),
                                                  ncol = NULL,
                                                  legend = NULL, 
                                                  common.legend = FALSE)

customer_stack_year_plot_all_grid

## Saving the grid plots
### Simple plots
ggsave(plot=customer_descriptive_plot_grid, height = 6, width = 11.5,
       filename = paste0("customer_descriptive_plot",".png"),
       path = output_plots_Dir, bg='white')

### stacked plots
ggsave(plot=customer_stack_plot_all_grid, height = 7, width = 11.5,
       filename = paste0("customer_stack_plot",".png"),
       path = output_plots_Dir, bg='white')

### stacked plots - panels
ggsave(plot=customer_stack_year_plot_all_grid, height = 7, width = 14,
       filename = paste0("customer_stack_year_plot",".png"),
       path = output_plots_Dir, bg='white')

