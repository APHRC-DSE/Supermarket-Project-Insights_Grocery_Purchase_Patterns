library(dplyr)
library(ggplot2)
library(ggstats)
library(ggpubr)
library(sjlabelled)

working_directory

ggtheme_descriptive_plot()

## Plots
### Simple plots
nova_item_descriptive_plot <- single_plot(df = df_loyalty_analysis %>%
                                            dplyr::select(nova), 
                                          variable = c("nova"), 
                                          rotate_axis = TRUE,
                                          percent_yaxis=TRUE
                                          )

print(nova_item_descriptive_plot)

### stacked plots
nova_item_stack_plot <- stacked_plot(df = df_loyalty_analysis %>%
                                       dplyr::select(customer_id, year, gender, age_group, county_name, nova) %>%
                                       mutate(year = sjlabelled::as_factor(year)), 
                       variable = c("gender", "age_group", "year", "county_name"),
                       fill_vars = c("nova")
                       )


### stacked plots - panels                  
nova_item_stack_year_plot <- stacked_plot(df = df_loyalty_analysis %>%
                                            dplyr::select(customer_id, year, gender, age_group, county_name, nova) %>%
                                            mutate(year = sjlabelled::as_factor(year)), 
                                          variable = c("gender", "age_group", "county_name"),
                                          fill_vars = c("nova"),
                                          facet_vars = c("year"), 
                                          facet_wrap=TRUE,
                                          nrow_legend = 1
                                          )


## Combined plots
### stacked plots
nova_item_stack_plot_grid <- ggpubr::ggarrange(plotlist = nova_item_stack_plot, 
                       ncol = 2,
                       nrow = 2,
                       legend = "bottom", 
                       common.legend = TRUE)

nova_item_stack_plot_grid

### stacked plots - panels
nova_item_stack_year_plot_grid <- ggpubr::ggarrange(plotlist = nova_item_stack_year_plot, 
                  ncol = 2,
                  nrow = 2,
                  legend = "bottom", 
                  common.legend = TRUE)

nova_item_stack_year_plot_grid

## Saving the grid plots
### Simple plots
for (i in seq(length(nova_item_descriptive_plot))) {
  ggsave(plot=nova_item_descriptive_plot[[i]], height = 5, width = 9,
         filename = paste0(nova_item_descriptive_plot[[i]][["labels"]][["title"]],"_item_descriptive_plot",".png"),
         path = output_plots_Dir, bg='white')  
}

### stacked plots
ggsave(plot=nova_item_stack_plot_grid, height = 6, width = 13.5,
       filename = paste0("nova_item_stack_plot",".png"),
       path = output_plots_Dir, bg='white')

### stacked plots - panels
ggsave(plot=nova_item_stack_year_plot_grid, height = 6, width = 13.5,
       filename = paste0("nova_item_stack_year_plot",".png"),
       path = output_plots_Dir, bg='white')
