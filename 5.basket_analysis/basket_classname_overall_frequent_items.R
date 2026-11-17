library(dplyr)
library(tidyr)
library(tibble)
library(arules)
#library(arulesViz)

working_directory

ggtheme_descriptive_plot()

## Market basket analysis - Frequent items Overall
overall_classname_frequent_items_plot <- sapply(
  unique(df_loyalty_basket_classname$customer_type[!is.na(df_loyalty_basket_classname$customer_type)]), function(x){
    nn <- x
    data <- df_loyalty_basket_classname %>%
      dplyr::select(-c(gender, age_group, county_name, year)) %>%
      tidyr::drop_na(customer_type) %>%
      dplyr::filter(customer_type == nn) %>%
      dplyr::select(-c(customer_type)) %>%
      tibble::column_to_rownames(var="transaction_id")
    
    matrix_data <- as.matrix(data)
    
    ### convert data frame to transaction class
    transactions <- arules::transactions(matrix_data)
    
    df <- arules::itemFrequency(transactions,type="relative") %>%
      as.data.frame() %>%
      tibble::rownames_to_column() %>%
      dplyr::mutate(level = nn) %>%
      dplyr::slice_max(tibble(.), n = 20) %>%
      dplyr::mutate(rowname = as_factor(rowname))
    
    plot <- ggplot(df, aes(x=rowname, y=.)) +
      geom_bar(stat = "identity", fill = "#31859C") +
      scale_y_continuous(n.breaks = 5, limits = c(0, 0.5),
                         expand = expansion(mult = c(0.01,0.01))) +
      scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 15)) +
      guides(fill = "none") + 
      labs(x=NULL,y=NULL) +
      theme(
        axis.text.x = element_text(size = 9, angle = 90, lineheight = 0.7, vjust = 0.5),
        axis.text.y = element_text(size = 9)
      )
    
  } , simplify = FALSE)

print(overall_classname_frequent_items_plot)

## Saving the class name overall frequent items plots
for (i in seq(length(overall_classname_frequent_items_plot))) {
  ggsave(plot=overall_classname_frequent_items_plot[[i]], height = 7.5, width = 9,
         filename = paste0(names(overall_classname_frequent_items_plot)[i],"_classname_frequent_items_plot",".png"),
         path = subDir_output_plots, bg='white'
         )  
}


