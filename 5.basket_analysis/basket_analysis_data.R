library(dplyr)
library(tidyr)
library(stringr)
library(purrr)

working_directory

## dataset with variables for market basket analysis

df_loyalty_basket_sociodemo <- df_loyalty_analysis %>%
  dplyr::select(customer_type, gender, age_group, county_name, year, transaction_id) %>%
  dplyr::distinct(transaction_id, .keep_all = TRUE)

df_loyalty_basket_classname <- df_loyalty_analysis %>%
  dplyr::select(transaction_id, class_name) %>%
  tidyr::drop_na(class_name) %>%
  tidyr::pivot_wider(names_from = class_name,
                     values_from = class_name,
                     values_fn = ~1, #values_fn = length
                     values_fill = 0, 
                     names_expand = TRUE, #Forces implicit(unused) factor levels to explicit. be represented in result
                     names_prefix = "class_name_"
                     ) %>%
  purrr::set_names(~str_replace(.x, "class_name_", "") %>%
                     stringr::str_to_lower()
                   ) %>%
  #dplyr::select(-any_of("na")) %>%
  dplyr::left_join(df_loyalty_basket_sociodemo, 
                   by = "transaction_id"
                   )

