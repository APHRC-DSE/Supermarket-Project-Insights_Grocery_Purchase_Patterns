library(dplyr)
library(tidyr)
library(labelled)

working_directory

## dataset with variables for descriptive and inferential statistics

### loyalty customers data for food items
df_loyalty <- df_clean_a %>%
  dplyr::filter(customer_type == "Loyalty card") %>%
  tidyr::drop_na(id) %>%
  dplyr::select(id, gender, description, price, quantity, quantity_new, total, total_new, trnref, transaction_id, sdatetime, 
                paymentmode, branch, branch_name, customer_type, month_date, month_name, year, quarter_date, age, age_group,
                county_name, sub_county_name, supermarket_name, item_type, class_name, subclass_name, nova, standard_uom,
                quantity_uom, price_uom, class_name_uom
                ) 

### unique id for loyalty customers
df_loyalty_customer_id <- df_loyalty %>%
  dplyr::select(id, branch) %>%
  dplyr::distinct() %>%
  dplyr::arrange(branch, id) %>%
  dplyr::mutate(customer_id = paste0(branch, "-", id))

### repeat/non repeat loyalty customers
df_loyalty_repeat <- df_loyalty %>%
  dplyr::select(id, branch, trnref, age) %>%
  dplyr::distinct() %>%
  dplyr::arrange(branch, id, trnref, age) %>%
  dplyr::group_by(branch, id) %>%
  dplyr::summarise(repeat_customer = length(trnref), #repeat_customer = n()
            mean_age = mean(age, na.rm = TRUE), .groups = "drop") %>%
  dplyr::mutate(repeat_customer = if_else(repeat_customer <2, "Non-Repeat", "Repeat")
                , mean_agegroup = if_else(mean_age < 26, "18-25",
                                                if_else(mean_age < 36, "26-35",
                                                       if_else(mean_age < 50, "36-49", "50 and above" )) 
                                         #group age
                                         )
                )

### mean year age
df_loyalty_year_age <- df_loyalty %>%
  dplyr::select(id, branch, trnref, year, age) %>%
  dplyr::distinct() %>%
  dplyr::arrange(branch, id, trnref, year, age) %>%
  dplyr::group_by(branch, id, year) %>%
  dplyr::summarise(meanyear_age = mean(age, na.rm = TRUE), .groups = "drop") %>%
  dplyr::mutate(meanyear_agegroup = if_else(meanyear_age < 26, "18-25",
                                                   if_else(meanyear_age < 36, "26-35",
                                                           if_else(meanyear_age < 50, "36-49", "50 and above" )) 
                                           #group age
                                           )
                )

### merging loyalty customers data with columns for unique id and repeat/non repeat
df_loyalty_analysis <- df_loyalty %>%
  dplyr::left_join(df_loyalty_customer_id,
                   by = c("id", "branch")
                   ) %>%
  dplyr::left_join(df_loyalty_repeat,
                   by = c("id", "branch")
                   ) %>%
  dplyr::left_join(df_loyalty_year_age,
                   by = c("id", "branch", "year")
                   ) %>%
  dplyr::filter(year > 2021) %>% 
  labelled::set_variable_labels(!!!new_labels[names(new_labels) %in% names(.)] #labeling variables from created named vector
                                ) %>%
  dplyr::select(description, gender, quantity_new, total_new, transaction_id, month_date,year, quarter_date, age,
                age_group, county_name, class_name, subclass_name, nova, quantity_uom, price_uom, class_name_uom,
                customer_id, customer_type, repeat_customer, mean_age, mean_agegroup, meanyear_age, meanyear_agegroup
                )


