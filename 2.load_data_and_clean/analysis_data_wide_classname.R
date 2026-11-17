library(dplyr)
library(tidyr)
library(purrr)
library(stringr)

working_directory

### wide data of class name

df_loyalty_analysis_class_name <- df_loyalty_analysis %>%
  dplyr::mutate(row_number = 1:n()) %>%
  dplyr::select(row_number, gender, age_group, county_name, year, class_name) %>%
  tidyr::drop_na(class_name) %>%
  tidyr::pivot_wider(names_from = class_name, 
                     values_from = class_name,
                     values_fn = ~"Yes", 
                     values_fill = "No",
                     names_expand = TRUE, #Forces implicit(unused) factor levels to explicit. be represented in result
                     names_prefix = "class_name_"
                     ) %>%
  purrr::set_names(~str_replace(.x, "class_name_", "") %>%
                     stringr::str_to_lower()
                   ) %>%
  dplyr::mutate(canned_vegetables_fruits_spices = if_else(`canned fruits` == "Yes"
                                                          | `canned vegetables` == "Yes"
                                                          | `canned spices` == "Yes", "Yes", "No")
                ,maplesyrup_sugarmolasses = if_else(`maple syrup` == "Yes"
                                                    | `sugar - molasses` == "Yes", "Yes", "No")
                ,gram_carrot_flour = if_else(`gram flour` == "Yes"
                                             | `carrot flour` == "Yes", "Yes", "No")
                )

