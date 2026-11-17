library(dplyr)
library(tidyr)
library(purrr)
library(stringr)

working_directory

### wide data of nova

df_loyalty_analysis_nova <- df_loyalty_analysis %>%
  dplyr::mutate(row_number = 1:n()) %>%
  dplyr::select(row_number, gender, age_group, county_name, year, nova) %>%
  tidyr::drop_na(nova) %>%
  tidyr::pivot_wider(names_from = nova, 
                     values_from = nova,
                     values_fn = ~"Yes", 
                     values_fill = "No",
                     names_expand = TRUE, #Forces implicit(unused) factor levels to explicit. be represented in result
                     names_prefix = "nova_"
                     ) %>%
  purrr::set_names(~str_replace(.x, "nova_", "") %>%
                     stringr::str_to_lower()
                   )

