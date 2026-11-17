library(dplyr)
library(readr)
library(lubridate)
library(janitor)
library(stringr)
library(labelled)

working_directory

## unit of measure data frame and expression words
uom_vars_df <- recode_file[["uom"]] %>%
  janitor::clean_names() %>%
  dplyr::distinct(uom_criteria, .keep_all = TRUE) %>%
  dplyr::mutate(uom_criteria = gsub("S/KG"," /KG", uom_criteria, fixed = TRUE), #put a space before "/KG"
                uom_criteria = gsub("S/PC"," /PC", uom_criteria, fixed = TRUE), #put a space before "/PC"
                uom_regex = paste0("\\b", uom_criteria, "\\b"),
                uom_regex = gsub(".","\\.", uom_regex, fixed = TRUE),
                uom_regex = gsub("*","\\*", uom_regex, fixed = TRUE),
                uom_regex = gsub("(", "\\(", uom_regex, fixed = TRUE)
                )

uom_words <- regex(paste(c(uom_vars_df$uom_regex), sep = "", collapse = '|'),
                   ignore_case = TRUE)

## Clean food composition df
nutrient_composition_df <- (nutrient_composition_df %>%
                              janitor::clean_names() %>%
                              dplyr::mutate(across(where(is.character) & !c(nutrient_classification_document, food_group,
                                                                         code, food_name, edible_conversion_factor),
                                                   ~readr::parse_number(.x)
                                                   )
                                            , across(!c(nutrient_classification_document, food_group,
                                                        code, food_name, edible_conversion_factor),
                                                     ~if_else(.x == 0, NA, .x)
                                                     )
                                            )
                            )

## unique items data frame joined with unit of measure data frame
unique_items_vars_df <- recode_file[["unique_items"]] %>% 
  janitor::clean_names() %>%
  dplyr::distinct(description, .keep_all = TRUE) %>%
  dplyr::left_join(nutrient_composition_df %>%
                     dplyr::select(-c(food_group, food_name, edible_conversion_factor)
                                   ),
                   by = c("nutrient_classification_document", "code")
                   ) %>%
  dplyr::select(-c(nutrient_classification_document, code)
                ) %>%
  dplyr::mutate(uom_criteria = stringr::str_extract(description, uom_words)
                ) %>%
  dplyr::left_join(uom_vars_df %>%
                     dplyr::select(-c(uom_regex, standard_uom_criteria)),
                   by = c("uom_criteria")
                   )

df_clean_a <- df_clean_supermarket_a %>%
  dplyr::mutate(price = if_else(price == 0, NA, price) #Replacing price 0 with NA
                , sdatetime = as.Date(sdatetime) #format doesn't store any time information
                , month_date = lubridate::floor_date(sdatetime, unit = "month")
                ) %>%
  dplyr::group_by(description, month_date) %>%
  dplyr::mutate(price = if_else(is.na(price), round(mean(price, na.rm= TRUE),2),
                                price) #Replacing where price is NA with mean for items
                ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate( total_new = price*quantity
                 , quantity_new = if_else((quantity %% 1) > 0, 1, quantity) #modulo operator with decimal numbers
                 , price_new = total_new/quantity_new
                 , customer_type = if_else(!is.na(id) | paymentmode == "Loyalty Redemption", "Loyalty card", "No-Loyalty card")
                 , month_name = lubridate::month(sdatetime,  label = TRUE, abbr = FALSE)
                 , year = lubridate::year(sdatetime)
                 , quarter_date = as.factor(lubridate::quarter(sdatetime, type = "year.quarter"))
                 , age = round(lubridate::time_length(difftime(sdatetime, dob_new, units = "auto"),
                                                      unit = "year"),0 #calculating age
                               )
                 , age = if_else(age <18, NA, age) #convert less than 18 to missing
                 , age_group = if_else(age < 26, "18-25",
                                               if_else(age < 36, "26-35",
                                                       if_else(age < 50, "36-49", "50 and above" )) #group age
                                      )
                 ) %>%
  dplyr::left_join(unique_items_vars_df
                   , by = c("description")
                   ) %>%
  dplyr::mutate(quantity_uom = if_else(is.na(conversion), quantity
                                       , quantity*conversion
                                       )
                , price_uom = total_new/quantity_uom
                , across(c(price_uom), ~replace(.x, is.nan(.x), 0))
                , class_name_uom = paste0(class_name, " (", standard_uom, ")")
                ) %>%
  dplyr::filter(quantity > 0, item_type == "Food Item") %>%
  labelled::set_variable_labels(!!!new_labels[names(new_labels) %in% names(.)] #labeling variables from named vector
                                )

