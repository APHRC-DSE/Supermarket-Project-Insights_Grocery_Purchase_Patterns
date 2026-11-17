library(dplyr)
library(readxl)
library(stringr)
library(tibble)

working_directory

## Reading the recode file sheet
recode_file <- read_excel_allsheets("./2.load_data_and_clean/supermarket_a_recode_file.xlsx")

branches_vars_df <- recode_file[["branches"]] #df for branches

rename_vars_df <- recode_file[["rename_vars"]] #df for renaming variable labels

nutrient_composition_df <- recode_file[["food_nutrient_composition"]] #df for food nutrient composition

## Creating a named vector to quickly assign the new variable labels
rename_vars_df <- (rename_vars_df %>%
                     dplyr::mutate(new_label = stringr::str_to_sentence(new_label))
                   )

new_labels <- rename_vars_df %>%
  dplyr::select(new_variable, new_label) %>%
  tidyr::drop_na(new_variable) %>%
  tibble::deframe()

