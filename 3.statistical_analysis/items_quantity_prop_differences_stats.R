library(dplyr)
library(tidyr)
library(gtsummary)

working_directory

my_gtsummary_theme

gtsummary_compact_theme

## proportion differences

### items quantity

descriptive_prop_differences_items_quantity <- descriptive_table(
  df = df_loyalty_analysis_class_name %>%
    dplyr::slice(rep(seq(n()), times = quantity_new)),
  foot_note = "n (%)",
  caption = "",
  categorical_proportion_digits = 2,
  flex_table = FALSE,
  include = names(df_loyalty_analysis_class_name)[!names(df_loyalty_analysis_class_name) %in% 
                                                    c("description", "quantity_new", "gender", "age_group", "year",
                                                      "repeat_customer", "county_name", "row_number")]
  )

## proportion by group differences

### items by gender, age-group, year, repeat customer
inferential_prop_differences_items_quantity <- inferential_table(
  df = df_loyalty_analysis_class_name %>%
    dplyr::slice(rep(seq(n()), times = quantity_new)), 
  foot_note = "n (%)",
  caption = "",
  categorical_proportion_digits = 2,
  flex_table = FALSE,
  by_vars = c("gender", "age_group", "repeat_customer", "year"),
  include = names(df_loyalty_analysis_class_name)[!names(df_loyalty_analysis_class_name) %in% 
                                                    c("description", "quantity_new","gender", "age_group", "year",
                                                      "repeat_customer", "county_name", "row_number")]
  )

## proportion strata by group differences

### strata year nova items quantity by gender, age-group, repeat customer
# inferential_strata_year_prop_differences_items_quantity <- 
#   inferential_strata_table(
#     df = df_loyalty_analysis_class_name %>%
#       dplyr::slice(rep(seq(n()), times = quantity_new)), 
#     foot_note = "n (%); Mean (SD); Median (IQR); Range",
#     caption = "class_name",
#     categorical_proportion_digits = 2,
#     strata_vars = "year",
#     by_vars = c("gender", "age_group", "repeat_customer", "county_name"),
#     include = names(df_loyalty_analysis_class_name)[!names(df_loyalty_analysis_class_name) %in% 
#                                                       c("description", "quantity_new", "gender", "age_group", "year",
#                                                         "repeat_customer", "county_name", "row_number")]
#     )
# 
# print(inferential_strata_year_prop_differences_items_quantity)


## Merging gtsummary tables

### Merging proportion differences and proportion by group differences
descriptive_inferential_prop_differences_items_quantity_merge <- 
  gtsummary::tbl_merge(tbls= c(list(descriptive_prop_differences_items_quantity),
                               inferential_prop_differences_items_quantity )
                       ) %>%
  gtsummary::as_flex_table()

print(descriptive_inferential_prop_differences_items_quantity_merge)

