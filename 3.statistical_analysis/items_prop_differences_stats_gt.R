library(dplyr)
library(tidyr)
library(gtsummary)

working_directory

my_gtsummary_theme

gtsummary_compact_theme

## proportion differences

### items

descriptive_prop_differences_items_gt <- descriptive_table(
  df = df_loyalty_analysis_class_name,
  foot_note = "%",
  caption = "",
  categorical_proportion_digits = 3,
  statistic_categorical = "{p}",
  flex_table = FALSE,
  include = names(df_loyalty_analysis_class_name)[!names(df_loyalty_analysis_class_name) %in% 
                                                    c("gender", "age_group", "year", "county_name", "row_number")]
  )

## proportion by group differences

### items by gender, age-group, year, repeat customer
inferential_prop_differences_items_gt <- inferential_table(
  df = df_loyalty_analysis_class_name, 
  foot_note = "%",
  caption = "",
  categorical_proportion_digits = 3,
  statistic_categorical = "{p}",
  flex_table = FALSE,
  by_vars = c("gender", "age_group", "county_name", "year"),
  include = names(df_loyalty_analysis_class_name)[!names(df_loyalty_analysis_class_name) %in% 
                                                    c("gender", "age_group", "year", "county_name", "row_number")]
  )

## stratify proportion by group differences

### stratify gender - food categories by age-group, county

inferential_strata_gender_prop_differences_items_gt <- 
  inferential_strata_table(
    df = df_loyalty_analysis_class_name, 
    foot_note = "%",
    caption = "",
    categorical_proportion_digits = 3,
    statistic_categorical = "{p}",
    flex_table = TRUE,
    strata_vars = "gender",
    by_vars = c("age_group","county_name"),
    include = names(df_loyalty_analysis_class_name)[!names(df_loyalty_analysis_class_name) %in% 
                                                      c("gender", "age_group", "year", "county_name", "row_number")]
    )

print(inferential_strata_gender_prop_differences_items_gt)

### stratify age-group - food categories by gender, county

inferential_strata_age_prop_differences_items_gt <- 
  inferential_strata_table(
    df = df_loyalty_analysis_class_name, 
    foot_note = "%",
    caption = "",
    categorical_proportion_digits = 3,
    statistic_categorical = "{p}",
    flex_table = TRUE,
    strata_vars = "age_group",
    by_vars = c("gender", "county_name"),
    include = names(df_loyalty_analysis_class_name)[!names(df_loyalty_analysis_class_name) %in% 
                                                      c("gender", "age_group", "year", "county_name", "row_number")]
  )

print(inferential_strata_age_prop_differences_items_gt)

### stratify county - food categories by gender, age-group

inferential_strata_county_prop_differences_items_gt <- 
  inferential_strata_table(
    df = df_loyalty_analysis_class_name, 
    foot_note = "%",
    caption = "",
    categorical_proportion_digits = 3,
    statistic_categorical = "{p}",
    flex_table = TRUE,
    strata_vars = "county_name",
    by_vars = c("gender","age_group"),
    include = names(df_loyalty_analysis_class_name)[!names(df_loyalty_analysis_class_name) %in% 
                                                      c("gender", "age_group", "year", "county_name", "row_number")]
  )

print(inferential_strata_county_prop_differences_items_gt)

## Merging gtsummary tables

### Merging proportion differences and proportion by group differences
descriptive_inferential_prop_differences_items_gt_merge <- gtsummary::tbl_merge(tbls= c(list(descriptive_prop_differences_items_gt),
                                                                                     inferential_prop_differences_items_gt )
) %>%
  gtsummary::as_flex_table()

print(descriptive_inferential_prop_differences_items_gt_merge)

