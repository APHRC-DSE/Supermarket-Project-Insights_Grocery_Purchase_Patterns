library(dplyr)
library(gtsummary)

working_directory

my_gtsummary_theme

gtsummary_compact_theme

## Descriptive statistics for nova items

### nova
descriptive_nova_items_gt <- descriptive_table(
  df = df_loyalty_analysis %>%
    dplyr::select(nova, customer_id),
  foot_note = "n (%)",
  caption = "Nova Classification",
  categorical_proportion_digits = 3,
  flex_table = FALSE,
  include = c("nova")
  )

## Inferential statistics

### nova classification by year
inferential_year_nova_items_gt <- 
  inferential_table(
    df = df_loyalty_analysis %>%
      dplyr::select(nova, year, customer_id), 
    foot_note = "n (%)",
    caption = "Nova Classification",
    categorical_proportion_digits = 3,
    flex_table = FALSE,
    by_vars = c("year"),
    include = c("year", "nova")
  )

### gender, branch, age-group, repeat customer by nova classification
inferential_nova_items_gt <- 
  inferential_table(
    df = df_loyalty_analysis %>%
      dplyr::select(nova, year, customer_id, gender, age_group, county_name), 
    foot_note = "n (%)",
    caption = "Nova Classification",
    categorical_proportion_digits = 3,
    by_vars = c("nova"),
    percent = "row",
    include = c("gender", "age_group", "county_name", "year", "nova")
  )

print(inferential_nova_items_gt)


### stratify gender - nova classification by age-group, county, year
inferential_strata_gender_nova_items_gt <- 
  inferential_strata_table(
    df = df_loyalty_analysis %>%
      dplyr::select(nova, year, customer_id, gender, age_group, county_name), 
    foot_note = "n (%)",
    caption = "Nova Classification - Strata Gender",
    categorical_proportion_digits = 3,
    strata_vars = "gender",
    by_vars = c("age_group", "county_name", "year"),
    percent = "column",
    include = c("nova")
    )

print(inferential_strata_gender_nova_items)

### stratify age-group - nova classification by gender, county, year
inferential_strata_age_nova_items_gt <- 
  inferential_strata_table(
    df = df_loyalty_analysis %>%
      dplyr::select(nova, year, customer_id, gender, age_group, county_name), 
    foot_note = "n (%)",
    caption = "Nova Classification - Strata Age group",
    categorical_proportion_digits = 3,
    strata_vars = "age_group",
    by_vars = c("gender", "county_name", "year"),
    percent = "column",
    include = c("nova")
    )

print(inferential_strata_age_nova_items_gt)

### stratify county - nova classification by gender, age-group, year
inferential_strata_county_nova_items_gt <- 
  inferential_strata_table(
    df = df_loyalty_analysis %>%
      dplyr::select(nova, year, customer_id, gender, age_group, county_name), 
    foot_note = "n (%)",
    caption = "Nova Classification - Strata County",
    categorical_proportion_digits = 3,
    strata_vars = "county_name",
    by_vars = c("gender", "age_group", "year"),
    percent = "column",
    include = c("nova")
  )

print(inferential_strata_county_nova_items_gt)

## Merging gtsummary tables

### Merging descriptive and inferential year
descriptive_inferential_year_nova_items_gt_merge <- gtsummary::tbl_merge(tbls= c(list(descriptive_nova_items_gt),
                                                                             inferential_year_nova_items_gt )
                                                                ) %>%
  gtsummary::as_flex_table()

print(descriptive_inferential_year_nova_items_gt_merge)

