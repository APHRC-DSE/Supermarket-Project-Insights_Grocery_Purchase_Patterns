library(dplyr)
library(gtsummary)

working_directory

my_gtsummary_theme

gtsummary_compact_theme

## Descriptive statistics

### unique loyalty customers
descriptive_customers <- descriptive_table(
  df = df_loyalty_analysis %>%
    dplyr::select(customer_id, gender, mean_age, mean_agegroup, county_name)%>%
    distinct(customer_id, .keep_all = TRUE),
  flex_table = FALSE,
  foot_note = "n (%); Mean (SD); Median (IQR); Range",
  caption = "Unique Loyalty Customers",
  include = c("gender", "mean_age" ,"mean_agegroup", "county_name")
  )

## Inferential statistics

### unique loyalty customers by year
inferential_year_customers <- 
  inferential_table(
    df = df_loyalty_analysis %>%
      dplyr::select(customer_id, year, gender, meanyear_age, meanyear_agegroup, county_name) %>%
      distinct(customer_id, year, .keep_all = TRUE),
    flex_table = FALSE,
    foot_note = "n (%); Mean (SD); Median (IQR); Range",
    caption = "Unique Loyalty Customers",
    by_vars = c("year"),
    include = c("gender", "meanyear_age" ,"meanyear_agegroup", "county_name")
    )


### unique loyalty customers by gender, age-group, county
inferential_customers <- 
  inferential_table(
    df = df_loyalty_analysis %>%
      dplyr::select(customer_id, gender, mean_age, mean_agegroup, county_name) %>%
      distinct(customer_id, .keep_all = TRUE), 
    flex_table = FALSE,
    foot_note = "n (%); Mean (SD); Median (IQR); Range",
    caption = "Unique Loyalty Customers",
    by_vars = c("gender", "mean_agegroup", "county_name"),
    include = c("gender", "mean_age" ,"mean_agegroup", "county_name")
    )

## Merging gtsummary tables

### Merging descriptive and inferential year
descriptive_inferential_year_customers_merge <- gtsummary::tbl_merge(tbls= c(list(descriptive_customers),
                                                                       inferential_year_customers )
                                                               ) %>%
  gtsummary::as_flex_table()

print(descriptive_inferential_year_customers_merge)

### Merging inferential 
inferential_customers_merge <- gtsummary::tbl_merge(tbls= c(list(descriptive_customers),
                                                            inferential_customers )
                                                    ) %>%
  gtsummary::as_flex_table()

print(inferential_customers_merge)
