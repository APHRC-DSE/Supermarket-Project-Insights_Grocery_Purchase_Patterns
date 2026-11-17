library(dplyr)
library(janitor)
library(haven)
library(sjlabelled)

working_directory

## Saving Analysis clean data set as a spss file

haven::write_sav(df_loyalty_analysis %>%
                   dplyr::mutate(across(c(gender, age_group, county_name, nova, year), 
                                        ~sjlabelled::as_factor(.x)
                                        )
                                 ),
                 path = base::file.path(output_Dir, "loyalty_analysis_clean_data.sav"),
                 compress = TRUE,
                 adjust_tz = TRUE
                 )

haven::write_sav(df_loyalty_analysis_class_name %>%
                  janitor::clean_names() %>%
                  dplyr::mutate(across(!c(row_number), ~as.factor(.x))),
                path = base::file.path(output_Dir, "loyalty_analysis_class_name_clean_data.sav"),
                compress = TRUE,
                adjust_tz = TRUE
                )

haven::write_sav(df_loyalty_analysis %>%
                   dplyr::select(customer_id, gender, mean_age, mean_agegroup, county_name)%>%
                   distinct(customer_id, .keep_all = TRUE) %>%
                   dplyr::mutate(across(!c(customer_id, mean_age), ~as.factor(.x))),
                 path = base::file.path(output_Dir, "unique_customers_loyalty_analysis_clean_data.sav"),
                 compress = TRUE,
                 adjust_tz = TRUE
                 )

haven::write_sav(df_loyalty_analysis %>%
                   dplyr::select(customer_id, year, gender, meanyear_age, meanyear_agegroup, county_name) %>%
                   distinct(customer_id, year, .keep_all = TRUE) %>%
                   dplyr::mutate(across(!c(customer_id, meanyear_age), ~as.factor(.x))),
                 path = base::file.path(output_Dir, "unique_year_customers_loyalty_analysis_clean_data.sav"),
                 compress = TRUE,
                 adjust_tz = TRUE
                 )


