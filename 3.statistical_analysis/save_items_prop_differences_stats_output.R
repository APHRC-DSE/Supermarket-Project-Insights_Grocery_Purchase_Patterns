library(dplyr)
library(writexl)

working_directory

## Saving descriptive and inferential output

### item proportion differences

writexl::write_xlsx(list(descriptive_prop = df_descriptive_prop_differences_items,
                         inferential_prop = df_inferential_prop_differences_items,
                         descriptive_age_gender_prop = df_descriptive_age_gender_prop_differences_items,
                         inferential_age_gender_prop = df_inferential_age_gender_prop_differences_items,
                         descriptive_gender_age_prop = df_descriptive_gender_age_prop_differences_items,
                         inferential_gender_age_prop = df_inferential_gender_age_prop_differences_items,
                         descriptive_gender_county_prop = df_descriptive_gender_county_prop_differences_items,
                         inferential_gender_county_prop = df_inferential_gender_county_prop_differences_items,
                         descriptive_county_gender_prop = df_descriptive_county_gender_prop_differences_items,
                         inferential_county_gender_prop = df_inferential_county_gender_prop_differences_items
                         ),
                    path = base::file.path(output_Dir, "descriptive_inferential_prop_differences_items.xlsx" )
                    )

