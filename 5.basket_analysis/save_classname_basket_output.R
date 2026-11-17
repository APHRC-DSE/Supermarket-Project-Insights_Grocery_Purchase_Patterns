library(dplyr)
library(writexl)

working_directory

## Saving Overall basket analysis Output - classname

writexl::write_xlsx(list(overall_rules = df_overall_classname_rules,
                         gender_rules = df_gender_classname_rules,
                         age_rules = df_age_classname_rules,
                         year_rules = df_year_classname_rules,
                         county_rules = df_county_classname_rules
                         ),
                    path = base::file.path(output_Dir, "basket_overall_classname_analysis.xlsx" )
                    )

## Saving Socio_by basket analysis Output - classname
writexl::write_xlsx(list(age_gender_rules = df_age_gender_classname_rules,
                         age_county_rules = df_age_county_classname_rules,
                         gender_county_rules = df_gender_county_classname_rules
                         ),
                    path = base::file.path(output_Dir, "basket_socio_by_classname_analysis.xlsx" )
                    )

## Saving Year_by basket analysis Output - classname
writexl::write_xlsx(list(year_gender_rules = df_year_gender_classname_rules,
                         year_age_rules = df_year_age_classname_rules,
                         year_county_rules = df_year_county_classname_rules
                         ),
                    path = base::file.path(output_Dir, "basket_year_by_classname_analysis.xlsx" )
                    )


