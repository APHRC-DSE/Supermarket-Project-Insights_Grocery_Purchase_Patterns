library(dplyr)

working_directory

## Set thresholds for rules parameters

### Support
support_classname_overall <- 0.01
support_classname_gender <- 0.01
support_classname_age <- 0.01
support_classname_year <- 0.01
support_classname_county <- 0.01

support_classname_age_gender <- 0.01
support_classname_age_county <- 0.01
support_classname_gender_county <- 0.01

support_classname_year_gender <- 0.01
support_classname_year_age <- 0.01
support_classname_year_county <- 0.01


### Confidence

confidence_classname_overall <- 0.5
confidence_classname_gender <- 0.5
confidence_classname_age <- 0.5
confidence_classname_year <- 0.5
confidence_classname_county <- 0.5

confidence_classname_age_gender <- 0.5
confidence_classname_age_county <- 0.5
confidence_classname_gender_county <- 0.5

confidence_classname_year_gender <- 0.5
confidence_classname_year_age <- 0.5
confidence_classname_year_county <- 0.5

