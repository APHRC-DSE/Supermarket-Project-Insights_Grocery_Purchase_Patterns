library(dplyr)
library(forcats)
library(tidyr)
library(rstatix)
library(tibble)

working_directory

##############################################################################################
## proportions - items

### overall, gender, age-group, county, year
descriptive_prop_differences_items <- sapply(c("customer_type","gender", "age_group", "county_name", "year"), function(x){
  nn <- x
  out <- df_loyalty_analysis %>%
    tidyr::drop_na(any_of(nn), class_name) %>%
    dplyr::mutate( class_name = as.factor(class_name),
                   class_name = forcats::fct_collapse(class_name
                                                      ,"CANNED VEGETABLES/FRUITS/SPICES" = c("CANNED FRUITS", "CANNED SPICES",
                                                                                             "CANNED VEGETABLES")
                                                      ,"MAPLE SYRUP/SUGAR - MOLASSES" = c("MAPLE SYRUP", "SUGAR - MOLASSES")
                                                      ,"GRAM/CARROT FLOUR" = c("CARROT FLOUR", "GRAM FLOUR")
                                                      )
                  ) %>%
    dplyr::select(all_of(nn), class_name, nova) %>%
    dplyr::add_count(.data[[nn]], name = "total") %>%
    dplyr::group_by(.data[[nn]], class_name, nova, total) %>%
    dplyr::summarise(n = n(), .groups = "drop") %>%
    dplyr::mutate(prop = round((n/total)*100,3)) %>%
    dplyr::rename(group = nn)
  
}, simplify = FALSE
)

df_descriptive_prop_differences_items <- do.call("rbind", descriptive_prop_differences_items) %>%
  tibble::remove_rownames() %>%
  tidyr::pivot_wider(names_from = group,
                     names_glue = "{group}_{.value}",
                     names_vary = "slowest",
                     values_from = c(prop, n, total),
                     values_fill = 0
                     ) %>%
  dplyr::arrange(as.character(class_name)) 

### age-gender
descriptive_age_gender_prop_differences_items <- sapply(
  sort(unique(df_loyalty_analysis$age_group[!is.na(df_loyalty_analysis$age_group)])), function(x){
    
  nn <- x
  out <- df_loyalty_analysis %>%
    tidyr::drop_na(age_group, gender, class_name) %>%
    dplyr::mutate( class_name = as.factor(class_name),
                   class_name = forcats::fct_collapse(class_name
                                                      ,"CANNED VEGETABLES/FRUITS/SPICES" = c("CANNED FRUITS", "CANNED SPICES",
                                                                                             "CANNED VEGETABLES")
                                                      ,"MAPLE SYRUP/SUGAR - MOLASSES" = c("MAPLE SYRUP", "SUGAR - MOLASSES")
                                                      ,"GRAM/CARROT FLOUR" = c("CARROT FLOUR", "GRAM FLOUR")
                   )
                   ) %>%
    dplyr::filter(age_group == nn) %>%
    dplyr::select(age_group, gender, class_name, nova) %>%
    dplyr::add_count(gender, name = "total") %>%
    dplyr::group_by(gender, class_name, nova, total) %>%
    dplyr::summarise(n = n(), .groups = "drop") %>%
    dplyr::mutate(prop = round((n/total)*100,3))
  
  out$age_group <- nn
  
  return(out)
  
}, simplify = FALSE
)

df_descriptive_age_gender_prop_differences_items <- do.call("rbind", descriptive_age_gender_prop_differences_items) %>%
  tibble::remove_rownames() %>%
  tidyr::pivot_wider(names_from = c(age_group, gender),
                     names_glue = "{age_group}_{gender}_{.value}",
                     names_vary = "slowest",
                     values_from = c(prop, n, total),
                     values_fill = 0
                     ) %>%
  dplyr::arrange(as.character(class_name))

### gender-age
descriptive_gender_age_prop_differences_items <- sapply(
  sort(unique(df_loyalty_analysis$gender[!is.na(df_loyalty_analysis$gender)])), function(x){
    
    nn <- x
    out <- df_loyalty_analysis %>%
      tidyr::drop_na(age_group, gender, class_name) %>%
      dplyr::mutate( class_name = as.factor(class_name),
                     class_name = forcats::fct_collapse(class_name
                                                        ,"CANNED VEGETABLES/FRUITS/SPICES" = c("CANNED FRUITS", "CANNED SPICES",
                                                                                               "CANNED VEGETABLES")
                                                        ,"MAPLE SYRUP/SUGAR - MOLASSES" = c("MAPLE SYRUP", "SUGAR - MOLASSES")
                                                        ,"GRAM/CARROT FLOUR" = c("CARROT FLOUR", "GRAM FLOUR")
                                                        )
                     ) %>%
      dplyr::filter(gender == nn) %>%
      dplyr::select(age_group, gender, class_name, nova) %>%
      dplyr::add_count(age_group, name = "total") %>%
      dplyr::group_by(age_group, class_name, nova, total) %>%
      dplyr::summarise(n = n(), .groups = "drop") %>%
      dplyr::mutate(prop = round((n/total)*100,3))
    
    out$gender <- nn
    
    return(out)
    
  }, simplify = FALSE
)

df_descriptive_gender_age_prop_differences_items <- do.call("rbind", descriptive_gender_age_prop_differences_items) %>%
  tibble::remove_rownames() %>%
  tidyr::pivot_wider(names_from = c(gender, age_group),
                     names_glue = "{gender}_{age_group}_{.value}",
                     names_vary = "slowest",
                     values_from = c(prop, n, total),
                     values_fill = 0
                     ) %>%
  dplyr::arrange(as.character(class_name))

### gender-county
descriptive_gender_county_prop_differences_items <- sapply(
  sort(unique(df_loyalty_analysis$gender[!is.na(df_loyalty_analysis$gender)])), function(x){
    
    nn <- x
    out <- df_loyalty_analysis %>%
      tidyr::drop_na(county_name, gender, class_name) %>%
      dplyr::mutate( class_name = as.factor(class_name),
                     class_name = forcats::fct_collapse(class_name
                                                        ,"CANNED VEGETABLES/FRUITS/SPICES" = c("CANNED FRUITS", "CANNED SPICES",
                                                                                               "CANNED VEGETABLES")
                                                        ,"MAPLE SYRUP/SUGAR - MOLASSES" = c("MAPLE SYRUP", "SUGAR - MOLASSES")
                                                        ,"GRAM/CARROT FLOUR" = c("CARROT FLOUR", "GRAM FLOUR")
                     )
      ) %>%
      dplyr::filter(gender == nn) %>%
      dplyr::select(county_name, gender, class_name, nova) %>%
      dplyr::add_count(county_name, name = "total") %>%
      dplyr::group_by(county_name, class_name, nova, total) %>%
      dplyr::summarise(n = n(), .groups = "drop") %>%
      dplyr::mutate(prop = round((n/total)*100,3))
    
    out$gender <- nn
    
    return(out)
    
  }, simplify = FALSE
)

df_descriptive_gender_county_prop_differences_items <- do.call("rbind", descriptive_gender_county_prop_differences_items) %>%
  tibble::remove_rownames() %>%
  tidyr::pivot_wider(names_from = c(gender, county_name),
                     names_glue = "{gender}_{county_name}_{.value}",
                     names_vary = "slowest",
                     values_from = c(prop, n, total),
                     values_fill = 0
                     ) %>%
  dplyr::arrange(as.character(class_name))

### county-gender
descriptive_county_gender_prop_differences_items <- sapply(
  sort(unique(df_loyalty_analysis$county_name[!is.na(df_loyalty_analysis$county_name)])), function(x){
    
    nn <- x
    out <- df_loyalty_analysis %>%
      tidyr::drop_na(county_name, gender, class_name) %>%
      dplyr::mutate( class_name = as.factor(class_name),
                     class_name = forcats::fct_collapse(class_name
                                                        ,"CANNED VEGETABLES/FRUITS/SPICES" = c("CANNED FRUITS", "CANNED SPICES",
                                                                                               "CANNED VEGETABLES")
                                                        ,"MAPLE SYRUP/SUGAR - MOLASSES" = c("MAPLE SYRUP", "SUGAR - MOLASSES")
                                                        ,"GRAM/CARROT FLOUR" = c("CARROT FLOUR", "GRAM FLOUR")
                                                        )
                     ) %>%
      dplyr::filter(county_name == nn) %>%
      dplyr::select(county_name, gender, class_name, nova) %>%
      dplyr::add_count(gender, name = "total") %>%
      dplyr::group_by(gender, class_name, nova, total) %>%
      dplyr::summarise(n = n(), .groups = "drop") %>%
      dplyr::mutate(prop = round((n/total)*100,3))
    
    out$county_name <- nn
    
    return(out)
    
  }, simplify = FALSE
)

df_descriptive_county_gender_prop_differences_items <- do.call("rbind", descriptive_county_gender_prop_differences_items) %>%
  tibble::remove_rownames() %>%
  tidyr::pivot_wider(names_from = c(county_name, gender),
                     names_glue = "{county_name}_{gender}_{.value}",
                     names_vary = "slowest",
                     values_from = c(prop, n, total),
                     values_fill = 0
                     ) %>%
  dplyr::arrange(as.character(class_name))

##############################################################################################
## group differences proportions - items 

### gender, age-group, county, year
inferential_prop_differences_items <- sapply(c("gender", "age_group", "county_name", "year"), function(x){
  nn <- x
  
  items <- names(df_loyalty_analysis_class_name)[!names(df_loyalty_analysis_class_name) %in% 
                                                   c("row_number", "gender", "age_group", "county_name", "year",
                                                     "canned fruits", "canned spices", "canned vegetables",
                                                     "maple syrup", "sugar - molasses", "carrot flour", "gram flour")]
  
  df_new <- df_loyalty_analysis_class_name %>%
    dplyr::select(any_of(c(nn, items)))
  
  out <- sapply(items, function(y){
    table_out <- base::table(df_new[[y]], df_new[[nn]],
                             useNA = c("no")
                             )
    
    chi_square_out <- rstatix::chisq_test(table_out)$p
    
  }, simplify = TRUE
  )
  
  out_ <- tibble::tibble( group = nn,
                          items = names(out),
                          pvalue = out
                          ) %>%
    dplyr::mutate(pvalue = ifelse(pvalue < 0.001, "<0.001",
                                  ifelse(pvalue > 0.999, ">0.999", round(pvalue,3)
                                         )
                                  )
                  )
  
}, simplify = FALSE
)

df_inferential_prop_differences_items <- do.call("rbind", inferential_prop_differences_items) %>%
  tibble::remove_rownames() %>%
  tidyr::pivot_wider(names_from = group,
                     names_glue = "{group}_{.value}",
                     names_vary = "slowest",
                     values_from = c(pvalue)
                     ) %>%
  dplyr::mutate(items = stringr::str_to_upper(items),
                items = ifelse(items == "CANNED_VEGETABLES_FRUITS_SPICES", "CANNED VEGETABLES/FRUITS/SPICES",
                               ifelse(items == "MAPLESYRUP_SUGARMOLASSES", "MAPLE SYRUP/SUGAR - MOLASSES",
                                      ifelse(items == "GRAM_CARROT_FLOUR", "GRAM/CARROT FLOUR", items 
                                      )
                               )
                               )
                ) %>%
  dplyr::arrange(items)

### age-gender
inferential_age_gender_prop_differences_items <- sapply(
  sort(unique(df_loyalty_analysis_class_name$age_group[!is.na(df_loyalty_analysis_class_name$age_group)])), function(x){
  nn <- x
  
  items <- names(df_loyalty_analysis_class_name)[!names(df_loyalty_analysis_class_name) %in% 
                                                   c("row_number", "gender", "age_group", "county_name", "year",
                                                     "canned fruits", "canned spices", "canned vegetables",
                                                     "maple syrup", "sugar - molasses", "carrot flour", "gram flour")]
  
  df_new <- df_loyalty_analysis_class_name %>%
    dplyr::filter(age_group == nn) %>%
    dplyr::select(any_of(c(items)), gender)
  
  out <- sapply(items, function(y){
    table_out <- base::table(df_new[[y]], df_new[["gender"]],
                             useNA = c("no")
                             )
    
    chi_square_out <- rstatix::chisq_test(table_out)$p
    
  }, simplify = TRUE
  )
  
  out_ <- tibble::tibble( age_group = nn,
                          items = names(out),
                          pvalue = out
                          ) %>%
    dplyr::mutate(pvalue = ifelse(pvalue < 0.001, "<0.001",
                                  ifelse(pvalue > 0.999, ">0.999", round(pvalue,3)
                                  )
                                  )
                  )
  
}, simplify = FALSE
)

df_inferential_age_gender_prop_differences_items <- do.call("rbind", inferential_age_gender_prop_differences_items) %>%
  tibble::remove_rownames() %>%
  tidyr::pivot_wider(names_from = age_group,
                     names_glue = "{age_group}_{.value}",
                     names_vary = "slowest",
                     values_from = c(pvalue)
                     ) %>%
  dplyr::mutate(items = stringr::str_to_upper(items),
                items = ifelse(items == "CANNED_VEGETABLES_FRUITS_SPICES", "CANNED VEGETABLES/FRUITS/SPICES",
                               ifelse(items == "MAPLESYRUP_SUGARMOLASSES", "MAPLE SYRUP/SUGAR - MOLASSES",
                                      ifelse(items == "GRAM_CARROT_FLOUR", "GRAM/CARROT FLOUR", items 
                                      )
                               )
                               )
                ) %>%
  dplyr::arrange(items)

### gender-age
inferential_gender_age_prop_differences_items <- sapply(
  sort(unique(df_loyalty_analysis_class_name$gender[!is.na(df_loyalty_analysis_class_name$gender)])), function(x){
    nn <- x
    
    items <- names(df_loyalty_analysis_class_name)[!names(df_loyalty_analysis_class_name) %in% 
                                                     c("row_number", "gender", "age_group", "county_name", "year",
                                                       "canned fruits", "canned spices", "canned vegetables",
                                                       "maple syrup", "sugar - molasses", "carrot flour", "gram flour")]
    
    df_new <- df_loyalty_analysis_class_name %>%
      dplyr::filter(gender == nn) %>%
      dplyr::select(any_of(c(items)), age_group)
    
    out <- sapply(items, function(y){
      table_out <- base::table(df_new[[y]], df_new[["age_group"]],
                               useNA = c("no")
      )
      
      chi_square_out <- rstatix::chisq_test(table_out)$p
      
    }, simplify = TRUE
    )
    
    out_ <- tibble::tibble( gender = nn,
                            items = names(out),
                            pvalue = out
                            ) %>%
      dplyr::mutate(pvalue = ifelse(pvalue < 0.001, "<0.001",
                                    ifelse(pvalue > 0.999, ">0.999", round(pvalue,3)
                                    )
                                    )
                    )
    
  }, simplify = FALSE
)

df_inferential_gender_age_prop_differences_items <- do.call("rbind", inferential_gender_age_prop_differences_items) %>%
  tibble::remove_rownames() %>%
  tidyr::pivot_wider(names_from = gender,
                     names_glue = "{gender}_{.value}",
                     names_vary = "slowest",
                     values_from = c(pvalue)
                     ) %>%
  dplyr::mutate(items = stringr::str_to_upper(items),
                items = ifelse(items == "CANNED_VEGETABLES_FRUITS_SPICES", "CANNED VEGETABLES/FRUITS/SPICES",
                               ifelse(items == "MAPLESYRUP_SUGARMOLASSES", "MAPLE SYRUP/SUGAR - MOLASSES",
                                      ifelse(items == "GRAM_CARROT_FLOUR", "GRAM/CARROT FLOUR", items 
                                      )
                                      )
                               )
                ) %>%
  dplyr::arrange(items)

### gender-county
inferential_gender_county_prop_differences_items <- sapply(
  sort(unique(df_loyalty_analysis_class_name$gender[!is.na(df_loyalty_analysis_class_name$gender)])), function(x){
    nn <- x
    
    items <- names(df_loyalty_analysis_class_name)[!names(df_loyalty_analysis_class_name) %in% 
                                                     c("row_number", "gender", "age_group", "county_name", "year",
                                                       "canned fruits", "canned spices", "canned vegetables",
                                                       "maple syrup", "sugar - molasses", "carrot flour", "gram flour")]
    
    df_new <- df_loyalty_analysis_class_name %>%
      dplyr::filter(gender == nn) %>%
      dplyr::select(any_of(c(items)), county_name)
    
    out <- sapply(items, function(y){
      table_out <- base::table(df_new[[y]], df_new[["county_name"]],
                               useNA = c("no")
                               )
      
      chi_square_out <- rstatix::chisq_test(table_out)$p
      
    }, simplify = TRUE
    )
    
    out_ <- tibble::tibble( gender = nn,
                            items = names(out),
                            pvalue = out
                            ) %>%
      dplyr::mutate(pvalue = ifelse(pvalue < 0.001, "<0.001",
                                    ifelse(pvalue > 0.999, ">0.999", round(pvalue,3)
                                    )
                                    )
                    )
    
  }, simplify = FALSE
)

df_inferential_gender_county_prop_differences_items <- do.call("rbind", inferential_gender_county_prop_differences_items) %>%
  tibble::remove_rownames() %>%
  tidyr::pivot_wider(names_from = gender,
                     names_glue = "{gender}_{.value}",
                     names_vary = "slowest",
                     values_from = c(pvalue)
                     ) %>%
  dplyr::mutate(items = stringr::str_to_upper(items),
                items = ifelse(items == "CANNED_VEGETABLES_FRUITS_SPICES", "CANNED VEGETABLES/FRUITS/SPICES",
                               ifelse(items == "MAPLESYRUP_SUGARMOLASSES", "MAPLE SYRUP/SUGAR - MOLASSES",
                                      ifelse(items == "GRAM_CARROT_FLOUR", "GRAM/CARROT FLOUR", items 
                                      )
                               )
                               )
                ) %>%
  dplyr::arrange(items)


### county-gender
inferential_county_gender_prop_differences_items <- sapply(
  sort(unique(df_loyalty_analysis_class_name$county_name[!is.na(df_loyalty_analysis_class_name$county_name)])), function(x){
    nn <- x
    
    items <- names(df_loyalty_analysis_class_name)[!names(df_loyalty_analysis_class_name) %in% 
                                                     c("row_number", "gender", "age_group", "county_name", "year",
                                                       "canned fruits", "canned spices", "canned vegetables",
                                                       "maple syrup", "sugar - molasses", "carrot flour", "gram flour")]
    
    df_new <- df_loyalty_analysis_class_name %>%
      dplyr::filter(county_name == nn) %>%
      dplyr::select(any_of(c(items)), gender)
    
    out <- sapply(items, function(y){
      table_out <- base::table(df_new[[y]], df_new[["gender"]],
                               useNA = c("no")
                               )
      
      chi_square_out <- rstatix::chisq_test(table_out)$p
      
    }, simplify = TRUE
    )
    
    out_ <- tibble::tibble( county_name = nn,
                            items = names(out),
                            pvalue = out
                            ) %>%
      dplyr::mutate(pvalue = ifelse(pvalue < 0.001, "<0.001",
                                    ifelse(pvalue > 0.999, ">0.999", round(pvalue,3)
                                    )
                                    )
                    )
    
  }, simplify = FALSE
)

df_inferential_county_gender_prop_differences_items <- do.call("rbind", inferential_county_gender_prop_differences_items) %>%
  tibble::remove_rownames() %>%
  tidyr::pivot_wider(names_from = county_name,
                     names_glue = "{county_name}_{.value}",
                     names_vary = "slowest",
                     values_from = c(pvalue)
                     ) %>%
  dplyr::mutate(items = stringr::str_to_upper(items),
                items = ifelse(items == "CANNED_VEGETABLES_FRUITS_SPICES", "CANNED VEGETABLES/FRUITS/SPICES",
                               ifelse(items == "MAPLESYRUP_SUGARMOLASSES", "MAPLE SYRUP/SUGAR - MOLASSES",
                                      ifelse(items == "GRAM_CARROT_FLOUR", "GRAM/CARROT FLOUR", items 
                                      )
                                      )
                               )
                ) %>%
  dplyr::arrange(items)

