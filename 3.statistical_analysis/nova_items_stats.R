library(dplyr)
library(tidyr)
library(rstatix)
library(tibble)

working_directory

##############################################################################################
## proportions - nova

### overall, gender, age-group, county, year
descriptive_prop_differences_nova <- sapply(c("customer_type", "gender", "age_group", "county_name", "year"), function(x){
  nn <- x
  out <- df_loyalty_analysis %>%
    tidyr::drop_na(any_of(nn), nova) %>%
    dplyr::mutate(nova = as.factor(nova)) %>%
    dplyr::select(all_of(nn), nova) %>%
    dplyr::add_count(.data[[nn]], name = "total") %>%
    dplyr::group_by(.data[[nn]], nova, total) %>%
    dplyr::summarise(n = n(), .groups = "drop") %>%
    dplyr::mutate(prop = round((n/total)*100,3)) %>%
    dplyr::rename(group = nn)
  
}, simplify = FALSE
)

df_descriptive_prop_differences_nova <- do.call("rbind", descriptive_prop_differences_nova) %>%
  tibble::remove_rownames() %>%
  tidyr::pivot_wider(names_from = group,
                     names_glue = "{group}_{.value}",
                     names_vary = "slowest",
                     values_from = c(prop, n, total),
                     values_fill = 0
                     )

### age-gender
descriptive_age_gender_prop_differences_nova <- sapply(
  sort(unique(df_loyalty_analysis$age_group[!is.na(df_loyalty_analysis$age_group)])), function(x){
    nn <- x
    out <- df_loyalty_analysis %>%
      tidyr::drop_na(age_group, gender, nova) %>%
      dplyr::filter(age_group == nn) %>%
      dplyr::mutate(nova = as.factor(nova)) %>%
      dplyr::select(age_group, gender, nova) %>%
      dplyr::add_count(gender, name = "total") %>%
      dplyr::group_by(age_group, gender, nova, total) %>%
      dplyr::summarise(n = n(), .groups = "drop") %>%
      dplyr::mutate(prop = round((n/total)*100,3))
  
}, simplify = FALSE
)

df_descriptive_age_gender_prop_differences_nova <- do.call("rbind", descriptive_age_gender_prop_differences_nova) %>%
  tibble::remove_rownames() %>%
  tidyr::pivot_wider(names_from = c(age_group, gender),
                     names_glue = "{age_group}_{gender}_{.value}",
                     names_vary = "slowest",
                     values_from = c(prop, n, total),
                     values_fill = 0
                     )

### gender-age
descriptive_gender_age_prop_differences_nova <- sapply(
  sort(unique(df_loyalty_analysis$gender[!is.na(df_loyalty_analysis$gender)])), function(x){
    nn <- x
    out <- df_loyalty_analysis %>%
      tidyr::drop_na(age_group, gender, nova) %>%
      dplyr::filter(gender == nn) %>%
      dplyr::mutate(nova = as.factor(nova)) %>%
      dplyr::select(age_group, gender, nova) %>%
      dplyr::add_count(age_group, name = "total") %>%
      dplyr::group_by(age_group, gender, nova, total) %>%
      dplyr::summarise(n = n(), .groups = "drop") %>%
      dplyr::mutate(prop = round((n/total)*100,3))
    
  }, simplify = FALSE
)

df_descriptive_gender_age_prop_differences_nova <- do.call("rbind", descriptive_gender_age_prop_differences_nova) %>%
  tibble::remove_rownames() %>%
  tidyr::pivot_wider(names_from = c(gender, age_group),
                     names_glue = "{gender}_{age_group}_{.value}",
                     names_vary = "slowest",
                     values_from = c(prop, n, total),
                     values_fill = 0
                     )

### gender-county
descriptive_gender_county_prop_differences_nova <- sapply(
  sort(unique(df_loyalty_analysis$gender[!is.na(df_loyalty_analysis$gender)])), function(x){
    nn <- x
    out <- df_loyalty_analysis %>%
      tidyr::drop_na(county_name, gender, nova) %>%
      dplyr::filter(gender == nn) %>%
      dplyr::mutate(nova = as.factor(nova)) %>%
      dplyr::select(county_name, gender, nova) %>%
      dplyr::add_count(county_name, name = "total") %>%
      dplyr::group_by(county_name, gender, nova, total) %>%
      dplyr::summarise(n = n(), .groups = "drop") %>%
      dplyr::mutate(prop = round((n/total)*100,3))
    
  }, simplify = FALSE
)

df_descriptive_gender_county_prop_differences_nova <- do.call("rbind", descriptive_gender_county_prop_differences_nova) %>%
  tibble::remove_rownames() %>%
  tidyr::pivot_wider(names_from = c(gender, county_name),
                     names_glue = "{gender}_{county_name}_{.value}",
                     names_vary = "slowest",
                     values_from = c(prop, n, total),
                     values_fill = 0
                     )

### county-gender
descriptive_county_gender_prop_differences_nova <- sapply(
  sort(unique(df_loyalty_analysis$county_name[!is.na(df_loyalty_analysis$county_name)])), function(x){
    nn <- x
    out <- df_loyalty_analysis %>%
      tidyr::drop_na(county_name, gender, nova) %>%
      dplyr::filter(county_name == nn) %>%
      dplyr::mutate(nova = as.factor(nova)) %>%
      dplyr::select(county_name, gender, nova) %>%
      dplyr::add_count(gender, name = "total") %>%
      dplyr::group_by(county_name, gender, nova, total) %>%
      dplyr::summarise(n = n(), .groups = "drop") %>%
      dplyr::mutate(prop = round((n/total)*100,3))
    
  }, simplify = FALSE
)

df_descriptive_county_gender_prop_differences_nova <- do.call("rbind", descriptive_county_gender_prop_differences_nova) %>%
  tibble::remove_rownames() %>%
  tidyr::pivot_wider(names_from = c(county_name, gender),
                     names_glue = "{county_name}_{gender}_{.value}",
                     names_vary = "slowest",
                     values_from = c(prop, n, total),
                     values_fill = 0
                     )

##############################################################################################
## group differences proportions - nova

### gender, age-group, county, year
inferential_prop_differences_nova <- sapply(c("gender", "age_group", "county_name", "year"), function(x){
  nn <- x
  
  nova <- names(df_loyalty_analysis_nova)[!names(df_loyalty_analysis_nova) %in% 
                                                   c("row_number", "gender", "age_group", "county_name", "year")]
  
  df_new <- df_loyalty_analysis_nova %>%
    dplyr::select(any_of(c(nn, nova)))
  
  out <- sapply(nova, function(y){
    table_out <- base::table(df_new[[y]], df_new[[nn]],
                             useNA = c("no")
                             )
    
    chi_square_out <- rstatix::chisq_test(table_out)$p
    
  }, simplify = TRUE
  )
  
  out_ <- tibble::tibble( group = nn,
                          nova = names(out),
                          pvalue = out
                          ) %>%
    dplyr::mutate(pvalue = ifelse(pvalue < 0.001, "<0.001",
                                  ifelse(pvalue > 0.999, ">0.999", round(pvalue,3)
                                         )
                                  )
                  )
  
}, simplify = FALSE
)

df_inferential_prop_differences_nova <- do.call("rbind", inferential_prop_differences_nova) %>%
  tibble::remove_rownames() %>%
  tidyr::pivot_wider(names_from = group,
                     names_glue = "{group}_{.value}",
                     names_vary = "slowest",
                     values_from = c(pvalue)
                     ) %>%
  dplyr::arrange(nova)

### age-gender
inferential_age_gender_prop_differences_nova <- sapply(
  sort(unique(df_loyalty_analysis_nova$age_group[!is.na(df_loyalty_analysis_nova$age_group)])), function(x){
    nn <- x
    
    nova <- names(df_loyalty_analysis_nova)[!names(df_loyalty_analysis_nova) %in% 
                                              c("row_number", "gender", "age_group", "county_name", "year")]
    
    df_new <- df_loyalty_analysis_nova %>%
      dplyr::filter(age_group == nn) %>%
      dplyr::select(any_of(c(nova)), gender)
    
    out <- sapply(nova, function(y){
      table_out <- base::table(df_new[[y]], df_new[["gender"]],
                               useNA = c("no")
                               )
      
      chi_square_out <- rstatix::chisq_test(table_out)$p
      
    }, simplify = TRUE
    )
    
    out_ <- tibble::tibble( age_group = nn,
                            nova = names(out),
                            pvalue = out
                            ) %>%
      dplyr::mutate(pvalue = ifelse(pvalue < 0.001, "<0.001",
                                    ifelse(pvalue > 0.999, ">0.999", round(pvalue,3)
                                    )
                                    )
                    )
    
  }, simplify = FALSE
)

df_inferential_age_gender_prop_differences_nova <- do.call("rbind", inferential_age_gender_prop_differences_nova) %>%
  tibble::remove_rownames() %>%
  tidyr::pivot_wider(names_from = age_group,
                     names_glue = "{age_group}_{.value}",
                     names_vary = "slowest",
                     values_from = c(pvalue)
                     ) %>%
  dplyr::arrange(nova)

### gender-age
inferential_gender_age_prop_differences_nova <- sapply(
  sort(unique(df_loyalty_analysis_nova$gender[!is.na(df_loyalty_analysis_nova$gender)])), function(x){
    nn <- x
    
    nova <- names(df_loyalty_analysis_nova)[!names(df_loyalty_analysis_nova) %in% 
                                              c("row_number", "gender", "age_group", "county_name", "year")]
    
    df_new <- df_loyalty_analysis_nova %>%
      dplyr::filter(gender == nn) %>%
      dplyr::select(any_of(c(nova)), age_group)
    
    out <- sapply(nova, function(y){
      table_out <- base::table(df_new[[y]], df_new[["age_group"]],
                               useNA = c("no")
                               )
      
      chi_square_out <- rstatix::chisq_test(table_out)$p
      
    }, simplify = TRUE
    )
    
    out_ <- tibble::tibble( gender = nn,
                            nova = names(out),
                            pvalue = out
                            ) %>%
      dplyr::mutate(pvalue = ifelse(pvalue < 0.001, "<0.001",
                                    ifelse(pvalue > 0.999, ">0.999", round(pvalue,3)
                                    )
                                    )
                    )
    
  }, simplify = FALSE
)

df_inferential_gender_age_prop_differences_nova <- do.call("rbind", inferential_gender_age_prop_differences_nova) %>%
  tibble::remove_rownames() %>%
  tidyr::pivot_wider(names_from = gender,
                     names_glue = "{gender}_{.value}",
                     names_vary = "slowest",
                     values_from = c(pvalue)
                     ) %>%
  dplyr::arrange(nova)

### gender-county
inferential_gender_county_prop_differences_nova <- sapply(
  sort(unique(df_loyalty_analysis_nova$gender[!is.na(df_loyalty_analysis_nova$gender)])), function(x){
    nn <- x
    
    nova <- names(df_loyalty_analysis_nova)[!names(df_loyalty_analysis_nova) %in% 
                                              c("row_number", "gender", "age_group", "county_name", "year")]
    
    df_new <- df_loyalty_analysis_nova %>%
      dplyr::filter(gender == nn) %>%
      dplyr::select(any_of(c(nova)), county_name)
    
    out <- sapply(nova, function(y){
      table_out <- base::table(df_new[[y]], df_new[["county_name"]],
                               useNA = c("no")
      )
      
      chi_square_out <- rstatix::chisq_test(table_out)$p
      
    }, simplify = TRUE
    )
    
    out_ <- tibble::tibble( gender = nn,
                            nova = names(out),
                            pvalue = out
    ) %>%
      dplyr::mutate(pvalue = ifelse(pvalue < 0.001, "<0.001",
                                    ifelse(pvalue > 0.999, ">0.999", round(pvalue,3)
                                    )
      )
      )
    
  }, simplify = FALSE
)

df_inferential_gender_county_prop_differences_nova <- do.call("rbind", inferential_gender_county_prop_differences_nova) %>%
  tibble::remove_rownames() %>%
  tidyr::pivot_wider(names_from = gender,
                     names_glue = "{gender}_{.value}",
                     names_vary = "slowest",
                     values_from = c(pvalue)
                     ) %>%
  dplyr::arrange(nova)


### county-gender
inferential_county_gender_prop_differences_nova <- sapply(
  sort(unique(df_loyalty_analysis_nova$county_name[!is.na(df_loyalty_analysis_nova$county_name)])), function(x){
    nn <- x
    
    nova <- names(df_loyalty_analysis_nova)[!names(df_loyalty_analysis_nova) %in% 
                                              c("row_number", "gender", "age_group", "county_name", "year")]
    
    df_new <- df_loyalty_analysis_nova %>%
      dplyr::filter(county_name == nn) %>%
      dplyr::select(any_of(c(nova)), gender)
    
    out <- sapply(nova, function(y){
      table_out <- base::table(df_new[[y]], df_new[["gender"]],
                               useNA = c("no")
                               )
      
      chi_square_out <- rstatix::chisq_test(table_out)$p
      
    }, simplify = TRUE
    )
    
    out_ <- tibble::tibble( county_name = nn,
                            nova = names(out),
                            pvalue = out
                            ) %>%
      dplyr::mutate(pvalue = ifelse(pvalue < 0.001, "<0.001",
                                    ifelse(pvalue > 0.999, ">0.999", round(pvalue,3)
                                    )
                                    )
                    )
    
  }, simplify = FALSE
)

df_inferential_county_gender_prop_differences_nova <- do.call("rbind", inferential_county_gender_prop_differences_nova) %>%
  tibble::remove_rownames() %>%
  tidyr::pivot_wider(names_from = county_name,
                     names_glue = "{county_name}_{.value}",
                     names_vary = "slowest",
                     values_from = c(pvalue)
  ) %>%
  dplyr::arrange(nova)


