library(dplyr)
library(tidyr)
library(tibble)
library(arules)
#library(arulesViz)

working_directory

## Market basket analysis - Year_age

year_age_classname_rules <- sapply(
  unique(df_loyalty_basket_classname$year[!is.na(df_loyalty_basket_classname$year)]), function(x){
    nn <- x
    age_group <- sort(unique(df_loyalty_basket_classname$age_group[!is.na(df_loyalty_basket_classname$age_group)]))
    out_ <- sapply(age_group, function(y){
      data <- df_loyalty_basket_classname %>%
        dplyr::select(-c(customer_type, gender, county_name)) %>%
        tidyr::drop_na(year, age_group) %>%
        dplyr::filter(year == nn, age_group == y) %>%
        dplyr::select(-c(year, age_group)) %>%
        tibble::column_to_rownames(var="transaction_id")
      
      matrix_data <- as.matrix(data)
      
      ### convert data frame to transaction class
      transactions <- arules::transactions(matrix_data)
      ### Get the rules
      rules <- arules::apriori(transactions,
                               parameter = list(support = support_classname_year_age, 
                                                confidence = confidence_classname_year_age,
                                                minlen = 2,
                                                target = "rules"
                                                ),
                               control = list(verbose=FALSE, load = FALSE)
                               )
      
      out <-sort(rules, by="confidence", decreasing=TRUE)
      df <- data.frame( number_transactions = summary(out)@info$ntransactions,
                        set_support = summary(out)@info$support,
                        set_confidence = summary(out)@info$confidence,
                        number_rules = length(out),
                        items_rules = nitems(out),
                        density_lhs = summary(lhs(out))@density,
                        density_rhs = summary(rhs(out))@density,
                        frequent_items_lhs = as.character(list(summary(lhs(out))@itemSummary)),
                        size_lhs = size(lhs(out)),
                        frequent_size_lhs = as.character(list(summary(lhs(out))@lengths)),
                        summary_size_lhs = as.character(list(summary(lhs(out))@lengthSummary)),
                        frequent_items_rhs = as.character(list(summary(rhs(out))@itemSummary)),
                        size_rhs = size(rhs(out)),
                        frequent_size_rhs = as.character(list(summary(rhs(out))@lengths)),
                        summary_size_rhs = as.character(list(summary(rhs(out))@lengthSummary)),
                        lhs = labels(lhs(out)),
                        rhs = labels(rhs(out)),
                        out@quality
      )
      df$year <- nn
      df$level <- y
      df$group <- "age_group"
      df$pos_confidence <- seq(nrow(df))
      return(df)
      
    } , simplify = FALSE)
    
    out_ <- do.call("rbind", out_)
    
  } , simplify = FALSE)

df_year_age_classname_rules <- do.call("rbind", year_age_classname_rules) %>%
  dplyr::mutate(across(where(is.numeric), ~round(.x, 5)))

