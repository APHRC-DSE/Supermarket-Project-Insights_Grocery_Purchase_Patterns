library(dplyr)
library(tidyr)
library(tibble)
library(arules)
#library(arulesViz)

working_directory

## Market basket analysis - Year_county

year_county_classname_rules <- sapply(
  unique(df_loyalty_basket_classname$year[!is.na(df_loyalty_basket_classname$year)]), function(x){
    nn <- x
    county_name <- unique(df_loyalty_basket_classname$county_name[!is.na(df_loyalty_basket_classname$county_name)])
    out_ <- sapply(county_name, function(y){
      data <- df_loyalty_basket_classname %>%
        dplyr::select(-c(customer_type, gender, age_group)) %>%
        tidyr::drop_na(year, county_name) %>%
        dplyr::filter(year == nn, county_name == y) %>%
        dplyr::select(-c(year, county_name)) %>%
        tibble::column_to_rownames(var="transaction_id")
      
      matrix_data <- as.matrix(data)
      
      ### convert data frame to transaction class
      transactions <- arules::transactions(matrix_data)
      ### Get the rules
      rules <- arules::apriori(transactions,
                               parameter = list(supp = support_classname_year_county, 
                                                conf = confidence_classname_year_county,
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
      df$group <- "county_name"
      df$pos_confidence <- seq(nrow(df))
      return(df)
      
    } , simplify = FALSE)
    
    out_ <- do.call("rbind", out_)
    
  } , simplify = FALSE)

df_year_county_classname_rules <- do.call("rbind", year_county_classname_rules) %>%
  dplyr::mutate(across(where(is.numeric), ~round(.x, 5)))

