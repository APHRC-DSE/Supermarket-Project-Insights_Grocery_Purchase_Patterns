library(dplyr)
library(tidyr)
library(tibble)
library(arules)
#library(arulesViz)

working_directory

## Market basket analysis - County

county_classname_rules <- sapply(
  unique(df_loyalty_basket_classname$county_name[!is.na(df_loyalty_basket_classname$county_name)]), function(x){
    nn <- x
    data <- df_loyalty_basket_classname %>%
      dplyr::select(-c(customer_type, age_group, gender, year)) %>%
      tidyr::drop_na(county_name) %>%
      dplyr::filter(county_name == nn) %>%
      dplyr::select(-c(county_name)) %>%
      tibble::column_to_rownames(var="transaction_id")
    
    matrix_data <- as.matrix(data)
    
    ### convert data frame to transaction class
    transactions <- arules::transactions(matrix_data)
    ### Get the rules
    rules <- arules::apriori(transactions,
                             parameter = list(support = support_classname_county,
                                              confidence = confidence_classname_county,
                                              minlen = 2,
                                              target = "rules"
                                              ),
                             control = list(verbose=FALSE, load = FALSE
                                            )
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
    df$level <- nn
    df$pos_confidence <- seq(nrow(df))
    df$group <- "county_name"
    
    return(df)
    
  } , simplify = FALSE)

df_county_classname_rules <- do.call("rbind", county_classname_rules) %>%
  dplyr::mutate(across(where(is.numeric), ~round(.x, 5)))

