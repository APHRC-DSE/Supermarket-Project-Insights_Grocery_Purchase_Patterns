library(dplyr)
library(tidyr)
library(tibble)
library(arules)
#library(arulesViz)

working_directory

## Market basket analysis - Overall Age

age_classname_rules <- sapply(
  sort(unique(df_loyalty_basket_classname$age_group[!is.na(df_loyalty_basket_classname$age_group)])), function(x){
    nn <- x
    data <- df_loyalty_basket_classname %>%
      dplyr::select(-c(customer_type, gender, county_name, year)) %>%
      tidyr::drop_na(age_group) %>%
      dplyr::filter(age_group == nn) %>%
      dplyr::select(-c(age_group)) %>%
      tibble::column_to_rownames(var="transaction_id")
    
    matrix_data <- as.matrix(data)
    
    ### convert data frame to transaction class
    transactions <- arules::transactions(matrix_data)
    ### Get the rules
    rules <- arules::apriori(transactions,
                             parameter = list(support = support_classname_age,
                                              confidence = confidence_classname_age,
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
    df$group <- "age_group"
    
    return(df)
    
  } , simplify = FALSE)

df_age_classname_rules <- do.call("rbind", age_classname_rules) %>%
  dplyr::mutate(across(where(is.numeric), ~round(.x, 5)))

