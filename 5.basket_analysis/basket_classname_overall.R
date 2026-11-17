library(dplyr)
library(tidyr)
library(tibble)
library(arules)
#library(arulesViz)

working_directory

## Market basket analysis - Overall
overall_classname_rules <- sapply(
  unique(df_loyalty_basket_classname$customer_type[!is.na(df_loyalty_basket_classname$customer_type)]), function(x){
    nn <- x
    data <- df_loyalty_basket_classname %>%
      dplyr::select(-c(gender, age_group, county_name, year)) %>%
      tidyr::drop_na(customer_type) %>%
      dplyr::filter(customer_type == nn) %>%
      dplyr::select(-c(customer_type)) %>%
      tibble::column_to_rownames(var="transaction_id")
    
    matrix_data <- as.matrix(data)
    
    ### convert data frame to transaction class
    transactions <- arules::transactions(matrix_data)
    ### Get the rules
    rules <- arules::apriori(transactions,
                             parameter = list(support = support_classname_overall, 
                                              conf = confidence_classname_overall,
                                              minlen = 2,
                                              target = "rules"
                                              ),
                             control = list(verbose=FALSE, load = FALSE
                                            )
                             )
    
    out <-sort(rules, by="confidence", decreasing=TRUE)
    
    other_measures <- arules::interestMeasure(out) %>%
      dplyr::select(-c(support, confidence, lift, count, coverage) #included in quality dataframe
                    )
    
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
                      out@quality,
                      other_measures
                      )
    
    df$level <- nn
    df$pos_confidence <- seq(nrow(df))
    df$group <- "customer_type"
    
    return(df)
    
  } , simplify = FALSE)

df_overall_classname_rules <- do.call("rbind", overall_classname_rules) %>%
  dplyr::mutate(across(where(is.numeric), ~round(.x, 5)))

