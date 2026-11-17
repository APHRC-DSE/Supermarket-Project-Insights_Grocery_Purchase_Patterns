library(dplyr)
library(tidyr)


working_directory

## Transactions description

items_per_transaction_prop <- df_loyalty_analysis %>%
  dplyr::select(transaction_id, class_name) %>%
  dplyr::group_by(transaction_id) %>%
  dplyr::summarise(n_items_transactions = length(class_name), .groups = 'drop') %>%
  dplyr::add_count(name = "total_transactions") %>%
  dplyr::group_by(n_items_transactions, total_transactions) %>%
  dplyr::summarise(transactions = length(transaction_id), .groups = 'drop') %>%
  dplyr::mutate(prop = round((transactions/total_transactions)*100,3))



