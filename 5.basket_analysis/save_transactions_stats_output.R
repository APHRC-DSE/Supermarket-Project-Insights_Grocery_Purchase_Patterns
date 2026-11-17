library(dplyr)
library(writexl)

working_directory

## Saving transactions descriptives output

writexl::write_xlsx(list(items_per_transaction = items_per_transaction_prop
                         ),
                    path = base::file.path(output_Dir, "transactions_descriptives.xlsx" )
                    )




