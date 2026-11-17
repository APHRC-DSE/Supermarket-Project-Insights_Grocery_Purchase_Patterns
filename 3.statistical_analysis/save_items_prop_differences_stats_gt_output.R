library(dplyr)
library(flextable)
library(officer)

working_directory

## Saving descriptive and inferential output

### item proportion differences

flextable::save_as_docx(descriptive_inferential_prop_differences_items_gt_merge, 
                        path = base::file.path(output_Dir, "descriptive_inferential_prop_differences_items_gt.docx"),
                        align = "center", #left, center (default) or right.
                        pr_section = officer::prop_section(
                          page_size = officer::page_size(orient = "landscape"), #Use NULL (default value) for no content.
                          page_margins = officer::page_mar(), #Use NULL (default value) for no content.
                          type = "nextPage", # "continuous", "evenPage", "oddPage", "nextColumn", "nextPage"
                          section_columns = NULL, #Use NULL (default value) for no content.
                          header_default = NULL, #Use NULL (default value) for no content.
                          header_even = NULL, #Use NULL (default value) for no content.
                          header_first = NULL, #Use NULL (default value) for no content.
                          footer_default = NULL, #Use NULL (default value) for no content.
                          footer_even = NULL, #Use NULL (default value) for no content.
                          footer_first = NULL #Use NULL (default value) for no content.
                          )
                        )

flextable::save_as_docx(values = inferential_strata_gender_prop_differences_items_gt, 
                        path = base::file.path(output_Dir, "inferential_strata_gender_prop_differences_items.docx"),
                        align = "center", #left, center (default) or right.
                        pr_section = officer::prop_section(
                          page_size = officer::page_size(orient = "landscape"), #Use NULL (default value) for no content.
                          page_margins = officer::page_mar(), #Use NULL (default value) for no content.
                          type = "nextPage", # "continuous", "evenPage", "oddPage", "nextColumn", "nextPage"
                          )
                        )

flextable::save_as_docx(values = inferential_strata_age_prop_differences_items_gt, 
                        path = base::file.path(output_Dir, "inferential_strata_age_prop_differences_items.docx"),
                        align = "center", #left, center (default) or right.
                        pr_section = officer::prop_section(
                          page_size = officer::page_size(orient = "landscape"), #Use NULL (default value) for no content.
                          page_margins = officer::page_mar(), #Use NULL (default value) for no content.
                          type = "nextPage", # "continuous", "evenPage", "oddPage", "nextColumn", "nextPage"
                          )
                        )

flextable::save_as_docx(values = inferential_strata_county_prop_differences_items_gt, 
                        path = base::file.path(output_Dir, "inferential_strata_county_prop_differences_items.docx"),
                        align = "center", #left, center (default) or right.
                        pr_section = officer::prop_section(
                          page_size = officer::page_size(orient = "landscape"), #Use NULL (default value) for no content.
                          page_margins = officer::page_mar(), #Use NULL (default value) for no content.
                          type = "nextPage", # "continuous", "evenPage", "oddPage", "nextColumn", "nextPage"
                          )
                        )

