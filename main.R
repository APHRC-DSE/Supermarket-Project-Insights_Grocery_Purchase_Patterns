######################################################################
### Restart R
#.rs.restartR()

### Start with a clean environment by removing objects in workspace
rm(list=ls())

### Setting work directory
working_directory <- base::setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#working_directory <- base::setwd(".")

### Load Rdata
Rdata_files <- list.files(path = working_directory, pattern = "*.RData", full.names = T) #No subdirectories

if ( length(Rdata_files) >0) {
  invisible(lapply(Rdata_files,load,.GlobalEnv))
} else {
  paste(c(".RData files", "do not exist"), collapse = " ")
}

### Install required packages
source("./1.setup/requirements.R")

### helper/customized functions
source("./1.setup/helperfuns_1.R")
source("./1.setup/helperfuns_2.R")
source("./1.setup/helperfuns_plots_3.R")
source("./1.setup/helperfuns_plots_4.R")

######################################################################

### process flowchart
source("./2.load_data_and_clean/flowchart.R")

#df_clean_supermarket_a <- readRDS("df_raw_a.rds")

#df_clean_a <- readRDS(base::file.path(output_Dir, "df_clean_a.rds" ))

#df_loyalty <- readRDS(base::file.path(output_Dir, "df_loyalty.rds"))

#df_loyalty_analysis <- readRDS(base::file.path(output_Dir, "df_loyalty_analysis.rds"))

#df_loyalty_analysis_nova <- readRDS(base::file.path(output_Dir, "df_loyalty_analysis_nova.rds"))

#df_loyalty_analysis_class_name <- readRDS(base::file.path(output_Dir, "df_loyalty_analysis_class_name.rds"))

#df_loyalty_basket_classname <- readRDS(base::file.path(output_Dir, "df_loyalty_basket_classname.rds"))

### Load recode file
source("./2.load_data_and_clean/load_recode_file.R")

######################################################################
## Loading and Cleaning Data

### Load supermarketA Rdata file
source("./2.load_data_and_clean/load_data_local.R")

### Data cleaning
source("./2.load_data_and_clean/cleaning.R")

source("./2.load_data_and_clean/sample_supermarket_A.R")

######################################################################
#Create connection to Database
## Connecting to Supermarket local Database - connects to public schema

con <- DBI::dbConnect(drv = RPostgres::Postgres(),
                 dbname = 'supermarket', 
                 host = 'localhost', 
                 port = 5432, 
                 user = 'postgres',
                 #password = rstudioapi::askForPassword("Database password")
                 password = Sys.getenv("postgres_password")
                 
                 )

print(con)


## Inserting data to specific schema and table in database
DBI::dbWriteTable(con
                  , name = Id(schema = "raw_clean_a", table = "clean_merge_a")
                  , value = df_clean_a %>% dplyr::select(id:total_new, customer_type, year:age)
                  , overwrite = TRUE
                  , row.names = FALSE
                  )

DBI::dbWriteTable(con
                  , name = Id(schema = "raw_clean_a", table = "link_file_nova_nutrient_a")
                  , value = df_clean_a %>% 
                    dplyr::select(description, item_type:conversion) %>%
                    dplyr::distinct(description, .keep_all = TRUE)
                  , overwrite = TRUE
                  , row.names = FALSE
                  )

## Disconnect database 
DBI::dbDisconnect(con)

######################################################################

### Select variables for analysis
source("./2.load_data_and_clean/analysis_data.R")
source("./2.load_data_and_clean/analysis_data_wide_nova.R")
source("./2.load_data_and_clean/analysis_data_wide_classname.R")

### Save analysis clean data set - spss file
source("./2.load_data_and_clean/save_analysis_clean_data.R")

######################################################################
## Data Profiling - Loyalty customers

### Descriptive and Inferential stats
source("./3.statistical_analysis/customer_stats.R")
source("./3.statistical_analysis/nova_items_stats.R")
#source("./3.statistical_analysis/nova_items_stats_gt.R")
source("./3.statistical_analysis/items_prop_differences_stats.R")
#source("./3.statistical_analysis/items_prop_differences_stats_gt.R")

### Saving descriptive and inferential output
source("./3.statistical_analysis/save_customer_stats_output.R")
source("./3.statistical_analysis/save_nova_items_stats_output.R")
#source("./3.statistical_analysis/save_nova_items_stats_gt_output.R")
source("./3.statistical_analysis/save_items_prop_differences_stats_output.R")
#source("./3.statistical_analysis/save_items_prop_differences_stats_gt_output.R")

######################################################################
## Plots- Loyalty customers

### Descriptive plots - simple and stacked bar graphs
source("./4.descriptive_plots/customer_descriptive_plots.R")
source("./4.descriptive_plots/nova_items_descriptive_plots.R")

### Time series plots - proportion
source("./4.descriptive_plots/customer_month_trend_prop_plots.R")
source("./4.descriptive_plots/customer_quarter_trend_prop_plots.R")
source("./4.descriptive_plots/nova_month_trend_prop_plots.R")
source("./4.descriptive_plots/nova_quarter_trend_prop_plots.R")
source("./4.descriptive_plots/items_quantity_month_trend_prop_plots.R")
source("./4.descriptive_plots/items_quantity_quarter_trend_prop_plots.R")

### Age-Sex Time series plots - proportion
source("./4.descriptive_plots/customer_age_sex_month_trend_prop_plots.R")
source("./4.descriptive_plots/customer_age_sex_quarter_trend_prop_plots.R")
source("./4.descriptive_plots/nova_age_sex_month_trend_prop_plots.R")
source("./4.descriptive_plots/nova_age_sex_quarter_trend_prop_plots.R")
source("./4.descriptive_plots/items_quantity_count_age_sex_month_trend_prop_plots.R")
source("./4.descriptive_plots/items_quantity_count_age_sex_quarter_trend_prop_plots.R")

### Age-County Time series plots - proportion
source("./4.descriptive_plots/customer_age_county_month_trend_prop_plots.R")
source("./4.descriptive_plots/customer_age_county_quarter_trend_prop_plots.R")
source("./4.descriptive_plots/nova_age_county_month_trend_prop_plots.R")
source("./4.descriptive_plots/nova_age_county_quarter_trend_prop_plots.R")
source("./4.descriptive_plots/items_quantity_count_age_county_month_trend_prop_plots.R")
source("./4.descriptive_plots/items_quantity_count_age_county_quarter_trend_prop_plots.R")

### Sex-County Time series plots - proportion
source("./4.descriptive_plots/customer_sex_county_month_trend_prop_plots.R")
source("./4.descriptive_plots/customer_sex_county_quarter_trend_prop_plots.R")
source("./4.descriptive_plots/nova_sex_county_month_trend_prop_plots.R")
source("./4.descriptive_plots/nova_sex_county_quarter_trend_prop_plots.R")
source("./4.descriptive_plots/items_quantity_count_sex_county_month_trend_prop_plots.R")
source("./4.descriptive_plots/items_quantity_count_sex_county_quarter_trend_prop_plots.R")

######################################################################
## Market Basket Analysis - Loyalty customers

source("./5.basket_analysis/transactions_stats.R")
source("./5.basket_analysis/save_transactions_stats_output.R")

### Transform data to dummy
source("./5.basket_analysis/basket_analysis_data.R")

### Class name rules
source("./5.basket_analysis/basket_classname_thresholds.R")

source("./5.basket_analysis/basket_classname_overall.R")
source("./5.basket_analysis/basket_classname_overall_frequent_items.R")
source("./5.basket_analysis/basket_classname_gender.R")
source("./5.basket_analysis/basket_classname_age.R")
source("./5.basket_analysis/basket_classname_year.R")
source("./5.basket_analysis/basket_classname_county.R")

source("./5.basket_analysis/basket_classname_age_gender.R")
source("./5.basket_analysis/basket_classname_age_county.R")
source("./5.basket_analysis/basket_classname_gender_county.R")

source("./5.basket_analysis/basket_classname_year_gender.R")
source("./5.basket_analysis/basket_classname_year_age.R")
source("./5.basket_analysis/basket_classname_year_county.R")

### Saving class name Market Basket Analysis output
source("./5.basket_analysis/save_classname_basket_output.R")

### Visualize class name LHS Rules - Heat maps
source("./5.basket_analysis/lhs_rules_classname_heatmap.R")
source("./5.basket_analysis/lhs_rules_socio_by_classname_heatmap.R")
source("./5.basket_analysis/lhs_rules_year_by_classname_heatmap.R")

### Visualize class name RHS Rules - Heat maps
source("./5.basket_analysis/rhs_rules_classname_heatmap.R")
source("./5.basket_analysis/rhs_rules_socio_by_classname_heatmap.R")
source("./5.basket_analysis/rhs_rules_year_by_classname_heatmap.R")

######################################################################
## Save workspace at the end without working directory path

save(list = ls(all.names = TRUE)[ls(all.names = TRUE) %in% c("df_loyalty_analysis", "df_loyalty_analysis_class_name",
                                                             "df_loyalty_basket_classname", "df_loyalty_analysis_nova")],
     file = "df_loyalty_analysis.RData",
     envir = .GlobalEnv #parent.frame()
     )

save(list = ls(all.names = TRUE, pattern = "*_prop_differences_nova"),
     file = "nova_prop_differences_analysis.RData",
     envir = .GlobalEnv #parent.frame()
     )

save(list = ls(all.names = TRUE, pattern = "*_prop_differences_items"),
     file = "items_prop_differences_analysis.RData",
     envir = .GlobalEnv #parent.frame()
     )

save(list = ls(all.names = TRUE, pattern = "*_classname_rules"),
     file = "basket_classname_analysis.RData",
     envir = .GlobalEnv #parent.frame()
     )

#saveRDS(df_clean_supermarket_a, file = "df_raw_a.rds")
#saveRDS(df_clean_a, file = base::file.path(output_Dir, "df_clean_a.rds" )) 
#saveRDS(df_loyalty, file = base::file.path(output_Dir,"df_loyalty.rds"))
#saveRDS(df_loyalty_analysis, file = base::file.path(output_Dir, "df_loyalty_analysis.rds"))
#saveRDS(df_loyalty_analysis_nova, file = base::file.path(output_Dir, "df_loyalty_analysis_nova.rds"))
#saveRDS(df_loyalty_analysis_class_name, file = base::file.path(output_Dir, "df_loyalty_analysis_class_name.rds"))
#saveRDS(df_loyalty_basket_classname, file = base::file.path(output_Dir,"df_loyalty_basket_classname.rds"))

######################################################################

