## Setting working directory and output folder

working_directory

mainDir <- base::getwd()
subDir_data <- "data"
subDir_output <- "Output"
subDir_output_plots <- "Output_Plots"
subDir_output_prop_plots <- "Output_Prop_Plots"

data_Dir <- base::file.path(mainDir, subDir_data)
output_Dir <- base::file.path(mainDir, subDir_output)
output_plots_Dir <- base::file.path(mainDir, subDir_output_plots)
output_prop_plots_Dir <- base::file.path(mainDir, subDir_output_prop_plots)

### create data folder
base::ifelse(!base::dir.exists(data_Dir), base::dir.create(data_Dir), "Sub Directory exists")

### create output folders
base::ifelse(!base::dir.exists(output_Dir), base::dir.create(output_Dir), "Sub Directory exists")
base::ifelse(!base::dir.exists(output_plots_Dir), base::dir.create(output_plots_Dir), "Sub Directory exists")
base::ifelse(!base::dir.exists(output_prop_plots_Dir), base::dir.create(output_prop_plots_Dir), "Sub Directory exists")

## Install required packages

### Install CRAN packages
required_packages <- c("tidyverse", "readxl", "writexl", "janitor", "knitr", "kableExtra", "officer", "rstatix",
                       "lubridate", "gtsummary", "flextable", "labelled", "sjlabelled", "tibble", "scales",
                       "ggpubr", "rlang", "arules", "arulesViz", "haven", "DiagrammeR", "DiagrammeRsvg", "rsvg")

installed_packages <- required_packages %in% base::rownames(utils::installed.packages())

if (base::any(installed_packages==FALSE)) {
  utils::install.packages(required_packages[!installed_packages], repos = "http://cran.us.r-project.org"
                          )
}

### load libraries
base::invisible(base::lapply(required_packages, library, character.only=TRUE))

