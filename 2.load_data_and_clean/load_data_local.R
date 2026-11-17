library(dplyr)
library(googledrive)
library(janitor)
library(readxl)

working_directory

## Load Rdata from subdirectory data

invisible(
  lapply(X=list.files(path = data_Dir, pattern = "*.RData", full.names = T),load,.GlobalEnv)
  )


