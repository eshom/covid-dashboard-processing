## Required packages:
## install.packages(c("stringr", "reshape2", "dplyr", "googledrive",
##                    "openxlsx"))

scripts <- list.files("R", full.names = TRUE)

## Run them
invisible(lapply(scripts, source))
