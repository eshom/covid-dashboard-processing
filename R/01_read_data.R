## These vectors are used to validate the excel files are formatted correctly
## There are several formats.
## Never mix English and Hebrew on the same line of code
g_dashboard_v1_skip_lines <- 1
g_dashboard_v1_columns <- c("קבוצת גיל",
                            "מין",
                            "מספר מאומתים",
                            "מספר חולים קשה וקריטי",
                            "מספר מונשמים",
                            "מספר נפטרים")
g_dashboard_v1_age_groups <- c("0-9", "10-19", "20-29", "30-39", "40-49",
                               "50-59", "60-69", "70-79", "80-89", "90+")

g_dashboard_v2_skip_lines <- 1
g_dashboard_v2_columns <- c("תקופה",
                            "קבוצת גיל",
                            "מין",
                            "מספר מאומתים",
                            "אחוז מאומתים",
                            "מספר חולים קשה וקריטי",
                            "אחוז חולים קשה וקריטי",
                            "מספר מונשמים",
                            "אחוז מונשמים",
                            "מספר נפטרים",
                            "אחוז נפטרים")
g_dashboard_v2_age_groups <- c("0-9", "10-19", "20-29", "30-39", "40-49",
                               "50-59", "60-69", "70-79", "80-89", "90+")

## This one has several sheets in it. Each sheet is one measure collected on
## the date specified in the sheet name
g_dashboard_collected_lines <- 1:12
g_dashboard_collected_columns <- c("Age", "Women", "Men", "Both")
g_dashboard_collected_filename <- "dashboard_collected.xlsx"
g_dashboard_collected_sheetnames <- c("Confirmed cases 9Dec2020",
                                      "Deaths 9Dec2020",
                                      "Confirmed cases 21Dec2020",
                                      "Deaths 21Dec2020",
                                      "Confirmed cases 28Dec2020",
                                      "Deaths 28Dec2020",
                                      "Confirmed cases 11Jan2021",
                                      "Deaths 11Jan2021",
                                      "Confirmed cases 17Feb2021",
                                      "Deaths 17Feb2021")

## Download dashboard files to the specified download_directory
## The `googledrive` package is used here and will open browser for authentication
## Returns `FALSE` if there's an error connecting or downloading from googledrive
download_dashboard_files <- function(download_directory = "download",
                                     overwrite = TRUE) {
        error_fun <- function(e) {
                message("Error in ", sys.call(1), ": ",
                        "Unable to connect to Google drive.\n",
                        "In addition:")
                message(e)
                "error"
        }

        try_result <- tryCatch(dashboard_files <- googledrive::drive_ls(googledrive::as_id("1fPjYTbHVe07t6-PowLK3jUMKXr610z-Z")),
                               error = error_fun)

        if (identical(try_result, "error"))
                return(FALSE)

        current_wd <- getwd() # for on.exit

        ## returns to the regular working directory even if function exists early
        on.exit(setwd(current_wd))


        if (!dir.exists(download_directory)) {
                message("Creating missing download directory.")
                dir.create(download_directory)
        }

        message("Changing working directory to: ", download_directory)

        setwd(download_directory) # download files here

        try_result <- tryCatch(lapply(dashboard_files$id, googledrive::drive_download,
                                      overwrite = overwrite),
                               error = error_fun)

        if (identical(try_result, "error"))
                return(FALSE)

        lapply(dashboard_files$id, googledrive::drive_download,
               overwrite = overwrite)

        message("Changing working directory back to: ", current_wd)
        TRUE
}

read_dashboard_files <- function(files_directory = "download") {
        file_names <- list.files(files_directory)
        files <- file.path(files_directory, file_names)

        ## read all sheets of the collected excel file
        collected <- lapply(g_dashboard_collected_sheetnames,
                            openxlsx::read.xlsx,
                            xlsxFile = files[which(file_names == g_dashboard_collected_filename)],
                            rows = g_dashboard_collected_lines)

        collected <- setNames(collected, g_dashboard_collected_sheetnames)

        ## Remove the collected file from the list because it was already read
        collected_index <- which(file_names == g_dashboard_collected_filename)
        files <- files[-collected_index]
        file_names <- file_names[-collected_index]

        read_smart <- function(f) {
                if (try_dashboard_version(f, g_dashboard_v1_skip_lines,
                                          g_dashboard_v1_columns,
                                          g_dashboard_v1_age_groups)) {
                        v1 <- openxlsx::read.xlsx(f, startRow = 1 + g_dashboard_v1_skip_lines)
                        return(transform(v1, version = "v1"))
                }

                if (try_dashboard_version(f, g_dashboard_v2_skip_lines,
                                          g_dashboard_v2_columns,
                                          g_dashboard_v2_age_groups)) {
                        v2 <- openxlsx::read.xlsx(f, startRow = 1 + g_dashboard_v2_skip_lines)
                        return(transform(v2, version = "v2"))
                }

                ## If not match, return NULL and remove NULL later from the list
                message("In `", sys.call(-2), "`: File not in valid format.")
                NULL
        }

        dashboard <- lapply(files, read_smart)
        dashboard <- setNames(dashboard, file_names)

        null_elements <- sapply(dashboard, is.null)

        if (any(null_elements)) {
                message("Invalid files:")
                message(paste0(file_names[null_elements], "\n"))

                dashboard <- dashboard[!null_elements]
        }

        list(collected = collected, dashboard = dashboard)
}

## Checks validity of columns and age groups. Returns `TRUE` if valid.
## Otherwise `FALSE`
try_dashboard_version <- function(file, skip, cols, age_groups) {
        dat <- openxlsx::read.xlsx(file, startRow = 1 + skip)

        ## `read.xlsx()` replaces white spaces in column names with `.` by default
        cols <- gsub("\\s", ".", cols)

        ## Validates columns
        cols_match <- identical(colnames(dat), cols)

        age_match <- FALSE
        ## Validates age groups, but only if columns match
        if (cols_match) {
                age_match <- identical(sort(unique(dat[["קבוצת.גיל"]])),
                                       sort(age_groups))
        }

        cols_match && age_match
}

##message("Downloading dashboard data files from Google Drive")
if (!download_dashboard_files())
        stop("Failed to download dashboard files")

## message("Saving data to 'data' directory")
if (!dir.exists("data")) {
        message("Creating missing 'data' directory")
        dir.create("data")
}

saveRDS(read_dashboard_files(), file.path("data", "dashboard_raw.rds"))
