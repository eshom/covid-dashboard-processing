read_dashboard_data <- function(dashboard_file = file.path("data", "dashboard_raw.rds")) {
        message("Checking if the dashboard data file is available")

        ## This file should be generated at the end of the previous script
        if (!file.exists(dashboard_file))
                stop("Dashboard data file cannot be found in: ", dashboard_file)

        message("Reading from: ", dashboard_file)
        readRDS(dashboard_file)
}

## Calculate `AgeInt`. The `age` parameter is the `Age` column.
calc_ageint <- function(age, max_age = 105L) {
        age <- ifelse(age %in% c("UNK", "TOT"), NA, age)

        ## Throw a useful error in case age groups are not numeric.
        ## R by default only gives a warning, but does not stop execution.
        age <- tryCatch(as.integer(age),
                        warning = function(e) {
                                stop("Problem coercing input to integer. ",
                                     "Are some age groups not numbers?")

        })

        ## Count `NA` and add them later after calculation
        n_na <- sum(is.na(age))

        age <- na.omit(age)
        out <- c(age[-1], max_age) - age

        c(out, rep(NA, n_na))
}

## Calculate total counts for a subset of the data.
## Must not have duplicate age groups
calc_total <- function(data_subset) {
        stopifnot(!any(duplicated(data_subset$Age)))

        ## If there's already a total row or data frame empty, modify nothing
        if ("TOT" %in% data_subset$Age | nrow(data_subset) == 0)
                return(data_subset)

        last_row <- data_subset[nrow(data_subset), ]

        last_row$Age <- "TOT"
        last_row$Value <- sum(data_subset$Value)
        ## The rest of the columns should be constant

        rbind(data_subset, last_row)
}

dashboard <- read_dashboard_data(file.path("data", "dashboard_cleaned.rds"))

## Add TOT rows where missing
message("Calculating TOT rows where needed")
split_factors <- with(dashboard, list(Date, Sex))
splt <- split(dashboard, split_factors)
splt <- lapply(splt, calc_total)
dashboard <- do.call(rbind, splt)

## Add non-constant columns
message("Adding variable columns")
dashboard <- transform(dashboard,
                       AgeInt = ave(Age, Sex, Date, FUN = calc_ageint),
                       Code = paste0("IL", Date))

## Add constant columns
message("Adding constant columns")
dashboard <- transform(dashboard, Country = "Israel", Region = "All",
                       Metric = "Count")

## Finally sort rows and columns:
message("Sorting rows and columns")
dashboard <- with(dashboard, dashboard[order(as.Date(Date, "%d.%m.%Y"), Sex, Age),
                                       c("Country", "Region", "Code", "Date",
                                         "Sex", "Age", "AgeInt", "Metric",
                                         "Measure", "Value")])

## Save ready to upload/merge deaths data
write.csv(dashboard, file.path("data", "deaths.csv") ,row.names = FALSE)
