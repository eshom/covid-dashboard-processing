read_dashboard_data <- function(dashboard_file = file.path("data", "dashboard_raw.rds")) {
        message("Checking if the dashboard data file is available")

        ## This file should be generated at the end of the previous script
        if (!file.exists(dashboard_file))
                stop("Dashboard data file cannot be found in: ", dashboard_file)

        message("Reading from: ", dashboard_file)
        readRDS(dashboard_file)
}

## Check list names. `TRUE` if names are correct. `FALSE` otherwise.
validate_data_names <- function(dat, list_names = c("collected", "dashboard")) {
        identical(names(dat), list_names)
}

## From the input list, get and bind death count data
collected_get_deaths <- function(dat, collected_elem = "collected") {
        ## Extract death data. Dates are in the list element names.
        dat <- dat[[collected_elem]]
        dat_names <- names(dat)
        dat_names_deaths <- dat_names[grepl("^Deaths", dat_names)]
        dat_deaths <- dat[dat_names_deaths]
        dates <- stringr::str_extract(dat_names_deaths, "\\d.+\\w.+\\d.+")
        dates <- as.Date(dates, "%d%b%Y")

        ## Add the dates as column to the data
        out <- Map(function(d, n) transform(d, date = n), dat_deaths, dates)

        ## `Sex` needs to be a single column
        out <- lapply(out, reshape2::melt, id.vars = c("Age", "date"),
                      variable.name = "sex")

        out <- do.call(rbind, unname(out))

        transform(out, Measure = "Deaths")
}

extract_age_group_pattern <- function(age) {
        stringr::str_extract(age, "(\\d\\d?|Total)")
}

## Clean columns so they conform to the database format
collected_clean_cols <- function(dat) {
        out <- transform(dat,
                         Age = {
                                 x <- extract_age_group_pattern(Age)
                                 ifelse(x == "Total", "TOT", x)
                         },
                         Date = format(date, "%d.%m.%Y"),
                         Sex = dplyr::case_when(sex == "Women" ~ "f",
                                                sex == "Men" ~ "m",
                                                sex == "Both" ~ "b"),
                         Value = value)

        out[-which(colnames(out) %in% c("date", "sex", "value"))]
}

## Get and clean death counts from the raw dashboard files
dashboard_v1_get_deaths <- function(dat, dashboard_elem = "dashboard") {
        dat <- dat[[dashboard_elem]]

        ## subset to work with version 1 of the data only
        dat <- dat[sapply(dat, function(x) all(x$version == "v1"))]

        ## Select relevant columns
        dat <- lapply(dat, subset, select = c(
                                           "קבוצת.גיל",
                                           "מין",
                                           "מספר.נפטרים"
                                   ))

        ## Immediately translate to English
        dat <- lapply(dat, setNames, c("Age", "Sex", "Value"))

        ## Translate sex values from Hebrew to English
        dat <- lapply(dat, transform, Sex = dplyr::case_when(
                                                             Sex == "גברים" ~ "m",
                                                             Sex == "נשים" ~ "f"
                                                     ))

        dat <- lapply(dat, transform, Age = extract_age_group_pattern(Age))

        ## Add `Date` and `Measure` columns as well
        dat <- Map(transform, dat, Measure = "Deaths",
                      Date = stringr::str_extract(names(dat), "\\d.+\\.\\d+\\.\\d+"))

        do.call(rbind, unname(dat))

}

dashboard_v2_get_deaths <- function(dat, dashboard_elem = "dashboard") {
        dat <- dat[[dashboard_elem]]

        ## subset to work with version 2 of the data only
        dat <- dat[sapply(dat, function(x) all(x$version == "v2"))]

        ## Select relevant columns
        dat <- lapply(dat, subset,
                      `תקופה` ==
                      "מתחילת קורונה",
                      select = c(
                              "קבוצת.גיל",
                              "מין",
                              "מספר.נפטרים"
                      ))

        ## Immediately translate to English
        dat <- lapply(dat, setNames, c("Age", "Sex", "Value"))

        ## Translate sex values from Hebrew to English
        dat <- lapply(dat, transform, Sex = dplyr::case_when(
                                                             Sex == "גברים" ~ "m",
                                                             Sex == "נשים" ~ "f"
                                                     ))

        dat <- lapply(dat, transform, Age = extract_age_group_pattern(Age))

        ## Add `Date` and `Measure` columns as well
        dat <- Map(transform, dat, Measure = "Deaths",
                      Date = stringr::str_extract(names(dat), "\\d.+\\.\\d+\\.\\d+"))

        do.call(rbind, unname(dat))

}

dashboard <- read_dashboard_data()

if (!validate_data_names(dashboard))
        stop("List names need to be fixed so the rest of the script works")

message("Binding all sheets from the collected dataset")
collected <- collected_get_deaths(dashboard)
collected <- collected_clean_cols(collected)

dashboard_v1 <- dashboard_v1_get_deaths(dashboard)
dashboard_v2 <- dashboard_v2_get_deaths(dashboard)

message("Binding all version of the dashboard data")
dashboard_all <- rbind(dashboard_v1, dashboard_v2)

message("Merging collected and downloaded dashboard data")
output <- merge(collected, dashboard_all, all = TRUE)

saveRDS(output, file.path("data", "dashboard_cleaned.rds"))
