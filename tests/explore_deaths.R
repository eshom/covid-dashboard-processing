dat <- read.csv(file.path("data", "deaths.csv"))

splt <- split(dat, ~ Sex + Age)

with(splt$f.20, plot(as.Date(Date, "%d.%m.%Y"), Value))

splt_diff <- lapply(splt, transform, Value_diff = c(NA, diff(Value)))

negative_slope <- sapply(splt_diff, with, any(Value_diff < 0, na.rm = TRUE))

splt_negatives <- splt_diff[negative_slope]

splt_negatives_rows <- lapply(splt_negatives, subset, Value_diff < 0)

negative_rows <- do.call(rbind, splt_negatives_rows)

negative_dates <- unique(negative_rows$Date)


negative_rows
as.Date(negative_dates, "%d.%m.%Y") |> sort()

negative_rows[order(as.Date(negative_rows$Date, "%d.%m.%Y")), ] |>
        write.csv(file.path("tests", "negative_counts.csv"), row.names = FALSE)
