## Make sure working directory is the project root, and not "/tests"
dat <- read.csv(file.path("data", "deaths.csv"))

library(ggplot2)
ggplot(dat, aes(x = as.Date(Date, "%d.%m.%Y"), y = Value, colour = Sex)) +
        facet_wrap(~ Age, scales = "free") + geom_line()
