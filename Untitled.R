library(logrittr)
library(dplyr)
logrittr_options(big_mark = ",",
                 wrap_width = NULL,
                 max_cols = Inf)

iris %>=%
  as_tibble() %>=%
  filter(Sepal.Length < 5)  %>=%
  mutate(rn = row_number()) %>=%
  semi_join(
    iris %>% as_tibble() %>=%
      filter(Species == "setosa"),
    by = "Species"
  )  %>=%
  group_by(Species) %>=%
  summarise(n = n_distinct(rn))


nycflights13::flights %>=%
  as_tibble() %>=%
  group_by(year, month, day) %>=%
  count() %>=%
  tidyr::pivot_wider(values_from = "n", names_from = "day") %>=%
  glimpse()

library(lumberjack)
data(women)
women$id <- 1:15
out <- women %L>%
  start_log(logger = cellwise$new(key="id")) %L>%
  transform(height = height*0.0254 ) %L>%
  transform(weight = weight*0.453592) %L>%
  transform(bmi = weight/height^2) %L>%
  dump_log()
log <- read.csv("cellwise.csv")


l <- logrittr::logrittr_logger$new(verbose = TRUE)
logfile <- tempfile(fileext=".-r.log.csv")
iris %L>%
  start_log(log = l, label = "iris step") %L>%
  as_tibble() %L>%
  filter(Sepal.Length < 5) %L>%
  mutate(rn = row_number()) %L>%
  group_by(Species) %L>%
  summarise(n = n_distinct(rn)) %L>%
  dump_log(file=logfile, stop = FALSE)
