library(stringr)
input <- readLines("2018/04-input") |> sort()

times <- str_extract(input, "\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}") |>
  as.POSIXct()

inst <- str_remove(input, "\\[.*\\] ")

guard_id <- str_extract(inst, "\\d+")

shift <- cumsum(!is.na(guard_id))

for (i in seq_along(guard_id)) {
  if (i == 1) next

  if (is.na(guard_id[i])) {
    guard_id[i] <- guard_id[i - 1]
  }
}

library(dplyr)

sleep_calc <- function(x) {
  rowSums(outer(0:59, lubridate::minute(x)[-1], ">=")) %% 2
}

sleep_sched <- tibble(shift, guard_id, inst, times) %>%
  group_by(guard_id, shift) %>%
  summarise(
    minute = 0:59,
    slept = sleep_calc(times), .groups = "drop")

sleep_sched %>%
  group_by(guard_id) %>%
  summarise(sum = sum(slept)) %>%
  arrange(desc(sum)) %>%
  slice(1) %>%
  left_join(sleep_sched, by = "guard_id") %>%
  filter(slept == 1) %>%
  count(guard_id, minute, sort = TRUE) %>%
  slice(1) %>%
  mutate(res = as.integer(guard_id) * minute) %>%
  pull(res)
