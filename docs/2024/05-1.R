input <- readLines("2024/05-input")

sep <- which(input == "")

rules <- input[seq(1, sep - 1)] |>
  strsplit("\\|") |>
  lapply(as.integer)
updates <- input[seq(sep + 1, length(input))] |>
  strsplit(",") |>
  lapply(as.integer)

pass_rule <- function(rule, update) {
  order <- diff(match(rule, update))

  is.na(order) || order > 0
}

pass_rules <- function(update, rules) {
  all(vapply(rules, pass_rule, logical(1), update = update))
}

which_passes <- vapply(updates, pass_rules, logical(1), rules)

get_middle <- function(x) {
  mid <- length(x) / 2 + 1
  x[mid]
}

updates[which_passes] |>
  lapply(get_middle) |>
  unlist() |>
  sum()
