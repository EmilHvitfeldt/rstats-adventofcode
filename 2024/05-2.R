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

uncorrect <- updates[!which_passes]

fix_order <- function(rule, update) {
  matches <- match(rule, update)
  before <- matches[1]
  after <- matches[2]
  
  before_inds <- setdiff(seq(1, before), after)
  
  after_inds <- setdiff(seq_along(update), c(before_inds, after))
  
  new_order <- c(before_inds, after, after_inds)
  update[new_order]
}

fix_update <- function(update, rules) {
  while (!pass_rules(update, rules)) {
    for (rule in rules) {
      if (pass_rule(rule, update)) {
        next
      }
      update <- fix_order(rule, update)
    }
  }
  update
}

uncorrect |>
  lapply(fix_update, rules) |>
  lapply(get_middle) |>
  unlist() |>
  sum()
