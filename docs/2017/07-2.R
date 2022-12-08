input <- readLines("2017/07-input")

library(stringr)

loc <- str_subset(input, "->")[1] |>
  str_extract("[a-z]+")

repeat {

  next_id <- str_detect(input, paste0(".+", loc))

  if (all(!next_id)) break

  loc <- str_extract(input[next_id], "^[a-z]+")
}

get_children <- function(x, input) {
  str_subset(input, paste0("^", x)) |>
    str_remove(".*> ") |>
    str_split_1(", ")
}

get_weight <- function(x) {
  value <- str_subset(input, paste0("^", x))

  res <- str_extract(value, "[0-9]+") |> as.integer()
  if (str_detect(value, ">")) {
    res <- res + sum(purrr::map_int(get_children(x, input), get_weight))
  }
  res
}

bot <- loc

error <- function(x) max(x) - min(x)
wrong_amount  <- error(purrr::map_int(get_children(bot, input), get_weight))

repeat {

children <- get_children(bot, input)

weights <- purrr::map_int(children, get_weight)

if (length(table(weights)) == 1) break

bot <- children[names(table(weights)[table(weights) == 1]) == weights]

}

wrong_weight <- str_subset(input, paste(children, collapse = ", ")) |>
  str_extract("[0-9]+") |>
  as.integer()

wrong_weight - wrong_amount
