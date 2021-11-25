library(stringr)
library(purrr)

input <- readLines("2020/21-input")

allergies <- str_extract(input, "\\(.*")
allergies <- str_remove(allergies, "\\(contains ")
allergies <- str_remove(allergies, "\\)")
allergies <- str_split(allergies, ", ")

ingredient <- str_extract(input, "[ a-z]+")
ingredient <- str_extract_all(ingredient, "[a-z]+")

all_allergies <- unique(unlist(allergies))
all_ingredient <- character(length(all_allergies))

repeat {
  res <- length(unlist(ingredient))

  for (i in seq_along(all_allergies)) {
    x <- all_allergies[i]

    x_foods <- ingredient[map_lgl(allergies, ~any(.x %in% x))]

    candidates <- reduce(x_foods, intersect)
    if (length(candidates) != 1) next
    all_ingredient[i] <- candidates
    ingredient <- map(ingredient, setdiff, all_ingredient)
  }

  new_res <- length(unlist(ingredient))

  if (res == new_res) break
  res <- new_res
}

length(unlist(ingredient))
