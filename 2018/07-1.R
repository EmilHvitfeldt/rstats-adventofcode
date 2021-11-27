library(stringr)

input <- readLines("2018/07-input")

from <- str_sub(input, 6, 6)
to <- str_sub(input, 37, 37)

letters <- c(from, to) |> unique() |> sort()

graph <- purrr::map(letters, ~character(0)) |>
  setNames(letters)

for (i in seq_along(from)) {
  graph[[to[i]]] <- c(graph[[to[i]]], from[i])
}

res <- character()

repeat {
  selection <- names(which.min(lengths(graph)))[1]

  res <- c(res, selection)

  graph <- graph[names(graph) != selection]

  if (length(graph) == 0) break

  graph <- purrr::map(graph, setdiff, selection)
}

paste0(res, collapse = "")
