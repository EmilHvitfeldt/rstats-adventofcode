input <- readLines("2017/08-input")

highest <- 0

inc <- function(x, value) {
  name <- deparse(substitute(x))
  .GlobalEnv[[name]] <- .GlobalEnv[[name]] + value
  .GlobalEnv[["highest"]] <- max(.GlobalEnv[["highest"]], .GlobalEnv[[name]])
}

dec <- function(x, value) {
  name <- deparse(substitute(x))
  .GlobalEnv[[name]] <- .GlobalEnv[[name]] - value
  .GlobalEnv[["highest"]] <- max(.GlobalEnv[["highest"]], .GlobalEnv[[name]])
}

library(stringr)

registers <- str_extract_all(input, "[a-z]+") |>
  unlist() |>
  unique() |>
  setdiff(c("dec", "inc", "if"))

materialize <- function(name) {
  .GlobalEnv[[name]] <- 0
}

purrr::walk(registers, materialize)

reorganize <- function(x) {
  glue::glue("if ({x[5]} {x[6]} {x[7]}) {x[2]}({x[1]}, {x[3]})")
}

str_split(input, " ") |>
  purrr::map_chr(reorganize) |>
  parse(text = _) |>
  eval()

highest
