library(stringr)
library(purrr)

input <- readLines("2016/10-input")

int <- as.integer
chr <- as.character

values <- str_subset(input, "^value")
input <- str_subset(input, "^value", negate = TRUE)

info <- list(bot = list(), output = list())

values <- str_match_all(values, "\\d+")

for (value in values) {
  info$bot[[value[2, ]]] <- c(info$bot[[value[2, ]]], int(value[1, ]))
}

num <- str_extract_all(input, "\\d+")
des <- str_extract_all(input, "(bot|output)")

inst <- list()

for (i in seq_along(input)) {
  inst[[num[[i]][1]]] <- list(
    low = list(des = des[[i]][2], value = num[[i]][2]),
    high = list(des = des[[i]][3], value = num[[i]][3])
  )
}

repeat {
  selection <- info$bot[lengths(info$bot) == 2]
  if (length(selection) == 0) break
  if (length(selection) > 1) {
    selection <- selection[1]
  }
  name <- names(selection)
  if (identical(sort(selection[[1]]), c(17L, 61L))) {
    cat(name)
  }

  info[[inst[[name]]$low$des]][[inst[[name]]$low$value]] <-
    c(info[[inst[[name]]$low$des]][[inst[[name]]$low$value]], min(selection[[1]]))

  info[[inst[[name]]$high$des]][[inst[[name]]$high$value]] <-
    c(info[[inst[[name]]$high$des]][[inst[[name]]$high$value]], max(selection[[1]]))

  info$bot[[name]] <- NULL
}
