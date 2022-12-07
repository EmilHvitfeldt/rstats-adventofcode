library(tidyverse)

input <- readLines("2022/07-input")

disk <- list()
i <- 2
path <- c(NULL)

while (i <= length(input)) {
  if (input[i] == "$ ls") {
    repeat {
      i <- i + 1
      if (i > length(input)) break
      if (str_detect(input[i], "^\\$")) break
      if (str_detect(input[i], "^dir")) {
        dir_name <- str_remove(input[i], "dir ")
        disk[[c(path, dir_name)]] <- list()
      } else {
        file_name <- str_remove(input[i], "\\d+ ")
        file_value <- str_extract(input[i], "\\d+") |> as.numeric()
        disk[[c(path, file_name)]] <- file_value
      }
    }
  } else if (str_detect(input[i], "^\\$ cd \\.\\.")) {
    path <- path[-length(path)]
    i <- i + 1
  } else if (str_detect(input[i], "^\\$ cd")) {
    next_dir <- str_remove(input[i], "^\\$ cd ")
    path <- c(path, next_dir)
    i <- i + 1
  }
}

dir_sums <- list()

lists <- map_lgl(disk, is.list)
unused_folders <- as.list(names(disk)[lists])

repeat {
  if (length(unused_folders) == 0) break
  path <- unused_folders[[1]]
  lists <- map_lgl(disk[[path]], is.list)
  if (any(lists)) {
    new_lists <- names(lists)[lists]
    for (new_list in new_lists) {
      unused_folders <- c(list(c(path, new_list)), unused_folders)
    }
  } else {
    dir_sum <- sum(unlist(disk[[path]]))
    dir_sums[paste(path, collapse = "_")] <- dir_sum
    disk[[path]] <- dir_sum
    unused_folders[[1]] <- NULL
  }
}

dir_sums$total <- sum(unlist(disk))
dir_sums <- unlist(dir_sums)

dir_sums[dir_sums >= (30000000 - (70000000 - dir_sums["total"]))] |>
  sort() |>
  head(1)
