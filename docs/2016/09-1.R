library(stringr)
library(magrittr)

input <- readLines("2016/09-input")

decompress <- function(input) {
  output <- character()

  repeat {
    output <- c(output, str_extract(input, "^[A-Z]*"))
    input <- str_remove(input, "^[A-Z]*")

    if (str_count(input) == 0) break

    reps <- str_extract(input, "\\(.+?\\)") %>%
      str_extract_all("[0-9]+") %>%
      .[[1]] %>%
      as.numeric()
    input <- str_remove(input, "\\(.+?\\)")
    output <- c(output, str_sub(input, 1, reps[1]) %>% rep(reps[2]))
    input <- str_remove(input, paste0(".{", reps[1], "}", collapse = ""))

    if (str_count(input) == 0) break

  }

  paste0(output, collapse = "")
}

nchar(decompress(input))
