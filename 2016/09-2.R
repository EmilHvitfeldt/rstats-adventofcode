library(stringr)

input <- readLines("2016/09-input")

decompress_length <- function(input) {

  output <- numeric()

  repeat {
    output <- c(output, nchar(str_extract(input, "^[A-Z]*")))
    input <- str_remove(input, "^[A-Z]*")

    if (str_count(input) == 0) break

    reps <- str_extract(input, "\\(.+?\\)") %>%
      str_extract_all("[0-9]+") %>%
      .[[1]] %>%
      as.numeric()

    section <- str_remove(input, "\\(.+?\\)") %>%
      str_sub(1, reps[1])

    if (str_detect(section, "\\(.+?\\)")) {
      len <- decompress_length(section)
    } else {
      len <- nchar(section)
    }
    output <- c(output, len * reps[2])

    input <- str_remove(input, "\\(.+?\\)")
    input <- str_remove(input, paste0(".{", reps[1], "}", collapse = ""))

    if (str_count(input) == 0) break

  }

  sum(output)
}

decompress_length(input)
