input <- readLines("2015/12_input")

library(stringr)

check_red <- function(x) {
  if (length(x) > 1) return(FALSE)

  if (is.list(x)) return(FALSE)

  x == "red"
}

parse_red <- function(input) {
  repeat {
    end <- str_locate(input, "\\}")[1, 1]

    if (all(is.na(end))) break

    starts <- str_locate_all(input, "\\{")[[1]][, 1]
    start <- max(starts[starts < end])

    extraction <- str_sub(input, start, end)

    json <- jsonlite::parse_json(extraction)

    if (!any(purrr::map_lgl(json, check_red))) {
      res <- str_extract_all(extraction, "-{0,1}[0-9]+")[[1]]
      res <- sum(as.numeric(res), na.rm = TRUE)
    } else {
      res <- 0
    }

    str_sub(input, start, end) <- res
  }

  res <- str_extract_all(input, "-{0,1}[0-9]+")[[1]]
  res <- sum(as.numeric(res), na.rm = TRUE)
  res
}

parse_red(input)
