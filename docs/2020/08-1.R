input <- readLines("2020/08-input")

run <- function(input, part2 = FALSE) {
  len <- length(input)
  n_times <- integer(len)

  accumulator <- 0

  i <- 1

  repeat {
    x <- strsplit(input[i], " ")[[1]]

    n_times[i] <- n_times[i] + 1

    if (i > len) {
      return(accumulator)
    }
    if (n_times[i] == 2) {
      if (part2) {
        return(NA)
      } else {
        return(accumulator)
      }
    }

    if (x[1] == "nop") {
      i <- i + 1
    } else if (x[1] == "acc") {
      accumulator <- accumulator + readr::parse_number(x[2])
      i <- i + 1
    } else if (x[1] == "jmp") {
      i <- i + readr::parse_number(x[2])
    }
  }
}

run(input)
