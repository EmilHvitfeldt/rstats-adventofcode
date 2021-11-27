input <- readLines("2016/12-input")

reg <- c(a = 0, b = 0, c = 0, d = 0)

index <- 1
input_len <- length(input)


get_value <- function(values, reg) {
  check_val <- values[2]
  if (check_val %in% c("a", "b", "c", "d")) {
    check_val <- reg[values[2]]
  } else {
    check_val <- as.integer(check_val)
  }
  check_val
}

repeat {
  command <- substr(input[index], 1, 3)
  values <- strsplit(input[index], " ")[[1]]

  if (command == "cpy") {
    reg[values[3]] <- get_value(values, reg)
    index <- index + 1
  } else if (command == "inc") {
    reg[values[2]] <- reg[values[2]] + 1
    index <- index + 1
  } else if (command == "dec") {
    reg[values[2]] <- reg[values[2]] - 1
    index <- index + 1
  } else if (command == "jnz") {

    check_val <- get_value(values, reg)
    if (check_val != 0) {
      index <- index + as.integer(values[3])
    } else {
      index <- index + 1
    }
  }
  if (index > input_len) break
  reg
}

reg[["a"]]
