library(stringr)
library(magrittr)
library(bit64)

input <- readLines("2015/23-input")

computer <- function(a, b, input) {
  loc <- 1
  reg <- list(a = as.integer64(a), b = as.integer64(b))
  repeat {
    if (loc > length(input)) break
    ins <- stringr::str_sub(input[loc], 1, 3)

    if (ins == "jio") {
      parts <- str_remove(input[loc], ".{4}") %>%
        str_split(", \\+{0,1}") %>%
        .[[1]]

      if (reg[[parts[1]]] == 1) {
        loc <- loc + as.numeric(parts[2])
      } else {
        loc <- loc + 1
      }

    } else if (ins == "inc") {
      regis_id <- str_remove(input[loc], ".{4}")
      reg[[regis_id]] <-reg[[regis_id]] + 1
      loc <- loc + 1
    } else if (ins == "tpl") {

      regis_id <- str_remove(input[loc], ".{4}")
      reg[[regis_id]] <- reg[[regis_id]] * 3
      loc <- loc + 1
    } else if (ins == "jmp") {
      offset <- str_remove(input[loc], "jmp \\+{0,1}")
      loc <- loc + as.numeric(offset)
    } else if (ins == "jie") {
      parts <- str_remove(input[loc], ".{4}") %>%
        str_split(", \\+{0,1}") %>%
        .[[1]]

      if((reg[[parts[1]]] %% 2) == 0) {
        loc <- loc + as.numeric(parts[2])
      } else {
        loc <- loc + 1
      }
    } else if (ins == "hlf") {
      regis_id <- str_remove(input[loc], ".{4}")
      reg[[regis_id]] <- reg[[regis_id]] / 2
      loc <- loc + 1
    }
  }

  reg$b
}

computer(1, 0, input)
