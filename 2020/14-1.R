library(stringr)

input <- readLines("2020/14-input")

intTo36 <- function(x) {
  as.character(c(rep(0, 4), rev(as.integer(intToBits(x)))))
}

`[<-.memory` <- function(x, i, value) {
  res <- intTo36(value)
  res[mask != "X"] <- mask[mask != "X"]
  x[[i]] <- res
  x
}

mem <- list()
attr(mem, "class") <- "memory"

for (line in input) {
  if (grepl("^mask", line)) {
    mask <- strsplit(str_extract(line, "[X0-9]+"), "")[[1]]
    next
  }
  eval(parse(text = line))
}

options(scipen = 999)
sum(map_dbl(mem, ~ sum(as.numeric(.x) * (2 ^ seq(35, 0)))))
