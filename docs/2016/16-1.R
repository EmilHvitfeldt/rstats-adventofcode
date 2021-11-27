input <- "10111100110001111"

a <- c("1" = T, "0" = F)[strsplit(input, "")[[1]]]

len <- 272

while (length(a) < len) {
  a <- c(a, FALSE, !rev(a))
}
a <- a[seq_len(len)]

is_even <- function(x) (x %% 2) == 0
is_odd <- function(x) (x %% 2) == 1

repeat {
  if (is_odd(length(a))) {
    break
  }

  a <- purrr::map_lgl(
    split(a, rep(seq_len(length(a)/2), each = 2)),
    ~ .x[1] == .x[2]
  )
}

paste0(as.integer(a), collapse = "")
