input <- scan("2017/03-input")

get_sum <- function(x, values) {
  neightbor_locs <- purrr::pmap_chr(
    expand.grid(x = c(-1, 0, 1) + x[1], y = c(-1, 0, 1) + x[2]),
    function(x, y) paste0(x, "_", y)
  )
  sum(unlist(values[neightbor_locs]), na.rm = TRUE)
}

loc <- c(0, 0)

values <- list()

values[paste0(loc, collapse = "_")] <- 1

ring <- 1

# repeat
ring <- 0
repeat {
  if (value > input) break
  ring <- ring + 1

  # right (advance)
  loc <- loc + c(1, 0)

  value <- get_sum(loc, values)
  values[paste0(loc, collapse = "_")] <- value
  cat("loc:", loc, "value:", value, "\n")
  if (value > input) break

  # go up
  while (value <= input) {
    loc <- loc + c(0, 1)

    value <- get_sum(loc, values)
    values[paste0(loc, collapse = "_")] <- value
    cat("loc:", loc, "value:", value, "\n")
    if (identical(loc, c(ring, ring))) break
    if (value > input) break
  }

  # go left
  while (value <= input) {
    loc <- loc + c(-1, 0)

    value <- get_sum(loc, values)
    values[paste0(loc, collapse = "_")] <- value
    cat("loc:", loc, "value:", value, "\n")
    if (identical(loc, c(-ring, ring))) break
    if (value > input) break
  }

  # go down
  while (value <= input) {
    loc <- loc + c(0, -1)

    value <- get_sum(loc, values)
    values[paste0(loc, collapse = "_")] <- value
    cat("loc:", loc, "value:", value, "\n")
    if (identical(loc, c(-ring, -ring))) break
    if (value > input) break
  }

  # go right
  while (value <= input) {
    loc <- loc + c(1, 0)

    value <- get_sum(loc, values)
    values[paste0(loc, collapse = "_")] <- value
    cat("loc:", loc, "value:", value, "\n")
    if (identical(loc, c(ring, -ring))) break
    if (value > input) break
  }
}

value
