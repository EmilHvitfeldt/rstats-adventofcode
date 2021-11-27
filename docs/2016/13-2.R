code <- function(x, y) x*x + 3*x + 2*x*y + y + y*y

number <- 1352
room <- function(x, y, number) {
  sum(intToBits(code(x, y) + number) == 1) %% 2
}

size <- 75
maze <- matrix(nrow = size, ncol = size)
maze[2, 2] <- 0 # 0-index

for (x in seq_len(size-1)) {
  for (y in seq_len(size-1)) {
    if (room(x-1, y-1, number) == 1) { # 0-index
      maze[x, y] <- Inf
    }
  }
}

around <- function(x, y) {
  list(
    list(x = x + 0, y = y + 1),
    list(x = x + 1, y = y + 0),
    list(x = x + 0, y = y - 1),
    list(x = x - 1, y = y + 0)
  )
}

validate <- function(l, m) {

  valid <- purrr::map_lgl(l, ~.x$x <= (size-1) && .x$y <= (size-1) && .x$x >= 1 && .x$y >= 1 && is.na(m[.x$x, .x$y]))

  l <- l[valid]
  l
}

append <- function(x, y) {
  c(y, x)
}

find_lowest <- function(place, maze) {
  neighbors <- around(place$x, place$y)
  values <- purrr::map_dbl(neighbors, ~{
    if (.x$x == 0 || .x$y == 0 || .x$y == size || .x$y == size) return(Inf)
    maze[.x$x, .x$y]
  }
  )
  min(values, na.rm = TRUE)
}

places <- list()

places <- around(2, 2) |> # 0-index
  validate(maze) |>
  append(places)

while (length(places) > 0) {
  lowest <- find_lowest(places[[1]], maze)
  maze[places[[1]]$x, places[[1]]$y] <- lowest + 1

  places <- around(places[[1]]$x, places[[1]]$y) |>
    validate(maze) |>
    append(places)

  places[[1]] <- NULL
}

sum(maze <= 50 & is.finite(maze))
