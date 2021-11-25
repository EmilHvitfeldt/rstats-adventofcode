input <- readLines("2020/12-input")

ship <- c(0, 0)
direction <- c(1, 0)

rotate <- function(x, phi) {
  c(x[1] * cos(phi * pi / 180) - x[2] * sin(phi * pi / 180),
    x[1] * sin(phi * pi / 180) + x[2] * cos(phi * pi / 180))
}

for (i in input) {
  value <- as.numeric(str_extract(i, "[0-9]+"))
  switch(
    substr(i, 1, 1),
    N = {ship[2] <- ship[2] + value},
    S = {ship[2] <- ship[2] - value},
    E = {ship[1] <- ship[1] + value},
    W = {ship[1] <- ship[1] - value},
    L = {direction <- rotate(direction, value)},
    R = { direction <- rotate(direction, -value)},
    F = {ship <- ship + direction * value}
  )
}

sum(abs(ship))
