input <- readLines("2020/12-input")

ship <- c(0, 0)

waypoint <- c(10, 1)

rotate <- function(x, phi) {
  c(x[1] * cos(phi * pi / 180) - x[2] * sin(phi * pi / 180),
    x[1] * sin(phi * pi / 180) + x[2] * cos(phi * pi / 180))
}

for (i in input) {
  value <- as.numeric(str_extract(i, "[0-9]+"))

  switch(
    substr(i, 1, 1),
    N = {waypoint[2] <- waypoint[2] + value},
    S = {waypoint[2] <- waypoint[2] - value},
    E = {waypoint[1] <- waypoint[1] + value},
    W = {waypoint[1] <- waypoint[1] - value},
    L = {waypoint <- rotate(waypoint, value)},
    R = {waypoint <- rotate(waypoint, -value)},
    F = {ship <- ship + waypoint * value}
  )
}

sum(abs(ship))
