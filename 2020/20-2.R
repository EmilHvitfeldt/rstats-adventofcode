library(tidyverse)

input <- read_file("2020/20-input")
input <- str_remove(input, "\n$")

tiles <- str_split(input, "\n\n")[[1]]

tile_info <- function(x) {
  lines <- str_split(x, "\n")[[1]]

  id <- str_extract(lines[1], "[0-9]+")
  grid <- lines[-1]

  sides <- c(
    paste0(str_sub(grid, 1, 1), collapse = ""),
    paste0(str_sub(grid, length(grid), length(grid)), collapse = ""),
    grid[c(1, length(grid))]
  )

  tibble(id, side = c(sides, stringi::stri_reverse(sides)))
}

tile_sides <- map_dfr(tiles, tile_info)

options(scipen = 999)
joints <- inner_join(tile_sides, tile_sides, by = "side") %>%
  filter(id.x != id.y) %>%
  transmute(pair = map2_chr(id.x, id.y, ~
                              paste(sort(c(.x, .y)), collapse = " ")
  )) %>%
  distinct() %>%
  separate(pair, c("a", "b"), " ")

current <- joints %>%
  pivot_longer(everything()) %>%
  count(value) %>%
  filter(n == 2) %>%
  slice(1) %>%
  pull(value)

middle <- joints %>%
  pivot_longer(everything()) %>%
  count(value) %>%
  filter(n == 4) %>%
  pull(value)

non_middle <- joints %>%
  filter(!a %in% middle, !b %in% middle)

places <- matrix(NA, 12, 12)

used <- c()
round <- c()

repeat {
  round <- c(round, current)

  next_tile <- non_middle %>%
    filter(a %in% current | b %in% current, !a %in% used, !b %in% used) %>%
    slice(1) %>%
    unlist() %>%
    setdiff(current)

  if(length(next_tile) == 0) break
  used <- c(used, current)
  current <- next_tile
}

places[1, 1:12] <- round[1:12]
places[2:12, 12] <- round[13:23]
places[12, 11:1] <- round[24:34]
places[11:2, 1] <- round[35:44]

for (i in 2:11) {
  for (j in 2:11) {
    partners <- c(places[i-1, j], places[i, j-1])

    places[i, j] <- intersect(
      joints %>%
        filter(a %in% partners[1] | b %in% partners[1]) %>%
        unlist() %>%
        setdiff(partners[1]),
      joints %>%
        filter(a %in% partners[2] | b %in% partners[2]) %>%
        unlist() %>%
        setdiff(partners[1])
    ) %>%
      setdiff(places[i - 1, j - 1])
  }
}

extract_full_grid <- function(x) {
  str_split(x, "\n")[[1]][-1] %>%
    str_split("") %>%
    unlist() %>%
    matrix(nrow = 10, byrow = TRUE)
}

full_tiles <- map(tiles, extract_full_grid)
names(full_tiles) <- str_extract(tiles, "[0-9]+")

rotate <- function(x) t(apply(x, 2, rev))

all_tiles <- map(1:12, ~map(1:12, ~list()))

all_tiles[[1]][[1]] <- full_tiles[[places[1, 1]]][, 10:1]

all_symmmetries <- list(
  function(x) x,
  function(x) rotate(x),
  function(x) rotate(rotate(x)),
  function(x) rotate(rotate(rotate(x))),
  function(x) x[, 10:1],
  function(x) rotate(x[, 10:1]),
  function(x) rotate(rotate(x[, 10:1])),
  function(x) rotate(rotate(rotate(x[, 10:1])))
)

for (i in 2:12) {
  tile <- full_tiles[[places[1, i]]]

  tile_symmetries <- map(all_symmmetries, ~.x(tile))

  tile_which <- map_lgl(tile_symmetries, ~ all(all_tiles[[1]][[i - 1]][, 10] == .x[, 1]))

  all_tiles[[1]][[i]] <- tile_symmetries[[which(tile_which)]]
}

for (j in 2:12) {
  for (i in 1:12) {
    tile <- full_tiles[[places[j, i]]]

    tile_symmetries <- map(all_symmmetries, ~.x(tile))

    tile_which <- map_lgl(tile_symmetries, ~ all(all_tiles[[j - 1]][[i]][10, ] == .x[1, ]))

    all_tiles[[j]][[i]] <- tile_symmetries[[which(tile_which)]]
  }
}

x <- all_tiles[[1]][[1]]

get_inner <- function(x) {
  x[2:9, 2:9]
}

lake <- reduce(map(all_tiles, ~reduce(map(.x, get_inner), cbind)), rbind)

monster <- c(
  "                  # ",
  "#    ##    ##    ###",
  " #  #  #  #  #  #   "
) %>%
  str_split("") %>%
  unlist() %>%
  matrix(nrow = 3, byrow = TRUE)


for (turn in 1:8) {
  for (i in 1:94) {
    for (j in 1:77) {
      inlake <- lake[seq(i, i + 2), seq(j, j + 19)][monster == "#"]

      if (all(inlake %in% c("#", "0"))) {
        lake[seq(i, i + 2), seq(j, j + 19)][monster == "#"] <- "0"
      }
    }
  }
  lake <- rotate(lake)
  if(turn == 4) {
    lake <- lake[, seq_len(nrow(lake))]
  }
}

sum(lake == "#")
