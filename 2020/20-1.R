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

joints %>%
  pivot_longer(everything()) %>%
  count(value) %>%
  filter(n == 2) %>%
  summarize(res = prod(as.numeric(value))) %>%
  pull(res)
