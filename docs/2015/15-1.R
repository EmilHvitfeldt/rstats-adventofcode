library(dplyr)

# Hand imported input
values <- list(
  Sprinkles =    c(capacity = 5, durability =-1, flavor = 0, texture = 0, calories = 5),
  PeanutButter = c(capacity =-1, durability = 3, flavor = 0, texture = 0, calories = 1),
  Frosting =     c(capacity = 0, durability =-1, flavor = 4, texture = 0, calories = 6),
  Sugar =        c(capacity =-1, durability = 0, flavor = 0, texture = 2, calories = 8)
)

fourway_sum <- function(n) {
  expand.grid(Sprinkles = 0:100,
              PeanutButter = 0:100,
              Frosting = 0:100) %>%
    filter(Sprinkles + PeanutButter + Frosting == n) %>%
    mutate(Sugar = 100 - n)
}

combinations <- purrr::map_dfr(0:100, fourway_sum)

batter_mizer <- function(Sprinkles, PeanutButter, Frosting, Sugar) {
  sum <- values[["Sprinkles"]] * Sprinkles +
         values[["PeanutButter"]] * PeanutButter +
         values[["Frosting"]] * Frosting +
         values[["Sugar"]] * Sugar

  sum <- pmax(sum, 0)

  prod(sum[1:4])
}

res <- purrr::pmap_dbl(combinations, batter_mizer)
max(res)
