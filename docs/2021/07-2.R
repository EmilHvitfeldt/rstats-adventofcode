input <- scan("2021/07-input", sep = ",")

values <- seq(min(input), max(input))

adjust <- function(n) n * (n + 1) / 2

fuels <- purrr::map_dbl(values, ~sum(adjust(abs(input - .x))))
min(fuels)

# Trick from https://twitter.com/skyetetra
min(
  sum(adjust(abs(floor(mean(input)) - input))),
  sum(adjust(abs(ceiling(mean(input)) - input)))
)
