input <- scan("2021/07-input", sep = ",")

values <- seq(min(input), max(input))

fuels <- purrr::map_dbl(values, ~ sum(abs(input - .x)))
min(fuels)

# Trick from https://twitter.com/skyetetra
sum(abs(median(input) - input))
