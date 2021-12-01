input <- readLines("2021/01-input") |>
  as.numeric()

sum3 <- slider::slide_dbl(input, mean, .before = 2)

sum(diff(sum3) > 0)
