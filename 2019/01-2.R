input <- scan("2019/01-input")

calc_fuel <- function(x) {
  total <- 0
  repeat {
    fuel <- floor(x / 3) - 2
    if (fuel <= 0) return(total)
    total <- total + fuel
    x <- fuel
  }
}
vapply(input, calc_fuel, FUN.VALUE = numeric(1)) |>
  sum()
