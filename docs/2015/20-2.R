input <- 34000000

library(numbers)

register <- numeric(input)

i <- 0

repeat {
  i <- i + 1

  divs <- divisors(i)

  register[divs] <- register[divs] + 1

  if (sum((register[divs] <= 50) * divs) * 11 >= input) break

}
i
