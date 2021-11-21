input <- 34000000

library(numbers)

i <- 0

repeat {
  i <- i + 1
  if (sum(divisors(i)) * 10 >= input) break
}
i
