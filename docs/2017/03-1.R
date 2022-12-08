input <- scan("2017/03-input")

center <- 1

right <- 2
up <- 4
left <- 6
down <- 8

ring <- 1

repeat {
  ring <- ring + 1
  right <- down + ring + (ring - 1)
  up <- right + ring * 2
  left <- up + ring * 2
  down <- left + ring * 2
  if (any(c(right, up, left, down) >= input)) break
}

min(abs(c(right, up, left, down) - input)) + ring
