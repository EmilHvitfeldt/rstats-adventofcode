div <- 20201227

card_public <- 10943862
door_public <- 12721030

transform <- function(subject, lpt) {
  value <- 1
  c <- 0
  while (c < lpt) {
    value <- value * subject
    value <- value %% div
    c <- c + 1
  }
  value
}

bruteforce <- function(value_goal) {
  lpt <- 1
  value <- 1
  repeat {
    value <- value * 7
    value <- value %% div
    if (value == value_goal) {
      return(lpt)
    }
    lpt <- lpt + 1
  }
}

card_lpt_size <- bruteforce(card_public)
door_lpt_size <- bruteforce(door_public)

transform(door_public, card_lpt_size)
