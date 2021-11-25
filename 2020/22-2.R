library(purrr)

input <- readLines("2020/22-input")

bp <- which(input == "")

player1 <- as.numeric(input[seq(2, bp - 1)])
player2 <- as.numeric(input[seq(bp + 2, length(input))])

play_game <- function(hand1, hand2, subgame = FALSE) {
  previous_hands1 <- list()
  previous_hands2 <- list()
  repeat {
    if (any(map_lgl(previous_hands1, ~identical(.x, hand1))) &
        any(map_lgl(previous_hands2, ~identical(.x, hand2)))) {
      if (subgame) {
        return(TRUE)
      } else {
        return(sum(hand1 * rev(seq_along(hand1))) +
                 sum(hand2 * rev(seq_along(hand2))))
      }
    }

    draw1 <- hand1[1]
    draw2 <- hand2[1]

    optim <- (max(hand1) > max(hand2)) &
      max(hand1) > (length(c(hand1, hand2))) &
      subgame
    if(optim) {
      return(TRUE)
    }


    if (draw1 < length(hand1) & draw2 < length(hand2)) {
      winner <- play_game(hand1[seq_len(draw1) + 1],
                          hand2[seq_len(draw2) + 1], subgame = TRUE)
    } else {
      winner <- hand1[1] > hand2[1]
    }

    previous_hands1 <- c(previous_hands1, list(hand1))
    previous_hands2 <- c(previous_hands2, list(hand2))

    if (winner) {
      hand1 <- c(hand1[-1], hand1[1], hand2[1])
      hand2 <- hand2[-1]
    } else {
      hand2 <- c(hand2[-1], hand2[1], hand1[1])
      hand1 <- hand1[-1]
    }
    if (length(hand1) == 0 | length(hand2) == 0) break
  }

  if (subgame) {
    return(length(hand2) == 0)
  } else {
    return(sum(hand1 * rev(seq_along(hand1))) +
           sum(hand2 * rev(seq_along(hand2))))
  }
}

play_game(player1, player2)
