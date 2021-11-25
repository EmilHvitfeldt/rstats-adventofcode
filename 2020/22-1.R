input <- readLines("2020/22-input")

bp <- which(input == "")

player1 <- as.numeric(input[seq(2, bp-1)])
player2 <- as.numeric(input[seq(bp + 2, length(input))])

repeat {
  if (player1[1] > player2[1]) {
    player1 <- c(player1[-1], player1[1], player2[1])
    player2 <- player2[-1]
  } else {
    player2 <- c(player2[-1], player2[1], player1[1])
    player1 <- player1[-1]
  }
  if (length(player1) == 0 | length(player2) == 0) break
}

sum(player1 * rev(seq_along(player1))) +
  sum(player2 * rev(seq_along(player2)))
