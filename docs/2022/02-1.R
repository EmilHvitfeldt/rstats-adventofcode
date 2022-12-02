input <- readLines("2022/02-input")

dict <- c(A = "rock", B = "paper", C = "scissors",
          X = "rock", Y = "paper", Z = "scissors")

you <- dict[substr(input, 1, 1)]
me <- dict[substr(input, 3, 3)]

mat <- matrix(
  c("tie", "lose", "win",
    "win", "tie", "lose",
    "lose", "win", "tie"),
  byrow = TRUE, ncol = 3,
  dimnames = list(c("rock", "paper", "scissors"),
                  c("rock", "paper", "scissors"))
)

scores <- c(win = 6, tie = 3, lose = 0, rock = 1, paper = 2, scissors = 3)

sum(scores[mat[cbind(me, you)]]) + sum(scores[me])
