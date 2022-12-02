input <- readLines("2022/02-input")

dict <- c(A = "rock", B = "paper", C = "scissors",
          X = "lose", Y = "tie", Z = "win")

you <- dict[substr(input, 1, 1)]
me <- dict[substr(input, 3, 3)]

mat <- matrix(
  c("scissors", "rock", "paper",
    "paper", "scissors", "rock",
    "rock", "paper", "scissors"),
  byrow = TRUE, ncol = 3,
  dimnames = list(c("lose", "win", "tie"),
                  c("rock", "paper", "scissors"))
)
mat

scores <- c(win = 6, tie = 3, lose = 0, rock = 1, paper = 2, scissors = 3)

sum(scores[mat[cbind(me, you)]]) + sum(scores[me])
