input <- c(4, 7, 6, 1, 3, 8, 2, 5, 9)
current <- input[1]

a <- numeric(length(input))

input_len <- length(input)

for (i in seq_along(a)) {
  which_i <- which(input == i)
  if (which_i == input_len) {
    a[i] <- input[1]
  } else {
    a[i] <- input[which_i + 1]
  }
}

for (i in 1:100) {

  pick1 <- a[current]
  pick2 <- a[pick1]
  pick3 <- a[pick2]
  pick4 <- a[pick3]

  a[current] <- pick4

  dest <- current - 1

  if (dest == 0) {
    dest <- input_len
  }

  while (dest %in% c(pick1, pick2, pick3)) {
    dest <- dest - 1
  }

  if (dest == 0) {
    dest <- input_len
  }

  while (dest %in% c(pick1, pick2, pick3)) {
    dest <- dest - 1
  }

  end <- a[dest]

  a[pick3] <- end
  a[dest] <- pick1

  current <- a[current]
}

res <- numeric(length(a))
res[1] <- 1

for (i in 2:length(a)) {
  res[i] <- a[res[i-1]]
}

paste0(res[-1], collapse = "")
