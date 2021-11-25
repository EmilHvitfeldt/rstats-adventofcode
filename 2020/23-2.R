input <- c(4, 7, 6, 1, 3, 8, 2, 5, 9)
current <- input[1]

a <- numeric(length(input))

input_len <- length(input)

a <- c(3, 5, 8, 7, 9, 1, 6, 2, 10,  seq(11, 1000000), 4)

tictoc::tic()
for (i in 1:10000000) {

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
tictoc::toc()

a[1] * a[a[1]]
