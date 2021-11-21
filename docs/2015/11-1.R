input <- "hxbxwxba"

pass <- match(strsplit(input, "")[[1]], letters)

increment0 <- function(x, i) {

  x[i] <- x[i] + 1

  if (any(x %in% c(9, 15, 12))) {

    which_min <- min(which(x %in% c(9, 15, 12)))

    x[which_min] <- x[which_min] + 1
    if (which_min < 8) {
      x[seq(min(which_min + 1, 8), 8)] <- 1
    }
  }

  if (x[i] == 27) {

    x[i] <- 1
    if (i > 1) {
      x <- increment0(x, i - 1)
    }

  }
  x
}

checker <- function(x) {
  rle1 <- rle(diff(x))

  check1 <- any(rle1$lengths[rle1$values == 1] >= 2)
  check2 <- all(!c(9, 15, 12) %in% x)
  check3 <- sum(rle(x)$lengths >= 2) >= 2

  check1 & check2 & check3
}

repeat {
  pass <- increment0(pass, 8)
  if (checker(pass)) break
}

paste0(letters[pass], collapse = "")
