input <- "ckczppom"
number <- seq(3000000, 4000000)

md5 <- digest::getVDigest()

hash <- vapply(
  paste0(input, number),
  md5,
  FUN.VALUE = character(1),
  serialize = FALSE
)

which(substr(hash, 1, 6) == "000000")
