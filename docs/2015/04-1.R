input <- "ckczppom"
number <- seq_len(1000000)

md5 <- digest::getVDigest()

hash <- vapply(
  paste0(input, number),
  md5,
  FUN.VALUE = character(1),
  serialize = FALSE
)

which(substr(hash, 1, 5) == "00000")
