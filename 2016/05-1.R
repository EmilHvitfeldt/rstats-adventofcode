input <- "uqwqemis"

i <- 0

md5 <- digest::getVDigest()

password <- character()

repeat {
  i <- i + 1
  hash <- md5(paste0(input, i), serialize = FALSE)
  if (stringr::str_sub(hash, 1, 5) == "00000") {
    password <- c(password, stringr::str_sub(hash, 6, 6))
    cat("found new one at ", i, "\n")
    if (length(password) == 8) break
  }
}

paste(password, collapse = "")
