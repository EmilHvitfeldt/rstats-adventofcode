input <- "uqwqemis"

i <- 0

md5 <- digest::getVDigest()

password <- list()

repeat {
  i <- i + 1
  hash <- md5(paste0(input, i), serialize = FALSE)
  if (stringr::str_sub(hash, 1, 5) == "00000") {
    location <- stringr::str_sub(hash, 6, 6)
    if (!(location %in% as.character(0:7))) next
    value <- stringr::str_sub(hash, 7, 7)
    if (!is.null(password[[location]])) next
    password[[location]] <- value

    cat("found new one at ", i, "\n")
    if (length(password) == 8) break
  }
}

paste(password[as.character(0:7)], collapse = "")
