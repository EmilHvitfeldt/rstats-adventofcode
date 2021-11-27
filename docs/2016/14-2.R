library(stringr)
library(purrr)
stretch_key <- function(d, n) {
  md5 <- digest::getVDigest()
  for (i in seq_len(n))
    d <- md5(d, serialize = FALSE)
  d
}

salt <- "ngcjuoqr"

hashes <- character(30000)

for (i  in seq_along(hashes)) {
  hashes[i] <- stretch_key(paste0(salt, i), 2017)
  cat(i, "\r")
}

keys <- character()
i <- 1
repeat {

  if (str_detect(hashes[i], "(.)\\1{2,}")) {
    pattern <- str_extract(hashes[i], "(.)\\1{2,}") |>
      str_sub(1, 1) |>
      strrep(5)

    if (any(str_detect(hashes[i + seq_len(1000)], pattern))) {
      keys <- c(keys, i)
      cat(length(keys), ": ", i, "\n", sep = "")
    }
  }

  if (length(keys) == 64) break
  i <- i + 1
}
i
