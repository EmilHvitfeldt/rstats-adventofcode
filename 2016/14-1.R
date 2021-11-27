library(stringr)
library(purrr)

md5 <- digest::getVDigest()

salt <- "ngcjuoqr"

keys <- c()
i <- 1
repeat {
  hash <- md5(paste0(salt, i), serialize = FALSE)
  if (str_detect(hash, "(.)\\1{2,}")) {
    pattern <- str_extract(hash, "(.)\\1{2,}") |>
      str_sub(1, 1) |>
      strrep(5)

    if (any(map_lgl(i + seq_len(1000), ~ str_detect(md5(paste0(salt, .x), serialize = FALSE), pattern)))) {
      keys <- c(keys, i)
      cat(length(keys), ": ", i, "\n", sep = "")
    }
  }

  if (length(keys) == 64) break
  i <- i + 1
}
i
