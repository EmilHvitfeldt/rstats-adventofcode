fun <- function(n) {
  parts <- seq_len(n) |> as.character()

  repeat {
    parts <- parts[-floor(length(parts) / 2 + 1)]
    if (length(parts) == 1) break
    parts <- parts[c(seq(2, length(parts)), 1)]
  }
  parts
}

which(purrr::map_chr(2:1000, fun) == "1")

3 ^ seq(0, 14)

3004953-1594323
