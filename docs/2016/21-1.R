library(stringr)
int <- as.integer
plus1 <- function(x) x + 1

input <- readLines("2016/21-input")

password <- strsplit("abcdefgh", "")[[1]]
password_len <- length(password)

rotate_left <- function(x, n) {
  c(password[-seq_len(1)], password[seq_len(1)])
}

for (command in input) {
  if (str_detect(command, "^swap position")) {
    pos <- str_extract_all(command, "\\d")[[1]] |> int() |> plus1()
    password[pos] <- rev(password[pos])
  }
  if (str_detect(command, "^swap letter")) {
    letter <- str_sub(command, c(13, 27), c(13, 27))
    pos <- match(letter, password)
    password[pos] <- rev(password[pos])
  }
  if (str_detect(command, "^reverse positions")) {
    pos <- str_extract_all(command, "\\d")[[1]] |> int() |> plus1()
    password[pos[1]:pos[2]] <- rev(password[pos[1]:pos[2]])
  }
  if (str_detect(command, "^rotate left")) {
    shift <- str_extract(command, "\\d") |> int()
    if (shift == 0) next
    password <- c(password[-seq_len(shift)], password[seq_len(shift)])
  }
  if (str_detect(command, "^rotate right")) {
    shift <- str_extract(command, "\\d") |> int()
    if (shift == 0) next
    password <- c(
      rev(rev(password)[seq_len(shift)]),
      rev(rev(password)[-seq_len(shift)])
    )
  }
  if (str_detect(command, "^move position")) {
    pos <- str_extract_all(command, "\\d")[[1]] |> int() |> plus1()
    element <- password[pos[1]]
    password <- password[-pos[1]]
    if (pos[2] == 1) {
      password <- c(element, password)
    } else if (pos[2] == password_len) {
      password <- c(password, element)
    } else
      password <- c(
        password[seq_len(pos[2]-1)],
        element,
        password[seq(pos[2], password_len-1)]
      )
  }
  if (str_detect(command, "^rotate based")) {
    letter <- str_sub(command, 36, 36)
    shift <- match(letter, password)
    shift <- shift
    password <- c(
      rev(rev(password)[seq_len(shift)]),
      rev(rev(password)[-seq_len(shift)])
    )
    if (shift > 4) {
      password <- c(
        rev(rev(password)[seq_len(1)]),
        rev(rev(password)[-seq_len(1)])
      )
    }
  }
}
cat(paste(password, collapse = ""), "\n")
