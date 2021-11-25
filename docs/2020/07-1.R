library(stringr)
input <- readLines("2020/07-input")

containing_bag <- function(x) {
  str_extract(str_subset(input, paste0(".+", x)), "^.+?bag")
}

used_bags <- c()

new_bags <- "shiny gold bag"

repeat {
  newer_bags <- unique(unlist(lapply(new_bags, containing_bag)))

  if (length(newer_bags) == 0) {
    used_bags <- c(used_bags, new_bags)
    break
  }

  used_bags <- c(used_bags, new_bags)

  new_bags <- setdiff(newer_bags, used_bags)
}

length(setdiff(used_bags, "shiny gold bag"))
