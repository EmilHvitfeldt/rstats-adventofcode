library(stringr)
library(purrr)
input <- readLines("2020/19-input")
break_point <- which(input == "")

rules_raw <- input[seq(1, break_point - 1)]
rules_raw <- str_remove_all(rules_raw, "\"")
rules_raw <- str_split(rules_raw, ": ")
rules <- map_chr(rules_raw, 2)
rules <- map_chr(str_split(rules, " "), ~paste(glue::glue("_{.x}_"),collapse = ""))
rules <- str_replace_all(rules, c("_\\|_" = "\\|", "_a_" = "a", "_b_" = "b"))
rules <- paste0("(", rules, ")")
names(rules) <- paste0("_", map(rules_raw, 1), "_")
rules["_8_"] <- "(_42_+)"
rules["_11_"] <- "(_42__31_|_42__42__31__31_|_42__42__42__31__31__31_|_42__42__42__42__31__31__31__31_|_42__42__42__42__42__31__31__31__31__31_|_42__42__42__42__42__42__42__31__31__31__31__31__31__31_)"

messages <- input[seq(break_point + 1, length(input))]

master <- "_0_"
while(str_detect(master, "[0-9]")) {
  master <- str_replace_all(master, rules)
}

sum(grepl(glue::glue("^{master}$"), messages))
