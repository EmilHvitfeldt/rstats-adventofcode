input <- readLines("2020/05-input")

input <- gsub(c("[FL]"), c("0"), input)
input <- gsub(c("[BR]"), c("1"), input)

id <- strtoi(input, base = 2)

max(id)
