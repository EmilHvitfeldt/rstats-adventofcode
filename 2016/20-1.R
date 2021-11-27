input <- readr::read_delim("2016/20-input", delim = "-", col_names = c("min", "max"))

min_target <- 0
max_target <- 4294967295

number <- 0

repeat {
  cond <- which(input$min <= min_target)
  if (length(cond) == 0) break
  cond_max <- input$max[cond]
  min_target <- min(cond_max) + 1
  input <- input[-cond[min(cond_max) == cond_max], ]
}

min_target
