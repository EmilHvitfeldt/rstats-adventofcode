input <- readr::read_delim(
  "2024/01-input", 
  delim = "   ", 
  col_names = FALSE,
  show_col_types = FALSE
)

sum(abs(sort(input$X1) - sort(input$X2)))
