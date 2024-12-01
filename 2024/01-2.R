input <- readr::read_delim(
  "2024/01-input", 
  delim = "   ", 
  col_names = FALSE,
  show_col_types = FALSE
)

counts <- table(input$X2)[as.character(input$X1)]

sum(abs(input$X1 * counts), na.rm = TRUE)
