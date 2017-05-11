# main function
main <- function() {
  source("boxplot.R")
  # draw!
  metrics_dir <- "processed"
  input_file <- "rest"
  output_file <- "memory.png"
  ylabel <- "Memory"
  logy <- TRUE
  #logy <- FALSE
  splot(metrics_dir, input_file, output_file, ylabel, logy)
}

main()
