# main function
main <- function() {
  source("plot.R")
  # draw!
  metrics_dir <- "processed"
  input_file <- "rest"
  output_file <- "memory.png"
  ylabel <- "Memory"
  splot(metrics_dir, input_file, output_file, ylabel)
}

main()
