# main function
main <- function() {
  source("linesplot.R")
  # draw!
  metrics_dir <- "processed"
  input_file <- "transmission"
  output_file <- "transmission.png"
  ylabel <- "Transmission"
  splot(metrics_dir, input_file, output_file, ylabel)
}

main()
