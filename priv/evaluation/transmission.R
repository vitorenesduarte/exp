# main function
main <- function() {
  source("linesplot.R")
  # draw!
  metrics_dir <- "processed"
  key <- "transmission"
  output_file <- "transmission.png"
  ylabel <- "Transmission"
  splot(metrics_dir, key, output_file, ylabel)
}

main()
