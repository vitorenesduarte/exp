# main function
main <- function() {
  source("linesplot.R")
  # draw!
  metrics_dir <- "processed"
  key <- "transmission"
  output_file <- "transmission_log.png"
  ylabel <- "Transmission (B)"
  splot(metrics_dir, key, output_file, ylabel, TRUE)

  output_file <- "transmission.png"
  splot(metrics_dir, key, output_file, ylabel, FALSE)
}

main()
