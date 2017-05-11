# main function
main <- function() {
  source("boxplot.R")
  # draw!
  metrics_dir <- "processed"
  key <- "memory_algorithm"
  output_file <- "memory.png"
  ylabel <- "Memory (B)"
  #logy <- TRUE
  logy <- FALSE
  splot(metrics_dir, key, output_file, ylabel, logy)
}

main()
