# main function
main <- function() {
  source("boxplot.R")
  # draw!
  metrics_dir <- "processed"
  key <- "memory_crdt"
  output_file <- "memory_crdt.png"
  ylabel <- "Memory (B)"
  #logy <- TRUE
  logy <- FALSE
  splot(metrics_dir, key, output_file, ylabel, logy)

  key <- "memory_algorithm"
  output_file <- "memory_algorithm.png"
  splot(metrics_dir, key, output_file, ylabel, logy)
}

main()
