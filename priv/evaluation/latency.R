# main function
main <- function() {
  source("boxplot.R")
  # draw!
  metrics_dir <- "processed"
  key <- "latency_local"
  output_file <- "latency_local.png"
  ylabel <- "Latency (ms)"
  logy <- FALSE
  splot(metrics_dir, key, output_file, ylabel, logy)

  key <- "latency_remote"
  output_file <- "latency_remote.png"
  splot(metrics_dir, key, output_file, ylabel, logy)
}

main()
