# main function
main <- function() {
  source("cdfplot.R")
  # draw!
  metrics_dir <- "processed"
  key <- "latency_local"
  output_file <- "latency_local.png"
  label <- "Latency (ms)"
  splot(metrics_dir, key, output_file, label)

  key <- "latency_remote"
  output_file <- "latency_remote.png"
  splot(metrics_dir, key, output_file, label)
}

main()
