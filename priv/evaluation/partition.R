# main function
main <- function() {
  source("partition_barplot.R")

  # draw!
  dir <- "processed"
  key <- "transmission"
  output_file <- paste("partition", ".png", sep="")
  label <- "Transmission (KB)"

  splot(dir, key, output_file, label)

  source("partition_cdfplot.R")

  # draw!
  dir <- "processed"
  keys <- c("latency_local", "latency_remote")
  output_file <- "latency.png"
  label <- "Latency (ms)"
  logx <- TRUE

  splot(dir, keys, output_file, label, logx)
}

main()
