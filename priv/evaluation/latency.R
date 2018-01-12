# main function
main <- function() {
  source("cdfplot.R")

  # draw!
  dir <- "processed"
  keys <- c("latency_local", "latency_remote")
  output_file <- "latency.png"
  label <- "Latency (ms)"
  logx <- TRUE

  splot(dir, keys, output_file, label, logx) 
}

main()
