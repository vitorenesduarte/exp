# main function
main <- function() {

  # draw!
  dir <- "processed"
  keys <- c("latency_local", "latency_remote")
  label <- "Processing (ms)"
  logx <- TRUE

  for(topo in c("tree", "chord")) {
    source("cdfplot.R")
    output_file <- paste("latency_", topo, ".png", sep="")
    keys <- c("latency_local", "latency_remote")
    splot(dir, keys, output_file, label, logx, topo) 

    source("cdfplot_all.R")
    output_file <- paste("latency_all_", topo, ".png", sep="")
    keys <- c("latency")
    splot(dir, keys, output_file, label, logx, topo) 
  }
}

main()
