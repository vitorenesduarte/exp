# main function
main <- function() {
    #source("cdfplot.R")
  source("linesplot.R")
  # draw!
  metrics_dir <- "processed"
  label <- "Latency (ms)"
  #  logx <- TRUE
    logy <- FALSE

  # list of simulations
  simulations <- list.files(metrics_dir)
  
  for(i in 1:length(simulations)) {
    simulation <- simulations[[i]]
    dir <- paste(metrics_dir, simulation, sep="/")
  
    # latency local
    key <- "latency_local"
    output_file <- paste(simulation, "_", key, ".pdf", sep="")
  	splot(dir, simulation, key, output_file, label, logy)

    # latency remote
    key <- "latency_remote"
    output_file <- paste(simulation, "_", key, ".pdf", sep="")
  	splot(dir, simulation, key, output_file, label, logy)
  }
}

main()
