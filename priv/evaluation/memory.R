# main function
main <- function() {
    #  source("boxplot.R")
  source("linesplot.R")
# draw!
  metrics_dir <- "processed"
  ylabel <- "Memory (B)"
  logy <- FALSE

  # list of simulations
  simulations <- list.files(metrics_dir)

  for(i in 1:length(simulations)) {
    simulation <- simulations[[i]]
    dir <- paste(metrics_dir, simulation, sep="/")
    
    # memory crdt
    #    key <- "memory_crdt"
    #    output_file <- paste(simulation, "_", key, ".pdf", sep="")
    #    splot(dir, simulation, key, output_file, ylabel, logy)

    # memory algorithm
    key <- "memory_algorithm"
    output_file <- paste(simulation, "_", key, ".pdf", sep="")
    splot(dir, simulation, key, output_file, ylabel, logy)
  }
}

main()
