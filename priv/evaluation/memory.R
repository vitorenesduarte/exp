# main function
main <- function() {
  source("boxplot.R")
  # draw!
  metrics_dir <- "processed"
  ylabel <- "Memory (B)"

  # list of simulations
  simulations <- list.files(metrics_dir)

  for(i in 1:length(simulations)) {
    simulation <- simulations[[i]]
    dir <- paste(metrics_dir, simulation, sep="/")
    
    for(f in c("boxplot", "linesplot")) {
      r <- paste(f, ".R", sep="")
      source(r)

      logy <- if(f == "boxplot") TRUE else FALSE

      for(key in c("memory_crdt", "memory_algorithm")) {
        output_file <- paste(simulation, "_",
                             key, "_",
                             f, ".png", sep="")
        splot(dir, simulation, key, output_file, ylabel, logy)
      }
    }
  }
}

main()
