# main function
main <- function() {
  source("linesplot.R")
  # draw!
  metrics_dir <- "processed"
  ylabel <- "Transmission (B)"
  logy <- FALSE
  key <- "transmission"

  # list of simulations
  simulations <- list.files(metrics_dir)

  for(i in 1:length(simulations)) {
    simulation <- simulations[[i]]
    dir <- paste(metrics_dir, simulation, sep="/")
    output_file <- paste(simulation, "_", key, ".png", sep="")
    splot(dir, simulation, key, output_file, ylabel, logy)
  }
}

main()
