# main function
main <- function() {
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

    source("linesplot.R")
    output_file <- paste(simulation, "_", key, "_line", ".png", sep="")
    splot(dir, simulation, key, output_file, ylabel, logy)

    source("barplot.R")
    output_file <- paste(simulation, "_", key, "_bar", ".png", sep="")
    splot(dir, simulation, key, output_file, ylabel)
  }
}

main()
