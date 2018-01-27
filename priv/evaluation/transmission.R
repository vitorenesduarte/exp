# main function
main <- function() {
  source("barplot.R")

  # draw!
  for(suffix in c("", "_driven")) {
    for(key in c("transmission", "transmission_metadata", "transmission_payload")) {
      dir <- paste("processed", suffix, sep="")
      output_file <- paste(key, "_bar", suffix, ".png", sep="")
      label <- "Transmission (KB)"

      bar_number <- if(dir == "processed") { 5 } else { 3 }

      splot(dir, key, output_file, label, bar_number)
    }
  }
}

main()
