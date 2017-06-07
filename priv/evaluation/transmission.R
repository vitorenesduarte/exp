# main function
main <- function() {
  # draw!
  dir <- "processed"
  label <- "Transmission (KB)"
  logy <- FALSE
  key <- "transmission"

  #source("linesplot.R")
  #output_file <- paste(key, "_line", ".png", sep="")
  #splot(dir, key, output_file, label, logy)

  source("barplot.R")
  output_file <- paste(key, "_bar", ".png", sep="")
  splot(dir, key, output_file, label)
}

main()
