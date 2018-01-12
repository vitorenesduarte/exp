# main function
main <- function() {
  source("barplot.R")

  # draw!
  dir <- "processed"
  key <- "transmission"
  output_file <- paste(key, "_bar", ".png", sep="")
  label <- "Transmission (KB)"

  splot(dir, key, output_file, label)
}

main()
