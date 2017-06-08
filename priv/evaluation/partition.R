# main function
main <- function() {
  source("partition_barplot.R")

  # draw!
  dir <- "processed"
  key <- "transmission"
  output_file <- paste("partition", ".png", sep="")
  label <- "Transmission (KB)"

  splot(dir, key, output_file, label)
}

main()
