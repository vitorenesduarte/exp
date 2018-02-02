# main function
main <- function() {
  source("lines_transmission.R")

  # draw!
  for(suffix in c("", "_break_link")) {
    #for(key_suffix in c("1", "4", "16", "32", "64", "metadata", "payload")) {
    for(key_suffix in c("1_compressed")) {
      key <- paste("transmission", key_suffix, sep="_")
      dir <- paste("processed", suffix, sep="")
      output_file <- paste(key, "_line", suffix, ".png", sep="")

      splot(dir, key, output_file)
    }
  }
}

main()
warnings()
