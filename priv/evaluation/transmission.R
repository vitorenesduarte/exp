# main function
main <- function() {

  # draw!
  #for(suffix in c("", "_break_link")) {
  suffix <- ""
    #for(key_suffix in c("1", "4", "16", "32", "64", "metadata", "payload")) {
    for(key_suffix in c("1")) {
      dir <- paste("processed", suffix, sep="")

      # source("lines_transmission.R")
      # key <- paste("transmission", key_suffix, "compressed", sep="_")
      # output_file <- paste(key, "_linesplot", suffix, ".png", sep="")
      # splot(dir, key, output_file)
      #
      # source("barplot.R")
      # key <- paste("transmission", key_suffix, sep="_")
      # output_file <- paste(key, "_barplot", suffix, ".png", sep="")
      # bar_number <- if(dir == "processed") { 5 } else { 3 }
      # splot(dir, key, output_file, bar_number)

      source("bar_and_lines.R")
      key <- paste("transmission", key_suffix, sep="_")
      output_file <- paste(key, "_barlinesplot", suffix, ".png", sep="")
      bar_number <- if(dir == "processed") { 5 } else { 3 }
      splot(dir, key, output_file, bar_number)
    }
  #}
}

main()
warnings()
