source("util.R")

# draw!
splot <- function(dir, simulation, key, output_file, ylabel, logy) {
  CLOSE_TO_ZERO <- 0.0000001

  load_dependencies(c("RColorBrewer"))
  files <- list.files(dir)

  # read all files
  ls <- lapply(
    files,
    function(file) {
      json(c(dir, file))[[key]]
    }
  )

  # log axis
  logaxis <- ""

  if(logy) {
    ls <- lapply(
      ls,
      function(line) {
        sapply(
          line,
          function(e) {
            if(e == 0) CLOSE_TO_ZERO
            else e
          }
        )
      }
    )


    logaxis <- "y"
  }

  # open device
  #png(filename=output_file, width=500, height=500, res=80)
  pdf(filename=output_file)

  # style stuff
  nol = length(ls)
  noc = if(nol >= 3) nol else 3
  colors <- brewer.pal(name="Set1", n=noc)
  line_types = c(1:nol)
  plot_chars <- seq(nol)

  # labels
  labels <- get_labels(files)

  par(xpd = T, mar = par()$mar + c(4.5,0,0,0))

  # configure plot
  boxplot(
    ls,
    main=get_title(simulation),
    xaxt="n", # remove automatic numbers
    xlab="", # x axis label
    ylab=ylabel, # y axis label
    col=colors,
    lty=line_types,
    log=logaxis
  )

  # legend
  legend(
   "bottom",
    inset=-0.53,
    # uncomment next line to reduce legend size
    #cex=0.8,
    legend=get_labels(files),
    col=colors,
    lty=line_types,
    pch=plot_chars
  )

  # close device
  dev.off()
}
