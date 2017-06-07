source("util.R")

# draw!
splot <- function(dir, simulation, key, output_file, xlabel) {
  load_dependencies(c("RColorBrewer"))
  files <- list.files(dir)

  # read all files
  ls <- lapply(
    files,
    function(file) {
      Reduce("+", json(c(dir, file))[[key]])
    }
  )

  # flatten list
  ls <- unlist(ls)

  # find the y max for all
  ymaximums <- lapply(ls, max)
  maxy <- Reduce(max, ymaximums)

  # open device
  #png(filename=output_file, width=500, height=500, res=80)
  png(filename=output_file, res=80)

  # style stuff
  nol <- length(ls)
  noc <- if(nol >= 3) nol else 3
  colors <- brewer.pal(name="Set1", n=noc)
  line_width <- 2
  line_types <- c(1:nol)
  plot_chars <- seq(nol)

  # change outer margins
  par(xpd=T, mar=par()$mar + c(8.5,0,0,0))

  # configure plot
  barplot(
    ls,
    main=get_title(simulation),
    col=colors,
    pch=plot_chars,
    xlab=xlabel, # x axis label
    horiz=TRUE
  )

  # legend
  legend(
   "bottom",
    inset=-1.05,
    # uncomment next line to reduce legend size
    #cex=0.8,
    legend=get_labels(files),
    col=colors,
    pch=15
  )

  # close device
  dev.off()
}
