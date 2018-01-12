source("util.R")

# draw!
splot <- function(dir, simulation, key, output_file, ylabel, logy) {
  load_dependencies(c("RColorBrewer"))
  files <- list.files(dir)

  # read all files
  ls <- lapply(
    files,
    function(file) {
      json(c(dir, file))[[key]]
    }
  )

  # find the y max for all
  yminimums <- lapply(ls, min)
  ymaximums <- lapply(ls, max)
  miny <- Reduce(min, yminimums)
  maxy <- Reduce(max, ymaximums)
  maxx <- Reduce(max, lapply(ls, length))

  miny <- if(logy && miny == 0) 0.001 else miny
  logaxis <- if(logy) "y" else ""

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
  plot(
    range(maxx),
    range(maxy),
    main=get_title(simulation),
    type="n",
    xlim=c(0, maxx), # max x
    ylim=c(miny, maxy), # max y
    xlab="Time (s)", # x axis label
    ylab=ylabel, # y axis label
    log=logaxis
  )

  # draw lines
  for(i in 1:length(ls)) {
    lines(
      ls[[i]],
      col=colors[[i]],
      type="b",
      lwd=line_width,
      lty=line_types[[i]],
      pch=plot_chars[[i]]
    )
  }

  # legend
  legend(
   "bottom",
    inset=-1.05,
    # uncomment next line to reduce legend size
    #cex=0.8,
    legend=get_labels(files),
    col=colors,
    lwd=line_width,
    lty=line_types,
    pch=plot_chars
  )

  # close device
  dev.off()
}
