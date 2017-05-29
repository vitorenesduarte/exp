source("util.R")

# draw!
splot <- function(dir, simulation, key, output_file, label) {
  load_dependencies(c("RColorBrewer"))
  files <- list.files(dir)

  # read all files
  ls <- lapply(
    files,
    function(file) {
      json(c(dir, file))[[key]]
    }
  )

  # find y max for all
  ymaximums = lapply(ls, max)
  maxy = Reduce(max, ymaximums)
  maxx = Reduce(max, lapply(ls, length))

  # open device
  #png(filename=output_file, width=500, height=500, res=80)
  png(filename=output_file, res=80)

  # style stuff
  nol = length(ls)
  noc = if(nol >= 3) nol else 3
  colors <- brewer.pal(name="Set1", n=noc)
  line_types = c(1:nol)
  plot_chars <- seq(nol)

  # labels
  labels <- get_labels(files)

  par(xpd = T, mar = par()$mar + c(6,0,0,0))

  plot(
    range(1),
    main=get_title(simulation),
    xlim=c(0, maxy),
    ylim=c(0, 1),
    xlab=label,
    ylab="CDF",
  )

  # configure plot
  for(i in 1:length(ls)) {
    plot(
      ecdf(ls[[i]]),
      verticals=TRUE,
      col=colors[[i]],
      lty=line_types[[i]],
      pch=plot_chars[[i]],
      add=TRUE
    )
  }

  # legend
  legend(
   "bottom",
    inset=-0.7,
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
