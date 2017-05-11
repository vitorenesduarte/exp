source("util.R")

# draw!
splot <- function(dir, input_file, output_file, ylabel) {
  load_dependencies(c("RColorBrewer"))
  files <- list.files(dir)

  # read all files
  ls <- lapply(
    files,
    function(file) {
      json(c(dir, file, input_file))
    }
  )

  # find the y max for all
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
  line_width = 2
  line_types = c(1:nol)
  plot_chars <- seq(nol)

  # change outer margins
  par(xpd = T, mar = par()$mar + c(4,0,0,0))

  # configure plot
  plot(
    range(maxx),
    range(maxy),
    main="GSet",
    type="n",
    xlim=c(0, maxx), # max x
    ylim=c(0, maxy), # max y
    xlab="Time (s)",, # x axis label
    ylab=paste(c(ylabel, "(B)"), collapse=" ") # y axis label
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
    inset=-0.5,
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
