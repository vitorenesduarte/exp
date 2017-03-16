# draw!
splot <- function(dir) {
  load_dependencies(c("RColorBrewer"))
  files <- list.files(dir)

  # read all files
  ls <- lapply(
    files,
    function(file) {
      json(c(dir, file, "transmission"))
    }
  )

  # find the y max for all
  ymaximums = lapply(ls, max)
  maxy = Reduce(max, ymaximums)
  maxx = Reduce(max, lapply(ls, length))

  # open device
  #png(filename="r.png", width=700,height=500)
  png(filename="r.png")

  # draw the first line
  first_line <- ls[[1]]

  # get colors
  nol = length(ls)
  noc = if(nol >= 3) nol else 3
  colors <- brewer.pal(name="Set1", n=noc)
  line_types = c(1:nol)
  plot_chars <- seq(nol)

  plot(
    range(maxx),
    range(maxy),
    type="n",
    xlim=c(0, maxx), # max x
    ylim=c(0, maxy), # max y
    xlab="Time (s)",, # x axis label
    ylab="Transmission (B)" # y axis label
  )

  # draw the rest of the lines
  for(i in 1:length(ls)) {
    lines(
      ls[[i]],
      col=colors[[i]],
      type="b",
      lwd=1.5,
      lty=line_types[[i]],
      pch=plot_chars[[i]]
    )
  }

  # legend
  legend(
    x=0,
    y=maxy,
    #cex=0.8, uncomment to reduce legend size
    legend=get_labels(files),
    col=colors,
    lty=line_types,
    pch=plot_chars
  )

  # close device
  dev.off()
}

# main function
main <- function() {
  source("util.R")
  # draw!
  metrics_dir <- "processed"
  splot(metrics_dir)
}

main()
