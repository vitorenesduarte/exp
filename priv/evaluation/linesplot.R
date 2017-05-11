source("util.R")

# draw!
splot <- function(dir, key, output_file, ylabel, logy) {
  CLOSE_TO_ZERO <- 1 # 10^-10

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
  ymaximums = lapply(ls, max)
  maxy = Reduce(max, ymaximums)
  maxx = Reduce(max, lapply(ls, length))

  # log axis
  logaxis <- ""
  ylimit <- c(0, maxy)

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
    ylimit <- c(CLOSE_TO_ZERO, maxy)
  }


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
  par(xpd = T, mar = par()$mar + c(6,0,0,0))

  # configure plot
  plot(
    range(maxx),
    range(maxy),
    main="GSet",
    type="n",
    xlim=c(0, maxx), # max x
    ylim=ylimit, # max y
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
    inset=-0.7,
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
