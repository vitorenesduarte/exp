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
  cols<-brewer.pal(name="Set1", n=noc)

  plot(
    first_line,
    col=cols[[1]],
    type="l", # lines
    xlim=c(0, maxx), # max x
    ylim=c(0, maxy), # max y
    xlab="Time (s)",, # x axis label
    ylab="Transmission (B)" # y axis label
  )

  # draw the rest of the lines
  if(nol > 1) {
    for(i in 2:length(ls)) {
      lines(ls[[i]], col=cols[[i]])
    }
  }

  # legend
  legend(
    x="topleft",
    legend=get_labels(files),
    col=cols,
    pch=15
  )

  # close device
  dev.off()
}

pplot <- function(dir) {
  load_dependencies(c("ggplot2", "reshape"))
  ls <- list()
  files <- list.files(dir)

  for(file in files){
    j <- json(c(dir, file, "transmission"))
    ls[[file]] <- j
  }

  df <- data.frame(sapply(ls, c))
  maxx = Reduce(max, lapply(ls, length))
  df[["time"]] <- c(0:(maxx-1))
  m <- melt(df, id.vars="time")

  p = ggplot(
    m,
    aes(
      x=time,
      y=value,
      colour=variable
    )
  ) + geom_line() + labs(x="Time (s)",y="Transmission (B)")

  ggsave(filename="r.png", plot=p)
}

# main function
main <- function() {
  source("util.R")
  # draw!
  metrics_dir <- "processed"
  splot(metrics_dir)
}

main()
