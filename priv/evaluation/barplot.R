source("util.R")

# draw!
splot <- function(dir, key, output_file, xlabel) {
  load_dependencies(c("RColorBrewer", "base"))
  files <- list.files(dir)

  # read all files
  ls <- lapply(
    files,
    function(file) {
      Reduce("+", json(c(dir, file))[[key]])
    }
  )

  # get clusters
  clusters <- c(
    "gcounter~line", "gset~line", "awset~line",
    "gcounter~ring", "gset~ring", "awset~ring",
    "gcounter~hyparview", "gset~hyparview", "awset~hyparview"
  )
  titles <- c(
    "GCounter - Line", "GSet - Line", "AWSet - Line",
    "GCounter - Ring", "GSet - Ring", "AWSet - Ring",
    "GCounter - HyParView", "GSet - HyParView", "AWSet - HyParView"
  )

  # avoid scientific notation
  options(scipen=999)

  # flatten list
  ls <- unlist(ls)

  # find the y max for all
  ymaximums <- lapply(ls, max)
  maxy <- Reduce(max, ymaximums)

  # open device
  png(filename=output_file, width=1600, height=1600, res=240)
  #png(filename=output_file, res=100)

  # change outer margins
  op <- par(
    oma=c(10,3,0,0),   # room for the legend
    mfrow=c(3,3),      # 3x3 matrix
    mar=c(3, 0, 4, 4) # spacing between plots
  )

  # bar number
  bar_number <- length(files) / length(clusters)

  # plot size
  PLOT_SIZE <-
    if(bar_number == 3) 2.5
    else if(bar_number == 5) 4
    else print("BAR NUMBER NOT FOUND")

  # bar width
  width <- PLOT_SIZE / bar_number

  for(i in 1:length(clusters)) {
    cluster <- clusters[i]
    title <- titles[i]
    indexes <- c()

    # get indexes for this cluster
    for(f in  1:length(files)) {
      file <- files[f]
      if(regexpr(cluster, file) > 0) {

        # if any of this
        # don't show the bar
        is_digest = regexpr("digest", file) > 0
        is_gset_or_gcounter = regexpr("gset", file) > 0 || regexpr("gcounter", file) > 0

        if(!(is_digest && is_gset_or_gcounter)) {
          indexes[length(indexes) + 1] <- f
        }
      }
    }

    # get lines of this cluster
    lines <- ls[indexes]

    # if not an awset

    # style stuff
    colors <- c(
      "snow3",
      "steelblue4",
      "springgreen4",
      "darkorange1",
      "darkgoldenrod1"
    )

    # configure plot
    barplot(
      lines,
      col=colors,
      ann=FALSE, # no axis label ?
      horiz=TRUE,
      ylim=c(0, bar_number), # number of bars
      width=width # bar width
    )
    # axis label
    mtext(
      side=1,
      text=xlabel,
      line=3,
      cex=.8 # size
    )
    title(title, line=0.5)
  }

  par(op) # Leave the last plot

  op <- par(usr=c(0,1,0,1), # Reset the coordinates
            xpd=NA)         # Allow plotting outside the plot region

  # legend
  legend(
    .32,
    0,
    cex=0.8,
    legend=get_labels(files[indexes]),
    col=colors,
    pch=15,
    box.col=NA # remove box
  )

  # close device
  dev.off()
}
