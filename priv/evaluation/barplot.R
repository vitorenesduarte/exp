source("util.R")

# draw!
splot <- function(dir, key, output_file, bar_number) {
  files <- list.files(dir)

  # read all files
  ls <- lapply(
    files,
    function(file) {
      Reduce("+", json(c(dir, file))[[key]])
    }
  )

  # clusters
  clusters <- c(
    "gcounter~tree", "gset~tree", "awset~tree",
    "gcounter~chord", "gset~chord", "awset~chord"
  )
  titles <- c(
    "GCounter - Tree", "GSet - Tree", "AWSet - Tree",
    "GCounter - Chord", "GSet - Chord", "AWSet - Chord"
  )

  # avoid scientific notation
  options(scipen=999)

  # flatten list
  ls <- unlist(ls)

  # open device
  png(filename=output_file, width=2000, height=900, res=240)
  #png(filename=output_file, res=100)

  # change outer margins
  op <- par(
    oma=c(1,3,0,12),   # room for the legend
    mfrow=c(2,3),      # 3x3 matrix
    mar=c(3.5, 2, 1, 3) # spacing between plots
  )

  # plot size
  PLOT_SIZE <-
    if(bar_number == 3) 2.5
    else if(bar_number == 4) 3.25
    else if(bar_number == 5) 4
    else print("BAR NUMBER NOT FOUND")

  # bar width
  width <- PLOT_SIZE / bar_number

  # angles and density
  angle1 <- c(0, 135, 45, 135, 45)
  angle2 <- c(0, 135, 45, 135, 45)
  density <- c(0, 10, 20, 40, 60)
  # colors <- 1

  colors <- c(
    "snow3",
    "steelblue4",
    "springgreen4",
    "darkorange1",
    "red4"
  )

  for(i in 1:length(clusters)) {
    cluster <- clusters[i]
    title <- titles[i]
    indexes <- c()

    # get indexes for this cluster
    for(f in 1:length(files)) {
      file <- files[f]
      if(regexpr(cluster, file) > 0) {

        # if any of this, hide
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

    # configure plot
    barplot(
      lines,
      col=colors,
      ann=FALSE, # no axis label ?
      horiz=TRUE,
      ylim=c(0, bar_number), # number of bars
      width=width, # bar width
      angle=angle1,
      density=density,
    )
    barplot(
      lines,
      add=TRUE,
      col=colors,
      ann=FALSE, # no axis label ?
      horiz=TRUE,
      ylim=c(0, bar_number), # number of bars
      width=width, # bar width
      angle=angle2,
      density=density,
    )
    # axis label
    mtext(
      side=1,
      text="Transmission",
      line=-.5,
      outer=TRUE,
      cex=.9 # size
    )
    title(title, line=0)
  }

  par(op) # Leave the last plot

  op <- par(usr=c(0,1,0,1), # Reset the coordinates
            xpd=NA)         # Allow plotting outside the plot region

  # legend
  legend(
    .8, # x
    .6, # y
    cex=0.8,
    angle=angle1,
    density=density,
    fill=colors,
    legend=get_labels(files[indexes]),
    #col=colors,
    #pch=15,
    box.col=NA # remove box
  )
  legend(
    .8, # x
    .6, # y
    cex=0.8,
    angle=angle2,
    density=density,
    fill=colors,
    legend=get_labels(files[indexes]),
    #col=colors,
    #pch=15,
    box.col=NA # remove box
  )

  # close device
  dev.off()
}
