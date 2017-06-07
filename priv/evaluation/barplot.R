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

	for(i in 1:length(clusters)) {
    cluster <- clusters[i]
    title <- titles[i]
    indexes <- c()

		# get indexes for this cluster
    for(f in  1:length(files)) {
      file <- files[f]
      if(regexpr(cluster, file) > 0) {
        indexes[length(indexes) + 1] <- f
      }
    }

    # get lines of this cluster
		lines <- ls[indexes]

		# style stuff
		colors <- c(
			"springgreen4",
      "steelblue4",
      "darkorange",
      "tomato1",
      "snow3"
    )
		
    # configure plot
    barplot(
      lines,
      col=colors,
      ann=FALSE, # no axis label ?
      horiz=TRUE
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
    .31, 0,
    # uncomment next line to reduce legend size
    cex=0.8,
    legend=get_labels(files[indexes]),
    col=colors,
    pch=15,
    # box.col=NA # remove box
  )

  # close device
  dev.off()
}
