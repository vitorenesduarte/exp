source("util.R")

# draw!
splot <- function(dir, key, output_file) {
  files <- list.files(dir)

  # read all files
  ls_x <- lapply(
    files,
    function(file) {
      json(c(dir, file))[[paste(key, "x", sep="_")]]
    }
  )
  ls_y <- lapply(
    files,
    function(file) {
      json(c(dir, file))[[key]]
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

  # open device
  png(filename=output_file, width=2000, height=900, res=240)
  #png(filename=output_file, res=100)

  # change outer margins
  op <- par(
    oma=c(1,3,0,12),   # room for the legend
    mfrow=c(2,3),      # 3x3 matrix
    mar=c(3.5, 2, 3, 3) # spacing between plots
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
    lines_x <- ls_x[indexes]
    lines_y <- ls_y[indexes]

    # style stuff
    #colors <- c(1, 1, 1, 1, 1)
    colors <- c(
      "snow3",
      "steelblue4",
      "springgreen4",
      "darkorange1",
      "red4"
    )

		# find the y max for all
		x_max <- Reduce(max, lapply(lines_x, max))
		y_max <- Reduce(max, lapply(lines_y, max))

    # configure plot
    plot(
      range(x_max),
      range(y_max),
			type="n",
			xlim=c(0, x_max), # max x
			ylim=c(0, y_max), # max y
      xlab="",
      ylab="",
    )
    # x axis label
    mtext(
      side=1, # bottom
      text="Time (s)",
      line=-.5, # closeness to plot
      outer=TRUE, # outside of the plot, thus only one
      cex=.9 # size
    )
    # y axis label
    mtext(
      side=2, # left
      las=0, # vertical text
      text="Transmission",
      line=1, # closeness to plot
      outer=TRUE, # outside of the plot, thus only one
      cex=.9 # size
    )
    title(title, line=0.5)

		for(i in 1:length(lines_y)) {
			lines(
				lines_x[[i]],
				lines_y[[i]],
				col=colors[[i]],
				type="b",
				pch=i
			)
		}
  }

  par(op) # Leave the last plot

  op <- par(usr=c(0,1,0,1), # Reset the coordinates
            xpd=NA)         # Allow plotting outside the plot region

  # legend
  legend(
    .80, # x
    .6,  # y 
    cex=0.8,
    legend=get_labels(files[indexes]),
    col=colors,
    pch=c(1:10),
    box.col=NA # remove box
  )

  # close device
  dev.off()
}
