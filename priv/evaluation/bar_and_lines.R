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
  ls_x <- lapply(
    files,
    function(file) {
      json(c(dir, file))[[paste(key, "compressed", "x", sep="_")]]
    }
  )
  ls_y <- lapply(
    files,
    function(file) {
      json(c(dir, file))[[paste(key, "compressed", sep="_")]]
    }
  )

  # clusters
  clusters <- c(
    "gcounter~tree", "gcounter~chord",
    "gset~tree", "gset~chord",
    "gset~tree", "gset~chord"
  )
  titles <- c(
    "GCounter - Tree", "GCounter - Chord",
    "GSet - Tree", "GSet - Chord",
    "GSet - Tree", "GSet - Chord"
  )
  barplots <- c(1, 1, 1, 1, 0, 0)

  # avoid scientific notation
  options(scipen=999)

  # flatten list
  ls <- unlist(ls)

  # open device
  png(filename=output_file, width=1800, height=1200, res=240)
  #png(filename=output_file, res=100)

  # change outer margins
  op <- par(
    oma=c(2,2,1,14),   # room for the legend
    mfrow=c(3,2),      # 2x3 matrix
    mar=c(4.5, 2, 1, 3) # spacing between plots (bot, left, top, right)
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
  angle <- c(0, 135, 45, 135, 45)
  density <- c(0, 10, 20, 40, 60)
  # colors <- 1

	colors <- c(
		"snow4",
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
    lines_x <- ls_x[indexes]
    lines_y <- ls_y[indexes]

    is_barplot <- barplots[i]

    if(is_barplot == 1) {

      # configure plot
      barplot(
        rev(lines),
        col=rev(colors),
        ann=FALSE, # no axis label ?
        horiz=TRUE,
        ylim=c(0, bar_number), # number of bars
        width=width, # bar width
        angle=rev(angle),
        density=rev(density),
      )
      # axis label
      mtext(
        side=1,
        text="Transmission",
        font=2,
        line=2, # spacing between plot
        #outer=TRUE,
        cex=.7 # size
      )
    }
    else {
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
        font=2,
				line=2, # closeness to plot
				#outer=TRUE, # outside of the plot, thus only one
				cex=.7 # size
			)
			# y axis label
			mtext(
				side=2, # left
				las=0, # vertical text
				text="Transmission",
        font=2,
				line=2, # closeness to plot
				#outer=TRUE, # outside of the plot, thus only one
				cex=.7 # size
			)
      
      for(i in 1:length(lines_y)) {
				lines(
					lines_x[[i]],
					lines_y[[i]],
					col=colors[[i]],
					type="b",
					pch=i,
          lty=3
				)
			}
    }
    title(title, line=0.3)
  }

  par(op) # Leave the last plot

  op <- par(usr=c(0,1,0,1), # Reset the coordinates
            xpd=NA)         # Allow plotting outside the plot region

  # legend
  legend(
    .76, # x
    .71, # y
    cex=0.8,
    legend=c(NA, NA, NA, NA, NA),
    col=colors,
    pch=c(1:10),
    box.col=NA # remove box
  )
  legend(
    .79, # x
    .71, # y
    cex=0.8,
    angle=angle,
    density=density,
    fill=colors,
    legend=get_labels(files[indexes]),
    #col=colors,
    #pch=15
    box.col=NA # remove box
  )

  # close device
  dev.off()
}
