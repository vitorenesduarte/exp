source("util.R")
source("generic.R")

# draw!
main <- function() {
  output_file <- "plot3.png"

  clusters <- c(
    "ls -d processed/* | grep gcounter~tree",
    "ls -d processed/* | grep gcounter~partialmesh",
    "ls -d processed/* | grep gset~tree",
    "ls -d processed/* | grep gset~partialmesh"
  )
  ## 0 transmission
  titles <- c(
    "GCounter - Tree",
    "GCounter - Mesh",
    "GSet - Tree",
    "GSet - Mesh"
  )
  labels <- c(
    "State-based",
    "Delta-based",
    "Delta-based BP",
    "Delta-based RR",
    "Delta-based BP+RR"
  )

  # avoid scientific notation
  options(scipen=999)

  # open device
  png(filename=output_file, width=2600, height=650, res=240)

  # change outer margins
  op <- par(
    oma=c(3,2,0,0),   # room for the legend
    mfrow=c(1,4),      # 2x4 matrix
    mar=c(4,3,2,2) # spacing between plots
  )

  # style stuff
  colors <- c(
    "snow4",
    "steelblue4",
    "springgreen4",
    "darkorange1",
    "red4"
  )
  angles <- c(0, 45, 135, 45, 135)
  densities <- c(0, 10, 20, 40, 60)

  for(i in 1:length(clusters)) {
    files <- system(clusters[i], intern=TRUE)

    # skip if no file
    if(length(files) == 0) next

    # keys
    key_a <- "memory_crdt"
    key_b <- "memory_algorithm"

		# data
		lines <- map(
			files,
			function(f) {
				data <- json(c(f))
				avg_a <- mean(data[[key_a]])
				avg_b <- mean(data[[key_b]])
				avg_a + avg_b
			}
		)

		# min (state-based)
		state_based = Reduce(min, lines)
		lines <- map(lines, function(v) { v / state_based })

		# plot bars
		y_min <- 0.5
		plot_bars(lines, y_min, colors, angles, densities)

		# axis labels
		y_axis_label("Memory ratio wrto State-based")

    # title
    title <- titles[i]
    title(title, line=0.5)
  }

  par(op) # Leave the last plot

  op <- par(usr=c(0,1,0,1), # Reset the coordinates
            xpd=NA)         # Allow plotting outside the plot region

  # legend
  pos <- legend(
    .1, # x
    -.7,  # y 
    cex=0.8,
    legend=labels,
    angle=angles,
    density=densities,
    fill=colors,
    horiz=TRUE,
    box.col=NA # remove box
  )

  # close device
  dev.off()
}

main()
warnings()
