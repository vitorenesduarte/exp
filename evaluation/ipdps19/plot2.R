source("util.R")
source("generic.R")

# draw!
main <- function() {
  output_file <- "plot2.png"

  clusters <- c(
    "ls -d processed/* | grep 10~gmap~tree",
    "ls -d processed/* | grep 30~gmap~tree",
    "ls -d processed/* | grep 60~gmap~tree",
    "ls -d processed/* | grep 100~gmap~tree",
    "ls -d processed/* | grep 10~gmap~partialmesh",
    "ls -d processed/* | grep 30~gmap~partialmesh",
    "ls -d processed/* | grep 60~gmap~partialmesh",
    "ls -d processed/* | grep 100~gmap~partialmesh"
  )
  ## 0 transmission
  titles <- c(
    "GMap 10% - Tree",
    "GMap 30% - Tree",
    "GMap 60% - Tree",
    "GMap 100% - Tree",
    "GMap 10% - Mesh",
    "GMap 30% - Mesh",
    "GMap 60% - Mesh",
    "GMap 100% - Mesh"
  )
  labels <- c(
    "State-based",
    "Scuttlebutt",
    "Delta-based",
    "Delta-based BP",
    "Delta-based RR",
    "Delta-based BP+RR"
  )

  # avoid scientific notation
  options(scipen=999)

  # open device
  png(filename=output_file, width=2600, height=1200, res=240)

  # change outer margins
  op <- par(
    oma=c(5,3,0,0),   # room for the legend
    mfrow=c(2,4),      # 2x4 matrix
    mar=c(2,2,3,1) # spacing between plots
  )

  # style stuff
  colors <- c(
    "snow4",
    "steelblue4",
    "springgreen4",
    "darkorange1",
    "red4",
    "gray22"
  )

  for(i in 1:length(clusters)) {
    files <- system(clusters[i], intern=TRUE)

    # skip if no file
    if(length(files) == 0) next

    # keys
    key_x <- "transmission_1_compressed_x"
    key_y <- "transmission_1_compressed"

    # data
    title <- titles[i]
    lines_x <- lapply(files, function(f) { json(c(f))[[key_x]] })
    lines_y <- lapply(files, function(f) { json(c(f))[[key_y]] })

    # plot lines
    plot_lines(title, lines_x, lines_y, colors)
  }

  # axis labels
  x_axis_label("Time (s)")
  y_axis_label("Transmission")

  par(op) # Leave the last plot
  op <- par(usr=c(0,1,0,1), # Reset the coordinates
            xpd=NA)         # Allow plotting outside the plot region

  # legend
  legend(
    -.03, # x
    -.2,  # y 
    cex=0.92,
    legend=labels,
    pch=c(1:10),
    col=colors,
    horiz=TRUE,
    box.col=NA # remove box
  )

  # close device
  dev.off()
}

main()
warnings()
