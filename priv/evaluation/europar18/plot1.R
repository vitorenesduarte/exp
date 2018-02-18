source("lib/util.R")
source("lib/generic.R")

# draw!
main <- function() {
  output_file <- "plot1.png"

  clusters <- c(
    "ls -d processed/* | grep gset~tree",
    "ls -d processed/* | grep gset~partialmesh",
    "ls -d processed/* | grep gcounter~tree",
    "ls -d processed/* | grep gcounter~partialmesh"
  )
  ## 0 transmission
  titles <- c(
    "GSet - Tree",
    "GSet - Mesh",
    "GCounter - Tree",
    "GCounter - Mesh"
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
    oma=c(5,3,0,0),   # room for the legend
    mfrow=c(1,4),      # 2x4 matrix
    mar=c(2,2,3,1) # spacing between plots
  )

  # style stuff
  colors <- c(
    "snow4",
    "steelblue4",
    "springgreen4",
    "darkorange1",
    "red4"
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
    -.75,  # y 
    cex=1.1,
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
