source("util.R")
source("generic.R")

# draw!
main <- function() {
  output_file <- "plot0.png"

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
    oma=c(4,3,3,0),   # room for the legend
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
  angles <- c(0, 45, 135, 45, 135)
  densities <- c(0, 15, 15, 30, 30)

  for(i in 1:length(clusters)) {
    files <- system(clusters[i], intern=TRUE)

    # skip if no file
    if(length(files) == 0) next

    # keys
    key <- "transmission_1"

    # data
    title <- titles[i]
    lines <- map(files, function(f) { sum(json(c(f))[[key]]) })

    # min (rr)
    rr <- Reduce(min, lines)
    lines <- map(lines, function(v) { v / rr })

    # plot lines
    y_min <- 0
    plot_bars(title, lines, y_min, colors, angles, densities)
  }

  # axis labels
  y_axis_label("Transmission ratio wrto BP+RR")

  par(op) # Leave the last plot
  op <- par(usr=c(0,1,0,1), # Reset the coordinates
            xpd=NA)         # Allow plotting outside the plot region

  # legend
  legend(
    -.05, # x
    -.5,  # y 
    cex=1.05,
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
