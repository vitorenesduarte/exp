source("util.R")
source("generic.R")

# draw!
main <- function() {
  output_file <- "plot6.png"

  clusters <- c(
    "ls -d processed/* | grep 0~gset~tree",
    "ls -d processed/* | grep 0~gcounter~tree",
    "ls -d processed/* | grep 10~gmap~tree",
    "ls -d processed/* | grep 100~gmap~tree",
    "ls -d processed/* | grep 0~gset~partialmesh",
    "ls -d processed/* | grep 0~gcounter~partialmesh",
    "ls -d processed/* | grep 10~gmap~partialmesh",
    "ls -d processed/* | grep 100~gmap~partialmesh"
  )
  titles <- c(
    "GSet - Tree",
    "GCounter - Tree",
    "GMap 10% - Tree",
    "GMap 100% - Tree",
    "GSet - Mesh",
    "GCounter - Mesh",
    "GMap 10% - Mesh",
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
    oma=c(3,3,0,0),   # room for the legend
    mfrow=c(2,4),      # 2x4 matrix
    mar=c(1,2,3,1) # spacing between plots
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
  angles <- c(0, 45, 135, 45, 135, 45)
  densities <- c(0, 15, 15, 30, 30, 45)

  for(i in 1:length(clusters)) {
    files <- system(clusters[i], intern=TRUE)

    # skip if no file
    if(length(files) == 0) next

    # keys
    key_a <- "latency_local"
    key_b <- "latency_remote"

    # data
    title <- titles[i]
    local <- lapply(files, function(f) { json(c(f))[[key_a]] })
    remote <- lapply(files, function(f) { json(c(f))[[key_b]] })
    lines <- lapply(1:length(labels), function(j) { sum(local[[j]]) + sum(remote[[j]]) })

    # wrto (state-based)
    state_based <- lines[[1]]
    lines <- map(lines, function(v) { v / state_based })

    # plot bars
    y_min <- 0
    plot_bars(title, lines, y_min, colors, angles, densities)
  }

  # axis labels
  y_axis_label("Total processing time wrto State-based")

  par(op) # Leave the last plot
  op <- par(usr=c(0,1,0,1), # Reset the coordinates
            xpd=NA)         # Allow plotting outside the plot region

  # legend
  legend(
    -.08, # x
    -.2,  # y 
    cex=0.92,
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
