source("util.R")
source("generic.R")

# draw!
main <- function() {
  output_file <- "processing.png"

  clusters <- c(
    "ls -d processed/* | grep 0~gset~partialmesh~15",
    "ls -d processed/* | grep 0~gcounter~partialmesh~15",
    "ls -d processed/* | grep 10~gmap~partialmesh~15",
    "ls -d processed/* | grep 100~gmap~partialmesh~15"
  )
  titles <- c(
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
  png(filename=output_file, width=800, height=650, res=130)

  # change outer margins
  op <- par(
    oma=c(3.5,2,0,0),   # room for the legend
    mfrow=c(2,2),      # 2x4 matrix
    mar=c(2,2,2,1) # spacing between plots
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
    key <- "processing"

    # data
    title <- titles[i]
    lines <- lapply(files, function(f) { json(c(f))[[key]] })

    # wrto (r)
    if(length(lines) == length(labels)) {
      rr_index <- length(labels)
      rr <- lines[[rr_index]]
      lines <- map(lines, function(v) { v / rr })

      # plot bars
      y_min <- 0
      plot_bars(title, lines, y_min, colors, angles, densities)
    }
  }

  # axis labels
  y_axis_label("CPU time ratio wrto BP+RR")

  par(op) # Leave the last plot
  op <- par(usr=c(0,1,0,1), # Reset the coordinates
            xpd=NA)         # Allow plotting outside the plot region

  # legend
  legend(
    0.1, # x
    -.06,  # y 
    cex=1,
    legend=labels,
    angle=angles,
    density=densities,
    fill=colors,
    ncol=2,
    box.col=NA # remove box
  )

  # close device
  dev.off()
}

main()
warnings()
