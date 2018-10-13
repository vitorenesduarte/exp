source("util.R")
source("generic.R")

# draw!
main <- function() {
  output_file <- "metadata.png"

  clusters <- c(
    "ls -d processed/* | grep partialmesh~8",
    "ls -d processed/* | grep partialmesh~16",
    "ls -d processed/* | grep partialmesh~32"
  )
  ## 0 transmission
  titles <- c(
    "8 Replicas",
    "16 Replicas",
    "32 Replicas"
  )
  labels <- c(
    "Scuttlebutt",
    "Delta-based BP+RR"
  )

  # avoid scientific notation
  options(scipen=999)

  # open device
  png(filename=output_file, width=800, height=350, res=130)

  # change outer margins
  op <- par(
    oma=c(2.5,2,0,0),   # room for the legend
    mfrow=c(1, 3),      # 2x4 matrix
    mar=c(2,2,2,1) # spacing between plots
  )

  # style stuff
  colors <- c(
    "steelblue4",
    "gray22"
  )
  angles <- c(0, 45)
  densities <- c(0, 45)

  for(i in 1:length(clusters)) {
    files <- system(clusters[i], intern=TRUE)

    # skip if no file
    if(length(files) == 0) next

    # keys
    key <- "transmission_crdt"
    key <- "transmission_metadata"

    id_size = 8

    # data
    title <- titles[i]
    lines <- map(files, function(f) {
      entries <- json(c(f))[[key]]
      sum(entries) / length(entries) * 8
    })

    # plot bars
    y_min <- 0
    plot_bars(title, lines, y_min, colors, angles, densities, 38000)
  }

  # axis labels
  y_axis_label("Avg. metadata (KB)")

  par(op) # Leave the last plot
  op <- par(usr=c(0,1,0,1), # Reset the coordinates
            xpd=NA)         # Allow plotting outside the plot region

  # legend
  legend(
    0.1, # x
    -.6,  # y 
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
