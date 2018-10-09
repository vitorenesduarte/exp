source("util.R")
source("generic.R")

# draw!
main <- function() {
  output_file <- "second.png"

  cluster <- "ls -d processed/* | grep gset~partialmesh | grep -v scuttlebutt | grep -v False~True | grep -v True~False"

  labels <- c(
    "State-based",
    "Delta-based",
    "This paper"
  )

  # avoid scientific notation
  options(scipen=999)

  # open device
  png(filename=output_file, width=200, height=410, res=110)
5
  # change outer margins
  op <- par(
    oma=c(5,2,2,0),   # room for the legend
    mfrow=c(1,1),      # 2x4 matrix
    mar=c(1.5,1.5,0.5,0.5) # spacing between plots
  )

  # style stuff
  colors <- c(
    "snow4",
    "springgreen4",
    "gray22"
  )
  angles <- c(0, 135, 45)
  densities <- c(0, 15, 45)

  files <- system(cluster, intern=TRUE)

  # skip if no file
  if(length(files) == 0) next

  # keys
  key <- "processing"

  # data
  title <- ""
  lines <- lapply(files, function(f) { json(c(f))[[key]] })

  # wrto (state-based)
  if(length(lines) == length(labels)) {
    state_based <- lines[[1]]
    lines <- map(lines, function(v) { v / state_based })

    # plot bars
    y_min <- 0
    plot_bars(title, lines, y_min, colors, angles, densities, 0.8)
  }

  # axis labels
  y_axis_label("Processing time ratio wrto State-based")

  par(op) # Leave the last plot
  op <- par(usr=c(0,1,0,1), # Reset the coordinates
            xpd=NA)         # Allow plotting outside the plot region

  # legend
  legend(
    -.65, # x
    -.11,  # y 
    cex=0.95,
    legend=labels,
    pch=c(1:10),
    col=colors,
    horiz=FALSE,
    box.col=NA # remove box
  )

  # close device
  dev.off()
}

main()
warnings()
