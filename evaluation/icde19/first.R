source("util.R")
source("generic.R")

# draw!
main <- function() {
  output_file <- "first.png"

  cluster <- "ls -d processed/* | grep 0~gset~partialmesh~15 | grep -v scuttlebutt | grep -v False~True | grep -v True~False"

  labels <- c(
    "State-based",
    "Delta-based",
    "This paper"
  )

  # avoid scientific notation
  options(scipen=999)

  # open device
  png(filename=output_file, width=600, height=400, res=110)
5
  # change outer margins
  op <- par(
    oma=c(4,2,0,0),   # room for the legend
    mfrow=c(1,1),      # 2x4 matrix
    mar=c(1.5,1.5,0.5,0.5) # spacing between plots
  )

  # style stuff
  colors <- c(
    "snow4",
    "springgreen4",
    "gray22"
  )

  files <- system(cluster, intern=TRUE)

  # skip if no file
  if(length(files) == 0) next

  # keys
  key_x <- "transmission_compressed_x"
  key_y <- "transmission_compressed"

  # data
  title <- ""
  lines_x <- lapply(files, function(f) { json(c(f))[[key_x]] })
  lines_y <- lapply(files, function(f) { json(c(f))[[key_y]] })

  # plot lines
  plot_lines(title, lines_x, lines_y, colors)

  # axis labels
  x_axis_label("Time (s)")
  y_axis_label("Number of set elements")

  par(op) # Leave the last plot
  op <- par(usr=c(0,1,0,1), # Reset the coordinates
            xpd=NA)         # Allow plotting outside the plot region

  # legend
  legend(
    0.05, # x
    -.32,  # y 
    cex=0.95,
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
