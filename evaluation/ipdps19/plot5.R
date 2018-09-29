source("util.R")
source("generic.R")

# draw!
main <- function() {
  output_file <- "plot5.png"

  clusters <- c(
    "ls -d processed/* | grep -v False~True | grep -v True~False | grep 10~gmap~partialmesh",
    "ls -d processed/* | grep -v False~True | grep -v True~False | grep 100~gmap~partialmesh"
  )
  titles <- c(
    "GMap 10%",
    "GMap 100%"
  )
  labels <- c(
    "State-based",
    "Scuttlebutt",
    "Delta-based",
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
    title_a <- paste(titles[i], "Sender", sep=" - ")
    title_b <- paste(titles[i], "Receiver", sep=" - ")
    lines_a <- lapply(files, function(f) { json(c(f))[[key_a]] })
    lines_b <- lapply(files, function(f) { json(c(f))[[key_b]] })

    # plot cdf
    plot_box(title_a, lines_a, colors)
    plot_box(title_b, lines_b, colors)
  }

  # axis labels
  x_axis_label("Processing (ms)")
  y_axis_label("CDF")

  par(op) # Leave the last plot
  op <- par(usr=c(0,1,0,1), # Reset the coordinates
            xpd=NA)         # Allow plotting outside the plot region

  # legend
  legend(
    "bottom",
    inset=-1.25,
    # 0, # x
    # -1,  # y 
    cex=0.92,
    legend=labels,
    col=colors,
    lty=c(1:3),
    lwd=c(1:3),
    horiz=TRUE,
    box.col=NA # remove box
  )

  # close device
  dev.off()
}

main()
warnings()
