source("util.R")
source("generic.R")

TO_KEEP <- "'(110|220|230|350|460|470|480|490)'"

# draw!
main <- function() {
  output_file <- "gmap.png"

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
  clusters <- map(clusters, function(c) {
      paste(c, " | grep -E ", TO_KEEP, sep="")
  })
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
    "Scuttlebutt-GC",
    "Op-based",
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
    "darkgoldenrod",
    "steelblue4",
    "yellow3",
    "springgreen4",
    "darkorange1",
    "red4",
    "gray22"
  )
  pch <- c(1,7,8,2,3,4,5,6)

  for(i in 1:length(clusters)) {
    files <- system(clusters[i], intern=TRUE)

    # skip if no file
    if(length(files) == 0) next

    # keys
    key_x <- "transmission_compressed_x"
    key_y <- "transmission_compressed"

    # data
    title <- titles[i]
    lines_x <- lapply(files, function(f) { json(c(f))[[key_x]] })
    lines_y <- lapply(files, function(f) { json(c(f))[[key_y]] })

    # metadata info
    # metadata_ratio <- map(
    #   files,
    #   function(f) {
    #     j <- json(c(f))
    #     r <- sum(j[["transmission_metadata"]]) / sum(j[["transmission"]])
    #     round(r, 3) * 100
    #   }
    # )
    # print(metadata_ratio)

    avgs <- map(
      files,
      function(f) {
        j <- json(c(f))
        sum(j[[key_y]]) / length(j[[key_y]])
      }
    )
    print(title)
    print(paste("scuttlebutt: ", 100 - 100 * round(avgs[[2]] / avgs[[1]], 2)))
    print(paste("scuttlebutt-gc: ", 100 - 100 * round(avgs[[3]] / avgs[[1]], 2)))
    print(paste("op-based: ", 100 - 100 * round(avgs[[4]] / avgs[[1]], 2)))
    print(paste("delta-based bp+rr: ", 100 - 100 * round(avgs[[8]] / avgs[[1]], 2)))

    # plot lines
    plot_lines(title, lines_x, lines_y, colors,
               pch=pch)
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
    pch=pch,
    text.width=c(0,0.09,0.085,0.092,0.087,0.089,0.095,0.098),
    col=colors,
    horiz=TRUE,
    box.col=NA # remove box
  )

  # close device
  dev.off()
}

main()
warnings()
