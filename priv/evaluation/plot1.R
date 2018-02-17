source("util.R")
source("generic.R")

# draw!
main <- function() {
  output_file <- "plot1.png"

  clusters <- c(
    "ls -d processed/* | grep gcounter~tree",
    "ls -d processed/* | grep gset~tree",
    "ls -d processed/* | grep gset~tree",
    "ls -d processed/* | grep gset~tree | grep delta_based | grep -v False~True | grep -v True~False",
    "ls -d processed/* | grep gcounter~partialmesh",
    "ls -d processed/* | grep gset~partialmesh",
    "ls -d processed/* | grep gset~partialmesh",
    "ls -d processed/* | grep gset~partialmesh | grep delta_based | grep -v False~True | grep -v True~False"
  )
  ## 0 transmission
  types <- c(0, 0, 1, 2, 0, 0, 1, 2)
  titles <- c(
    "GCounter - Tree",
    "GSet - Tree",
    "GSet - Tree",
    "GSet - Tree",
    "GCounter - Mesh",
    "GSet - Mesh",
    "GSet - Mesh",
    "GSet - Mesh"
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
  png(filename=output_file, width=2600, height=1200, res=240)

  # change outer margins
  op <- par(
    oma=c(3,2,0,0),   # room for the legend
    mfrow=c(2,4),      # 2x4 matrix
    mar=c(4,3,2,2) # spacing between plots
  )

  # style stuff
  colors <- c(
    "snow4",
    "steelblue4",
    "springgreen4",
    "darkorange1",
    "red4"
  )
  angles <- c(0, 135, 45, 135, 45)
  densities <- c(0, 10, 20, 40, 60)

  for(i in 1:length(clusters)) {
    files <- system(clusters[i], intern=TRUE)

    # skip if no file
    if(length(files) == 0) next

    type <- types[i]
    title <- titles[i]

    if(type == 0) { # transmission
      # keys
      key_x <- "transmission_1_compressed_x"
      key_y <- "transmission_1_compressed"

      # data
      lines_x <- lapply(files, function(f) { json(c(f))[[key_x]] })
      lines_y <- lapply(files, function(f) { json(c(f))[[key_y]] })

      # plot lines
      plot_lines(lines_x, lines_y, colors)

      # axis labels
      x_axis_label("Time (s)")
      y_axis_label("Transmission")
    }

    if(type == 1) { # memory
      # keys
      key_a = "memory_crdt"
      key_b = "memory_algorithm"

      # data
      lines <- map(
        files,
        function(f) {
          data <- json(c(f))
          avg_a <- mean(data[[key_a]])
          avg_b <- mean(data[[key_b]])
          avg_a + avg_b
        }
      )

      # min (state-based)
      state_based = Reduce(min, lines)
      lines <- map(lines, function(v) { v / state_based })

      # plot bars
      y_min <- 0.5
      plot_bars(lines, y_min, colors, angles, densities)

      # axis labels
      y_axis_label("Memory ratio wrto State-based")
    }

    if(type == 2) { # cdf
      # keys
      key <- "latency"

      # data
      lines <- lapply(files, function(f) { json(c(f))[[key]] })

      # plot cdf
      plot_cdf(lines, colors[c(2, 5)])

      # axis labels
      x_axis_label("Processing (ms)")
      y_axis_label("CDF")
    }

    # title
    title(title, line=0.5)
  }

  par(op) # Leave the last plot

  op <- par(usr=c(0,1,0,1), # Reset the coordinates
            xpd=NA)         # Allow plotting outside the plot region

  # legend
  # plot shaded squares
  pos <- legend(
    .1, # x
    -.19,  # y 
    cex=0.8,
    legend=labels,
    angle=angles,
    density=densities,
    fill=colors,
    horiz=TRUE,
    box.col=NA # remove box
  )

  # plot symbols
  points(
    x=pos$text$x - 0.033,
    y=pos$text$y,
    pch=c(1:10),
    col=colors
  )

  # close device
  dev.off()
}

main()
warnings()
