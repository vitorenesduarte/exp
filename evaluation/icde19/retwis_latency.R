source("util.R")
source("generic.R")

get_lines <- function(cluster, key, file_index) {
  files <- system(cluster, intern=TRUE)

  # skip if no file
  if(length(files) == 2) {
    map(json(c(files[file_index]))[["latency"]][[key]],
        function(micro) { micro / 1000 })
  }
  else { c() }
}

get_all_lines <- function(cluster, key) {
  lines_y <- list()
  lines_y[[1]] <- get_lines(cluster, key, 1)
  lines_y[[2]] <- get_lines(cluster, key, 2)
  lines_y
}

# draw!
main <- function() {
  output_file <- "retwis_latency.png"

  cluster <- "ls -d processed/* | grep ~100~0~retwis"
  labels <- c(
    "Delta-based",
    "Delta-based BP+RR"
  )

  # avoid scientific notation
  options(scipen=999)

  # open device
  png(filename=output_file, width=850, height=320, res=130)

  # change outer margins
  op <- par(
    oma=c(5,3,0,0),   # room for the legend
    mfrow=c(1,3),      # 2x4 matrix
    mar=c(2,2,3,1) # spacing between plots
  )

  # style stuff
  colors <- c(
    "springgreen4",
    "gray22"
  )

  # follow plot
  key <- "follow"
  lines <- get_all_lines(cluster, key)
  title <- "Follow"

  # plot cdf
  y_max <- .94
  y_step <- 0.01
  plot_cdf(title, lines, colors, y_max, y_step)

  # post plot
  key <- "post"
  lines <- get_all_lines(cluster, key)
  title <- "Post"

  # plot cdf
  y_max <- .94
  y_step <- 0.01
  plot_cdf(title, lines, colors, y_max, y_step)

  # update plot
  key <- "timeline"
  lines <- get_all_lines(cluster, key)
  title <- "Timeline"

  # plot cdf
  y_max <- .94
  y_step <- 0.01
  plot_cdf(title, lines, colors, y_max, y_step)

  # axis labels
  x_axis_label("Latency (ms)")
  y_axis_label("CDF")

  par(op) # Leave the last plot
  op <- par(usr=c(0,1,0,1), # Reset the coordinates
            xpd=NA)         # Allow plotting outside the plot region

  # legend
  legend(
    "bottom",
    inset=-1.6,
    # 0, # x
    # -1,  # y 
    cex=0.92,
    legend=labels,
    col=colors,
    lty=c(1:4),
    lwd=c(1:4),
    horiz=TRUE,
    box.col=NA # remove box
  )

  # close device
  dev.off()
}

main()
warnings()
