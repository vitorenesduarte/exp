source("util.R")
source("generic.R")

# draw!
main <- function() {
  output_file <- "retwis.png"

  clusters <- c(
    "ls -d processed/* | grep ~25~0~retwis",
    "ls -d processed/* | grep ~50~0~retwis",
    "ls -d processed/* | grep ~75~0~retwis",
    "ls -d processed/* | grep ~100~0~retwis",
    "ls -d processed/* | grep ~125~0~retwis",
    "ls -d processed/* | grep ~150~0~retwis"
  )
  title <- "Retwis"
  labels <- c(
    "Delta-based",
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
    "springgreen4",
    "gray22"
  )

  coefs <- c(0.25, 0.5, 0.75, 1, 1.25, 1.5)
  lines_x <- list()
  lines_x[[1]] <- coefs
  lines_x[[2]] <- coefs

  key <- "transmission"

  lines_y_1 <- map(clusters, function(cluster) {
    files <- system(cluster, intern=TRUE)

    # skip if no file
    if(length(files) == 2) { sum(json(c(files[1]))[[key]]) }
    else { 0 }
  })

  lines_y_2 <- map(clusters, function(cluster) {
    files <- system(cluster, intern=TRUE)

    # skip if no file
    if(length(files) == 2) { sum(json(c(files[2]))[[key]]) }
    else { 0 }
  })

  lines_y <- list()
  lines_y[[1]] <- lines_y_1
  lines_y[[2]] <- lines_y_2

  plot_lines_log(title, lines_x, lines_y, colors)

  # axis labels
  x_axis_label("Zipf coefficients")
  y_axis_label("Transmission (GB)")

  par(op) # Leave the last plot
  op <- par(usr=c(0,1,0,1), # Reset the coordinates
            xpd=NA)         # Allow plotting outside the plot region

  # legend
  legend(
    0.1, # x
    -.06,  # y 
    cex=1,
    legend=labels,
    pch=c(1:10),
    col=colors,
    ncol=2,
    box.col=NA # remove box
  )

  # close device
  dev.off()
}

main()
warnings()
