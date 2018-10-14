source("util.R")
source("generic.R")

get_lines <- function(clusters, key, file_index) {
  map(clusters, function(cluster) {
    files <- system(cluster, intern=TRUE)

    # skip if no file
    if(length(files) == 2) { json(c(files[file_index]))[[key]] }
    else { any_non_zero }
  })
}

get_all_lines <- function(clusters, key) {
  lines_y <- list()
  lines_y[[1]] <- (get_lines(clusters, key, 1) / get_lines(clusters, key, 2) * 100) - 100
  lines_y
}

# draw!
main <- function() {
  output_file <- "retwis_processing.png"

  clusters <- c(
    "ls -d processed/* | grep ~25~0~retwis",
    "ls -d processed/* | grep ~50~0~retwis",
    "ls -d processed/* | grep ~75~0~retwis",
    "ls -d processed/* | grep ~100~0~retwis",
    "ls -d processed/* | grep ~125~0~retwis",
    "ls -d processed/* | grep ~150~0~retwis"
  )

  # avoid scientific notation
  options(scipen=999)

  # open device
  png(filename=output_file, width=550, height=300, res=130)

  # change outer margins
  op <- par(
    oma=c(4,4,1,2),   # room for the legend
    mfrow=c(1,1),      # 2x4 matrix
    mar=c(0, 0, 0, 0) # spacing between plots
  )

  # style stuff
  colors <- c(
    "springgreen4"
  )

  coefs <- c(0.25, 0.5, 0.75, 1, 1.25, 1.5)
  lines_x <- list()
  lines_x[[1]] <- coefs
  x_lab <- "Zipf coefficients"

  # first plot
  key <- "processing"
  lines_y <- get_all_lines(clusters, key)
  y_lab <- "Processing overhead (%)"

  plot_lines_retwis(lines_x, lines_y, colors,
                    x_lab=x_lab,
                    y_lab=y_lab,
                    log="")

  # close device
  dev.off()
}

main()
warnings()
