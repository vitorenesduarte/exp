source("util.R")
source("generic.R")

get_lines <- function(clusters, key, file_index, first_entry, last_entry) {
  any_non_zero <- 0.1
  map(clusters, function(cluster) {
    files <- system(cluster, intern=TRUE)

    # skip if no file
    if(length(files) == 2) {
      lines <- json(c(files[file_index]))[[key]]
      entries <- lines[first_entry:last_entry]
      sum(entries) / length(entries) / 1000
    }
    else { any_non_zero }
  })
}

get_all_lines <- function(clusters, key, first_entry, last_entry) {
  lines_y <- list()
  lines_y[[1]] <- get_lines(clusters, key, 1, first_entry, last_entry)
  lines_y[[2]] <- get_lines(clusters, key, 2, first_entry, last_entry)
  lines_y
}

# draw!
main <- function() {
  output_file <- "retwis.png"

  clusters <- c(
    "ls -d processed/* | grep ~50~0~retwis",
    "ls -d processed/* | grep ~75~0~retwis",
    "ls -d processed/* | grep ~100~0~retwis",
    "ls -d processed/* | grep ~125~0~retwis",
    "ls -d processed/* | grep ~150~0~retwis"
  )
  labels <- c(
    "Delta-based",
    "Delta-based BP+RR"
  )

  # avoid scientific notation
  options(scipen=999)

  # open device
  png(filename=output_file, width=850, height=700, res=130)

  # change outer margins
  op <- par(
    oma=c(5,2,0.5,0),   # room for the legend
    mfrow=c(2,2),      # 2x4 matrix
    mar=c(1.5,2.5,2,1) # spacing between plots
  )

  # style stuff
  colors <- c(
    "springgreen4",
    "gray22"
  )

  coefs <- c(0.5, 0.75, 1, 1.25, 1.5)
  lines_x <- list()
  lines_x[[1]] <- coefs
  lines_x[[2]] <- coefs
  x_lab <- "Zipf coefficients"

  # first plot
  key <- "transmission_term_size"
  y_lab <- "Transmission (GB/s)"
  lines_y_1 <- get_all_lines(clusters, key, 0, 25)
  lines_y_2 <- get_all_lines(clusters, key, 25, 50)

  plot_lines_retwis(lines_x, lines_y_1, colors,
                    y_lab=y_lab,
                    y_max=2,
                    lwd=2)
  title("0%-50%", cex.main=1.3)

  plot_lines_retwis(lines_x, lines_y_2, colors,
                    y_max=2,
                    lwd=2)
  title("50%-100%", cex.main=1.3)
  print(lines_y_2)

  # second plot
  key <- "memory_term_size"
  y_lab <- "Avg. Memory (GB)"
  lines_y_1 <- get_all_lines(clusters, key, 0, 25)
  lines_y_2 <- get_all_lines(clusters, key, 25, 50)

  plot_lines_retwis(lines_x, lines_y_1, colors,
                    y_lab=y_lab,
                    y_max=2,
                    lwd=2)

  plot_lines_retwis(lines_x, lines_y_2, colors,
                    y_max=2,
                    lwd=2)
  print(lines_y_2)

  x_axis_label(x_lab)

  par(op) # Leave the last plot
  op <- par(usr=c(0,1,0,1), # Reset the coordinates
            xpd=NA)         # Allow plotting outside the plot region

  # legend
  legend(
    "bottom",
    # 0.1, # x
    # 0,  # y 
    inset=-.25,
    cex=1,
    legend=labels,
    pch=c(3,6),
    col=colors,
    ncol=2,
    box.col=NA # remove box
  )

  # close device
  dev.off()
}

main()
warnings()
