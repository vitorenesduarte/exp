source("util.R")
source("generic.R")

get_lines <- function(clusters, key, file_index,
                      first_entry=0) {
  any_non_zero <- 0.1
  last_entry <- 50

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

get_all_lines <- function(clusters, key,
                          first_entry=0) {
  lines_y <- list()
  lines_y[[1]] <- get_lines(clusters, key, 1,
                            first_entry=first_entry)
  lines_y[[2]] <- get_lines(clusters, key, 2,
                            first_entry=first_entry)
  lines_y
}

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
  labels <- c(
    "Delta-based",
    "Delta-based BP+RR"
  )

  # avoid scientific notation
  options(scipen=999)

  # open device
  png(filename=output_file, width=850, height=400, res=130)

  # change outer margins
  op <- par(
    oma=c(4,0.5,1,0),   # room for the legend
    mfrow=c(1,2),      # 2x4 matrix
    mar=c(2,3,0.5,1) # spacing between plots
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
  x_lab <- "Zipf coefficients"

  # first plot
  key <- "transmission_term_size"
  lines_y <- get_all_lines(clusters, key)
  y_lab <- "Transmission (GB/s)"

  plot_lines_retwis(lines_x, lines_y, colors,
                    x_lab=x_lab,
                    y_lab=y_lab)

  # second plot
  key <- "memory_term_size"
  lines_y <- get_all_lines(clusters, key, 25)
  y_lab <- "Avg. Memory (GB)"
  plot_lines_retwis(lines_x, lines_y, colors,
                    x_lab=x_lab,
                    y_lab=y_lab)

  # # title
  # text <- "Retwis"
  # title(
  #   text,
  #   cex.main=1.3,
  #   outer=TRUE
  # )

  par(op) # Leave the last plot
  op <- par(usr=c(0,1,0,1), # Reset the coordinates
            xpd=NA)         # Allow plotting outside the plot region

  # legend
  legend(
    0.1, # x
    0.4,  # y 
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
