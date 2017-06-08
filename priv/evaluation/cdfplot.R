source("util.R")

# draw!
splot <- function(dir, keys, output_file, label, logx) {
  load_dependencies(c("Hmisc"))
  files <- list.files(dir)

  # read all files
  jsons <- lapply(
    files,
    function(file) {
      json(c(dir, file))
    }
  )

  # topology
  topology <- "hyparview"

  # clusters
  clusters <- c(
    paste("gcounter", topology, sep="~"),
    paste("gset", topology, sep="~"),
    paste("awset", topology, sep="~")
  )
  titles <- c(
    "GCounter - Local",
    "GCounter - Remote",
    "GSet - Local",
    "GSet - Remote",
    "AWSet - Local",
    "AWSet - Remote"
  )
  title_index <- 1

  # avoid scientific notation
  options(scipen=999)

  # open device
  png(filename=output_file, width=1600, height=1600, res=240)
  #png(filename=output_file, res=100)

  print(output_file)

  # change outer margins
  op <- par(
    oma=c(10,3,0,0),   # room for the legend
    mfrow=c(3,2),      # 3x3 matrix
    mar=c(3.5, 2, 3, 3) # spacing between plots
  )

  # style stuff
  colors <- c(
    "snow3",
    "steelblue4",
    "springgreen4",
    "darkorange1",
    "darkgoldenrod1"
  )
  line_types <- c(1:length(colors))

  for(c in 1:length(clusters)) {
    for(key in keys) {
      cluster <- clusters[c]
      indexes <- c()

      # get indexes for this cluster
      for(f in 1:length(files)) {
        file <- files[f]

        if(regexpr(cluster, file) > 0) {

          # if any of this, hide
          is_digest = regexpr("digest", file) > 0
          is_gset_or_gcounter = regexpr("gset", file) > 0 || regexpr("gcounter", file) > 0

          if(!(is_digest && is_gset_or_gcounter)) {
            indexes[length(indexes) + 1] <- f
          }
        }
      }

      cluster_jsons <- jsons[indexes]
      # get lines
      ls <- lapply(
        cluster_jsons,
        function(json) {
          json[[key]]
        }
      )

      # find min and max
      xminimums <- lapply(ls, min)
      xmaximums <- lapply(ls, max)
      minx <- Reduce(min, xminimums)
      maxx <- Reduce(max, xmaximums)

      minx <- if(logx && minx == 0) 0.001 else minx
      logaxis <- if(logx) "x" else ""

      # configure plot
      Ecdf(
        range(1000),
        xlim=c(0.001, 40),
        xlab="",
        ylab="",
        log=logaxis
      )
      # axis labels
      mtext(
        side=1,
        text=label,
        line=2.5,
        cex=.8 # size
      )
      mtext(
        side=2,
        text="CDF",
        line=2.5,
        cex=.8 # size
      )
      title(titles[title_index], line=.8)

      # increment the title index
      title_index <- title_index + 1

      # add plot lines
      for(l in 1:length(ls)) {
        Ecdf(
          ls[[l]],
          col=colors[[l]],
          lty=line_types[[l]],
          lwd=2.5,
          add=TRUE
        )
      }
    }
  }

  par(op) # Leave the last plot

  op <- par(usr=c(0,1,0,1), # Reset the coordinates
            xpd=NA)         # Allow plotting outside the plot region

  # legend
  legend(
    1.89, 0,
    #"bottom",
    #inset=-.15,
    cex=0.8,
    legend=get_labels(files[indexes]),
    col=colors,
    lty=line_types,
    lwd=2.5,
    box.col=NA # remove box
  )

  # close device
  dev.off()
}
