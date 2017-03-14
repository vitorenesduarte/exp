# Given a package name,
# install it (if not already installed) and load it.
load <- function(package) {
  mirror <- "http://cran.us.r-project.org"

  if(!require(package, character.only=TRUE)) {
    install.packages(package, repos=mirror, dependencies=TRUE)
    require(package, character.only=TRUE)
  }
}

# Load a list of dependencies.
load_dependencies <- function(packages) {
  Map(load, packages)
}

# given the directory name and the file name
# return the json file
json <- function(dir, file) {
  file_path <- paste(dir, file, sep="/")
  fromJSON(file_path)
}

# compute label name given key.
get_labels <- function(keys) {
  labels = list()
  labels[["state_based_False"]] = "State-Based"
  labels[["delta_based_False"]] = "Delta-Based"
  labels[["delta_based_True"]] = "Delta-Based+"
  lapply(
    keys,
    function(key) {
      mode_and_jd <- paste(
          strsplit(key, "-")[[1]][5:6],
          collapse="_"
      )

      labels[[mode_and_jd]]
    }
  )
}

# draw!
draw <- function(dir) {
  files <- list.files(dir)

  # read all files
  lines <- lapply(
    files,
    function(file) {
      json(dir, file)
    }
  )

  # find the y max for all
  ymaximums = lapply(lines, max)
  maxy = Reduce(max, ymaximums)
  maxx = Reduce(max, lapply(lines, length))

  # open device
  png(filename="r.png")

  # draw the first line
  first_file <- files[[1]]
  first_line <- json(dir, first_file)

  # offset for labels
  offset = 15

  plot(
    first_line,
    type="l", # lines
    xlim=c(0, maxx + offset), # max x
    ylim=c(0, maxy), # max y
    xlab="Time (s)",, # x axis label
    ylab="Transmission (B)" # y axis label
  )

  # draw the rest of the lines
  for(i in 2:length(files)) { 
    file <- files[[i]]
    line <- json(dir, file)
    lines(line)
  }
  
  text(
    x=rep(maxx, length(lines)),
    y=ymaximums,
    pos=4,
    labels=get_labels(files)
  )

  # close device
  dev.off()
}

# main function
main <- function() {
  # install and load needed packages
  packages <- c("jsonlite")
  load_dependencies(packages)

  # draw!
  metrics_dir <- "processed"
  draw(metrics_dir)
}

main()
