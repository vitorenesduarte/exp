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

# Given a directory with all the metrics (from all runs)
# return a map from run to list of files
get_metric_files <- function(metrics_dir) {
  m <- list()

  dirs <- setdiff(
    list.dirs(metrics_dir),
    c(metrics_dir)
  )

  for(dir in dirs) {
    metrics <- list.files(dir)
    m[[dir]] <- metrics
  }

  m
}

#
average <- function(m) {
  for(dir in names(m)) {
    for(file in m[[dir]]) {
      path <- paste(dir, file, sep="/")
      a <- fromJSON(file=path)
      print(a)
    }
  }
}

# main function
main <- function() {
  # install and load needed packages
  packages <- c("rjson")
  load_dependencies(packages)

  # get all files
  metrics_dir = "metrics"
  m <- get_metric_files(metrics_dir)
  print(m)

  # average
  average(m)
}

main()
