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

# Remove element from list
rm_elem <- function(e, l) {
  setdiff(l, c(e))
}

# Given a directory with all the metrics (from all runs)
# return a map from run to list of files
get_metric_files <- function(metrics_dir) {
  m <- list()

  dirs <- rm_elem(metrics_dir, list.dirs(metrics_dir))

  for(dir in dirs) {
    metrics <- list.files(dir)
    m[[dir]] <- metrics
  }

  m
}

# given the directory name and the file name
# return the json file
json <- function(dir, file) {
  file_path <- paste(dir, file, sep="/")
  fromJSON(file_path)
}

# create a key from rsg file
key <- function(rsg) {
  v <- c()
  for(key in names(rsg)) {
    v <- c(v, key)
    v <- c(v, rsg[[key]])
  }
  paste(v, collapse="_")
}

#
average <- function(m) {
  rsg_file = "rsg.json"
  to_average = list()

  for(dir in names(m)) {
    files <- rm_elem(rsg_file, m[[dir]])

    rsg <- json(dir, rsg_file)
    start_time <- rsg[["start_time"]]
    rsg[["start_time"]] <- NULL
    key <- key(rsg)

    cat("start_time ", start_time, "\n")
    cat("key ", key, "\n")

    for(file in files) {
      m <-json(dir, file)
      types <- names(m)
      for(type in types) {
        # subtract start time to all timestamps
        m[[type]][["timestamp"]] <-
          m[[type]][["timestamp"]] - start_time

        # save in to_average
        current <- to_average[[key]][[type]]
        new <- c(current, m[[type]])
        to_average[[key]][[type]] <- new
      }
    }

    for(a in names(to_average)) {
      print(a)
      print(length(to_average[[a]]))
      for(b in names(to_average[[a]])) {
        print(b)
        print(length(to_average[[a]][[b]]))
        for(c in to_average[[a]][[b]]) {
          print(c)
        }
      }
    }

    "all_timestamps <- c()
    all_json <- list()

    for(file in files) {
      m <- json(dir, file)
      all_json[[path]] <- m

      all_timestamps <- Reduce(
        function(acc, type) {
          union(
            acc,
            "
            #m[[type]][["timestamp"]]
            "
          )
        },
        names(m),
        all_timestamps
      )
    }

    print(all_timestamps)
    all_timestamps <- sort(all_timestamps)

    print(all_timestamps)
    print(length(all_timestamps))
    "
  }
}

# main function
main <- function() {
  # install and load needed packages
  packages <- c("jsonlite")
  load_dependencies(packages)

  # get all files
  metrics_dir = "metrics"
  m <- get_metric_files(metrics_dir)
  print(m)

  # average
  average(m)
}

main()
