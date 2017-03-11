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
  for(package in packages) {
    load(package)
  }
}

# Given a log directory name (e.g. "logs/1489232739370465988"
# returns only the timestamp
get_timestamp <- function(dir) {
  unlist(strsplit(dir, "/"))[2]
}

# main function
main <- function() {
  # install and load needed packages
  packages <- c("hash")
  load_dependencies(packages)

  # list directories
  log_dir = "logs"
  dirs <- setdiff(list.dirs(log_dir), c(log_dir))

  h <- hash()
  for(dir in dirs) {
    logs <- list.files(dir)
    timestamp <- get_timestamp(dir)
    .set(h, timestamp, logs)
  }

  print(h)
}

main()
