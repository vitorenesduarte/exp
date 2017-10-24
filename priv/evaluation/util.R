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

# given the vector of subpaths,
# return the json file
json <- function(v) {
  load("jsonlite")
  file_path <- paste(v, collapse="/")
  fromJSON(file_path)
}

# compute label name given key.
get_labels <- function(keys) {
  labels = list()
  labels[["trcb_Dots~trcb"]] = "Dots"
  labels[["trcb_VV~trcb"]] = "VVs"
  lapply(
    keys,
    function(key) {
      parts <-  strsplit(key, "~")[[1]]

      mode <- paste(
          parts[c(1, 2)],
          collapse="_"
      )
    }
  )
}

# get the plot title
get_title <- function(key) {
  titles = list()
  titles[["trcb"]] = "trcb"
  titles[["trcb_Dots"]] = "Dots"
  titles[["trcb_VV"]] = "VVs"
  
  titles[[key]]
}
