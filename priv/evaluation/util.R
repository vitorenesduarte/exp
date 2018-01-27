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
  labels[["state_based_none_undefined_undefined"]] = "State-Based"
  labels[["state_based_state_driven_undefined_undefined"]] = "State-Driven"
  labels[["state_based_digest_driven_undefined_undefined"]] = "Digest-Driven"
  labels[["delta_based_none_False_False"]] = "Delta-Based"
  labels[["delta_based_none_True_False"]] = "Delta-Based RR"
  labels[["delta_based_none_False_True"]] = "Delta-Based BP"
  labels[["delta_based_none_True_True"]] = "Delta-Based BP+RR"
  labels[["delta_based_state_driven_True_True"]] = "Delta-Based BP+RR SD"
  labels[["delta_based_digest_driven_True_True"]] = "Delta-Based BP+RR DD"
  lapply(
    keys,
    function(key) {
      parts <-  strsplit(key, "~")[[1]]

      mode <- paste(
          parts[c(6, 7, 8, 9)],
          collapse="_"
      )
      labels[[mode]]
    }
  )
}

# get the plot title
get_title <- function(key) {
  titles = list()
  titles[["awset"]] = "AWSet"
  titles[["gset"]] = "GSet"
  titles[["gcounter"]] = "GCounter"

  titles[[key]]
}
