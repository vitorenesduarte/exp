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
  labels[["state_based_none_False_False"]] = "State-Based"
  labels[["state_based_state_driven_False_False"]] = "State-Driven"
  labels[["state_based_digest_driven_False_False"]] = "Digest-Driven"
  labels[["delta_based_none_False_False"]] = "Delta-Based"
  labels[["delta_based_none_True_False"]] = "Delta-Based (Remove Redundant)"
  labels[["delta_based_none_False_True"]] = "Delta-Based (Back-Propagation)"
  labels[["delta_based_none_True_True"]] = "Delta-Based (Both)"
  lapply(
    keys,
    function(key) {
      mode_and_jd <- paste(
          strsplit(key, "-")[[1]][5:8],
          collapse="_"
      )

      labels[[mode_and_jd]]
    }
  )
}
