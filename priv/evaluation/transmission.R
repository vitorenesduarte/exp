# install and load needed packages
mirror <- "http://cran.us.r-project.org"
packages <- c("hash")

for(package in packages) {
  if(!require(package, character.only=TRUE)) {
    install.packages(package, repos=mirror, dependencies=TRUE)
    require(package, character.only=TRUE)
  }
}

# list directories
Dirs <- setdiff(list.dirs("logs"), c("."))
Dirs

h <- hash()
h
