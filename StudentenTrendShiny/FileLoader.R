LoadFolder <- function(path) {
  file.sources <- list.files(path=path, pattern="*.R", full.names=T, ignore.case=T)
  sapply(file.sources,source,.GlobalEnv)
}

LoadFolder("utils")
LoadFolder("menu")
LoadFolder("body")
LoadFolder("screens")