LoadFolder <- function(path) {
  file.sources <- list.files(path=path, pattern="*.R", full.names=T, ignore.case=T)
  sapply(file.sources,source,.GlobalEnv)
}

source("utils/Util.R")
source("utils/LogVar.R")
LoadFolder("menu")
LoadFolder("body")
LoadFolder("screens")