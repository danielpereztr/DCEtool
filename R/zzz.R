# zzz.R
.onLoad <- function(libname, pkgname) {
  shiny::addResourcePath(
    prefix = "assets",
    directoryPath = system.file(
      "assets",
      package = "DCEtool"
    )
  )
}

.onUnload <- function(libname, pkgname) {
  shiny::removeResourcePath("assets")
}