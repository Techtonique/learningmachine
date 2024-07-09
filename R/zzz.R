.onLoad <- function(libname, pkgname) {
  
  required_packages <- c("skimr", "bcn")
  
  ensure_package <- function(package_name) {
    if (!requireNamespace(package_name, quietly = TRUE)) {
      utils::install.packages(package_name, 
                              repos = c('https://techtonique.r-universe.dev', 
                                        'https://cloud.r-project.org'), 
                              dependencies = TRUE)
    }
    library(package_name, character.only = TRUE)
  }
  
  invisible(lapply(required_packages, ensure_package))
}

