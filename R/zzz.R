.onLoad <- function(libname, pkgname) {
  
  required_packages <- "bcn"
  
  ensure_package <- function(package_name) {
    if (!(package_name %in% rownames(installed.packages()))) {
      utils::install.packages(package_name, 
                              repos = c('https://techtonique.r-universe.dev', 
                                        'https://cloud.r-project.org'), 
                              dependencies = TRUE)
      
    }
  }
  invisible(lapply(required_packages, ensure_package))
}

