my_packages = c("shiny","shinythemes","shinydashboard","ggplot2","dplyr","tidyr","scales","gridExtra","future.apply")

install_if_missing = function(p) {
  if (p %in% rownames(installed.packages()) == FALSE) {
    install.packages(p)
  }
}

invisible(sapply(my_packages, install_if_missing))