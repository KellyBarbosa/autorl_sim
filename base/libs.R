# Libraries

libs <- c("shiny", "ggplot2", "shinyjs", "shinythemes", "rsm", "knitr", "lubridate", "kableExtra")

for(lib in libs){
  if(!require(lib, character.only = TRUE)){
    install.packages(lib)
    library(lib, character.only = TRUE)
  }
}

if (!requireNamespace("tinytex", quietly = TRUE)){
  tinytex::install_tinytex()
}