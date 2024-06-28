# Libraries

libs <- c("shiny", "ggplot2", "shinyjs", "shinythemes", "rsm", "knitr", "lubridate")

for(lib in libs){
  if(!require(lib, character.only = TRUE)){
    install.packages(lib)
    library(lib, character.only = TRUE)
  }
}

if (!(tinytex::is_tinytex())){
  tinytex::install_tinytex()
}