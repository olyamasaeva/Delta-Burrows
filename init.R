install_or_load_pack <- function(pack){
  create.pkg <- pack[!(pack %in% installed.packages()[, "Package"])]
  if (length(create.pkg))
    install.packages(create.pkg, dependencies = TRUE)
  sapply(pack, require, character.only = TRUE)
}
packages <- c("plyr",  "arm","reshape2","odbc","RMySQL","utf8","matrixStats","dplyr","quanteda","ggplot2","cluster","dendextend","purrr","shiny","epiDisplay","tm","eatGADS","magrittr","tidyr", "ggdendro")
install_or_load_pack(packages)
