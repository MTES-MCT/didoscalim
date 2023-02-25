# Ce script R permet de lancer automatiquement les tests dans un conteneur docker rocker:tidyverse :
#
# docker run -it -e TZ=Europe/Paris -v $HOME/.Renviron:/root/.Renviron -v /etc/hosts:/etc/hosts -v $(pwd):/root/didoscalim rocker/tidyverse:4.1 Rscript /root/didoscalim/rocker_test.R

setwd("/root/didoscalim")
library(devtools)
install_deps(".")
install_dev_deps(".")
test()
