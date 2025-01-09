##### my packages ################################################################################
## CRAN
cran.packages <- c('ggplot2','gridExtra','viridis','dplyr','plyr','reshape2','lubridate','PerformanceAnalytics', 'ggpubr','effects')
install.this <- cran.packages[!(cran.packages %in% utils::installed.packages()[,"Package"])]
if(length(install.this)>=1) install.packages(install.this)
dummy <- lapply(cran.packages, require, character.only = TRUE)


## github
git.packages <- c('catchR','DFOdata','CCAM')
install.this <- git.packages[!(git.packages %in% utils::installed.packages()[,"Package"])]
if('catchR' %in% install.this)  devtools::install_github("iml-assess/catchR@eli-parallel")
if('DFOdata' %in% install.this)  devtools::install_github("im-assess/DFOdata")
if('CCAM' %in% install.this)  devtools::install_github("elisvb/CCAM")
dummy <- lapply(git.packages, require, character.only = TRUE)

##### source R directory  ############################################################################
invisible(sapply(list.files(pattern="[.]R$", path="R/", full.names=TRUE), source))

##### my ggplot theme ################################################################################
theme_set(theme_mackerel())             # theme_mackerel from catchR
update_geom_defaults("line", list(size = 1))  # no idea why somethimes I get fat lines otherwise

##### passwords databases #############################################################################
source('../../passwords.R')
