## set R version (https://hub.docker.com/r/rocker/verse/tags)
FROM rocker/verse:4.4.1

## set up directories
RUN mkdir /home/rstudio/case-study /home/rstudio/data /home/rstudio/literature-review
COPY missSim.Rproj /home/rstudio/

## install R packages from CRAN the last day of the specified R version
## ncpus set to -1 (all available cores)
RUN install2.r --error --skipinstalled --ncpus -1 \
    tidyverse dplyr ggplot2 tidyr ggbeeswarm cowplot here lubridate readxl janitor writexl sysfonts scales ggpubr showtext pander
