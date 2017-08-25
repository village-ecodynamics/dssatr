## Adds ffmpeg and ghostscript to rocker/geospatial

FROM bocinsky/bocin_base

MAINTAINER Kyle Bocinsky <bocinsky@gmail.com>

RUN apt-get update

## Install dssatr
RUN r -e 'devtools::install_github("crowcanyon/dssatr")'

## Install dssat

