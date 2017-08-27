## Adds ffmpeg and ghostscript to rocker/geospatial

FROM bocinsky/bocin_base

MAINTAINER Kyle Bocinsky <bocinsky@gmail.com>

RUN apt-get update \
  && apt-get install -y --no-install-recommends \
    cmake

## Install dssatr
RUN r -e 'devtools::install_github("crowcanyon/dssatr")'

## Install dssat
RUN mkdir ~/DSSAT46 \
  && cp /usr/local/lib/R/site-library/dssatr/DSSAT46/dssat46.tar.gz ~/DSSAT46/dssat46.tar.gz \
  && tar -C ~/DSSAT46 -xzvf ~/DSSAT46/dssat46.tar.gz \
  && cd ~/DSSAT46/dssat-csm-4.6.5.8 \
  && mkdir build \
  && cd build \
  && cmake .. \
  && make
  
## Move to correct spot
RUN mkdir /DSSAT46 \
  && cd ~/DSSAT46/dssat-csm-4.6.5.8 \
  && cp build/bin/dscsm046.exe /DSSAT46/dscsm046.exe
  
## Copy the DSSATPRO.L46 file
COPY ./inst/DSSAT46/DSSATPRO.L46 /DSSAT46/DSSATPRO.L46

## Copy the Data directory
COPY -r ./inst/DSSAT46/Data/. /DSSAT46/
  
