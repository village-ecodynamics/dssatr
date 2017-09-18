## Adds dssatr to bocinsky/dssat-docker

FROM bocinsky/dssat-docker

MAINTAINER Kyle Bocinsky <bocinsky@gmail.com>

## Install dssatr from Github
RUN r -e 'devtools::install_github("crowcanyon/dssatr")'
