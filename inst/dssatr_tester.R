# dssatr tester
library(dssatr)

# Set a directory for testing
testDir <- "~/dssatr Test"

dir.create(testDir, showWarnings=F, recursive=T)

# Extract data for the Village Ecodynamics Project "VEPIIN" study area:
# http://village.anth.wsu.edu
data(mvnp)

output.dir <- paste0(testDir,"/OUTPUT/")
raw.dir <- paste0(output.dir,"/DATA/RAW/")
extraction.dir <- paste0(output.dir,"/DATA/EXTRACTIONS/")

test <- dssat_spatial(template = mvnp,
              label = "MVNP",
              output.dir = output.dir,
              raw.dir = raw.dir,
              extraction.dir = extraction.dir,
              force.redo = FALSE)
