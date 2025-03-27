library(tidyverse)
library(safepaths)
library(glue)
library(janitor)
library(e1071)

library(sf)

# library(cancensus) # nolint
# library(cansim) # nolint

# library(bcmaps) # nolint
# library(geojsonsf) # nolint
# library(jsonlite) imported by bcmaps

#library(cowplot) for aligning multiple plots # nolint
#library(patchwork) # nolint
# library(duckdb) # nolint

# Load the rlang package for the bang-bang operator - imported by cowplot
# library(rlang) # nolint


# set timeout on file load process
getOption("timeout")
options(timeout = 600)



