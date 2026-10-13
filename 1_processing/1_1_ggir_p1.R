rm(list = ls())
library(GGIR)

outputdir = "ggir_out"
datadirs = list.dirs("")

count = 1
# Run GGIR part 1
for (datadir in datadirs){
  print(Sys.time())
  print(paste0("DATADIR NUMBER ", count))
  print(datadir)
  GGIR(
    # maxNcores = 3,
    mode = c(1),
    datadir = datadir,
    outputdir = outputdir,
    windowsizes = c(5, 900, 3600),
    overwrite = FALSE,
    do.enmo = TRUE, # generate ENMO
    do.mad = TRUE, # generate MAD
    do.neishabouricounts = TRUE, #generate counts
    do.enmoa = TRUE,
    # do.imp = FALSE # no imputation, only affects part2
  )
  count = count + 1
}

