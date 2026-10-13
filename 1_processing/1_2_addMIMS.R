rm(list = ls())

source(".../addMIMS_func_uia.R")
outputdir = ""
datadirs = list.dirs("")

# Run addMIMS
addMIMS(
	datadir = datadirs,
	outputdir = outputdir,
	desiredtz = "",
	configtz = "",
	overwrite = FALSE
)
