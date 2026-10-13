rm(list = ls())
library(GGIR)
library(tidyverse)

outputdir = ""
datadirs = list.dirs("")
bins_df = read_csv("") |>
  mutate(
    MIMS = MIMS * 1000,
    CountsVM_M16 = CountsVM * 1000
    ) # deal with internal GGIR LFENMO and M16 scaling

for (col in colnames(bins_df)[c(1:5, 7)]){
  for (datadir in datadirs){

    datadir_short = datadir |>
      strsplit("/", fixed = TRUE) |>
      unlist() |>
      tail(1)

    GGIR(
      mode = c(2),
      datadir = datadir,
      outputdir = outputdir,
      windowsizes = c(5, 900, 3600),
      overwrite = TRUE,
      # do.imp = FALSE,
      includedaycrit = 10,
      ndayswindow = 7,
      data_masking_strategy = 5,
      winhr = c(12, 14, 16),
      iglevels = pull(bins_df, col)
    )

    file.copy(
      paste0(outputdir, "/output_", datadir_short, "/results/part2_daysummary.csv"),
      paste0(outputdir, "/part2_output/", datadir_short, "_", col, "_part2_daysummary.csv"),
      overwrite = TRUE
    )

    file.copy(
      paste0(outputdir, "/output_", datadir_short, "/results/part2_summary.csv"),
      paste0(outputdir, "/part2_output/", datadir_short, "_", col, "_part2_summary.csv"),
      overwrite = TRUE
    )
  }
}


