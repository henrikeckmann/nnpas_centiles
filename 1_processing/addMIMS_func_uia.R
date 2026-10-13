library(GGIR)
library(MIMSunit)
library(dplyr)
library(stringr)

# external function to loop through files and keep track of errors
addMIMS <- function(
    datadir, outputdir, desiredtz, configtz, overwrite=FALSE
  ){

  wear_data = read.csv("")

  # Convert paths from Windows specific slashed to generic slashes
  outputdir = gsub(pattern = "\\\\", replacement = "/", x = outputdir)
  datadir = gsub(pattern = "\\\\", replacement = "/", x = datadir)

  # Tilde-expansion
  outputdir = path.expand(outputdir)
  datadir = path.expand(datadir)
  print(paste("OUTPUTDIR PATH AFTER EXPANSION =", outputdir))
  print(paste("DATADIR PATH AFTER EXPANSION =", datadir))

  # handle paths with/without "/" at the end, and generate metadatadir
  datadir = paste(unlist(strsplit(datadir, "/")), collapse = "/")
  outputdir = paste(unlist(strsplit(outputdir, "/")), collapse = "/")

  outputfoldername = unlist(
    strsplit(datadir, "/")
  )[length(unlist(strsplit(datadir, "/")))]

  metadatadir = paste(
    outputdir, "/output_", outputfoldername, "/meta/basic", sep = ""
  )

  # read in list of meta files in metadatadir
  filenames = list.files(
    path=metadatadir,
    pattern=NULL,
    all.files=FALSE,
    full.names=FALSE,
  )

  file_n = 1 # counter

  #create output sink
  con_out_file <- file(
    paste0(sub("meta/basic", "", metadatadir), "console_output.txt"),
    open = "a"
    )
  sink(con_out_file, append = TRUE, type = "output", split = TRUE)
  print("")
  print("")
  print(Sys.time())
  print("STARTING NEW RUN")
  print(paste("OUTPUTDIR PATH AFTER EXPANSION =", outputdir))
  print(paste("DATADIR PATH AFTER EXPANSION =", datadir))
  print(paste("METADATDIR PATH =", metadatadir))

  if (file.exists(
    paste(sub("meta/basic", "", metadatadir), "addMIMS_errors.csv")
    )
  ){
    errors_df <- read.csv(
      paste(sub("meta/basic", "", metadatadir), "addMIMS_errors.csv")
    )
    errors <- c(errors_df$x, "FLAG, NEW RUN")
    rm(errors_df)
  } else {
    errors <- c() # vector to keep track of files that throw an error
  }
  err_length_start <- length(errors)

  if (file.exists(
    paste0(sub("meta/basic", "", metadatadir), "overwrites_skipped.csv")
    )
  ){
    overwrites_df <- read.csv(
      paste0(sub("meta/basic", "", metadatadir), "overwrites_skipped.csv")
    )
    overwrites_skipped <- c(overwrites_df$x, "FLAG, NEW RUN")
    rm(overwrites_df)
  } else {
    overwrites_skipped <- c() # vector to keep track of files that are skipped due to overwrite check
  }
  overwrites_length_start <- length(overwrites_skipped)

  # loop over metafiles
  for (filename in filenames){

    print(
      paste(
        format(Sys.time(), '%H:%M:%S'),
        "running file", file_n, "out of", length(filenames)
      )
    )

    print(filename)

    # try running internal addMIMS_internal function
    # and catch any errors
    err <- tryCatch(
      {
        addMIMS_internal(
          datadir = datadir,
          metadatadir = metadatadir,
          # dynrange = dynrange,
          filename = filename,
          desiredtz = desiredtz,
          configtz = configtz,
          overwrite = overwrite,
          wear_data = wear_data
        )
      }, error = function(e){
        print("ERROR, SKIPPING FILE")
        return(TRUE)
      }
    )

    # if file throws an error, add filename to errors vector
    if (err == TRUE){
      errors <- c(errors, filename)
    } else if (err == "OVERWRITE"){
      overwrites_skipped <- c(overwrites_skipped, filename)
    }
    file_n = file_n + 1 # increment counter
  }

  err_length <- length(errors) - err_length_start # count of errors, for final print statement

  # if no files have errored, add a line to vector saying no errors
  if (err_length == 0){
    errors <- c(errors, "NO addMIMS_func ERRORS")
  }

  # print .csv with files that errored
  write.csv(
    errors, paste(sub("meta/basic", "", metadatadir), "addMIMS_errors.csv")
  )

  overwrites_length <- length(overwrites_skipped) - overwrites_length_start # count of errors, for final print statement

  # if no files have errored, add a line to vector saying no errors
  if (overwrites_length == 0){
    overwrites_skipped <- c(overwrites_skipped, "NO FILES SKIPPED DUE TO OVERWRITE CHECK")
  }

  # print .csv with files that were skipped because they were already written
  write.csv(
    overwrites_skipped, paste0(sub("meta/basic", "", metadatadir), "overwrites_skipped.csv")
  )

  print(format(Sys.time(), '%H:%M:%S'))
  print(paste(
    "COMPLETED ALL",
    length(filenames),
    "FILES IN METAFILE DIRECTORY WITH",
    err_length,
    "ERRORS."
    )
  )

  rm(err_length); rm(err_length_start)
  rm(overwrites_length); rm(overwrites_length_start)

  # close output sink
  sink()
  close(con_out_file)
}

# internal function that loads each metafile and adds MIMS vector
addMIMS_internal <- function(
    datadir, metadatadir, filename, desiredtz, configtz, overwrite, wear_data
  ){

  # load metafile
  print(paste(format(Sys.time(), '%H:%M:%S'), "load metafile"))
  load(paste(metadatadir, "/", filename, sep = ""))

  if (M$filecorrupt == TRUE){
    print(paste(filename, "CORRUPT"))
    return(TRUE)
  }
  if (M$filetooshort == TRUE){
    print(paste(filename, "FILE TOO SHORT"))
    return(TRUE)
  }
  if (overwrite == FALSE){
    # if ("LFENMO" %in% colnames(M$metashort)){
    #   print(paste(filename, "ALREADY CONTAINS 'LFENMO' VECTOR. SKIPPING."))
    #   return("OVERWRITE")
    # }
    if (exists("mims_df")){
      print(paste(filename, "mims_df ALREADY EXISTS. SKIPPING."))
      return("OVERWRITE")
    }
  }

  # rawfile name from metafile name
  rawfile <- sub(".RData", "", filename)
  rawfile <- sub("meta_", "", rawfile)
  rawfile <- paste(datadir, "/", rawfile, sep = "")

  # prepare necessary parameters for g.readaccfile
  ws <- M$windowsizes[3]
  ws2 <- M$windowsizes[2]
  ws3 <- M$windowsizes[1]
  filequality <- data.frame(
    filetooshort = M$filetooshort, filecorrupt = M$filecorrupt,
    filedoesnotholdday = FALSE, NFilePagesSkipped = M$NFilePagesSkipped
  )

  hvars = g.extractheadervars(I)
  deviceSerialNumber = hvars$deviceSerialNumber
  rm(hvars); gc()

  # get now-wear, clip, and blocksize parameters (thresholds)
  ncb_params = get_nw_clip_block_params(
    chunksize = 1,
    dynrange = NULL,
    monc = I$monc, dformat = I$dformc,
    deviceSerialNumber = deviceSerialNumber,
    rmc.noise = c(),
    sf = I$sf,
    rmc.dynamic_range = c()
  )

  blocksize = ncb_params$blocksize * 100
  rm(ncb_params); gc()

  # read raw acceleration
  print(paste(format(Sys.time(), '%H:%M:%S'), "accread"))
  header = NULL
  PreviousLastValue = c(0, 0, 1)
  PreviousLastTime = NULL
  PreviousEndPage = c()
  i = 1

  accread = g.readaccfile(
    filename = rawfile, blocksize = blocksize, blocknumber = i,
    filequality = filequality, ws = ws, PreviousEndPage = PreviousEndPage,
    inspectfileobject = I, PreviousLastValue = PreviousLastValue,
    PreviousLastTime = PreviousLastTime, header = header
  )

  # deal with actigraph idle sleep mode
  print(paste(format(Sys.time(), '%H:%M:%S'), "g.imputeTimegaps"))
  P = g.imputeTimegaps(accread$P$data, sf = I$sf, k = 0.25,
                       PreviousLastValue = PreviousLastValue,
                       PreviousLastTime = PreviousLastTime,
                       epochsize = c(ws3, ws2))
  accread$P$data = P$x

  rm(filequality); rm(header); rm(i); rm(PreviousEndPage);
  rm(PreviousLastTime); rm(PreviousLastValue); rm(P); gc()

  # CALIBRATE
  print(paste(format(Sys.time(), '%H:%M:%S'), "apply calibration"))
  temperaturecolumn = which(colnames(accread$P$data) == "temperature")

  yy = as.matrix(cbind(
    as.numeric(accread$P$data[,temperaturecolumn]),
    as.numeric(accread$P$data[,temperaturecolumn]),
    as.numeric(accread$P$data[,temperaturecolumn]))
    )

  accread$P$data[,c("x", "y", "z")] = scale(
    as.matrix(accread$P$data[,c("x", "y", "z")]), center = -C$offset, scale = 1/C$scale
    )

  if(C$use.temp && length(C$meantempcal) > 0) {
    accread$P$data[,c("x", "y", "z")] = accread$P$data[,c("x", "y", "z")] + scale(
      yy, center = rep(C$meantempcal,3), scale = 1/C$tempoffset
      )
  }

  rm(yy); gc()

  # align data start time
  SWMT <- get_starttime_weekday_truncdata(
    I$monc, I$dformc, accread$P$data, header, desiredtz = desiredtz,
    I$sf, rawfile,  ws2, configtz = configtz
  )

  data <- SWMT$data

  rm(accread); rm(SWMT); gc()

  data$HEADER_TIME_STAMP = as.POSIXct(data$time)

  start_date = as.POSIXct(wear_data[which(wear_data$new_id == strsplit(I$filename, ".", fixed = TRUE)[[1]][1]), "date"])
  start_date = start_date - (60*60*24) # add 1 days margin before logged start date
  end_date = start_date + (9*60*60*24) # add 1 days margin after supposed end date
  data = data |>
    filter(
      HEADER_TIME_STAMP >= start_date & HEADER_TIME_STAMP <= end_date
    )

  # prepare data format for MIMS function
  print(paste(format(Sys.time(), '%H:%M:%S'), "generate MIMS"))
  data <- select(data, HEADER_TIME_STAMP, x, y, z)
  data <- rename(data, X = x, Y = y, Z = z)

  dynrange = as.integer(as.character(I$header[["value"]][15]))
  dynrange = c(-dynrange, dynrange)

  epoch = paste(M$windowsizes[1], "sec")
  # generate MIMS
  mims_df = mims_unit(
    data,
    before_df = NULL,
    after_df = NULL,
    dynamic_range = dynrange,
    epoch = epoch
  )


  # calling MIMS "LFENMO" to get GGIR part 2 to do IG
  # as it won't do it to embedded functions
  # M$metashort$LFENMO <- mims

  # save metafile and clean up before next loop
  print(paste(format(Sys.time(), '%H:%M:%S'), "saving"))
  save(mims_df, M, I, C, filename_dir, filefoldername, tail_expansion_log,
       file = paste(metadatadir, "/", filename, sep = ""))
  rm(M); rm(I); rm(C); rm(blocksize);
  rm(deviceSerialNumber); rm(epoch); rm(filefoldername); rm(filename_dir);
  rm(rawfile); rm(tail_expansion_log); rm(temperaturecolumn);
  rm(ws); rm(ws2)
  gc()
  print(paste(format(Sys.time(), '%H:%M:%S'), "done"))
  return(FALSE)
}

# internal wrapper function for mims_unit function from MIMSunit package
# simply discards dataframe only keeping MIMS vector, rounded to 4 decimals
genMIMS <- function(data, before_df, after_df, dynrange, epoch){
  mims_df <- mims_unit(
    data,
    before_df = before_df,
    after_df = after_df,
    dynamic_range = dynrange,
    epoch = epoch,
    #use_snapshot_to_check = TRUE,
  )
  mims <- round(mims_df$MIMS_UNIT, 4)
  return(mims)
}
