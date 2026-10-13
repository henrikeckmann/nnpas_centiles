rm(list = ls())
library(GGIR)
library(tidyverse)

print_filtered_rows <- function(dataframe, ...) {
  df <- dataframe
  vars = as.list(substitute(list(...)))[-1L]
  for(arg in vars) {
    dataframe <- df
    dataframe_new <- dataframe %>% filter(!!arg)
    rows_filtered <- nrow(df) - nrow(dataframe_new)
    cat(sprintf('Filtered out %s rows using: %s\n', rows_filtered, deparse(arg)))
    df = dataframe_new
  }
  return(dataframe_new)
}

outputdir = "ggir_out"
datadirs = list.dirs("")
bins_df = read_csv("") |>
  mutate(
    CountsVM_M16 = CountsVM
  )

col_count = 1
for (col in colnames(bins_df)[c(1:5, 7)]){
  dir_count = 1
  for (datadir in datadirs){
    datadir_short = datadir |>
      strsplit("/", fixed = TRUE) |>
      unlist() |>
      tail(1)

    if (col == "MIMS"){
      col1 = "LFENMO"
    } else if (col %in% c("CountsVM", "CountsVM_M16")){
      col1 = "NeishabouriCount_vm"
    } else if (col == "AGSA"){
      col1 = "ENMOa"
    } else {
      col1 = col
    }

    in_df = read_csv(
      paste0(outputdir, "/part2_output/", datadir_short, "_", col, "_part2_daysummary.csv"),
      # show_col_types = FALSE
      col_types = list(
        ID = col_character(),
        filename = col_character(),
        calendar_date = col_character(),
        bodylocation = col_character(),
        weekday = col_character(),
        measurementday = col_integer(),
        .default = col_double()
      )
    ) |>
      # filter(
      # !grepl("R", ID, fixed = TRUE) # exclude repeated measurements
      # ) |>
      mutate(
        date = date(calendar_date)
      ) |>
      select(
        ID,
        filename,
        date,
        weekday,
        "N valid hours",
        paste0("M12hr_", col1, "_mg_0-24hr"),
        paste0("M12_", col1, "_mg_0-24hr"),
        paste0("M12_ig_gradient_", col1, "_mg_0-24hr"),
        paste0("M12_ig_rsquared_", col1, "_mg_0-24hr"),
        paste0("M14hr_", col1, "_mg_0-24hr"),
        paste0("M14_", col1, "_mg_0-24hr"),
        paste0("M14_ig_gradient_", col1, "_mg_0-24hr"),
        paste0("M14_ig_rsquared_", col1, "_mg_0-24hr"),
        paste0("M16hr_", col1, "_mg_0-24hr"),
        paste0("M16_", col1, "_mg_0-24hr"),
        paste0("M16_ig_gradient_", col1, "_mg_0-24hr"),
        paste0("M16_ig_rsquared_", col1, "_mg_0-24hr"),
        paste0("mean_", col1, "_mg_0-24hr"),
        paste0("ig_gradient_", col1, "_0-24hr"),
        paste0("ig_rsquared_", col1, "_0-24hr"),
      ) |>
      rename(
        n_valid_hours = "N valid hours"
      )
    # rename_with(~ paste0(datadir_short, "_", .x), !c(ID, date, weekday, "N valid hours", filename)) |>
    # rename_with(~ paste0(datadir_short, "_", col1, "_", .x), c("N valid hours", filename))

    # deal with M16 scaling bug
    if (col == "CountsVM"){
      in_df = in_df |>
        select(
          !contains("M1")
        )
    } else if (col == "CountsVM_M16"){
      in_df = in_df |>
        select(
          contains("M1"),
          ID,
          date,
          weekday,
          n_valid_hours,
          filename
        )
    }

    if (dir_count == 1){
      all_df_dir = in_df
    } else {
      all_df_dir = bind_rows(all_df_dir, in_df)
    }
    dir_count = dir_count + 1
  }

  if (col_count == 1){
    all_df = all_df_dir
  } else {
    all_df = full_join(all_df, all_df_dir, by = c("ID", "date", "weekday", "n_valid_hours", "filename"))
  }
  col_count = col_count + 1
}

# divide M16 counts AvAcc by a thousand to correct for scaling error in GGIR
all_df = all_df |>
  mutate(
    across(contains("M12_NeishabouriCount_vm"), ~ .x / 1000),
    across(contains("M14_NeishabouriCount_vm"), ~ .x / 1000),
    across(contains("M16_NeishabouriCount_vm"), ~ .x / 1000),
    across(contains("M12_LFENMO"), ~ .x / 1000),
    across(contains("M14_LFENMO"), ~ .x / 1000),
    across(contains("M16_LFENMO"), ~ .x / 1000),
    across(contains("mean_LFENMO"), ~ .x / 1000),
  )

# calibration error and samplefrequency
count = 1
for (col in colnames(bins_df)[1]){
  for (datadir in datadirs){
    datadir_short = datadir |>
      strsplit("/", fixed = TRUE) |>
      unlist() |>
      tail(1)

    in_calib_df = read_csv(
      paste0(outputdir, "/part2_output/", datadir_short, "_", col, "_part2_summary.csv"),
      show_col_types = FALSE
    ) |>
      # filter(
      #   !grepl("R", ID, fixed = TRUE), # exclude repeated measurements
      #   !grepl("-R_", filename, fixed = TRUE), # exclude repeated measurements
      #   samplefreq == 100
      # ) |>
      select(
        ID,
        calib_err,
        samplefreq
      ) #|>
      # rename_with(~ paste0(datadir_short, "_", .x), c(calib_err, samplefreq))

    if (count == 1){
      all_calib_df = in_calib_df
      count = count + 1
    } else {
      all_calib_df = bind_rows(all_calib_df, in_calib_df)
    }
  }
}

device_specs = readRDS("") |>
  rename(ID = filename) |>
  select(ID, filetype, dynamic_range)

all_calib_df = full_join(all_calib_df, device_specs, by = "ID")

# participant info
participant_info = read_csv(
  ""
) |>
  mutate(
    ID = paste0(new_id, ".gt3x")
  ) |>
  select(!new_id)

all_calib_df = full_join(all_calib_df, participant_info, by = "ID")
na_count_calib_df = summarise(all_calib_df, across(everything(), ~ sum(is.na(.x))))
# all_calib_df = all_calib_df |> drop_na()

all_df = full_join(all_df, all_calib_df, by = "ID", na_matches = "never")

all_df_predrop = all_df

# all_df_predrop |>
#   filter(n_valid_hours > 10) |>
#   pull(n_valid_hours) |>
#   hist()

all_df_day = all_df_predrop |>
  select(
    ID, filename, date, weekday, n_valid_hours, calib_err, samplefreq,
    filetype, dynamic_range, gender, age, height, weight, bmi, edu,
    contains("M14")
  ) |>
  filter(
    n_valid_hours >= 14
  )

na_count_df_day = summarise(all_df_day, across(everything(), ~ sum(is.na(.x))))

all_df_participant = all_df_day |>
  group_by(ID) |>
  add_tally() |>
  summarise(
    across(where(is.character), ~ first(.x)),
    date = first(date),
    across(where(is.numeric), ~ mean(.x, na.rm = TRUE))
    )

na_count_df_participant = summarise(all_df_participant, across(everything(), ~ sum(is.na(.x))))

# oulier/unrealistic values
ENMO_q = all_df_participant |>
  pull(paste0("M14_ENMO_mg_0-24hr")) |>
  quantile(probs = 0.975, na.rm = TRUE) |>
  as.double() * 2

ENMOa_q = all_df_participant |>
  pull(paste0("M14_ENMOa_mg_0-24hr")) |>
  quantile(probs = 0.975, na.rm = TRUE) |>
  as.double() * 2

MAD_q = all_df_participant |>
  pull(paste0("M14_MAD_mg_0-24hr")) |>
  quantile(probs = 0.975, na.rm = TRUE) |>
  as.double() * 2

LFENMO_q = all_df_participant |>
  pull(paste0("M14_LFENMO_mg_0-24hr")) |>
  quantile(probs = 0.975, na.rm = TRUE) |>
  as.double() * 2

counts_q = all_df_participant |>
  pull(paste0("M14_NeishabouriCount_vm_mg_0-24hr")) |>
  quantile(probs = 0.975, na.rm = TRUE) |>
  as.double() * 2

# filtering

all_df_participant_dropna_exclmims = all_df_participant |>
  drop_na(!contains("LFENMO"))

all_df_participant_dropna = all_df_participant |>
  drop_na()

all_df_participant = all_df_participant |>
  drop_na() |>
  print_filtered_rows(
    calib_err <= 0.01,
    n >= 3,
    age > 19 & age < 81,
    `M14_ENMO_mg_0-24hr` < ENMO_q,
    `M14_ENMOa_mg_0-24hr` < ENMOa_q,
    `M14_MAD_mg_0-24hr` < MAD_q,
    `M14_LFENMO_mg_0-24hr` < LFENMO_q,
    `M14_NeishabouriCount_vm_mg_0-24hr` < counts_q
  )

# all_df_participant = all_df_participant |> drop_na(!contains("LFENMO"))
# all_df_participant_MIMSdropna = all_df_participant |> drop_na()

rm(
  all_df,
  all_calib_df,
  device_specs,
  all_df_dir,
  bins_df,
  in_df,
  in_calib_df,
  col,
  col1,
  col_count,
  dir_count,
  count,
  datadir,
  datadir_short,
  datadirs,
  outputdir,
  participant_info
)


