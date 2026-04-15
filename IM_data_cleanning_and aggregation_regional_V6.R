# ============================================================
# Batch regional IM cleaning + Regional IM repository builder
# Final corrected version
# ============================================================

pacman::p_load(
  tidyverse, lubridate, readxl, readr, tools, tibble, qs, stringr, arrow
)

# ============================================================
# USER PATHS
# ============================================================
input_folder <- "C:/Users/TOURE/Documents/PADACORD/IM/"
output_folder <- "C:/Users/TOURE/Documents/PADACORD/IM_c/"
qc_output_folder <- "C:/Users/TOURE/Documents/PADACORD/IM_c/QC/"
preparedness_file <- "C:/Users/TOURE/Documents/REPOSITORIES/LQAS_raw_data/harmonized_date/data (5).xlsx"

regional_repository_file <- file.path(output_folder, "Regional_IM_repository.csv")
regional_qc_repository_file <- file.path(output_folder, "Regional_IM_repository_QC.csv")
summary_file <- file.path(output_folder, "IM_processing_summary.csv")

if (!dir.exists(output_folder)) dir.create(output_folder, recursive = TRUE)
if (!dir.exists(qc_output_folder)) dir.create(qc_output_folder, recursive = TRUE)

# ============================================================
# CONSTANTS
# ============================================================
ALGERIA_IM_FORM_ID <- "8587"

# ============================================================
# HELPERS
# ============================================================
parse_mixed_dates <- function(x) {
  x <- as.character(x)
  suppressWarnings(
    dplyr::coalesce(
      ymd(x),
      dmy(x),
      mdy(x),
      ymd_hms(x),
      dmy_hms(x),
      mdy_hms(x)
    )
  )
}

clean_numeric <- function(x) {
  x <- as.character(x)
  x <- trimws(x)
  x[x %in% c("", " ", "n/a", "NA", "NaN", "null", "NULL")] <- "0"
  suppressWarnings(as.numeric(x))
}

clean_yes_no_numeric <- function(x) {
  x <- as.character(x)
  x <- trimws(x)
  dplyr::case_when(
    x %in% c("Y", "YES", "Yes", "yes", "1") ~ 1,
    x %in% c("N", "NO", "No", "no", "0") ~ 0,
    x %in% c("", " ", "n/a", "NA", "NaN", "null", "NULL") ~ 0,
    TRUE ~ suppressWarnings(as.numeric(x))
  )
}

safe_row_sum <- function(df, cols) {
  cols <- intersect(cols, names(df))
  if (length(cols) == 0) return(rep(0, nrow(df)))
  temp <- df[, cols, drop = FALSE]
  temp[] <- lapply(temp, function(x) suppressWarnings(as.numeric(as.character(x))))
  rowSums(temp, na.rm = TRUE)
}

safe_pick_first <- function(df, candidates, default = 0) {
  candidates <- intersect(candidates, names(df))
  if (length(candidates) == 0) return(rep(default, nrow(df)))
  suppressWarnings(as.numeric(as.character(df[[candidates[1]]])))
}

rename_repetitive_columns <- function(data) {
  pattern <- "^HH\\[\\d+\\]/HH/"
  new_columns <- sapply(colnames(data), function(col) {
    if (grepl(pattern, col)) gsub("HH/", "", col) else col
  })
  colnames(data) <- new_columns
  data
}

bind_rows_fill <- function(df_list) {
  df_list <- Filter(function(x) !is.null(x), df_list)
  if (length(df_list) == 0) return(tibble())
  
  all_cols <- unique(unlist(lapply(df_list, names)))
  
  prototypes <- list()
  for (col in all_cols) {
    for (df in df_list) {
      if (col %in% names(df)) {
        prototypes[[col]] <- df[[col]]
        break
      }
    }
  }
  
  df_list2 <- lapply(df_list, function(x) {
    missing <- setdiff(all_cols, names(x))
    
    if (length(missing) > 0) {
      for (m in missing) {
        proto <- prototypes[[m]]
        
        if (is.character(proto)) {
          x[[m]] <- rep(NA_character_, nrow(x))
        } else if (is.numeric(proto)) {
          x[[m]] <- rep(NA_real_, nrow(x))
        } else if (is.integer(proto)) {
          x[[m]] <- rep(NA_integer_, nrow(x))
        } else if (inherits(proto, "Date")) {
          x[[m]] <- as.Date(rep(NA_character_, nrow(x)))
        } else if (inherits(proto, "POSIXct")) {
          x[[m]] <- as.POSIXct(rep(NA_character_, nrow(x)), origin = "1970-01-01")
        } else if (is.logical(proto)) {
          x[[m]] <- rep(NA, nrow(x))
        } else {
          x[[m]] <- rep(NA_character_, nrow(x))
        }
      }
    }
    
    x[, all_cols, drop = FALSE]
  })
  
  bind_rows(df_list2)
}

find_similar_column <- function(target_name, df, exact_first = TRUE) {
  cols <- names(df)
  
  if (exact_first && target_name %in% cols) {
    return(target_name)
  }
  
  target_norm <- tolower(target_name)
  target_norm <- gsub("[\\[\\]/ _-]", "", target_norm)
  
  cols_norm <- tolower(cols)
  cols_norm <- gsub("[\\[\\]/ _-]", "", cols_norm)
  
  idx <- which(cols_norm == target_norm)
  if (length(idx) > 0) return(cols[idx[1]])
  
  idx2 <- which(grepl(target_norm, cols_norm, fixed = TRUE))
  if (length(idx2) > 0) return(cols[idx2[1]])
  
  NA_character_
}

get_regex_cols <- function(df, pattern) {
  grep(pattern, names(df), value = TRUE)
}

build_hh_candidate_names <- function(hh_num, suffix) {
  c(
    sprintf("HH[%s]/%s", hh_num, suffix),
    sprintf("HH[%s]/group1/%s", hh_num, suffix),
    sprintf("HH[%s]/group2/%s", hh_num, suffix),
    sprintf("HH[%s]/group4/%s", hh_num, suffix),
    sprintf("HH_%s_%s", hh_num, suffix),
    sprintf("HH%s_%s", hh_num, suffix)
  )
}

# ============================================================
# INPUT READER
# ============================================================
read_input_data <- function(input_file) {
  ext <- tolower(tools::file_ext(input_file))
  
  message("Reading file: ", basename(input_file), " [.", ext, "]")
  
  if (ext == "csv") {
    return(readr::read_csv(input_file, show_col_types = FALSE) %>% as_tibble())
  }
  
  if (ext %in% c("xlsx", "xls")) {
    return(readxl::read_excel(input_file) %>% as_tibble())
  }
  
  if (ext == "parquet") {
    return(arrow::read_parquet(input_file) %>% as_tibble())
  }
  
  if (ext == "qs") {
    return(qs::qread(input_file) %>% as_tibble())
  }
  
  if (ext == "rds") {
    obj <- tryCatch(
      readRDS(input_file),
      error = function(e) {
        message("readRDS failed for ", basename(input_file), "; trying qs::qread() fallback.")
        qs::qread(input_file)
      }
    )
    return(as_tibble(obj))
  }
  
  stop("Unsupported file extension: ", ext, " for file: ", input_file)
}

# ============================================================
# STANDARD COLUMN DEFINITIONS
# ============================================================
required_columns <- c(
  "Country", "Region", "District", "Response", "roundNumber",
  "Type_Monitoring", "date_monitored", "HH_count", "Total_U5_Present",
  "TotalFM", "sum_missed_children", "Total_Absent", "Total_refusal"
)

hh_patterns_standard <- c(
  "Total_U5_Present_HH", "U5_Vac_FM_HH", "Tot_child_Absent_HH",
  "Tot_child_NC_HH", "Tot_child_NotVisited_HH", "Tot_child_NotRevisited",
  "Tot_child_Asleep_HH", "Tot_child_VaccinatedRoutine", "Tot_child_Others_HH",
  "Parent_Caregive_Inform_HH"
)

hh_patterns_algeria <- c(
  "Total_U6_Present_HH", "U6_Vac_FM_HH", "Tot_child_Absent_HH",
  "Tot_child_NC_HH", "Tot_child_NotVisited_HH", "Tot_child_NotRevisited",
  "Tot_child_Asleep_HH", "Tot_child_VaccinatedRoutine", "Tot_child_Others_HH",
  "Parent_Caregive_Inform_HH"
)

absence_total_candidates <- list(
  r_abs_play_areas    = c("Tot_child_Abs_Play_areas_T"),
  r_abs_market        = c("Tot_child_Abs_Market_T"),
  r_abs_school        = c("Tot_child_Abs_School_T"),
  r_abs_farm          = c("Tot_child_Abs_Farm_T"),
  r_abs_social_event  = c("Tot_child_Abs_SocialEvent"),
  r_abs_travelling    = c("Sum_child_Abs_Travelling"),
  r_abs_parent_absent = c("Sum_child_Abs_Parent_Absent"),
  r_abs_other_detail  = c("Tot_child_Abs_Other_T")
)

nc_total_candidates <- list(
  r_nc_religious_beliefs = c("Tot_child_NC_Religious_beliefs_T"),
  r_nc_side_effects      = c("Tot_child_NC_sideEffects"),
  r_nc_too_many_doses    = c("Sum_Too_many_doses"),
  r_nc_child_sick        = c("Sum_Child_sick", "Tot_child_NC_ChildSick_T"),
  r_nc_covid             = c("Sum_NC_COVID"),
  r_nc_other_detail      = c("Sum_NC_Others", "Tot_child_NC_Others_T")
)

algeria_abs_hh_patterns <- list(
  r_abs_sick_hh       = "^HH\\[[0-9]+\\]/group2/Tot_child_Abs_Sick$",
  r_abs_school_hh     = "^HH\\[[0-9]+\\]/group2/Tot_child_Abs_School$",
  r_abs_play_hh       = "^HH\\[[0-9]+\\]/group2/Tot_child_Abs_Play_areas$",
  r_abs_social_hh     = "^HH\\[[0-9]+\\]/group2/Tot_child_Abs_Social_event$",
  r_abs_travel_hh     = "^HH\\[[0-9]+\\]/group2/Tot_child_Abs_Travelling$",
  r_abs_other_hh      = "^HH\\[[0-9]+\\]/group2/Other_Reason_Absent$"
)

algeria_nc_hh_patterns <- list(
  r_nc_child_sick_hh  = "^HH\\[[0-9]+\\]/group4/Tot_child_NC_Child_was_sick$",
  r_nc_not_decided_hh = "^HH\\[[0-9]+\\]/group4/Tot_child_NC_pas_decide$",
  r_nc_polio_free_hh  = "^HH\\[[0-9]+\\]/group4/Tot_child_NC_PolioFree$",
  r_nc_nopv_hh        = "^HH\\[[0-9]+\\]/group4/Tot_child_NC_nOPV$",
  r_nc_other_hh       = "^HH\\[[0-9]+\\]/group4/Tot_child_NC_Other$"
)

# ============================================================
# PREPAREDNESS LOOKUP
# ============================================================
load_preparedness_lookup <- function(preparedness_file) {
  date <- read_excel(preparedness_file)
  
  date <- date %>%
    mutate(`Round Number` = case_when(
      `Round Number` == "Round 0" ~ "Rnd0",
      `Round Number` == "Round 1" ~ "Rnd1",
      `Round Number` == "Round 2" ~ "Rnd2",
      `Round Number` == "Round 3" ~ "Rnd3",
      `Round Number` == "Round 4" ~ "Rnd4",
      `Round Number` == "Round 5" ~ "Rnd5",
      `Round Number` == "Round 6" ~ "Rnd6",
      TRUE ~ `Round Number`
    ))
  
  prep_data <- date %>%
    rename(
      Response = `OBR Name`,
      Vaccine.type = Vaccines,
      roundNumber = `Round Number`
    ) %>%
    mutate(
      round_start_date = as_date(`Round Start Date`),
      round_start_date = case_when(
        Country == "ALGERIA" & Response == "ALG-2024-01-01_nOPV" & roundNumber == "Rnd1" ~ as_date("2024-02-18"),
        TRUE ~ round_start_date
      ),
      start_date = round_start_date + 4,
      end_date = as_date(start_date) + 1
    ) %>%
    select(Response, Vaccine.type, roundNumber, round_start_date, start_date, end_date)
  
  as_tibble(prep_data) %>%
    mutate(
      start_date = as_date(start_date),
      end_date = as_date(end_date),
      round_start_date = as_date(round_start_date)
    )
}

lookup_table <- load_preparedness_lookup(preparedness_file)

# ============================================================
# TRANSFORMATION FUNCTIONS
# ============================================================
apply_country_specific_transformations <- function(data, file_name) {
  if (startsWith(file_name, "3550")) {
    data <- data %>% mutate(Country = "GHA")
  }
  
  if (startsWith(file_name, "8834")) {
    data <- data %>% mutate(Region = District)
  }
  
  if (startsWith(file_name, "4351")) {
    if ("district" %in% names(data)) {
      data <- data %>% mutate(District = district)
    }
  }
  
  data
}

select_columns_dynamically <- function(df, required_cols, hh_patterns, hh_count = 10) {
  selected_cols <- c()
  
  for (col in required_cols) {
    matched_col <- find_similar_column(col, df)
    if (!is.na(matched_col)) selected_cols <- c(selected_cols, matched_col)
  }
  
  for (hh_num in 1:hh_count) {
    for (pattern in hh_patterns) {
      candidates <- build_hh_candidate_names(hh_num, pattern)
      for (cand in candidates) {
        matched_col <- find_similar_column(cand, df, exact_first = TRUE)
        if (!is.na(matched_col)) {
          selected_cols <- c(selected_cols, matched_col)
          break
        }
      }
    }
  }
  
  detailed_cols <- unique(c(
    unlist(absence_total_candidates),
    unlist(nc_total_candidates),
    "Tot_child_NC_NotDecide_T",
    "Tot_child_NC_PolioFREE_T",
    "Tot_child_NC_nOPV_T",
    "Tot_child_NC_ChildSick_T",
    "Tot_child_NC_Others_T",
    "Tot_child_Abs_Sick_T"
  ))
  
  selected_cols <- unique(c(selected_cols, intersect(detailed_cols, names(df))))
  
  for (p in c(unlist(algeria_abs_hh_patterns), unlist(algeria_nc_hh_patterns))) {
    selected_cols <- unique(c(selected_cols, grep(p, names(df), value = TRUE)))
  }
  
  unique(selected_cols)
}

safe_filter_data <- function(df) {
  result <- df
  
  if ("Type_Monitoring" %in% names(result)) {
    result <- result %>% filter(Type_Monitoring == "EndProcess")
  }
  
  if ("Response" %in% names(result)) {
    result <- result %>% filter(!is.na(Response), Response != "", Response != "n/a", Response != "NA")
  }
  
  if ("roundNumber" %in% names(result)) {
    result <- result %>% filter(!is.na(roundNumber), roundNumber != "", roundNumber != "n/a", roundNumber != "NA")
  }
  
  if ("Total_U5_Present" %in% names(result)) {
    result <- result %>% filter(is.na(Total_U5_Present) | Total_U5_Present != "n/a")
  }
  
  if ("TotalFM" %in% names(result)) {
    result <- result %>% filter(!is.na(TotalFM))
  }
  
  result
}

standardize_districts <- function(df) {
  needed <- c("Country", "Region", "District")
  if (!all(needed %in% names(df))) {
    return(df)
  }
  
  df %>%
    mutate(District = case_when(
      Country == "COTE D'IVOIRE" & Region == "MORONOU" & District == "MBATTO" ~ "MBATTO",
      Country == "COTE D'IVOIRE" & Region == "MORONOU" & District == "M'BATTO" ~ "MBATTO",
      Country == "COTE D'IVOIRE" & Region == "ABIDJAN1" & District == "ABOBO_EST" ~ "ABOBO EST",
      Country == "COTE D'IVOIRE" & Region == "ABIDJAN1" & District == "ABOBO_OUEST" ~ "ABOBO OUEST",
      Country == "CHAD" & Region == "BATHA" & District == "OUM_HADJER" ~ "OUM_HADJER",
      Country == "CHAD" & Region == "BATHA" & District == "OUM HADJER" ~ "OUM_HADJER",
      Country == "CHAD" & Region == "NDJAMENA" & District == "NDJAMENA_SUD" ~ "N'DJAMENA-SUD",
      Country == "CHAD" & Region == "NDJAMENA" & District == "NDJAMENA_NORD" ~ "N'DJAMENA-NORD",
      Country == "BENIN" & Region == "LITTORAL" & District == "COTONOU 1" ~ "COTONOU 1",
      Country == "BENIN" & Region == "LITTORAL" & District == "Cotonou I" ~ "COTONOU 1",
      TRUE ~ District
    ))
}

standardize_responses <- function(df) {
  needed <- c("Country", "Response")
  if (!all(needed %in% names(df))) {
    return(df)
  }
  
  df %>%
    mutate(Response = case_when(
      Country == "COTE D'IVOIRE" & str_detect(Response, "ABENGOUROU|ABOBO_EST|ABOBO_OUEST|ABOISSO") ~ "CIV-113DS-09-2020",
      Country == "MAL" & str_detect(Response, "Arfounda|BAMAKO|Banamba|Nara") ~ "MLI-12DS-01-2021",
      TRUE ~ Response
    ))
}

assign_vaccine_types <- function(df) {
  if (!all(c("Country", "Response", "roundNumber") %in% names(df))) {
    return(df)
  }
  
  df %>%
    mutate(
      roundNumber = toupper(roundNumber),
      roundNumber = case_when(
        str_detect(roundNumber, "0") ~ "Rnd0",
        str_detect(roundNumber, "1") ~ "Rnd1",
        str_detect(roundNumber, "2") ~ "Rnd2",
        str_detect(roundNumber, "3") ~ "Rnd3",
        str_detect(roundNumber, "4") ~ "Rnd4",
        str_detect(roundNumber, "5") ~ "Rnd5",
        str_detect(roundNumber, "6") ~ "Rnd6",
        TRUE ~ roundNumber
      )
    ) %>%
    mutate(
      Vaccine.type = case_when(
        Country == "BENIN" & Response == "KETOU" ~ "mOPV",
        Country == "COG" & Response == "Congo" ~ "nOPV",
        Country == "GUI" & Response == "Conakry" ~ "mOPV",
        Country == "COTE D'IVOIRE" & Response == "CIV-113DS-09-2020" ~ "mOPV",
        Country == "MAL" & Response == "MLI-12DS-01-2021" ~ "mOPV",
        str_detect(Response, "CHD-2025-10-0n_bOPV-NIDs|GUI-2025-01-NID_bOPV-nOPV") ~ "nOPV2 & bOPV",
        str_detect(Response, "BITTOU|MENAKA-mOPV2|BAMAKO-mOPV2|KANKAN-mOPV|MLI-12DS-01-2021-mOPV2|CONAKRY-mOPV|Ouagadogou|Bangui 1|GOTHEY|YOPOUGON|Golfe|MDG-2023-03-01_bOPV|BEN-xxDS-02-2020|BEN-26DS-08-2020|Chavuma-mOPV|Luapula-mOPV") ~ "mOPV",
        str_detect(Response, "nOPV|VPOn|TSHUAPA|Tanganyika|Liberia|Mauritania|KOUIBLY|Sierra Leone|SEN|CEN|MAL|BEN-39DS-01-2021|BERTOUA|EBOLOWA|EXNORD|ExtNord2023|ADDIS ABABA|Mekelle|AMANSIE SOUTH|CAF-2020-002|CENBLOCK|CENTRALBLK|CHA-17DS-02-2020|DONOMANGA|GNBnOPV|GOLFE|GOTHEYE|KEN-13DS-02-2021|MopUp2022|SSD-79DS-09-2020|ALG-2023-09-01_nOPV|ALG-2024-01-01_nOPV|nOPV2022|BEN-2023-09-01_nOPV|BFA-2023-05-01_nOPV|BFA-2023-09-01_nOPV|BFA-2024-02-01_nOPV|BITTOU-mOPV2|Ouagadogou-mOPV2|BOT-2023-02-01_nOPV|CAM-2023-05-01_nOPV|CAM-2023-08-01_nOPV|CAM-2024-02-01_nOPV|nOPV2023|nVPO|nVPO_Maradi|nVPO_Zinder|nVPO2|May2021|OPVb2021|OPVb2022|RSSmOPV10C2021|SEN_VPOn|UGAnOPV|VPOb|VPOb13ProV") ~ "nOPV2",
        str_detect(Response, "BOPV|bOPV|OPVb|WPV1") ~ "bOPV",
        str_detect(Response, "mOPV") ~ "mOPV",
        str_detect(Response, "OPV") & !str_detect(Response, "nOPV|bOPV|mOPV") ~ "bOPV",
        TRUE ~ "other"
      ),
      Response = case_when(
        Response == "nOPV2022" & Country == "GHA" ~ "nOPV2022",
        Response == "CENTRALBLK" ~ "DRC-7DS-02-2022",
        Response == "nOPV2022" & Country == "RDC" ~ "DRC-39DS-01-2021",
        Response %in% c("Tshuapa", "TSHUAPA") ~ "DRC-23DS-12-2020",
        Response == "VPOb13ProV" ~ "DRC-39DS-01-2021",
        TRUE ~ Response
      )
    ) %>%
    mutate(
      Vaccine.type = case_when(
        Response == "DRC-2025-02-01_nOPV_sNID" &
          roundNumber == "Rnd1" &
          Region %in% c("HAUT KATANGA", "HAUT LOMAMI", "TANGANIKA", "KINSHASA") ~ "nOPV2",
        Response == "DRC-2025-02-01_nOPV_sNID" &
          roundNumber == "Rnd1" &
          Region == "TSHOPO" &
          District %in% c("ALUNGULI", "FEREKENI", "KAILO", "LUBUTU", "OBOKOTE", "OPIENGE") ~ "bOPV",
        TRUE ~ Vaccine.type
      )
    )
}

create_summary_columns <- function(df, file_name) {
  is_algeria_8587 <- identical(file_name, ALGERIA_IM_FORM_ID)
  
  if (is_algeria_8587) {
    present_cols <- names(df)[names(df) %in% sprintf("HH[%s]/Total_U6_Present_HH", 1:10)]
    fm_cols <- names(df)[names(df) %in% sprintf("HH[%s]/U6_Vac_FM_HH", 1:10)]
  } else {
    present_cols <- names(df)[names(df) %in% sprintf("HH[%s]/Total_U5_Present_HH", 1:10)]
    fm_cols <- names(df)[names(df) %in% sprintf("HH[%s]/U5_Vac_FM_HH", 1:10)]
  }
  
  abs_cols <- unique(c(
    get_regex_cols(df, "^HH\\[[0-9]+\\]/group1/Tot_child_Absent_HH$"),
    get_regex_cols(df, "^HH\\[[0-9]+\\]/Tot_child_Absent_HH$")
  ))
  
  nc_cols <- unique(c(
    get_regex_cols(df, "^HH\\[[0-9]+\\]/group1/Tot_child_NC_HH$"),
    get_regex_cols(df, "^HH\\[[0-9]+\\]/Tot_child_NC_HH$")
  ))
  
  notvisited_cols <- unique(c(
    get_regex_cols(df, "^HH\\[[0-9]+\\]/group1/Tot_child_NotVisited_HH$"),
    get_regex_cols(df, "^HH\\[[0-9]+\\]/Tot_child_NotVisited_HH$")
  ))
  
  notrevisited_cols <- unique(c(
    get_regex_cols(df, "^HH\\[[0-9]+\\]/group1/Tot_child_NotRevisited$"),
    get_regex_cols(df, "^HH\\[[0-9]+\\]/Tot_child_NotRevisited$")
  ))
  
  asleep_cols <- unique(c(
    get_regex_cols(df, "^HH\\[[0-9]+\\]/group1/Tot_child_Asleep_HH$"),
    get_regex_cols(df, "^HH\\[[0-9]+\\]/Tot_child_Asleep_HH$")
  ))
  
  routine_cols <- unique(c(
    get_regex_cols(df, "^HH\\[[0-9]+\\]/group1/Tot_child_VaccinatedRoutine$"),
    get_regex_cols(df, "^HH\\[[0-9]+\\]/Tot_child_VaccinatedRoutine$")
  ))
  
  other_cols <- unique(c(
    get_regex_cols(df, "^HH\\[[0-9]+\\]/group1/Tot_child_Others_HH$"),
    get_regex_cols(df, "^HH\\[[0-9]+\\]/Tot_child_Others_HH$")
  ))
  
  caregiver_cols <- unique(c(
    get_regex_cols(df, "^HH\\[[0-9]+\\]/group1/Parent_Caregive_Inform_HH$"),
    get_regex_cols(df, "^HH\\[[0-9]+\\]/Parent_Caregive_Inform_HH$")
  ))
  
  abs_sick_hh_cols   <- get_regex_cols(df, algeria_abs_hh_patterns$r_abs_sick_hh)
  abs_school_hh_cols <- get_regex_cols(df, algeria_abs_hh_patterns$r_abs_school_hh)
  abs_play_hh_cols   <- get_regex_cols(df, algeria_abs_hh_patterns$r_abs_play_hh)
  abs_social_hh_cols <- get_regex_cols(df, algeria_abs_hh_patterns$r_abs_social_hh)
  abs_travel_hh_cols <- get_regex_cols(df, algeria_abs_hh_patterns$r_abs_travel_hh)
  abs_other_hh_cols  <- get_regex_cols(df, algeria_abs_hh_patterns$r_abs_other_hh)
  
  nc_childsick_hh_cols <- get_regex_cols(df, algeria_nc_hh_patterns$r_nc_child_sick_hh)
  nc_notdecide_hh_cols <- get_regex_cols(df, algeria_nc_hh_patterns$r_nc_not_decided_hh)
  nc_poliofree_hh_cols <- get_regex_cols(df, algeria_nc_hh_patterns$r_nc_polio_free_hh)
  nc_nopv_hh_cols      <- get_regex_cols(df, algeria_nc_hh_patterns$r_nc_nopv_hh)
  nc_other_hh_cols     <- get_regex_cols(df, algeria_nc_hh_patterns$r_nc_other_hh)
  
  df %>%
    mutate(
      u5_present = safe_row_sum(., present_cols),
      u5_FM1 = safe_row_sum(., fm_cols),
      u5_FM = ifelse(u5_FM1 > u5_present & u5_present > 0, u5_present, u5_FM1),
      u5_FM = ifelse(is.na(u5_FM), 0, u5_FM),
      missed_child = pmax(0, u5_present - u5_FM),
      
      r_non_FM_Absent = safe_row_sum(., abs_cols),
      r_non_FM_NC = safe_row_sum(., nc_cols),
      r_non_FM_hh_notvisited = safe_row_sum(., notvisited_cols),
      r_non_FM_hh_notrevisited = safe_row_sum(., notrevisited_cols),
      r_non_FM_sleep = safe_row_sum(., asleep_cols),
      r_non_FM_vaccinatedRoutine = safe_row_sum(., routine_cols),
      r_non_FM_other = safe_row_sum(., other_cols),
      care_Giver_Informed_SIA = safe_row_sum(., caregiver_cols),
      
      r_abs_play_areas = safe_pick_first(., absence_total_candidates$r_abs_play_areas),
      r_abs_market = safe_pick_first(., absence_total_candidates$r_abs_market),
      r_abs_school = safe_pick_first(., absence_total_candidates$r_abs_school),
      r_abs_farm = safe_pick_first(., absence_total_candidates$r_abs_farm),
      r_abs_social_event = safe_pick_first(., absence_total_candidates$r_abs_social_event),
      r_abs_travelling = safe_pick_first(., absence_total_candidates$r_abs_travelling),
      r_abs_parent_absent = safe_pick_first(., absence_total_candidates$r_abs_parent_absent),
      r_abs_other_detail_form = safe_pick_first(., absence_total_candidates$r_abs_other_detail),
      
      r_abs_sick = safe_row_sum(., abs_sick_hh_cols),
      r_abs_school_hh = safe_row_sum(., abs_school_hh_cols),
      r_abs_play_hh = safe_row_sum(., abs_play_hh_cols),
      r_abs_social_hh = safe_row_sum(., abs_social_hh_cols),
      r_abs_travel_hh = safe_row_sum(., abs_travel_hh_cols),
      r_abs_other_hh = safe_row_sum(., abs_other_hh_cols),
      
      r_nc_religious_beliefs = safe_pick_first(., nc_total_candidates$r_nc_religious_beliefs),
      r_nc_side_effects = safe_pick_first(., nc_total_candidates$r_nc_side_effects),
      r_nc_too_many_doses = safe_pick_first(., nc_total_candidates$r_nc_too_many_doses),
      r_nc_child_sick_form = safe_pick_first(., nc_total_candidates$r_nc_child_sick),
      r_nc_covid = safe_pick_first(., nc_total_candidates$r_nc_covid),
      r_nc_other_detail_form = safe_pick_first(., nc_total_candidates$r_nc_other_detail),
      
      r_nc_child_sick_hh = safe_row_sum(., nc_childsick_hh_cols),
      r_nc_not_decided = safe_row_sum(., nc_notdecide_hh_cols),
      r_nc_polio_free = safe_row_sum(., nc_poliofree_hh_cols),
      r_nc_nopv = safe_row_sum(., nc_nopv_hh_cols),
      r_nc_other_hh = safe_row_sum(., nc_other_hh_cols)
    ) %>%
    mutate(
      r_abs_other_detail = pmax(r_abs_other_detail_form, r_abs_other_hh, na.rm = TRUE),
      r_abs_school = pmax(r_abs_school, r_abs_school_hh, na.rm = TRUE),
      r_abs_play_areas = pmax(r_abs_play_areas, r_abs_play_hh, na.rm = TRUE),
      r_abs_social_event = pmax(r_abs_social_event, r_abs_social_hh, na.rm = TRUE),
      r_abs_travelling = pmax(r_abs_travelling, r_abs_travel_hh, na.rm = TRUE),
      
      r_nc_child_sick = pmax(r_nc_child_sick_form, r_nc_child_sick_hh, na.rm = TRUE),
      r_nc_other_detail = pmax(r_nc_other_detail_form, r_nc_other_hh, na.rm = TRUE),
      
      abs_detail_total =
        r_abs_sick + r_abs_play_areas + r_abs_market + r_abs_school +
        r_abs_farm + r_abs_social_event + r_abs_travelling +
        r_abs_parent_absent + r_abs_other_detail,
      
      nc_detail_total =
        r_nc_religious_beliefs + r_nc_side_effects + r_nc_too_many_doses +
        r_nc_child_sick + r_nc_covid + r_nc_other_detail +
        r_nc_not_decided + r_nc_polio_free + r_nc_nopv
    )
}

process_final_data <- function(df) {
  df %>%
    mutate(
      Country = case_when(
        Country == "DRC" ~ "RDC",
        Country == "Camerooun" ~ "CAE",
        Country == "BURKINA_FASO" ~ "BFA",
        Country == "CAMEROON" ~ "CAE",
        Country == "CHAD" ~ "CHD",
        TRUE ~ Country
      ),
      roundNumber = case_when(
        roundNumber == "RND2" ~ "Rnd2",
        TRUE ~ roundNumber
      ),
      reasons_total =
        r_non_FM_Absent +
        r_non_FM_NC +
        r_non_FM_hh_notvisited +
        r_non_FM_hh_notrevisited +
        r_non_FM_sleep +
        r_non_FM_vaccinatedRoutine +
        r_non_FM_other,
      check_missed = missed_child - reasons_total,
      unexplained_missed = pmax(check_missed, 0),
      overreported_reasons = pmax(-check_missed, 0),
      explained_ratio = ifelse(missed_child > 0, round(reasons_total / missed_child, 3), NA_real_),
      unexplained_ratio = ifelse(missed_child > 0, round(unexplained_missed / missed_child, 3), NA_real_),
      check_abs_detail = r_non_FM_Absent - abs_detail_total,
      check_nc_detail = r_non_FM_NC - nc_detail_total,
      reconciliation_flag = case_when(
        check_missed == 0 ~ "Consistent",
        check_missed > 0 & reasons_total == 0 ~ "No reasons recorded",
        check_missed > 0 ~ "Partial reasons recorded",
        check_missed < 0 ~ "Overlapping reasons",
        TRUE ~ "Unknown"
      ),
      abs_detail_flag = case_when(
        check_abs_detail == 0 ~ "Abs detail consistent",
        check_abs_detail > 0 ~ "Abs detail incomplete",
        check_abs_detail < 0 ~ "Abs detail overlapping",
        TRUE ~ "Unknown"
      ),
      nc_detail_flag = case_when(
        check_nc_detail == 0 ~ "NC detail consistent",
        check_nc_detail > 0 ~ "NC detail incomplete",
        check_nc_detail < 0 ~ "NC detail overlapping",
        TRUE ~ "Unknown"
      ),
      qc_flag = case_when(
        check_missed == 0 & check_abs_detail == 0 & check_nc_detail == 0 ~ "OK",
        TRUE ~ "Needs review"
      )
    )
}

# ============================================================
# MAIN FILE PROCESSOR
# ============================================================
process_im_file <- function(input_file, output_folder, qc_output_folder, lookup_table) {
  file_name <- tools::file_path_sans_ext(basename(input_file))
  output_file <- file.path(output_folder, paste0(file_name, ".csv"))
  qc_output_file <- file.path(qc_output_folder, paste0(file_name, "_QC.csv"))
  
  message("\n============================================================")
  message("Processing file: ", basename(input_file))
  message("============================================================")
  
  data <- read_input_data(input_file)
  
  # quick schema check: skip obvious non-IM files
  minimum_im_markers <- c("Response", "roundNumber", "Type_Monitoring")
  if (!any(minimum_im_markers %in% names(data))) {
    stop("File does not look like an IM dataset.")
  }
  
  if (!"Country" %in% names(data)) {
    data$Country <- NA_character_
  }
  
  data <- apply_country_specific_transformations(data, file_name)
  data <- rename_repetitive_columns(data)
  
  active_hh_patterns <- if (identical(file_name, ALGERIA_IM_FORM_ID)) {
    hh_patterns_algeria
  } else {
    hh_patterns_standard
  }
  
  columns_to_select <- select_columns_dynamically(data, required_columns, active_hh_patterns)
  
  GF <- data %>%
    safe_filter_data() %>%
    select(any_of(columns_to_select))
  
  if (nrow(GF) == 0) {
    message("Warning: No data after filtering. Using original data with selected columns.")
    GF <- data %>% select(any_of(columns_to_select))
  }
  
  hh_cols <- names(GF)[str_detect(names(GF), "^HH\\[")]
  
  for (col in hh_cols) {
    if (str_detect(col, "Parent_Caregive_Inform_HH")) {
      GF[[col]] <- clean_yes_no_numeric(GF[[col]])
    } else {
      GF[[col]] <- clean_numeric(GF[[col]])
    }
  }
  
  numeric_cols <- intersect(
    c(
      "HH_count", "Total_U5_Present", "TotalFM", "sum_missed_children",
      "Total_Absent", "Total_refusal",
      unlist(absence_total_candidates),
      unlist(nc_total_candidates),
      "Tot_child_NC_NotDecide_T", "Tot_child_NC_PolioFREE_T",
      "Tot_child_NC_nOPV_T", "Tot_child_NC_ChildSick_T",
      "Tot_child_NC_Others_T", "Tot_child_Abs_Sick_T"
    ),
    names(GF)
  )
  
  if (length(numeric_cols) > 0) {
    GF[numeric_cols] <- lapply(GF[numeric_cols], clean_numeric)
  }
  
  if ("date_monitored" %in% names(GF)) {
    GF <- GF %>% mutate(date_monitored = parse_mixed_dates(date_monitored))
  }
  
  GH <- create_summary_columns(GF, file_name = file_name)
  
  if ("HH_count" %in% names(GH)) {
    GH <- GH %>%
      mutate(Number_of_HH_visited = suppressWarnings(as.numeric(HH_count)))
  } else if ("Number_of_HH_visited" %in% names(GH)) {
    GH <- GH %>%
      mutate(Number_of_HH_visited = suppressWarnings(as.numeric(Number_of_HH_visited)))
  } else {
    GH$Number_of_HH_visited <- NA_real_
  }
  
  if (!"Total_U5_Present" %in% names(GH)) GH$Total_U5_Present <- NA_real_
  if (!"TotalFM" %in% names(GH)) GH$TotalFM <- NA_real_
  
  GJ <- GH %>%
    mutate(
      Total_U5_Present = suppressWarnings(as.numeric(Total_U5_Present)),
      TotalFM = suppressWarnings(as.numeric(TotalFM))
    ) %>%
    standardize_districts()
  
  GO <- GJ %>% standardize_responses()
  GK <- GO %>% assign_vaccine_types()
  GL <- GK %>% process_final_data()
  
  required_final_columns <- c(
    "Country", "Region", "District", "Response", "Vaccine.type", "roundNumber",
    "date_monitored", "Number_of_HH_visited", "u5_present", "u5_FM", "missed_child",
    "r_non_FM_Absent", "r_non_FM_NC", "r_non_FM_hh_notvisited", "r_non_FM_hh_notrevisited",
    "r_non_FM_sleep", "r_non_FM_vaccinatedRoutine", "r_non_FM_other",
    "care_Giver_Informed_SIA",
    "r_abs_sick", "r_abs_play_areas", "r_abs_market", "r_abs_school",
    "r_abs_farm", "r_abs_social_event", "r_abs_travelling", "r_abs_parent_absent", "r_abs_other_detail",
    "r_nc_religious_beliefs", "r_nc_side_effects", "r_nc_too_many_doses",
    "r_nc_child_sick", "r_nc_covid", "r_nc_other_detail", "r_nc_not_decided",
    "r_nc_polio_free", "r_nc_nopv",
    "abs_detail_total", "nc_detail_total", "reasons_total",
    "check_missed", "check_abs_detail", "check_nc_detail",
    "unexplained_missed", "overreported_reasons",
    "explained_ratio", "unexplained_ratio",
    "reconciliation_flag", "abs_detail_flag", "nc_detail_flag", "qc_flag"
  )
  
  for (col in required_final_columns) {
    if (!col %in% names(GL)) {
      GL[[col]] <- NA
    }
  }
  
  F5 <- GL %>%
    mutate(
      start_date = as_date(date_monitored),
      end_date = as_date(date_monitored),
      year = year(start_date),
      cv = ifelse(u5_present > 0, round(u5_FM / u5_present, 2), NA_real_),
      percent_care_Giver_Informed_SIA = ifelse(
        Number_of_HH_visited > 0,
        round((care_Giver_Informed_SIA / Number_of_HH_visited) * 100, 2),
        NA_real_
      )
    ) %>%
    group_by(Country, Region, District, Response, Vaccine.type, roundNumber) %>%
    summarise(
      start_date = min(start_date, na.rm = TRUE),
      end_date = max(end_date, na.rm = TRUE),
      Number_of_HH_visited = sum(Number_of_HH_visited, na.rm = TRUE),
      u5_present = sum(u5_present, na.rm = TRUE),
      u5_FM = sum(u5_FM, na.rm = TRUE),
      missed_child = sum(missed_child, na.rm = TRUE),
      r_non_FM_Absent = sum(r_non_FM_Absent, na.rm = TRUE),
      r_non_FM_NC = sum(r_non_FM_NC, na.rm = TRUE),
      r_non_FM_hh_notvisited = sum(r_non_FM_hh_notvisited, na.rm = TRUE),
      r_non_FM_hh_notrevisited = sum(r_non_FM_hh_notrevisited, na.rm = TRUE),
      r_non_FM_sleep = sum(r_non_FM_sleep, na.rm = TRUE),
      r_non_FM_vaccinatedRoutine = sum(r_non_FM_vaccinatedRoutine, na.rm = TRUE),
      r_non_FM_other = sum(r_non_FM_other, na.rm = TRUE),
      care_Giver_Informed_SIA = sum(care_Giver_Informed_SIA, na.rm = TRUE),
      r_abs_sick = sum(r_abs_sick, na.rm = TRUE),
      r_abs_play_areas = sum(r_abs_play_areas, na.rm = TRUE),
      r_abs_market = sum(r_abs_market, na.rm = TRUE),
      r_abs_school = sum(r_abs_school, na.rm = TRUE),
      r_abs_farm = sum(r_abs_farm, na.rm = TRUE),
      r_abs_social_event = sum(r_abs_social_event, na.rm = TRUE),
      r_abs_travelling = sum(r_abs_travelling, na.rm = TRUE),
      r_abs_parent_absent = sum(r_abs_parent_absent, na.rm = TRUE),
      r_abs_other_detail = sum(r_abs_other_detail, na.rm = TRUE),
      r_nc_religious_beliefs = sum(r_nc_religious_beliefs, na.rm = TRUE),
      r_nc_side_effects = sum(r_nc_side_effects, na.rm = TRUE),
      r_nc_too_many_doses = sum(r_nc_too_many_doses, na.rm = TRUE),
      r_nc_child_sick = sum(r_nc_child_sick, na.rm = TRUE),
      r_nc_covid = sum(r_nc_covid, na.rm = TRUE),
      r_nc_other_detail = sum(r_nc_other_detail, na.rm = TRUE),
      r_nc_not_decided = sum(r_nc_not_decided, na.rm = TRUE),
      r_nc_polio_free = sum(r_nc_polio_free, na.rm = TRUE),
      r_nc_nopv = sum(r_nc_nopv, na.rm = TRUE),
      abs_detail_total = sum(abs_detail_total, na.rm = TRUE),
      nc_detail_total = sum(nc_detail_total, na.rm = TRUE),
      reasons_total = sum(reasons_total, na.rm = TRUE),
      unexplained_missed = sum(unexplained_missed, na.rm = TRUE),
      overreported_reasons = sum(overreported_reasons, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      cv = ifelse(u5_present > 0, round(u5_FM / u5_present, 2), NA_real_),
      year = year(start_date),
      percent_care_Giver_Informed_SIA = ifelse(
        Number_of_HH_visited > 0,
        round((care_Giver_Informed_SIA / Number_of_HH_visited) * 100, 2),
        NA_real_
      ),
      check_missed = missed_child - reasons_total,
      check_abs_detail = r_non_FM_Absent - abs_detail_total,
      check_nc_detail = r_non_FM_NC - nc_detail_total,
      explained_ratio = ifelse(missed_child > 0, round(reasons_total / missed_child, 3), NA_real_),
      unexplained_ratio = ifelse(missed_child > 0, round(unexplained_missed / missed_child, 3), NA_real_),
      reconciliation_flag = case_when(
        check_missed == 0 ~ "Consistent",
        check_missed > 0 & reasons_total == 0 ~ "No reasons recorded",
        check_missed > 0 ~ "Partial reasons recorded",
        check_missed < 0 ~ "Overlapping reasons",
        TRUE ~ "Unknown"
      ),
      abs_detail_flag = case_when(
        check_abs_detail == 0 ~ "Abs detail consistent",
        check_abs_detail > 0 ~ "Abs detail incomplete",
        check_abs_detail < 0 ~ "Abs detail overlapping",
        TRUE ~ "Unknown"
      ),
      nc_detail_flag = case_when(
        check_nc_detail == 0 ~ "NC detail consistent",
        check_nc_detail > 0 ~ "NC detail incomplete",
        check_nc_detail < 0 ~ "NC detail overlapping",
        TRUE ~ "Unknown"
      ),
      qc_flag = case_when(
        check_missed == 0 & check_abs_detail == 0 & check_nc_detail == 0 ~ "OK",
        TRUE ~ "Needs review"
      )
    ) %>%
    filter(start_date > as_date("2019-10-01"))
  
  FI <- F5 %>%
    left_join(
      lookup_table,
      by = c("Response", "Vaccine.type", "roundNumber"),
      suffix = c("", "_lookup")
    ) %>%
    mutate(
      start_date = coalesce(start_date_lookup, start_date),
      end_date = coalesce(end_date_lookup, end_date),
      round_start_date = coalesce(round_start_date, start_date - days(4))
    ) %>%
    select(-ends_with("_lookup")) %>%
    filter(District != "NA")
  
  FE <- FI %>%
    select(
      country = Country,
      province = Region,
      district = District,
      response = Response,
      vaccine.type = Vaccine.type,
      roundNumber,
      round_start_date,
      start_date_IM_end = start_date,
      end_date_IM_end = end_date,
      year,
      Number_of_HH_visited,
      u5_present,
      u5_FM,
      missed_child,
      cv,
      r_non_FM_Absent,
      r_non_FM_NC,
      r_non_FM_hh_notvisited,
      r_non_FM_hh_notrevisited,
      r_non_FM_sleep,
      r_non_FM_vaccinatedRoutine,
      r_non_FM_other,
      r_abs_sick,
      r_abs_play_areas,
      r_abs_market,
      r_abs_school,
      r_abs_farm,
      r_abs_social_event,
      r_abs_travelling,
      r_abs_parent_absent,
      r_abs_other_detail,
      r_nc_religious_beliefs,
      r_nc_side_effects,
      r_nc_too_many_doses,
      r_nc_child_sick,
      r_nc_covid,
      r_nc_other_detail,
      r_nc_not_decided,
      r_nc_polio_free,
      r_nc_nopv,
      care_Giver_Informed_SIA,
      percent_care_Giver_Informed_SIA,
      reasons_total,
      abs_detail_total,
      nc_detail_total,
      check_missed,
      check_abs_detail,
      check_nc_detail,
      unexplained_missed,
      overreported_reasons,
      explained_ratio,
      unexplained_ratio,
      reconciliation_flag,
      abs_detail_flag,
      nc_detail_flag,
      qc_flag
    ) %>%
    arrange(start_date_IM_end)
  
  FE_QC <- FE %>%
    filter(
      qc_flag == "Needs review" |
        abs(check_missed) >= 5 |
        abs(check_abs_detail) >= 3 |
        abs(check_nc_detail) >= 3
    ) %>%
    arrange(desc(abs(check_missed)), desc(abs(check_abs_detail)), desc(abs(check_nc_detail)))
  
  write_csv(FE, output_file)
  write_csv(FE_QC, qc_output_file)
  
  message("Done: ", basename(input_file))
  message("  Output: ", output_file)
  message("  QC: ", qc_output_file)
  
  list(
    data = FE,
    qc = FE_QC,
    summary = tibble(
      file = basename(input_file),
      rows_output = nrow(FE),
      rows_qc = nrow(FE_QC),
      countries = paste(unique(FE$country), collapse = ", "),
      min_date = suppressWarnings(min(FE$start_date_IM_end, na.rm = TRUE)),
      max_date = suppressWarnings(max(FE$start_date_IM_end, na.rm = TRUE))
    )
  )
}

# ============================================================
# REGIONAL REPOSITORY BUILDER
# ============================================================
build_regional_im_repository <- function(processed_results, regional_repository_file, regional_qc_repository_file) {
  clean_list <- lapply(processed_results, function(x) x$data)
  qc_list <- lapply(processed_results, function(x) x$qc)
  
  regional_im <- bind_rows_fill(clean_list) %>%
    arrange(country, province, district, start_date_IM_end)
  
  regional_im_qc <- bind_rows_fill(qc_list) %>%
    arrange(desc(abs(check_missed)), desc(abs(check_abs_detail)), desc(abs(check_nc_detail)))
  
  write_csv(regional_im, regional_repository_file)
  write_csv(regional_im_qc, regional_qc_repository_file)
  
  message("\nRegional IM repository saved to: ", regional_repository_file)
  message("Regional IM QC repository saved to: ", regional_qc_repository_file)
  
  list(
    regional_im = regional_im,
    regional_im_qc = regional_im_qc
  )
}

# ============================================================
# BATCH RUNNER
# ============================================================
process_all_im_files <- function(input_folder, output_folder, qc_output_folder, lookup_table) {
  files <- list.files(
    input_folder,
    pattern = "\\.(rds|qs|csv|xlsx|xls|parquet)$",
    full.names = TRUE,
    ignore.case = TRUE
  )
  
  if (length(files) == 0) {
    stop("No supported files found in: ", input_folder)
  }
  
  # Exclude obvious non-IM / generated / log files
  bad_patterns <- c(
    "fetch_log",
    "processing_summary",
    "regional_im_repository",
    "_qc",
    "^qc$",
    "repository"
  )
  
  keep_file <- function(f) {
    b <- tolower(basename(f))
    !any(stringr::str_detect(b, bad_patterns))
  }
  
  files <- files[vapply(files, keep_file, logical(1))]
  
  if (length(files) == 0) {
    stop("No valid IM input files found after excluding logs/repository files in: ", input_folder)
  }
  
  message("Found ", length(files), " files to process.")
  
  processed_results <- list()
  summary_results <- list()
  
  for (f in files) {
    res <- tryCatch(
      process_im_file(f, output_folder, qc_output_folder, lookup_table),
      error = function(e) {
        message("ERROR in file ", basename(f), ": ", e$message)
        list(
          data = NULL,
          qc = NULL,
          summary = tibble(
            file = basename(f),
            rows_output = NA_integer_,
            rows_qc = NA_integer_,
            countries = NA_character_,
            min_date = as.Date(NA),
            max_date = as.Date(NA)
          )
        )
      }
    )
    
    processed_results[[basename(f)]] <- res
    summary_results[[basename(f)]] <- res$summary
  }
  
  list(
    processed_results = processed_results,
    summary_table = bind_rows(summary_results)
  )
}

# ============================================================
# RUN FULL PIPELINE
# ============================================================
batch_run <- process_all_im_files(
  input_folder = input_folder,
  output_folder = output_folder,
  qc_output_folder = qc_output_folder,
  lookup_table = lookup_table
)

summary_table <- batch_run$summary_table
processed_results <- batch_run$processed_results

write_csv(summary_table, summary_file)

regional_repo <- build_regional_im_repository(
  processed_results = processed_results,
  regional_repository_file = regional_repository_file,
  regional_qc_repository_file = regional_qc_repository_file
)

print(summary_table)

message("\nBatch processing completed.")
message("Summary file: ", summary_file)
message("Regional repository rows: ", nrow(regional_repo$regional_im))
message("Regional QC rows: ", nrow(regional_repo$regional_im_qc))