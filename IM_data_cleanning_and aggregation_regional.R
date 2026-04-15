# ============================================================
# Generic IM data cleaning and aggregation for AFRO countries
# Supports Algeria + other regional standardized IM datasets
# ============================================================

# ---- Libraries ----
library(tidyverse)
library(dplyr)
library(stringr)
library(stringi)
library(lubridate)
library(readr)

# ============================================================
# USER PARAMETERS
# ============================================================

input_file <- "C:/Users/TOURE/Mes documents/REPOSITORIES/IM_raw_data/IM_raw/INPUT_IM.csv"
output_file <- "C:/Users/TOURE/Mes documents/REPOSITORIES/IM_raw_data/IM_level/OUTPUT_IM.csv"
qc_output_file <- "C:/Users/TOURE/Mes documents/REPOSITORIES/IM_raw_data/IM_level/OUTPUT_IM_QC_flags.csv"

# If TRUE, aggregate consecutive monitoring dates into periods
use_period_grouping <- TRUE

# Minimum accepted start date
min_date <- as.Date("2019-10-01")

# ============================================================
# HELPERS
# ============================================================

parse_mixed_dates <- function(x) {
  x <- as.character(x)
  
  suppressWarnings(
    coalesce(
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
  x[x %in% c("n/a", "NA", "", " ", "null", "NULL", "NaN")] <- "0"
  suppressWarnings(as.numeric(x))
}

safe_row_sum <- function(data, cols) {
  cols <- intersect(cols, names(data))
  if (length(cols) == 0) return(rep(0, nrow(data)))
  rowSums(data[, cols, drop = FALSE], na.rm = TRUE)
}

safe_pick_first <- function(data, candidate_names, default = 0) {
  existing <- intersect(candidate_names, names(data))
  if (length(existing) == 0) return(rep(default, nrow(data)))
  data[[existing[1]]]
}

build_hh_cols <- function(df_names, suffixes, hh_range = 1:10) {
  out <- c()
  for (hh in hh_range) {
    for (sfx in suffixes) {
      p1 <- paste0("HH[", hh, "]/", sfx)
      p2 <- paste0("HH[", hh, "]/group1/", sfx)
      p3 <- paste0("HH[", hh, "]/group2/", sfx)
      p4 <- paste0("HH[", hh, "]/group3/", sfx)
      p5 <- paste0("HH[", hh, "]/group4/", sfx)
      out <- c(out, p1, p2, p3, p4, p5)
    }
  }
  intersect(unique(out), df_names)
}

sum_existing_cols <- function(df, cols) {
  cols <- intersect(cols, names(df))
  if (length(cols) == 0) return(rep(0, nrow(df)))
  rowSums(df[, cols, drop = FALSE], na.rm = TRUE)
}

# ============================================================
# STANDARDIZED REGIONAL COLUMN DEFINITIONS
# ============================================================

columns <- c(
  "Country", "Region", "District", "Response", "roundNumber",
  "Type_Monitoring", "date_monitored", "HH_count", "Total_U5_Present",
  "TotalFM", "sum_missed_children", "Total_Absent", "Total_refusal"
)

# HH patterns expected in regional standardized datasets
hh_patterns <- c(
  "Total_U5_Present_HH", "U5_Vac_FM_HH", "Tot_child_Absent_HH",
  "Tot_child_NC_HH", "Tot_child_NotVisited_HH", "Tot_child_NotRevisited",
  "Tot_child_Asleep_HH", "Tot_child_VaccinatedRoutine", "Tot_child_Others_HH",
  "Parent_Caregive_Inform_HH"
)

# Algeria special HH variants
algeria_special_patterns <- c(
  "Total_U6_Present_HH", "U6_Vac_FM_HH"
)

# Detailed absence totals (form-level)
absence_total_candidates <- list(
  r_abs_play_areas      = c("Tot_child_Abs_Play_areas_T"),
  r_abs_market          = c("Tot_child_Abs_Market_T"),
  r_abs_school          = c("Tot_child_Abs_School_T"),
  r_abs_farm            = c("Tot_child_Abs_Farm_T"),
  r_abs_social_event    = c("Tot_child_Abs_SocialEvent"),
  r_abs_travelling      = c("Sum_child_Abs_Travelling"),
  r_abs_parent_absent   = c("Sum_child_Abs_Parent_Absent"),
  r_abs_other_detail    = c("Tot_child_Abs_Other_T")
)

# Detailed NC totals (form-level)
nc_total_candidates <- list(
  r_nc_religious_beliefs = c("Tot_child_NC_Religious_beliefs_T"),
  r_nc_side_effects      = c("Tot_child_NC_sideEffects"),
  r_nc_too_many_doses    = c("Sum_Too_many_doses"),
  r_nc_child_sick        = c("Sum_Child_sick", "Tot_child_NC_ChildSick_T"),
  r_nc_covid             = c("Sum_NC_COVID"),
  r_nc_other_detail      = c("Sum_NC_Others", "Tot_child_NC_Others_T")
)

# Algeria detailed HH-level absence
algeria_abs_hh_candidates <- list(
  r_abs_sick         = "^HH\\[[0-9]+\\]/group2/Tot_child_Abs_Sick$",
  r_abs_school_hh    = "^HH\\[[0-9]+\\]/group2/Tot_child_Abs_School$",
  r_abs_play_hh      = "^HH\\[[0-9]+\\]/group2/Tot_child_Abs_Play_areas$",
  r_abs_social_hh    = "^HH\\[[0-9]+\\]/group2/Tot_child_Abs_Social_event$",
  r_abs_travel_hh    = "^HH\\[[0-9]+\\]/group2/Tot_child_Abs_Travelling$",
  r_abs_other_hh     = "^HH\\[[0-9]+\\]/group2/Other_Reason_Absent$"
)

# Algeria detailed HH-level NC
algeria_nc_hh_candidates <- list(
  r_nc_child_sick_hh = "^HH\\[[0-9]+\\]/group4/Tot_child_NC_Child_was_sick$",
  r_nc_not_decided_hh = "^HH\\[[0-9]+\\]/group4/Tot_child_NC_pas_decide$",
  r_nc_polio_free_hh  = "^HH\\[[0-9]+\\]/group4/Tot_child_NC_PolioFree$",
  r_nc_nopv_hh        = "^HH\\[[0-9]+\\]/group4/Tot_child_NC_nOPV$",
  r_nc_other_hh       = "^HH\\[[0-9]+\\]/group4/Tot_child_NC_Other$"
)

# ============================================================
# READ DATA
# ============================================================

DF <- read_csv(input_file, show_col_types = FALSE)

# ============================================================
# CORE FILTER
# ============================================================

GF <- DF %>%
  filter(
    !is.na(TotalFM),
    Type_Monitoring == "EndProcess",
    !is.na(Response),
    Response != "",
    Response != "n/a",
    Response != "NA",
    !is.na(roundNumber),
    roundNumber != "",
    roundNumber != "n/a"
  )

# ============================================================
# DETECT AVAILABLE COLUMNS
# ============================================================

df_names <- names(GF)

# Core columns that actually exist
core_cols_present <- intersect(columns, df_names)

# All HH-like columns to clean
hh_cols_present <- grep("^HH\\[", df_names, value = TRUE)

# Main U5/U6 HH columns
u_present_cols <- unique(c(
  grep("^HH\\[[0-9]+\\]/Total_U5_Present_HH$", df_names, value = TRUE),
  grep("^HH\\[[0-9]+\\]/Total_U6_Present_HH$", df_names, value = TRUE)
))

u_fm_cols <- unique(c(
  grep("^HH\\[[0-9]+\\]/U5_Vac_FM_HH$", df_names, value = TRUE),
  grep("^HH\\[[0-9]+\\]/U6_Vac_FM_HH$", df_names, value = TRUE)
))

# Main top-level HH reason columns
abs_main_cols <- grep("^HH\\[[0-9]+\\]/group1/Tot_child_Absent_HH$|^HH\\[[0-9]+\\]/Tot_child_Absent_HH$", df_names, value = TRUE)
nc_main_cols <- grep("^HH\\[[0-9]+\\]/group1/Tot_child_NC_HH$|^HH\\[[0-9]+\\]/Tot_child_NC_HH$", df_names, value = TRUE)
notvisited_main_cols <- grep("^HH\\[[0-9]+\\]/group1/Tot_child_NotVisited_HH$|^HH\\[[0-9]+\\]/Tot_child_NotVisited_HH$", df_names, value = TRUE)
notrevisited_main_cols <- grep("^HH\\[[0-9]+\\]/group1/Tot_child_NotRevisited$|^HH\\[[0-9]+\\]/Tot_child_NotRevisited$", df_names, value = TRUE)
asleep_main_cols <- grep("^HH\\[[0-9]+\\]/group1/Tot_child_Asleep_HH$|^HH\\[[0-9]+\\]/Tot_child_Asleep_HH$", df_names, value = TRUE)
routine_main_cols <- grep("^HH\\[[0-9]+\\]/group1/Tot_child_VaccinatedRoutine$|^HH\\[[0-9]+\\]/Tot_child_VaccinatedRoutine$", df_names, value = TRUE)
other_main_cols <- grep("^HH\\[[0-9]+\\]/group1/Tot_child_Others_HH$|^HH\\[[0-9]+\\]/Tot_child_Others_HH$", df_names, value = TRUE)
caregiver_info_cols <- grep("^HH\\[[0-9]+\\]/group1/Parent_Caregive_Inform_HH$|^HH\\[[0-9]+\\]/Parent_Caregive_Inform_HH$", df_names, value = TRUE)

# Algeria HH detailed patterns if present
get_regex_cols <- function(pattern) grep(pattern, df_names, value = TRUE)

alg_abs_sick_hh    <- get_regex_cols(algeria_abs_hh_candidates$r_abs_sick)
alg_abs_school_hh  <- get_regex_cols(algeria_abs_hh_candidates$r_abs_school_hh)
alg_abs_play_hh    <- get_regex_cols(algeria_abs_hh_candidates$r_abs_play_hh)
alg_abs_social_hh  <- get_regex_cols(algeria_abs_hh_candidates$r_abs_social_hh)
alg_abs_travel_hh  <- get_regex_cols(algeria_abs_hh_candidates$r_abs_travel_hh)
alg_abs_other_hh   <- get_regex_cols(algeria_abs_hh_candidates$r_abs_other_hh)

alg_nc_childsick_hh <- get_regex_cols(algeria_nc_hh_candidates$r_nc_child_sick_hh)
alg_nc_notdecide_hh <- get_regex_cols(algeria_nc_hh_candidates$r_nc_not_decided_hh)
alg_nc_poliofree_hh <- get_regex_cols(algeria_nc_hh_candidates$r_nc_polio_free_hh)
alg_nc_nopv_hh      <- get_regex_cols(algeria_nc_hh_candidates$r_nc_nopv_hh)
alg_nc_other_hh     <- get_regex_cols(algeria_nc_hh_candidates$r_nc_other_hh)

# Form-level totals present
form_total_cols <- unique(c(
  unlist(absence_total_candidates),
  unlist(nc_total_candidates),
  "Tot_child_NC_NotDecide_T",
  "Tot_child_NC_PolioFREE_T",
  "Tot_child_NC_nOPV_T",
  "Tot_child_NC_ChildSick_T",
  "Tot_child_NC_Others_T",
  "Tot_child_Abs_Sick_T"
))
form_total_cols <- intersect(form_total_cols, df_names)

# ============================================================
# CLEAN NUMERIC + DATE
# ============================================================

GF <- GF %>%
  mutate(across(all_of(hh_cols_present), clean_numeric))

if (length(form_total_cols) > 0) {
  GF <- GF %>%
    mutate(across(all_of(form_total_cols), clean_numeric))
}

# Clean selected core numeric columns when present
numeric_core_candidates <- intersect(
  c("HH_count", "Total_U5_Present", "TotalFM", "sum_missed_children", "Total_Absent", "Total_refusal"),
  names(GF)
)

if (length(numeric_core_candidates) > 0) {
  GF <- GF %>%
    mutate(across(all_of(numeric_core_candidates), clean_numeric))
}

GF <- GF %>%
  mutate(date_monitored = parse_mixed_dates(date_monitored)) %>%
  filter(!is.na(date_monitored))

# ============================================================
# ROW-LEVEL RECONSTRUCTION
# ============================================================

GH <- GF %>%
  mutate(
    # Core reconstructed indicators
    u5_present_hh = sum_existing_cols(., u_present_cols),
    u5_fm_hh_raw  = sum_existing_cols(., u_fm_cols),
    u5_FM = pmin(u5_fm_hh_raw, u5_present_hh),
    u5_present = u5_present_hh,
    missed_child = pmax(0, u5_present - u5_FM),
    
    # Top-level reasons from HH counts
    r_non_FM_Absent         = sum_existing_cols(., abs_main_cols),
    r_non_FM_NC             = sum_existing_cols(., nc_main_cols),
    r_non_FM_not_visited    = sum_existing_cols(., notvisited_main_cols),
    r_non_FM_not_revisited  = sum_existing_cols(., notrevisited_main_cols),
    r_non_FM_asleep         = sum_existing_cols(., asleep_main_cols),
    r_non_FM_routine        = sum_existing_cols(., routine_main_cols),
    r_non_FM_other          = sum_existing_cols(., other_main_cols),
    hh_parent_caregiver_informed = sum_existing_cols(., caregiver_info_cols),
    
    # Detailed absence
    r_abs_play_areas = safe_pick_first(., absence_total_candidates$r_abs_play_areas),
    r_abs_market = safe_pick_first(., absence_total_candidates$r_abs_market),
    r_abs_school = safe_pick_first(., absence_total_candidates$r_abs_school),
    r_abs_farm = safe_pick_first(., absence_total_candidates$r_abs_farm),
    r_abs_social_event = safe_pick_first(., absence_total_candidates$r_abs_social_event),
    r_abs_travelling = safe_pick_first(., absence_total_candidates$r_abs_travelling),
    r_abs_parent_absent = safe_pick_first(., absence_total_candidates$r_abs_parent_absent),
    r_abs_other_detail_form = safe_pick_first(., absence_total_candidates$r_abs_other_detail),
    
    # Algeria HH-level detailed absence fallback/additional fields
    r_abs_sick_hh = sum_existing_cols(., alg_abs_sick_hh),
    r_abs_school_hh = sum_existing_cols(., alg_abs_school_hh),
    r_abs_play_hh = sum_existing_cols(., alg_abs_play_hh),
    r_abs_social_hh = sum_existing_cols(., alg_abs_social_hh),
    r_abs_travel_hh = sum_existing_cols(., alg_abs_travel_hh),
    r_abs_other_hh = sum_existing_cols(., alg_abs_other_hh),
    
    # Detailed NC
    r_nc_religious_beliefs = safe_pick_first(., nc_total_candidates$r_nc_religious_beliefs),
    r_nc_side_effects = safe_pick_first(., nc_total_candidates$r_nc_side_effects),
    r_nc_too_many_doses = safe_pick_first(., nc_total_candidates$r_nc_too_many_doses),
    r_nc_child_sick_form = safe_pick_first(., nc_total_candidates$r_nc_child_sick),
    r_nc_covid = safe_pick_first(., nc_total_candidates$r_nc_covid),
    r_nc_other_detail_form = safe_pick_first(., nc_total_candidates$r_nc_other_detail),
    
    # Algeria HH-level detailed NC fallback/additional fields
    r_nc_child_sick_hh = sum_existing_cols(., alg_nc_childsick_hh),
    r_nc_not_decided_hh = sum_existing_cols(., alg_nc_notdecide_hh),
    r_nc_polio_free_hh = sum_existing_cols(., alg_nc_poliofree_hh),
    r_nc_nopv_hh = sum_existing_cols(., alg_nc_nopv_hh),
    r_nc_other_hh = sum_existing_cols(., alg_nc_other_hh)
  ) %>%
  mutate(
    # Harmonized detailed totals
    r_abs_other_detail = pmax(r_abs_other_detail_form, r_abs_other_hh, na.rm = TRUE),
    r_nc_child_sick = pmax(r_nc_child_sick_form, r_nc_child_sick_hh, na.rm = TRUE),
    r_nc_other_detail = pmax(r_nc_other_detail_form, r_nc_other_hh, na.rm = TRUE),
    
    # Keep Algeria extras separately too
    r_abs_sick = r_abs_sick_hh,
    r_nc_not_decided = r_nc_not_decided_hh,
    r_nc_polio_free = r_nc_polio_free_hh,
    r_nc_nopv = r_nc_nopv_hh
  ) %>%
  mutate(
    abs_detail_total = r_abs_play_areas + r_abs_market + r_abs_school +
      r_abs_farm + r_abs_social_event + r_abs_travelling +
      r_abs_parent_absent + r_abs_other_detail + r_abs_sick,
    
    nc_detail_total = r_nc_religious_beliefs + r_nc_side_effects +
      r_nc_too_many_doses + r_nc_child_sick + r_nc_covid +
      r_nc_other_detail + r_nc_not_decided + r_nc_polio_free + r_nc_nopv
  )

# ============================================================
# DAILY AGGREGATION
# ============================================================

daily_im <- GH %>%
  select(
    any_of(c(
      "Country", "Region", "District", "Response", "roundNumber", "date_monitored",
      "u5_present", "u5_FM", "missed_child",
      "r_non_FM_Absent", "r_non_FM_NC", "r_non_FM_not_visited",
      "r_non_FM_not_revisited", "r_non_FM_asleep", "r_non_FM_routine",
      "r_non_FM_other", "hh_parent_caregiver_informed",
      "r_abs_sick", "r_abs_play_areas", "r_abs_market", "r_abs_school",
      "r_abs_farm", "r_abs_social_event", "r_abs_travelling",
      "r_abs_parent_absent", "r_abs_other_detail",
      "r_nc_religious_beliefs", "r_nc_side_effects", "r_nc_too_many_doses",
      "r_nc_child_sick", "r_nc_covid", "r_nc_other_detail",
      "r_nc_not_decided", "r_nc_polio_free", "r_nc_nopv",
      "abs_detail_total", "nc_detail_total"
    ))
  ) %>%
  group_by(Country, Region, District, Response, roundNumber, date_monitored) %>%
  summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE)), .groups = "drop")

# ============================================================
# PERIOD BUILDING
# ============================================================

if (use_period_grouping) {
  period_im <- daily_im %>%
    group_by(Country, Region, District, Response, roundNumber) %>%
    arrange(date_monitored, .by_group = TRUE) %>%
    mutate(
      gap_days = as.integer(date_monitored - lag(date_monitored)),
      new_period = if_else(is.na(gap_days) | gap_days != 1, 1L, 0L),
      period = cumsum(new_period)
    ) %>%
    ungroup()
} else {
  period_im <- daily_im %>%
    mutate(
      period = 1L
    )
}

# ============================================================
# FINAL AGGREGATION
# ============================================================

final_im <- period_im %>%
  group_by(Country, Region, District, Response, roundNumber, period) %>%
  summarise(
    start_date = min(date_monitored, na.rm = TRUE),
    end_date = max(date_monitored, na.rm = TRUE),
    across(where(is.numeric), ~ sum(.x, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  mutate(
    cv = ifelse(u5_present > 0, round(u5_FM / u5_present, 2), NA_real_),
    
    reasons_total =
      r_non_FM_Absent +
      r_non_FM_NC +
      r_non_FM_not_visited +
      r_non_FM_not_revisited +
      r_non_FM_asleep +
      r_non_FM_routine +
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
    ),
    
    year = year(start_date)
  ) %>%
  filter(!is.na(start_date), start_date >= min_date) %>%
  arrange(start_date)

# ============================================================
# OPTIONAL VACCINE TYPE DERIVATION
# ============================================================

final_im <- final_im %>%
  mutate(
    Vaccine_type = case_when(
      str_detect(Response, regex("nOPV|VPOn|nVPO", ignore_case = TRUE)) ~ "nOPV2",
      str_detect(Response, regex("bOPV|WPV1|VPOb|OPV", ignore_case = TRUE)) ~ "bOPV",
      str_detect(Response, regex("mOPV", ignore_case = TRUE)) ~ "mOPV",
      TRUE ~ NA_character_
    )
  )

# ============================================================
# FINAL OUTPUT DATASET
# ============================================================

IM_repository <- final_im %>%
  select(
    Country, Region, District, Response, roundNumber, Vaccine_type,
    start_date, end_date, year,
    u5_present, u5_FM, missed_child, cv,
    
    # Top-level reasons
    r_non_FM_Absent, r_non_FM_NC, r_non_FM_not_visited,
    r_non_FM_not_revisited, r_non_FM_asleep, r_non_FM_routine,
    r_non_FM_other,
    
    # HH info
    hh_parent_caregiver_informed,
    
    # Detailed absence
    r_abs_sick, r_abs_play_areas, r_abs_market, r_abs_school,
    r_abs_farm, r_abs_social_event, r_abs_travelling,
    r_abs_parent_absent, r_abs_other_detail,
    
    # Detailed NC
    r_nc_religious_beliefs, r_nc_side_effects, r_nc_too_many_doses,
    r_nc_child_sick, r_nc_covid, r_nc_other_detail,
    r_nc_not_decided, r_nc_polio_free, r_nc_nopv,
    
    # Totals
    abs_detail_total, nc_detail_total, reasons_total,
    explained_ratio, unexplained_ratio,
    unexplained_missed, overreported_reasons,
    
    # QC
    check_missed, check_abs_detail, check_nc_detail,
    reconciliation_flag, abs_detail_flag, nc_detail_flag, qc_flag
  )

# ============================================================
# QC FILE
# ============================================================

IM_repository_QC <- IM_repository %>%
  filter(
    qc_flag == "Needs review" |
      abs(check_missed) >= 5 |
      abs(check_abs_detail) >= 3 |
      abs(check_nc_detail) >= 3
  ) %>%
  arrange(desc(abs(check_missed)), desc(abs(check_abs_detail)), desc(abs(check_nc_detail)))

# ============================================================
# EXPORT
# ============================================================

write_csv(IM_repository, output_file)
write_csv(IM_repository_QC, qc_output_file)

cat("Regional IM repository saved to:\n", output_file, "\n")
cat("Regional IM QC file saved to:\n", qc_output_file, "\n")

cat("\n--- QC Summary ---\n")
print(IM_repository %>% count(reconciliation_flag))
print(IM_repository %>% count(abs_detail_flag))
print(IM_repository %>% count(nc_detail_flag))
print(IM_repository %>% count(qc_flag))