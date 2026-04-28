library(data.table)
library(stringr)
library(qs)

# setwd("C:/Users/TOURE/Mes documents/REPOSITORIES/IM_raw_data/IM_raw/") #6427  8587
# setwd("C:/Users/TOURE/Documents/PADACORD/LQAS/")

rds_file <- "C:/Users/TOURE/Documents/PADACORD/IM/7178.rds"

# data <- read_csv(input_file, show_col_types = FALSE)
# rds_file <- "C:/Users/TOURE/Documents/PADACORD/IM/7178.rds"

# input_file  <-  "7178.rds" #"4500.rds" 
# input_folder <- "C:/Users/TOURE/Documents/PADACORD/IM/"
# ============================================================
# Read raw data
# ============================================================
# data <- qread(input_file)

data <- qread(rds_file)
# file.info(rds_file)$size

# data <- qread(rds_file)

dt <- as.data.table(data)



# #columns starting with Reason ^NOimmReas NOimmReas_*_other
reason_cols <- grep("Other_Reason", names(dt), value = TRUE, ignore.case = TRUE)

reason_cols


# social mobilisation columns
reason_cols <- grep("Source", names(dt), value = TRUE, ignore.case = TRUE)

reason_cols


# #columns starting with Reason ^NOimmReas NOimmReas_*_other
reason_cols <- grep("_NC_", names(dt), value = TRUE, ignore.case = TRUE)

reason_cols

reason_cols <- grep("_NC_(?!HH)", names(dt), value = TRUE, ignore.case = TRUE, perl = TRUE)
reason_cols 



#check other for algeria

# #columns starting with Reason ^NOimmReas NOimmReas_*_other
reason_cols <- grep("_NC_Other", names(dt), value = TRUE, ignore.case = TRUE)

reason_cols


#other reasons
# #columns starting with NOimmReas_ and ending with other
reason_cols <- grep("^NOimmReas_.*other$", names(dt), value = TRUE, ignore.case = TRUE)

reason_cols


#other reasons
# #columns starting with NOimmReas_ and ending with other
reason_cols <- grep("Count_HH", names(dt), value = TRUE, ignore.case = TRUE)

reason_cols


# distinct values
all_values_clean <- unique(
  trimws(tolower(unlist(dt[, ..reason_cols])))
)

all_values_clean <- all_values_clean[!is.na(all_values_clean) & all_values_clean != ""]

all_values_clean

# distinct values
all_values_clean <- unique(
  trimws(tolower(unlist(dt[, ..reason_cols])))
)

all_values_clean <- all_values_clean[!is.na(all_values_clean) & all_values_clean != ""]

all_values_clean

#Clean step-by-step
clean_values <- all_values_clean[!grepl("^\\d+$", all_values_clean)]
clean_values <- clean_values[!grepl("^[a-zA-Z]$", clean_values)]
clean_values <- clean_values[nchar(clean_values) > 2]
clean_values





