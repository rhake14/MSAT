#MSAT_dict_raw <- readRDS("data_raw/MSAT_dict.RDS")
MSAT_dict_raw <- readxl::read_xlsx("data_raw/MSAT_dict.xlsx")
#names(MSAT_dict_raw) <- c("key", "DE", "EN")
MSAT_dict_raw <- MSAT_dict_raw[,c("key", "EN", "DE", "RU", "NL")]
MSAT_dict <- psychTestR::i18n_dict$new(MSAT_dict_raw)
usethis::use_data(MSAT_dict, overwrite = TRUE)
