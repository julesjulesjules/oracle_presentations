library(tidyverse)
library(epidatr)

file_storage <- "C:/Users/juliegil/Documents/Delphi_Work/oracle/hold/"

### api key
code_loc1 <- "C:/Users/juliegil/Documents/Delphi_Work/"
#DELPHI_EPIDATA_KEY <- read.table(paste0(code_loc1, "delphi_key.txt"))[1, 1]

key <- read.table(paste0(code_loc1, "delphi_key.txt"))[1, 1]
Sys.setenv(DELPHI_EPIDATA_KEY = key)


query_start_time <- Sys.time()
nssp1 <- epidata_snapshot(
  source = "nssp", 
  signals =  c("smoothed_pct_ed_visits_rsv", "smoothed_pct_ed_visits_influenza", "smoothed_pct_ed_visits_covid", 
               "smoothed_pct_ed_visits_combined", "pct_ed_visits_rsv", "pct_ed_visits_influenza", "pct_ed_visits_covid", 
               "pct_ed_visits_combined", "pct_ed_visits_ari"), 
  geo_type = "msa", 
  geo_values = "*",
  reference_time = epirange(from = "20260701", to = "20290101"), 
  fill_method = "source"
)
query_end_time <- Sys.time()
query_time <- query_end_time - query_start_time


query_start_time <- Sys.time()
nwss1 <- epidata_snapshot(
  source = "nwss", 
  signals =  c("mpox_all_mic_lin"), 
  geo_type = "sewershed", 
  geo_values = "*",
  reference_time = epirange(from = "20260701", to = "20290101"), 
  fill_method = "source"
)
query_end_time <- Sys.time()
query_time <- query_end_time - query_start_time




query_start_time <- Sys.time()
vha1 <- epidata_snapshot(
  source = "vha_respiratory_cases", 
  signals =  c("rsv_cases_cumulative", "flu_cases_cumulative"), 
  geo_type = "nation", 
  geo_values = "*",
  reference_time = epirange(from = "20260701", to = "20290101"), 
  fill_method = "ave"
)
query_end_time <- Sys.time()
query_time <- query_end_time - query_start_time




source_choice <- "nhsn"

nhsn_signals <- epidata_meta(source = source_choice)$signals
nhsn_geos <- c("census_division", "census_region")

all_nhsn <- data.frame()

for (every_signal in nhsn_signals){
  
  for (each_geo_type in nhsn_geos){
    
    query_start_time <- Sys.time()
    
    nhsn1 <- epidata_snapshot(
      source = source_choice, 
      signals =  every_signal, 
      geo_type = each_geo_type, 
      geo_values = "*",
      reference_time = epirange(from = "20260701", to = "20290101")#, 
      #fill_method = "source"
    )
    
    query_end_time <- Sys.time()
    query_time <- query_end_time - query_start_time
    
    query_add <- data.frame(source = source_choice, 
                            signals = every_signal, 
                            geo_type = each_geo_type, 
                            reference_time_start = "20260701",
                            reference_time_end = "20290101", 
                            queried_on = Sys.Date(), 
                            query_start = query_start_time, 
                            query_end = query_end_time, 
                            total_query_time = query_time, 
                            rows_returned = nrow(nhsn1), 
                            cols_returned = ncol(nhsn1))
    
    all_query_records <- rbind(all_query_records, query_add)
    
    all_nhsn <- rbind(all_nhsn, nhsn1)
    
  }
}


query_start_time <- Sys.time()
pop1 <- epidata_snapshot(
  source = "pophive", 
  signals =  c("rsv_pct_ed", "rsv_n_ed", "flu_pct_ed", "flu_n_ed", "covid_pct_ed", "covid_n_ed", "all_n_encounters_ed"), 
  geo_type = "hhs", 
  geo_values = "*",
  reference_time = epirange(from = "20260701", to = "20290101"), 
  fill_method = "ave"
)
query_end_time <- Sys.time()
query_time <- query_end_time - query_start_time


