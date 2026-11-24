### Oracle #3 idea
### "Network"/genetic tree representation of correlations between data in EpiData
### 2026-11-23
### Julie (Jules) Gilbert

library(tidyverse)
library(epidatr)

code_loc1 <- "C:/Users/juliegil/Documents/Delphi_Work/"

key <- read.table(paste0(code_loc1, "delphi_key.txt"))[1, 1]
Sys.setenv(DELPHI_EPIDATA_KEY = key)

file_storage <- "C:/Users/juliegil/Documents/Delphi_Work/oracle/hold_for_003/"

### pull a copy of data from all data sources (year)

source_set <- c('claims_inpatient', 'claims_outpatient',
                'vha_respiratory_vaccinations', 'vha_respiratory_cases', 
                'sleepcycle', 'nwss', 
                'pophive', 
                'nhsn', 'nssp',
                'nchs_mortality', 'rvdss', 
                'fluview_ilinet',
                'fluview_resp_lab_clinical', 'fluview_resp_lab_ph', 
                'flusurv', 
                'nrevss')

for (i in source_set){
  
      all_a <- data.frame()
      
      source_choice <- i
      
      a_signals <- epidata_meta(source = source_choice)$signals
      a_geos <- c("state", "county")
      
      if (source_choice == "nwss"){
        a_geos <- c("sewershed")
      } else if (source_choice %in% c('nrevss', 'flusurv', 'fluview_resp_lab_clinical', 'fluview_resp_lab_ph', "fluview_ilinet", "nchs_mortality", "nhsn", "pophive", "vha_respiratory_vaccinations", 'vha_respiratory_cases')){
        a_geos <- c("state")
      } else if (source_choice == "rvdss"){
        a_geos <- c("lab", "region")
      }
  
      for (every_signal in a_signals){
        
            for (each_geo_type in a_geos){
              
                  a1 <- epidata_snapshot(
                    source = source_choice,
                    signals =  every_signal,
                    geo_type = each_geo_type,
                    geo_values = "*",
                    reference_time = epirange(from = "20251101", to = "20290101")
                  )
                  
                  a1$source <- source_choice
                  a1$reference_time_start <- "20251101"
                  a1$reference_time_end <- "20290101"
                  a1$queried_on <- Sys.Date()
                  a1$total_rows <- nrow(a1)
                  a1$total_columns <-  ncol(a1)
                  
                  all_a <- rbind(all_a, a1)
              
            }
      }

      write.csv(all_a, paste0(file_storage, "all_indicators_", i, "_", gsub("-", "", Sys.Date()), ".csv"), row.names = FALSE, na = '')
      
      
}


### run correlations on all data sets / indicators against each other and 
### record the results

### computationally taking a long time - will need to set up on the hpc, i think
### to get everything i would want, not sure if it's what i want to do
nwss <- read.csv(paste0(file_storage, "all_indicators_nwss_20261123.csv"))


### run cross correlations on all data sets against each other and record the 
### results - we need the lag relationships

### graph the data as a set of relationships clustering those with high correlations 
### together, slightly farther out those with high correlations if the data 
### is lagged against itself, farthest out those with low correlations