### idea: what is real-time, and is Delphi API it?
### julie (jules) gilbert
### started: 2026-08-26

### libraries
library(tidyverse)
library(epidatr)

file_storage <- "C:/Users/juliegil/Documents/Delphi_Work/oracle/hold/"

### api key
code_loc1 <- "C:/Users/juliegil/Documents/Delphi_Work/"
#DELPHI_EPIDATA_KEY <- read.table(paste0(code_loc1, "delphi_key.txt"))[1, 1]

key <- read.table(paste0(code_loc1, "delphi_key.txt"))[1, 1]
Sys.setenv(DELPHI_EPIDATA_KEY = key)

### goal - request as much/many delphi signals as possible
### select most recent data point(s) available
### calculate lag
### display as chart, repeat daily

# v5 (for newness)
# epidata_snapshot(), epidata_archive(), and epidata_meta() 


all_query_records <- data.frame()

# nssp ---- 

nssp_signals <- epidata_meta(source = "nssp")$signals
nssp_geos <- epidata_meta(source = "nssp")$geo_types

# $nssp$signals
# [1] "pct_ed_visits_ari"                "pct_ed_visits_combined"          
# [3] "pct_ed_visits_covid"              "pct_ed_visits_influenza"         
# [5] "pct_ed_visits_rsv"                "smoothed_pct_ed_visits_combined" 
# [7] "smoothed_pct_ed_visits_covid"     "smoothed_pct_ed_visits_influenza"
# [9] "smoothed_pct_ed_visits_rsv"

all_nssp <- data.frame()

for (every_signal in nssp_signals){

    for (each_geo_type in nssp_geos){
      
      query_start_time <- Sys.time()
        nssp1 <- epidata_snapshot(
          source = "nssp", 
          signals =  every_signal, 
          geo_type = each_geo_type, 
          geo_values = "*",
          reference_time = epirange(from = "20260701", to = "20290101"), 
          fill_method = "source"
        )
      query_end_time <- Sys.time()
      query_time <- query_end_time - query_start_time
      
      query_add <- data.frame(source = "nssp", 
                              signals = every_signal, 
                              geo_type = each_geo_type, 
                              reference_time_start = "20260701",
                              reference_time_end = "20290101", 
                              queried_on = Sys.Date(), 
                              query_start = query_start_time, 
                              query_end = query_end_time, 
                              total_query_time = query_time, 
                              rows_returned = nrow(nssp1), 
                              cols_returned = ncol(nssp1))
      
      all_query_records <- rbind(all_query_records, query_add)
      
        all_nssp <- rbind(all_nssp, nssp1)
        
    }
}

write.csv(all_nssp, paste0(file_storage, "nssp_", gsub("-", "", Sys.Date()), ".csv"), row.names = FALSE, na = '')

# then calculate lag

nssp_lag_calc <- filter(all_nssp, !is.na(value)) %>% 
  group_by(signal, geo_type, fill_method) %>%
  summarize(max_date = max(reference_time))

nssp_lag_calc <- nssp_lag_calc %>% mutate(today = Sys.Date(), 
                                          source = "nssp")

nssp_lag_calc <- nssp_lag_calc %>% 
  mutate(lag_time = as.numeric(difftime(today, max_date, units = "days")))

# nhsn ---- 

source_choice <- "nhsn"

nhsn_signals <- epidata_meta(source = source_choice)$signals
nhsn_geos <- epidata_meta(source = source_choice)$geo_types

all_nhsn <- data.frame()

for (every_signal in nhsn_signals){
  
  for (each_geo_type in nhsn_geos){
    
    query_start_time <- Sys.time()
    
    nhsn1 <- epidata_snapshot(
      source = source_choice, 
      signals =  every_signal, 
      geo_type = each_geo_type, 
      geo_values = "*",
      reference_time = epirange(from = "20260701", to = "20290101"), 
      fill_method = "source"
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

write.csv(all_nhsn, paste0(file_storage, source_choice, "_", gsub("-", "", Sys.Date()), ".csv"), row.names = FALSE, na = '')

# then calculate lag

nhsn_lag_calc <- filter(all_nhsn, !is.na(value)) %>% 
  group_by(signal, geo_type, fill_method) %>%
  summarize(max_date = max(reference_time))

nhsn_lag_calc <- nhsn_lag_calc %>% mutate(today = Sys.Date(), 
                                          source = source_choice)

nhsn_lag_calc <- nhsn_lag_calc %>% 
  mutate(lag_time = as.numeric(difftime(today, max_date, units = "days")))


# pophive ---- 

source_choice <- "pophive"

pophive_signals <- epidata_meta(source = source_choice)$signals
pophive_geos <- epidata_meta(source = source_choice)$geo_types

all_pophive <- data.frame()

for (every_signal in pophive_signals){
  
  for (each_geo_type in pophive_geos){
    
    query_start_time <- Sys.time()
    
    pophive1 <- epidata_snapshot(
      source = source_choice, 
      signals =  every_signal, 
      geo_type = each_geo_type, 
      geo_values = "*",
      reference_time = epirange(from = "20260701", to = "20290101"), 
      fill_method = "source"
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
                            rows_returned = nrow(pophive1), 
                            cols_returned = ncol(pophive1))
    
    all_query_records <- rbind(all_query_records, query_add)
    
    
    all_pophive <- rbind(all_pophive, pophive1)
    
  }
}

write.csv(all_pophive, paste0(file_storage, source_choice, "_", gsub("-", "", Sys.Date()), ".csv"), row.names = FALSE, na = '')

# then calculate lag

pophive_lag_calc <- filter(all_pophive, !is.na(value)) %>% 
  group_by(signal, geo_type, fill_method) %>%
  summarize(max_date = max(reference_time))

pophive_lag_calc <- pophive_lag_calc %>% mutate(today = Sys.Date(), 
                                          source = source_choice)

pophive_lag_calc <- pophive_lag_calc %>% 
  mutate(lag_time = as.numeric(difftime(today, max_date, units = "days")))



# nwss ---- 

source_choice <- "nwss"

nwss_signals <- epidata_meta(source = source_choice)$signals
nwss_geos <- epidata_meta(source = source_choice)$geo_types

all_nwss <- data.frame()

for (every_signal in nwss_signals){
  
  for (each_geo_type in nwss_geos){
    
    query_start_time <- Sys.time()
    
    nwss1 <- epidata_snapshot(
      source = source_choice, 
      signals =  every_signal, 
      geo_type = each_geo_type, 
      geo_values = "*",
      reference_time = epirange(from = "20260701", to = "20290101"), 
      fill_method = "source"
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
                            rows_returned = nrow(nwss1), 
                            cols_returned = ncol(nwss1))
    
    all_query_records <- rbind(all_query_records, query_add)
    
    
    all_nwss <- rbind(all_nwss, nwss1)
    
  }
}

write.csv(all_nwss, paste0(file_storage, source_choice, "_", gsub("-", "", Sys.Date()), ".csv"), row.names = FALSE, na = '')

# then calculate lag

nwss_lag_calc <- filter(all_nwss, !is.na(value)) %>% 
  group_by(signal, geo_type, fill_method) %>%
  summarize(max_date = max(reference_time))

nwss_lag_calc <- nwss_lag_calc %>% mutate(today = Sys.Date(), 
                                          source = source_choice)

nwss_lag_calc <- nwss_lag_calc %>% 
  mutate(lag_time = as.numeric(difftime(today, max_date, units = "days")))



# claims_data_outpatient ---- 

source_choice <- "claims_outpatient"

cdo_signals <- epidata_meta(source = source_choice)$signals
cdo_geos <- epidata_meta(source = source_choice)$geo_types

all_cdo <- data.frame()

for (every_signal in cdo_signals){
  
  for (each_geo_type in cdo_geos){
    
    query_start_time <- Sys.time()
    
    cdo1 <- epidata_snapshot(
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
                            rows_returned = nrow(cdo1), 
                            cols_returned = ncol(cdo1))
    
    all_query_records <- rbind(all_query_records, query_add)
    
    
    all_cdo <- rbind(all_cdo, cdo1)
    
  }
}

write.csv(all_cdo, paste0(file_storage, source_choice, "_", gsub("-", "", Sys.Date()), ".csv"), row.names = FALSE, na = '')

# then calculate lag

cdo_lag_calc <- filter(all_cdo, !is.na(value)) %>% 
  group_by(signal, geo_type, fill_method) %>%
  summarize(max_date = max(reference_time))

cdo_lag_calc <- cdo_lag_calc %>% mutate(today = Sys.Date(), 
                                          source = source_choice)

cdo_lag_calc <- cdo_lag_calc %>% 
  mutate(lag_time = as.numeric(difftime(today, max_date, units = "days")))




# claims_data_inpatient ---- 

source_choice <- "claims_inpatient"

cdi_signals <- epidata_meta(source = source_choice)$signals
cdi_geos <- epidata_meta(source = source_choice)$geo_types

all_cdi <- data.frame()

for (every_signal in cdi_signals){
  
  for (each_geo_type in cdi_geos){
    
    query_start_time <- Sys.time()
    
    cdi1 <- epidata_snapshot(
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
                            rows_returned = nrow(cdi1), 
                            cols_returned = ncol(cdi1))
    
    all_query_records <- rbind(all_query_records, query_add)
    
    
    all_cdi <- rbind(all_cdi, cdi1)
    
  }
}

write.csv(all_cdi, paste0(file_storage, source_choice, "_", gsub("-", "", Sys.Date()), ".csv"), row.names = FALSE, na = '')

# then calculate lag

cdi_lag_calc <- filter(all_cdi, !is.na(value)) %>% 
  group_by(signal, geo_type, fill_method) %>%
  summarize(max_date = max(reference_time))

cdi_lag_calc <- cdi_lag_calc %>% mutate(today = Sys.Date(), 
                                        source = source_choice)

cdi_lag_calc <- cdi_lag_calc %>% 
  mutate(lag_time = as.numeric(difftime(today, max_date, units = "days")))



# va_respiratory ---- 

source_choice <- "vha_respiratory_vaccinations"

vares_signals <- epidata_meta(source = source_choice)$signals
vares_geos <- epidata_meta(source = source_choice)$geo_types

all_vares <- data.frame()

for (every_signal in vares_signals){
  
  for (each_geo_type in vares_geos){
    
    query_start_time <- Sys.time()
    
    vares1 <- epidata_snapshot(
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
                            rows_returned = nrow(vares1), 
                            cols_returned = ncol(vares1))
    
    all_query_records <- rbind(all_query_records, query_add)
    
    
    all_vares <- rbind(all_vares, vares1)
    
  }
}

write.csv(all_vares, paste0(file_storage, source_choice, "_", gsub("-", "", Sys.Date()), ".csv"), row.names = FALSE, na = '')

# then calculate lag

vares_lag_calc <- filter(all_vares, !is.na(value)) %>% 
  group_by(signal, geo_type, fill_method) %>%
  summarize(max_date = max(reference_time))

vares_lag_calc <- vares_lag_calc %>% mutate(today = Sys.Date(), 
                                        source = source_choice)

vares_lag_calc <- vares_lag_calc %>% 
  mutate(lag_time = as.numeric(difftime(today, max_date, units = "days")))



# va_respiratory 2 ---- 

source_choice <- "vha_respiratory_cases"

vares_signals <- epidata_meta(source = source_choice)$signals
vares_geos <- epidata_meta(source = source_choice)$geo_types

all_vares2 <- data.frame()

for (every_signal in vares_signals){
  
  for (each_geo_type in vares_geos){
    
    query_start_time <- Sys.time()
    
    vares1 <- epidata_snapshot(
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
                            rows_returned = nrow(vares1), 
                            cols_returned = ncol(vares1))
    
    all_query_records <- rbind(all_query_records, query_add)
    
    
    all_vares2 <- rbind(all_vares2, vares1)
    
  }
}

write.csv(all_vares2, paste0(file_storage, source_choice, "_", gsub("-", "", Sys.Date()), ".csv"), row.names = FALSE, na = '')

# then calculate lag

vares_lag_calc2 <- filter(all_vares2, !is.na(value)) %>% 
  group_by(signal, geo_type, fill_method) %>%
  summarize(max_date = max(reference_time))

vares_lag_calc2 <- vares_lag_calc2 %>% mutate(today = Sys.Date(), 
                                            source = source_choice)

vares_lag_calc2 <- vares_lag_calc2 %>% 
  mutate(lag_time = as.numeric(difftime(today, max_date, units = "days")))



# sleepcycle ---- 

source_choice <- "sleepcycle"

sc_signals <- epidata_meta(source = source_choice)$signals
sc_geos <- epidata_meta(source = source_choice)$geo_types

all_sc <- data.frame()

for (every_signal in sc_signals){

  for (each_geo_type in sc_geos){

    query_start_time <- Sys.time()

    sc1 <- epidata_snapshot(
      source = source_choice,
      signals =  every_signal,
      geo_type = each_geo_type,
      geo_values = "*",
      reference_time = epirange(from = "20260701", to = "20290101"),
      fill_method = "source"
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
                            rows_returned = nrow(sc1),
                            cols_returned = ncol(sc1))

    all_query_records <- rbind(all_query_records, query_add)


    all_sc <- rbind(all_sc, sc1)

  }
}

write.csv(all_sc, paste0(file_storage, source_choice, "_", gsub("-", "", Sys.Date()), ".csv"), row.names = FALSE, na = '')

# then calculate lag

sc_lag_calc <- filter(all_sc, !is.na(value)) %>%
  group_by(signal, geo_type, fill_method) %>%
  summarize(max_date = max(reference_time))

sc_lag_calc <- sc_lag_calc %>% mutate(today = Sys.Date(),
                                            source = source_choice)

sc_lag_calc <- sc_lag_calc %>%
  mutate(lag_time = as.numeric(difftime(today, max_date, units = "days")))



# nchs + ---- 

source_set <- c('nchs_mortality', 'rvdss', 'fluview_ilinet',
                'fluview_resp_lab_clinical', 'fluview_resp_lab_ph', 'flusurv', 'nrevss')
all_lag_calc <- data.frame()

for (i in source_set){

    source_choice <- i
    
    a_signals <- epidata_meta(source = source_choice)$signals
    a_geos <- epidata_meta(source = source_choice)$geo_types
    
    all_a <- data.frame()
    
    for (every_signal in a_signals){
      
      for (each_geo_type in a_geos){
        
        query_start_time <- Sys.time()
        
        a1 <- epidata_snapshot(
          source = source_choice,
          signals =  every_signal,
          geo_type = each_geo_type,
          geo_values = "*",
          reference_time = epirange(from = "20260701", to = "20290101"),
          fill_method = "source"
        )
        
        query_end_time <- Sys.time()
        query_time <- query_end_time - query_start_time
        
        a1$source <- source_choice
        
        query_add <- data.frame(source = source_choice,
                                signals = every_signal,
                                geo_type = each_geo_type,
                                reference_time_start = "20260701",
                                reference_time_end = "20290101",
                                queried_on = Sys.Date(),
                                query_start = query_start_time,
                                query_end = query_end_time,
                                total_query_time = query_time,
                                rows_returned = nrow(sc1),
                                cols_returned = ncol(sc1))
        
        query_add$source <- source_choice
        
        all_query_records <- rbind(all_query_records, query_add)
        
        
        all_a <- rbind(all_a, a1)
        
        
      }
    }
    
    write.csv(all_a, paste0(file_storage, "all_a_", gsub("-", "", Sys.Date()), ".csv"), row.names = FALSE, na = '')
    
    # then calculate lag
    
    a_lag_calc <- filter(all_a, !is.na(value)) %>%
      group_by(source, signal, geo_type, fill_method) %>%
      summarize(max_date = max(reference_time))
    
    a_lag_calc <- a_lag_calc %>% mutate(today = Sys.Date())
    
    a_lag_calc <- a_lag_calc %>%
      mutate(lag_time = as.numeric(difftime(today, max_date, units = "days")))
    
    all_lag_calc <- rbind(all_lag_calc, a_lag_calc)

}

# all together, look at lag ---- 

all_lags <- rbind(nssp_lag_calc, nhsn_lag_calc, pophive_lag_calc, 
                  nwss_lag_calc, cdo_lag_calc, cdi_lag_calc, 
                  vares_lag_calc, vares_lag_calc2, sc_lag_calc, all_lag_calc)

write.csv(all_lags, paste0("C:/Users/juliegil/Documents/Delphi_Work/oracle/fp_lags_", gsub("-", "", Sys.Date()), ".csv"), row.names = FALSE, na = '')

write.csv(all_query_records, paste0("C:/Users/juliegil/Documents/Delphi_Work/oracle/query_times_", gsub("-", "", Sys.Date()), ".csv"), row.names = FALSE, na = '')

ggplot(all_lags, aes(x = lag_time, y = reorder(signal, -lag_time), fill = source)) + 
  geom_col(color = 'black') + 
  theme_bw() + 
  scale_fill_manual(values = c("#588b8b", "#ffffff", "#ffd5c2", 
                               "#f28f3b", "#c8553d", "#2d3047", "#93b7be", "#588b8b", "#ffffff")) + 
  geom_vline(xintercept = c(7, 14, 21)) + 
  labs(x = "Lag in Days", 
       y = "", 
       title = "2026-08-27 Results", 
       fill = "Source") + 
  scale_x_continuous(minor_breaks = seq(0, 30, 2)) + 
  facet_wrap(.~geo_type+fill_method, scale = c("free_y"), ncol = 2)


ggsave(filename = paste0("overall_lags_", gsub('-', "", Sys.Date()), ".png"),
       device = "png", 
       path = "C:/Users/juliegil/Documents/Delphi_Work/oracle/images/", 
      width = 16, 
      height = 36, 
      units = 'in')

