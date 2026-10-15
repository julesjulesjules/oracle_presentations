### Oracle Idea
### Julie (Jules) Gilbert
### 2026-10-13

### Wastewater data (NWSS) from v5 - does it align with other sources? how? what
### effect does the data delay (new data only 1x per week) have on what could 
### be better/faster data?

library(tidyverse)
library(epidatr)

## flu data pull

nwss_main_flu <- epidata_snapshot(
  source = "nwss",
  signals = "flu_avg_conc", 
  geo_type = "sewershed"
)

nwss_aux_flu <- nwss_main_flu %>%
  epidata_aux()

### rsv pull

nwss_main_rsv <- epidata_snapshot(
  source = "nwss",
  signals = "rsv_avg_conc", 
  geo_type = "sewershed"
)

nwss_aux_rsv <- nwss_main_rsv %>%
  epidata_aux()

# a <- filter(nwss_aux_rsv, nwss_source == "WastewaterSCAN")
# length(unique(a$geo_value))

### covid pull

nwss_main_covid <- epidata_snapshot(
  source = "nwss",
  signals = "covid_avg_conc", 
  geo_type = "sewershed"
)

nwss_aux_covid <- nwss_main_covid %>%
  epidata_aux()

### let's look at just one state to start? need to pick something managable to start
### maybe. 

### what is the site distribution? ---- 

sites_per_state_flu <- filter(nwss_aux_flu, year(reference_time) == 2026) %>% 
  select(state_territory, geo_value) %>% group_by(state_territory) %>% 
  summarize(num_sites = length(unique(geo_value)))

sites_per_state_flu$about <- "NWSS, FluA Results, 2026"

sites_per_state_rsv <- filter(nwss_aux_rsv, year(reference_time) == 2026) %>% 
  select(state_territory, geo_value) %>% group_by(state_territory) %>% 
  summarize(num_sites = length(unique(geo_value)))

sites_per_state_rsv$about <- "NWSS, RSV Results, 2026"

sites_per_state_covid <- filter(nwss_aux_covid, year(reference_time) == 2026) %>% 
  select(state_territory, geo_value) %>% group_by(state_territory) %>% 
  summarize(num_sites = length(unique(geo_value)))

sites_per_state_covid$about <- "NWSS, COVID Results, 2026"

quantile(sites_per_state_flu$num_sites, c(0.2, 0.4, 0.6, 0.8))
quantile(sites_per_state_rsv$num_sites, c(0.2, 0.4, 0.6, 0.8))
quantile(sites_per_state_covid$num_sites, c(0.2, 0.4, 0.6, 0.8))

sites_per_state <- rbind(sites_per_state_flu, sites_per_state_rsv, sites_per_state_covid)



sites_per_state <- reshape2::dcast(sites_per_state, state_territory ~ about, value.var = "num_sites")
sites_per_state[is.na(sites_per_state)] <- 0

sites_per_state$AllEqual <- apply(sites_per_state[, c(2:4)], 1, function(row) all(row == row[1]))

sites_per_state <- reshape2::melt(sites_per_state, id.vars = c("state_territory", "AllEqual"))

colnames(sites_per_state) <- c("state_territory", "AllEqual", "about", "num_sites")

sites_per_state <- sites_per_state %>% mutate(AllEqual = case_when(AllEqual ~ "", 
                                                                   T ~ "x"))

sites_per_state <- sites_per_state %>% mutate(categories = case_when(num_sites < 6.5 ~ "1", 
                                                                     num_sites < 13.5 ~ "2", 
                                                                     num_sites < 25.7 ~ "3", 
                                                                     num_sites < 33.1 ~ "4", 
                                                                     T ~ "5"))


ggplot(sites_per_state, aes(x = reorder(state_territory, -num_sites), y = num_sites, fill = categories)) + 
  geom_col(stat = "identity") + 
  geom_text(data = sites_per_state, aes(x = reorder(state_territory, -num_sites), y = 10, label = AllEqual)) +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
  scale_fill_manual(values = c("#132a13", "#31572c", "#4f772d", "#90a955", "#ecf39e")) + 
  facet_wrap(.~about, ncol = 1) + 
  labs(title = "Number of Sites per State/Territory",
       x = "", 
       y = "Site Count", 
       fill = "", 
       caption = "Bars are colored by quartile levels based on number of sites\nXs mark states that have a different number of sites per testing pathogen")

# what is the source distribution? ---- 

source_per_state_flu <- filter(nwss_aux_flu, year(reference_time) == 2026) %>% 
  select(state_territory, geo_value, nwss_source) %>% group_by(state_territory, nwss_source) %>% 
  mutate(num_sites = length(unique(geo_value))) %>% distinct()

source_per_state_flu$about <- "NWSS, FluA Results, 2026"

source_per_state_rsv <- filter(nwss_aux_rsv, year(reference_time) == 2026) %>% 
  select(state_territory, geo_value, nwss_source) %>% group_by(state_territory, nwss_source) %>% 
  mutate(num_sites = length(unique(geo_value))) %>% distinct()

source_per_state_rsv$about <- "NWSS, RSV Results, 2026"

source_per_state_covid <- filter(nwss_aux_covid, year(reference_time) == 2026) %>% 
  select(state_territory, geo_value, nwss_source) %>% group_by(state_territory, nwss_source) %>% 
  mutate(num_sites = length(unique(geo_value))) %>% distinct()

source_per_state_covid$about <- "NWSS, COVID Results, 2026"

source_per_state <- rbind(source_per_state_flu, source_per_state_rsv, source_per_state_covid)

source_per_state <- source_per_state %>% select(-geo_value) %>% distinct()

ggplot(source_per_state, aes(x = reorder(state_territory, -num_sites), y = num_sites, fill = nwss_source)) + 
  geom_col(stat = "identity") + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
  scale_fill_manual(values = c("#D0BCD5", "#53B3CB", "#124559")) + 
  facet_wrap(.~about, ncol = 1) + 
  labs(title = "Number of Sites per State/Territory and Source",
       x = "", 
       y = "Site Count", 
       fill = "")


## NWSS RSV Data Align with Delphi Source(s)? ---- 

# pulling just illinois data, to start

il_rsv_adm <- epidata_snapshot(source = "nhsn", 
                 signals = "confirmed_admissions_rsv_ew", 
                 geo_type = "state", 
                 geo_values = "il")

illinois_rsv <- filter(nwss_aux_rsv, state_territory == "il")

ggplot(illinois_rsv, aes(x = reference_time, y = value/1000, group = geo_value)) + 
  geom_line(alpha = 0.5) + 
  geom_line(data = il_rsv_adm, aes(x = reference_time, y = value), color = "red") + 
  theme_bw() + 
  coord_cartesian(xlim = c(as_date('2022-07-01'), as_date('2026-10-13'))) + 
  labs(x = "Sample Collection Date", 
       y = "", 
       title = "RSV Concentration Value compared with RSV Hospitalizations - Illinois", 
       subtitle = paste0("NWSS, ", length(unique(illinois_rsv$geo_value)), " Sites"))



cov_adm <- epidata_snapshot(source = "nhsn", 
                               signals = "confirmed_admissions_covid_ew", 
                               geo_type = "state")


cov_adm$state_territory <- cov_adm$geo_value


cov_ed <- epidata_snapshot(source = "nssp", 
                           signals = "pct_ed_visits_covid", 
                           geo_type = "state")

cov_ed$state_territory <- cov_ed$geo_value

cov_claim <- epidata_snapshot(source = "pophive", 
                              signals = "covid_pct_ed", 
                              geo_type = "state")

cov_claim$state_territory <- cov_claim$geo_value

cov_claim <- filter(cov_claim, age_group == "all") %>% select(-age_group)


cov_deaths <- epidata_snapshot(source = "nchs_mortality", 
                               signals = "deaths_covid_incidence_prop", 
                               geo_type = "state")

cov_deaths$state_territory <- cov_deaths$geo_value

cov_inpat <- epidata_snapshot(source = "claims_inpatient", 
                               signals = "claims_inpatient_adm_pct_claims_covid", 
                               geo_type = "state")

cov_inpat$state_territory <- cov_inpat$geo_value

cov_outpat <- epidata_snapshot(source = "claims_outpatient", 
                               signals = "claims_outpatient_ov_pct_claims_covid", 
                               geo_type = "state")

cov_outpat$state_territory <- cov_outpat$geo_value

# ggplot() + 
#   geom_line(data = nwss_aux_rsv, aes(x = reference_time, y = value/1000, group = geo_value), alpha = 0.5) + 
#   geom_line(data = rsv_adm, aes(x = reference_time, y = value, group = state_territory), color = "red") + 
#   theme_bw() + 
#   coord_cartesian(xlim = c(as_date('2022-07-01'), as_date('2026-10-13'))) + 
#   theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
#   labs(x = "Sample Collection Date", 
#        y = "", 
#        title = "NWSS RSV Concentration Value compared with NHSN RSV Hospital Admissions") + 
#   facet_wrap(.~state_territory, scales = "free_y")


ggplot() + 
  geom_line(data = nwss_aux_covid, aes(x = reference_time, y = value/1000, group = geo_value), alpha = 0.5) + 
  geom_line(data = cov_adm, aes(x = reference_time, y = value, group = state_territory), color = "red") + 
  geom_line(data = cov_ed, aes(x = reference_time, y = value*100, group = state_territory), color = "green") + 
  geom_line(data = cov_claim, aes(x = reference_time, y = value*100, group = state_territory), color = "blue") + 
  geom_line(data = cov_deaths, aes(x = reference_time, y = value, group = state_territory), color = "orange") + 
  geom_line(data = cov_inpat, aes(x = reference_time, y = value*100, group = state_territory), color = "purple") + 
  geom_line(data = cov_outpat, aes(x = reference_time, y = value*100, group = state_territory), color = "pink3") + 
  theme_bw() + 
  coord_cartesian(xlim = c(as_date('2022-07-01'), as_date('2026-10-13'))) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  labs(x = "Sample Collection Date", 
       y = "", 
       title = "NWSS COV Concentration Value compared with Relevant Delphi Sources", 
       caption = "Black = NWSS data, divided by 1000\nGreen = % NSSP COV ED Visits multiplied by 100; Orange = NCHS COV Mortality Rate\nRed = NHSN COV Hospital Admissions; Purple = % COVID Inpatient Claims\nBlue = Pophive COV % ED Encounters; Pink = % COVID Outpatient Claims") + 
  facet_wrap(.~state_territory, scales = "free_y")



ggplot() + 
  geom_line(data = filter(nwss_aux_covid, state_territory %in% c("ca", "mi", "pa", "la", "ms", "ma")), aes(x = reference_time, y = value/1000, group = geo_value), alpha = 0.5) + 
  geom_line(data = filter(cov_adm, state_territory %in% c("ca", "mi", "pa", "la", "ms", "ma")), aes(x = reference_time, y = value, group = state_territory), color = "red") + 
  geom_line(data = filter(cov_ed, state_territory %in% c("ca", "mi", "pa", "la", "ms", "ma")), aes(x = reference_time, y = value*100, group = state_territory), color = "green") + 
  geom_line(data = filter(cov_claim, state_territory %in% c("ca", "mi", "pa", "la", "ms", "ma")), aes(x = reference_time, y = value*100, group = state_territory), color = "blue") + 
  geom_line(data = filter(cov_deaths, state_territory %in% c("ca", "mi", "pa", "la", "ms", "ma")), aes(x = reference_time, y = value, group = state_territory), color = "orange") + 
  geom_line(data = filter(cov_inpat, state_territory %in% c("ca", "mi", "pa", "la", "ms", "ma")), aes(x = reference_time, y = value*100, group = state_territory), color = "purple") + 
  geom_line(data = filter(cov_outpat, state_territory %in% c("ca", "mi", "pa", "la", "ms", "ma")), aes(x = reference_time, y = value*100, group = state_territory), color = "pink3") + 
  theme_bw() + 
  coord_cartesian(xlim = c(as_date('2022-07-01'), as_date('2026-10-13'))) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  labs(x = "Sample Collection Date", 
       y = "", 
       title = "NWSS COV Concentration Value compared with Relevant Delphi Sources", 
       caption = "Black = NWSS data, divided by 1000\nGreen = % NSSP COV ED Visits multiplied by 100; Orange = NCHS COV Mortality Rate\nRed = NHSN COV Hospital Admissions; Purple = % COVID Inpatient Claims\nBlue = Pophive COV % ED Encounters; Pink = % COVID Outpatient Claims") + 
  facet_wrap(.~state_territory, scales = "free_y")


check_variable_num <- nwss_aux_covid %>% group_by(state_territory, geo_value) %>% summarize(count = length(unique(pcr_target_units)))

table(nwss_aux_covid$pcr_target_units, nwss_aux_covid$sample_type)
ggplot(check_variable_num, aes(x = geo_value, y = count)) + 
  geom_col() + 
  theme_bw() + 
  scale_x_discrete(labels = NULL, breaks = NULL) + 
  labs(title = "Number of Reported Units Per Site Across Sites",
       subtitle = "COV") + 
  facet_wrap(.~state_territory, scales = "free_x")


a <- filter(nwss_aux_covid, state_territory %in% c("ca"))
a <- a %>% group_by(geo_value) %>% mutate(max_value = max(value, na.rm = TRUE))
a <- filter(a, max_value != value)

ggplot() + 
  geom_line(data = filter(nwss_aux_covid, state_territory %in% c("ca")), aes(x = reference_time, y = value/1000, group = geo_value), alpha = 0.5) + 
  geom_line(data = filter(cov_adm, state_territory %in% c("ca")), aes(x = reference_time, y = value, group = state_territory), color = "red") + 
  geom_line(data = filter(cov_ed, state_territory %in% c("ca")), aes(x = reference_time, y = value*1000, group = state_territory), color = "green") + 
  geom_line(data = filter(cov_claim, state_territory %in% c("ca")), aes(x = reference_time, y = value*1000, group = state_territory), color = "blue") + 
  geom_line(data = filter(cov_deaths, state_territory %in% c("ca")), aes(x = reference_time, y = value*1000, group = state_territory), color = "orange") + 
  geom_line(data = filter(cov_inpat, state_territory %in% c("ca")), aes(x = reference_time, y = value*1000, group = state_territory), color = "purple") + 
  geom_line(data = filter(cov_outpat, state_territory %in% c("ca")), aes(x = reference_time, y = value*1000, group = state_territory), color = "pink3") + 
  theme_bw() + 
  coord_cartesian(xlim = c(as_date('2022-07-01'), as_date('2026-10-13')), ylim = c(0, 5000)) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  labs(x = "Sample Collection Date", 
       y = "", 
       title = "NWSS COV Concentration Value compared with Relevant Delphi Sources", 
       caption = "Black = NWSS data, divided by 1000\nGreen = % NSSP COV ED Visits multiplied by 100; Orange = NCHS COV Mortality Rate\nRed = NHSN COV Hospital Admissions; Purple = % COVID Inpatient Claims\nBlue = Pophive COV % ED Encounters; Pink = % COVID Outpatient Claims") + 
  facet_wrap(.~state_territory, scales = "free_y")


# ggplot() + 
#   geom_line(data = filter(nwss_aux_rsv, state_territory %in% c("sc", "wy", "ri", "sd", "wv", "ne", "nm", "ma", "ky", "tn", "nc", "la")), aes(x = reference_time, y = value/1000, group = geo_value), alpha = 0.5) + 
#   geom_line(data = filter(rsv_adm, state_territory %in% c("sc", "wy", "ri", "sd", "wv", "ne", "nm", "ma", "ky", "tn", "nc", "la")), aes(x = reference_time, y = value, group = state_territory), color = "red") + 
#   theme_bw() + 
#   coord_cartesian(xlim = c(as_date('2022-07-01'), as_date('2026-10-13'))) + 
#   theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
#   labs(x = "Sample Collection Date", 
#        y = "", 
#        title = "NWSS RSV Concentration Value compared with NHSN RSV Hospital Admissions") + 
#   facet_wrap(.~state_territory, scales = "free_y")


# setdiff(unique(nwss_aux_rsv$state_territory), unique(rsv_adm$geo_value))
# setdiff(unique(rsv_adm$geo_value), unique(nwss_aux_rsv$state_territory))




### correlations

nwss_aux_cov_wval <- nwss_aux_covid %>% group_by(state_territory, geo_value, pcr_target_units) %>% 
  mutate(baseline = quantile(value, .10, na.rm = TRUE), 
         stdev = sd(value, na.rm = TRUE))

nwss_aux_cov_wval <- nwss_aux_cov_wval %>%
  mutate(level = (value - baseline)/stdev)

nwss_aux_cov_wval <- nwss_aux_cov_wval %>% mutate(epi_week = epiweek(reference_time))

nwss_aux_cov_wval <- nwss_aux_cov_wval %>% 
  group_by(state_territory, year(reference_time), epi_week) %>% 
  summarize(average_level = weighted.mean(level, as.numeric(population_served)))


# colnames(rsv_claim)
# colnames(rsv_adm)
# colnames(rsv_ed)
# 
# rsv_adm <- rsv_adm %>% select(-epi_week, -`year(reference_time)`)
all_cov <- rbind(cov_claim, cov_ed, cov_adm, cov_deaths, cov_inpat, cov_outpat)
all_cov <- all_cov %>% mutate(epi_week = epiweek(reference_time), 
                              `year(reference_time)` = year(reference_time))

nwss_aux_cov_wval1 <- nwss_aux_cov_wval %>% mutate(signal = "covid_pct_ed")
nwss_aux_cov_wval2 <- nwss_aux_cov_wval %>% mutate(signal = "pct_ed_visits_covid")
nwss_aux_cov_wval3 <- nwss_aux_cov_wval %>% mutate(signal = "confirmed_admissions_covid_ew")

nwss_aux_cov_wval4 <- nwss_aux_cov_wval %>% mutate(signal = "deaths_covid_incidence_prop")
nwss_aux_cov_wval5 <- nwss_aux_cov_wval %>% mutate(signal = "claims_inpatient_adm_pct_claims_covid")
nwss_aux_cov_wval6 <- nwss_aux_cov_wval %>% mutate(signal = "claims_outpatient_ov_pct_claims_covid")


nwss_aux_cov_wval <- rbind(nwss_aux_cov_wval1, nwss_aux_cov_wval2, nwss_aux_cov_wval3, 
                           nwss_aux_cov_wval4, nwss_aux_cov_wval5, nwss_aux_cov_wval6)

cor_compare <- merge(nwss_aux_cov_wval, all_cov, by = c("state_territory", "year(reference_time)", "epi_week", "signal"), all = TRUE)

# cor_compare <- cor_compare %>% group_by(year(reference_time), epi_week, state_territory, signal) %>% 
#   summarize(average_level_week = mean(average_level, na.rm = TRUE), 
#             sum_hosp = sum(value, na.rm = TRUE))


### change to cor.test

full_storage <- data.frame()

for (each_state in unique(nwss_aux_cov_wval$state_territory)){
  
  set1 <- filter(nwss_aux_cov_wval, state_territory == each_state)
  
  for (each_signal in unique(set1$signal)){
    
    set2 <- filter(set1, signal == each_signal)
    
    set2 <- merge(set2, all_cov, by = c("state_territory", "year(reference_time)", "epi_week", "signal"))
    
    if (nrow(set2) > 50){
      
      cor1 <- cor.test(set2$average_level, set2$value)
      
      df1 <- data.frame(pvalue = cor1$p.value, 
                        ci_l = cor1$conf.int[1], 
                        ci_u = cor1$conf.int[2], 
                        cor = cor1$estimate)
      
      df1$state <- each_state
      df1$signal <- each_signal
      
      full_storage <- rbind(full_storage, df1)
      
      
    }
    
    
  }
  
  
}

full_storage <- full_storage %>% mutate(pvalue_01 = case_when(pvalue < 0.05 ~ "1", 
                                                              T ~ "0"))

ggplot() + 
  geom_tile(data = full_storage, aes(y = signal, x = state, fill = cor)) +
  geom_point(data = full_storage, aes(y = signal, x = state, shape = pvalue_01), color = "white") + 
  scale_shape_manual(values = c(1, 16)) + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
  guides(shape="none") + 
  scale_fill_gradient(low = "black", high = "violetred")




# ggplot() + 
#   geom_point(data = set2, aes(x = epi_week, y = value)) + 
#   geom_point(data = set2, aes(x = epi_week, y = average_level*10), color = "dodgerblue") + 
#   facet_wrap(.~`year(reference_time)`)



# ill <- filter(cor_compare, state_territory == "il")
# cor(ill$average_level_week, ill$sum_hosp, use = "complete.obs")

# ggplot(cor_compare2, aes(x = reorder(state_territory, -correlation), y = correlation)) + 
#   geom_bar(stat = "identity") + 
#   theme_bw() + 
#   theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
#   labs(x = "", 
#        y = "Correlation", 
#        title = "Overall Correlation for Available Data", 
#        subtitle = "COVID") +
#   facet_wrap(.~signal, ncol = 1)
# 
# cor_compare3 <- merge(cor_compare2, sites_per_state_covid, by = "state_territory", all = TRUE)
# 
# ggplot(filter(cor_compare3, !is.na(signal)), aes(x = num_sites, y = correlation, color = signal)) + 
#   geom_point(size = 3, alpha = 0.5) + 
#   scale_color_manual(values = c("#022F40", "#38AECC", "#FC7A1E")) + 
#   theme_bw() + 
#   labs(x = "Number of 2026 Testing Sites", 
#        y = "Correlation", 
#        title = "", 
#        color = "NWSS Correlation with")
# 
# ggplot(filter(cor_compare3, !is.na(signal)), aes(x = reorder(state_territory, -correlation), y = correlation, fill = signal)) + 
#   geom_bar(stat = "identity", position = "dodge") + 
#   theme_bw() + 
#   scale_fill_manual(values = c("#022F40", "#38AECC", "#FC7A1E")) + 
#   theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
#   labs(x = "", 
#        y = "Correlation", 
#        title = "Overall Correlation for Available Data", 
#        fill = "NWSS Correlation with",
#        subtitle = "COVID")


storage_set <- data.frame()

for (each_state in unique(nwss_aux_cov_wval$state_territory)){
  
  set1 <- filter(nwss_aux_cov_wval, state_territory == each_state)
  
  for (each_signal in unique(set1$signal)){
    
    set2 <- filter(set1, signal == each_signal)
    
    set2 <- merge(set2, all_cov, by = c("state_territory", "year(reference_time)", "epi_week", "signal"))
    
    set2 <- set2 %>% select(state_territory, signal, average_level, value)
    set2 <- set2[complete.cases(set2), ]
    
    
    if (nrow(set2) > 50 & sum(set2$value) != 0){
        # cor_hold <- cor.test(set2$average_level, set2$value)
        # 
        # cor <- cor_hold$estimate
        # pvalue <- cor_hold$p.value
        # ci_hi <- cor_hold$
        cor_hold <- ccf(set2$average_level, set2$value)
        
        d1 <- tibble(acf = cor_hold$acf, lag = cor_hold$lag)
        d1$state_territory <- each_state
        d1$signal <- each_signal
        
        storage_set <- rbind(storage_set, d1)
    }
    
  }
  
}

storage_set <- storage_set %>% group_by(state_territory, signal) %>% 
  mutate(max_value = max(acf, na.rm = TRUE)) %>% 
  mutate(align_max = case_when(acf == max_value ~ 1, 
                               T ~ NA))

ggplot(storage_set, aes(x = lag, y = acf, color = state_territory)) + 
  geom_line() + 
  geom_point(data = filter(storage_set, align_max == 1), aes(x = lag, y = acf), color = "black", alpha = 0.5) + 
  theme_bw() + 
  coord_cartesian(ylim = c(0, 1)) + 
  facet_wrap(.~signal, ncol = 2)
# it's what we'd move the delphi metric to get optimal correlation
# so we'd move delphi 2 weeks ahead to match nwss optimally, which means that 
# nwss does not lead.

# ggplot(filter(storage_set, state_territory %in% c("mi", "wy", "az", "de", "co", "mt", "ok", "ky", "dc")), aes(x = lag, y = acf, color = state_territory)) + 
#   geom_line() + 
#   geom_point(data = filter(storage_set, align_max == 1), aes(x = lag, y = acf), color = "black", alpha = 0.5) + 
#   theme_bw() + 
#   facet_wrap(.~signal, ncol = 1)

