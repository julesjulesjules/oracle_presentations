
library(tidyverse)
library(gt)

all_lag_files <- list.files(path = "C:/Users/juliegil/Documents/Delphi_Work/oracle/real_time_first_pass_lags/", 
           pattern = "fp_lags_*")

all_lags <- data.frame()

for (i in all_lag_files){
  
  fin <- read.csv(paste0("C:/Users/juliegil/Documents/Delphi_Work/oracle/real_time_first_pass_lags/", i))
  fin$file_name <- i
  all_lags <- rbind(all_lags, fin)
  
}

specific_look <- all_lags %>% group_by(source, signal, geo_type) %>%
  summarize(max = max(lag_time), 
            min = min(lag_time))

specific_look <- all_lags %>% group_by(source, signal, geo_type, fill_method, file_name) %>%
  summarize(max = max(lag_time), 
            min = min(lag_time))

a <- filter(specific_look, source == "va_respiratory")

ggplot(a, aes(x = signal, y = geo_type, fill = fill_method)) + 
  geom_tile(alpha = 0.5) + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
  labs(x = "", y = "", fill = "fill_method", title = "va_respiratory")
  

summarized <- all_lags %>% group_by(today, source, signal) %>%
  summarize(max = max(lag_time))

summarized <- reshape2::dcast(summarized, source + signal ~ today, value = c("max"))

for (i in unique(summarized$source)){

    filter(summarized, source == i) %>% gt(groupname_col = "source") %>% 
      tab_header(
      title = "Daily Lag Evaluation"
    )  %>% 
      tab_style(cell_fill("#93b7be"), cells_row_groups(everything())) %>% 
      gtsave(path = "C:/Users/juliegil/Documents/Delphi_Work/oracle/images/", filename = paste0(i, "_daily_lag_eval_table_", gsub('-', '', Sys.Date()), ".png"))

}


summarized_long <- all_lags %>% group_by(today, source, signal) %>%
  summarize(max = max(lag_time))

# overall density
ggplot() + 
  geom_density(data = summarized_long, aes(x = max)) + 
  theme_bw() + 
  labs(x = "Lag (days)", 
       y = "", 
       title = "Overall Data Source Lags")

ggplot() + 
  geom_hline(yintercept = c(7, 14), linetype = "dashed", color = "grey") + 
  geom_point(data = summarized_long, 
               aes(x = as_date(today), y = max, 
                   color = source), size = 2, alpha = 0.2) + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
  scale_x_date(breaks = "1 day", date_labels = "%m/%d") + 
  scale_color_manual(values = c("#588b8b", "#000000", "#ffd5c2", 
                               "#f28f3b", "#c8553d", "#2d3047", "#93b7be")) +
  labs(x = "Query Date", 
       y = "Lag (Days)", 
       title = "Overall Data Source Lags by Source and Signals") + 
  facet_wrap(.~source)

### at end, just want to double check that the lag is always the same
# for all geographies and fill methods (it was true on 8/27)

