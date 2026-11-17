
### check average query length (and more)

library(tidyverse)
library(gt)

all_q_files <- list.files(path = "C:/Users/juliegil/Documents/Delphi_Work/oracle/", 
                            pattern = "query_times_*")

all_q <- data.frame()

for (i in all_q_files){
  
  fin <- read.csv(paste0("C:/Users/juliegil/Documents/Delphi_Work/oracle/", i))
  
  all_q <- rbind(all_q, fin)
  
}

ggplot(all_q, aes(x = total_query_time, y = rows_returned, color = source)) + 
  geom_point(alpha = 0.5, size = 5) + 
  theme_bw() + 
  #geom_vline(xintercept = c(60, 120, 180, 240, 300, 360)) + 
  scale_color_manual(values = c("#588b8b", "#7b8f4b", "#ffd5c2", 
                               "#f28f3b", "#c8553d", "#2d3047", "#93b7be", 
                               "#3C1642", "#000000", 
                               "#588b8b", "#7b8f4b", "#ffd5c2", 
                               "#f28f3b", "#c8553d", "#2d3047", "#93b7be", 
                               "#3C1642", "#000000")) + 
  labs(title = "Query Time per Source, Signal, and Geographic Type", 
       x = "Query Time Elapsed (s)", 
       y = "Number of Rows Returned by the Query",
       color = "Source")# + 
  #facet_wrap(.~queried_on, ncol = 1)




ggplot(all_q, aes(x = total_query_time, y = rows_returned, color = source)) + 
  geom_point(alpha = 0.5, size = 5) + 
  theme_bw() + 
  coord_cartesian(xlim = c(0, 60)) + 
  #geom_vline(xintercept = c(60, 120, 180, 240, 300, 360)) + 
  scale_color_manual(values = c("#588b8b", "#7b8f4b", "#ffd5c2", 
                                "#f28f3b", "#c8553d", "#2d3047", "#93b7be", 
                                "#3C1642", "#000000", 
                                "#588b8b", "#7b8f4b", "#ffd5c2", 
                                "#f28f3b", "#c8553d", "#2d3047", "#93b7be", 
                                "#3C1642", "#000000")) + 
  labs(title = "Query Time per Source, Signal, and Geographic Type", 
       subtitle = "1 Minute or Less Query Length",
       x = "Query Time Elapsed (s)", 
       y = "Number of Rows Returned by the Query",
       color = "Source")# + 
#facet_wrap(.~queried_on, ncol = 1)


over_one_minute <- filter(all_q, total_query_time > 60)
no_rows <- filter(all_q, rows_returned == 0)

ggplot(no_rows, aes(x = geo_type, y = signals, color = source)) + 
  geom_point() + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
  facet_wrap(.~source, scales = "free_y")



library(tidyverse)
library(gt)

all_q_files <- list.files(path = "C:/Users/juliegil/Documents/Delphi_Work/oracle/real_time_first_pass_lags/", 
                          pattern = "query_times_*")

all_q <- data.frame()

for (i in all_q_files){
  
  fin <- read.csv(paste0("C:/Users/juliegil/Documents/Delphi_Work/oracle/real_time_first_pass_lags/", i))
  
  all_q <- rbind(all_q, fin)
  
}

ggplot(all_q, aes(x = total_query_time, y = rows_returned, color = source)) + 
  geom_vline(xintercept = seq(0, 500, by = 60), linetype = "dashed", color = "grey") + 
  geom_point(alpha = 0.5, size = 5) + 
  theme_bw() + 
   
  scale_color_manual(values = c("#588b8b", "#7b8f4b", "#ffd5c2", 
                                "#f28f3b", "#c8553d", "#2d3047", "#93b7be")) + 
  labs(title = "Query Time per Source, Signal, and Geographic Type", 
       x = "Query Time Elapsed (s)", 
       y = "Number of Rows Returned by the Query",
       color = "Source",
       caption = "Dashed lines demarcate minutes.") + 
  coord_cartesian(xlim = c(0, 500), ylim = c(0, 40000)) + 
  facet_wrap(.~queried_on, ncol = 2)



### will this change over time? 

### do the number of columns also matter?



### what queries return zero rows and should they?

zero_rows <- filter(all_q, rows_returned == 0)

zero_rows <- zero_rows %>% select(source, signals, geo_type, queried_on)

ggplot(zero_rows, aes(x = geo_type, y = signals, fill = queried_on)) + 
  geom_tile(alpha = 0.3) + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  labs(x = "Geo-type", 
       y = "", 
       fill = "Query Date", 
       title = "Query Combinations that Return Zero Rows")




ggplot(filter(all_q, source == "va_respiratory" & queried_on == "2026-08-28"), 
       aes(x = geo_type, y = signals, fill = rows_returned)) + 
  geom_tile() + 
  geom_text(data = filter(all_q, source == "va_respiratory" & queried_on == "2026-08-28"), 
            aes(x = geo_type, y = signals, label = rows_returned, color = rows_returned < 50000)) + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1), 
        legend.position="none") +
  scale_color_manual(values = c("black", "white")) + 
  labs(x = "Geo-type", 
       y = "", 
       fill = "Query Date", 
       title = "VA Respiratory Query Rows Returned", 
       subtitle = "2026-08-28")
