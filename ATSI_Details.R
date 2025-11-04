# combine ESSA with Dashboard to determine why student groups are in ATSI

library(tidyverse)

dashboard <- read.csv("data/solano_dashboard.csv")
essa <- read.csv("data/essa.csv")

thin_dashboard <- 
  dashboard %>% 
  filter(reportingyear == 2023) %>% 
  select(cds, studentGroupLong = student_group_long, 
         indicator, color, currdenom, currnumer, currstatus)

atsi_long <- 
  essa %>% 
  select(cds:Charter, ATSIsupport, atsi_years, studentGroupLong) %>% 
  filter(countyname == "Solano",
         ATSIsupport == 1) %>% 
  left_join(thin_dashboard, by = join_by(cds, studentGroupLong))
  
atsi_wide <- 
  atsi_long %>% 
  select(-starts_with("curr")) %>% 
  pivot_wider(names_from = indicator, values_from = color)

write_csv(atsi_long, "data/atsi_long.csv")
write_csv(atsi_wide, "data/atsi_wide.csv")
