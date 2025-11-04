library(tidyverse)

dashboard <- read_csv("data/solano_dashboard.csv") %>% 
  filter(districtname == "Vallejo City Unified",
         reportingyear > 2022)

write_csv(dashboard, "data/vcusd_23_24_dashboard.csv")
