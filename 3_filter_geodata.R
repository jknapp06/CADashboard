# Clean and filter geojson

library(tidyverse)
library(sf)

ca_districts <- read_sf("data/California_School_District_Areas_2022-23.geojson")
ca_schools <- read_sf("data/California_Schools_2022-23.geojson")

# dashboard <- read_csv("data/ca_dashboard.csv") %>% 
#   mutate(indicator_color = case_match(color,
#                                       0 ~ "#D3D3D3",
#                                       1 ~ "#FF6347",
#                                       2 ~ "#FFA500",
#                                       3 ~ "#FFFF00",
#                                       4 ~ "#3CB371",
#                                       5 ~ "#1E90FF",
#                                       .default = "#2F4F4F"))
# 
# districts <- 
#   dashboard %>% 
#   filter(rtype == "D")
# 
# schools <- 
#   dashboard %>% 
#   filter(rtype == "S")

district_geo <- 
  ca_districts %>% 
  filter(CountyName == "Solano") %>% 
  select(cds = CDSCode, AssistStatus:EnrollNonCharter, geometry, DistrctAreaSqMi, starts_with("Shape")) %>% 
  mutate(OBJECTID = row_number())

school_geo <- 
  ca_schools %>% 
  filter(Status == "Active",
         CountyName == "Solano") %>% 
  select(cds = CDSCode, -OBJECTID, -ends_with("count"), -ends_with("pct")) %>% 
  mutate(OBJECTID = row_number())

# write_sf(district_geo, "data/ca_districts.geojson")
# write_sf(school_geo, "data/ca_schools.geojson")

write_sf(district_geo, "data/solano_districts.geojson")
write_sf(school_geo, "data/solano_schools.geojson")