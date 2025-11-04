# Generate suspension reports

library(tidyverse)

dashboard <- read_csv("data/ca_dashboard.csv") %>% 
  filter(countyname == "Solano")

sus_college <- 
  dashboard %>% 
  filter(reportingyear == 2024) %>% 
  select(reportingyear, districtname, schoolname, student_group_long, indicator, currstatus) %>% 
  filter(indicator %in% c("suspension", "college/career")) %>% 
  pivot_wider(names_from = indicator, values_from = currstatus)

districts <-  
  sus_college %>% 
  filter(suspension > `college/career`) %>%
  pull(districtname) %>% 
  unique()

for (district in districts) {
  quarto::quarto_render("suspension.qmd",
                        output_file = paste(district, "suspension CCI report.docx"), 
                        execute_params = list("lea" = district))
}
