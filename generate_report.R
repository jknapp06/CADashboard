# Generate DA reports

library(tidyverse)

dashboard <- read_csv("data/ca_dashboard.csv") %>% 
  filter(countyname == "Solano")

# list all districts and DA eligible charters

districts <-  
  dashboard %>% 
  pull(districtname) %>% 
  unique()

da_eligible_charters <- 
  dashboard %>% 
  filter(reportingyear == 2024,
         charter_flag == "Y",
         assistance_status == "Differentiated Assistance") %>% 
  pull(schoolname) %>% 
  unique()

leas_to_report <- c(districts, da_eligible_charters)

for (lea in leas_to_report) {
  quarto::quarto_render("DA_one_page.qmd",
                        output_file = paste(lea, "DA one pager.pdf"), 
                        execute_params = list("lea" = lea))
}
