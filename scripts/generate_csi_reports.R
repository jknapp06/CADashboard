# Generate CSI reports

library(tidyverse)

essa <- read_csv("data/dashboard_essa.csv") %>% 
  filter(countyname == "Solano")

# list all districts and DA eligible charters

graduation <-  
  essa %>%
  filter(CSI_2024 == "CSI Grad") %>% 
  pull(schoolname) %>% 
  unique()

low_perform <-  
  essa %>%
  filter(CSI_2024 == "CSI Low Perform") %>% 
  pull(schoolname) %>% 
  unique()

for (school in graduation) {
    quarto::quarto_render("csi_graduation_one_page.qmd",
                          output_file = paste(school, "CSI Report.pdf"), 
                          execute_params = list("school" = school))
}
for (school in low_perform) {
    quarto::quarto_render("csi_low_perform_one_page.qmd",
                          output_file = paste(school, "CSI Report.pdf"), 
                          execute_params = list("school" = school))
}

