# Generate 52072 reports

library(tidyverse)

all_dashboard <- read_csv("data/ca_dashboard.csv") %>% 
  filter(countyname == "Solano")

all_da_groups <- 
  all_dashboard %>% 
  filter(reportingyear %in% c(2024, 2023, 2022, 2019, 2018, 2017),
         indicator_eligible == TRUE,
         rtype == "D" |
         charter_flag == "Y" & rtype == "S") %>% 
  select(reportingyear, districtname, schoolname, student_group_long, indicator) %>%
  arrange(reportingyear) %>% 
  pivot_wider(names_from = reportingyear, values_from = indicator,
              values_fn = list) %>% 
  arrange(districtname)%>%
  mutate(across(where(is.list), ~ map_chr(., ~ paste(., collapse = "; "))))%>% 
  mutate(da_years = rowSums(across(c(`2019`, `2022`, `2023`, `2024`), ~ . != ""))) %>% 
  filter(da_years >= 3)

leas_to_report <- unique(all_da_groups$districtname)

for (lea in leas_to_report) {
  quarto::quarto_render("52072 Reports.qmd",
                        output_file = paste(lea, "52072 Report.docx"), 
                        execute_params = list("lea" = lea))

}

