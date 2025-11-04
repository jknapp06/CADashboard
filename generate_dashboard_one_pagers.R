library(tidyverse)
library(openxlsx)
library(quarto)

schools <- read_csv("data/solano_schools.csv")

schools <- 
  schools %>% 
  filter(districtname == "Dixon Unified")

walk(schools$schoolname, ~ {
  district <- schools %>%
    filter(schoolname == .x) %>%
    pull(districtname) %>%
    nth(1)
  
  initials <- schools %>%
    filter(schoolname == .x) %>%
    pull(initials) %>%
    nth(1)
  
  print(paste(.x, district))
  quarto_render("dashboard_one_pager_any_lea.qmd",
                output_file = paste0(.x, " 2024 Dashboard Summary.pdf"),
                execute_params = list("school" = .x,
                                      "district" = district,
                                      "initials" = initials,
                                      "year" = "2024"))
})
