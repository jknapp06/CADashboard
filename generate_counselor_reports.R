# Generate counselor reports
################################### HAUNTED ###############################

library(tidyverse)
library(openxlsx)
library(quarto)

# calhope_schools <- read.xlsx("data/calhope_schools.xlsx", sheet = "Sheet1")
calhope_schools <- read_csv("data/calhope_schools.csv")

walk(calhope_schools$school, ~ {
  district <- calhope_schools %>%
    filter(school == .x) %>%
    pull(district) %>%
    nth(1)
  
  initials <- calhope_schools %>%
    filter(school == .x) %>%
    pull(initials) %>%
    nth(1)
  
  print(paste(.x, district))
  quarto_render("school_counseler_one_page.qmd",
                        output_file = paste0(.x, " 2024 Dashboard Summary.pdf"),
                        execute_params = list("school" = .x,
                                              "district" = district,
                                              "initials" = initials,
                                              "year" = "2024"))
})
