# Generate DA Tables

library(tidyverse)
library(openxlsx)
library(quarto)

lea_list <- read.xlsx("data/lea_list.xlsx")

district_list <- 
  lea_list %>% 
  filter(!is.na(initials))

charter_list <- 
  lea_list %>% 
  filter(is.na(initials))

for (i in 1:nrow(district_list)) {
  quarto_render("da_table.qmd",
                output_file = paste(district_list$lea[i], "DA Table.docx"),
                execute_params = list("lea" = district_list$lea[i]))
}

for (i in 1:nrow(charter_list)) {
  quarto_render("da_table.qmd",
                output_file = paste(charter_list$lea[i], "DA Table.docx"),
                execute_params = list("lea" = charter_list$lea[i]))
}
