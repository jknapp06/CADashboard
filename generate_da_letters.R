# Generate DA Letters

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
  quarto_render("da_letter_district.qmd",
                output_file = paste(district_list$lea[i], "DA Letter.docx"),
                execute_params = list("lea" = district_list$lea[i],
                                      "initials" = district_list$initials[i],
                                      "superintendent" = district_list$superintendent[i]))
}

for (i in 1:nrow(charter_list)) {
  quarto_render("da_letter_charter.qmd",
                output_file = paste(charter_list$lea[i], "DA Letter.docx"),
                execute_params = list("lea" = charter_list$lea[i],
                                      "superintendent" = charter_list$superintendent[i]))
}
