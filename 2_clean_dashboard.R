# Download, clean, and combine dashboard files
# Solano County Office of Education
# Nov 2023

library(tidyverse)
library(vroom)

# Create a tibble of URLs to download.
dashboard_files <- tribble(
  ~priority , ~indicator       , ~url                                                                             ,
          4 , "ELA"            , "https://www3.cde.ca.gov/researchfiles/cadashboard/eladownload2024.txt"          ,
          4 , "Math"           , "https://www3.cde.ca.gov/researchfiles/cadashboard/mathdownload2024.txt"         ,
          4 , "science"        , "https://www3.cde.ca.gov/researchfiles/cadashboard/sciencedownload2024.txt"      ,
          4 , "ELPI"           , "https://www3.cde.ca.gov/researchfiles/cadashboard/elpidownload2024.txt"         ,
          8 , "college/career" , "https://www3.cde.ca.gov/researchfiles/cadashboard/ccidownload2024.txt"          ,
          5 , "graduation"     , "https://www3.cde.ca.gov/researchfiles/cadashboard/graddownload2024.txt"         ,
          5 , "absenteeism"    , "https://www3.cde.ca.gov/researchfiles/cadashboard/chronicdownload2024.txt"      ,
          6 , "suspension"     , "https://www3.cde.ca.gov/researchfiles/cadashboard/suspdownload2024.txt"         ,
          4 , "ELA"            , "https://www3.cde.ca.gov/researchfiles/cadashboard/eladownload2023.txt"          ,
          4 , "Math"           , "https://www3.cde.ca.gov/researchfiles/cadashboard/mathdownload2023.txt"         ,
          4 , "ELPI"           , "https://www3.cde.ca.gov/researchfiles/cadashboard/elpidownload2023.txt"         ,
          8 , "college/career" , "https://www3.cde.ca.gov/researchfiles/cadashboard/ccidownload2023.txt"          ,
          5 , "graduation"     , "https://www3.cde.ca.gov/researchfiles/cadashboard/graddownload2023.txt"         ,
          5 , "absenteeism"    , "https://www3.cde.ca.gov/researchfiles/cadashboard/chronicdownload2023.txt"      ,
          6 , "suspension"     , "https://www3.cde.ca.gov/researchfiles/cadashboard/suspdownload2023.txt"         ,
          4 , "ELA"            , "https://www3.cde.ca.gov/researchfiles/cadashboard/eladownload2022.txt"          ,
          4 , "Math"           , "https://www3.cde.ca.gov/researchfiles/cadashboard/mathdownload2022.txt"         ,
          4 , "ELPI"           , "https://www3.cde.ca.gov/researchfiles/cadashboard/elpidownload2022.txt"         ,
          8 , "college/career" , "https://www3.cde.ca.gov/researchfiles/cadashboard/ccidownload2022.txt"          ,
          5 , "graduation"     , "https://www3.cde.ca.gov/researchfiles/cadashboard/graddownload2022.txt"         ,
          5 , "absenteeism"    , "https://www3.cde.ca.gov/researchfiles/cadashboard/chronicdownload2022.txt"      ,
          6 , "suspension"     , "https://www3.cde.ca.gov/researchfiles/cadashboard/suspdownload2022.txt"         ,
          4 , "ELA"            , "https://www3.cde.ca.gov/researchfiles/cadashboard/eladownload2019.txt"          ,
          4 , "Math"           , "https://www3.cde.ca.gov/researchfiles/cadashboard/mathdownload2019.txt"         ,
          4 , "ELPI"           , "https://www3.cde.ca.gov/researchfiles/cadashboard/elpidownload2019.txt"         ,
          8 , "college/career" , "https://www3.cde.ca.gov/researchfiles/cadashboard/ccidownload2019.txt"          ,
          5 , "graduation"     , "https://www3.cde.ca.gov/researchfiles/cadashboard/graddownload2019.txt"         ,
          5 , "absenteeism"    , "https://www3.cde.ca.gov/researchfiles/cadashboard/chronicdownload2019.txt"      ,
          6 , "suspension"     , "https://www3.cde.ca.gov/researchfiles/cadashboard/suspdownload2019.txt"         ,
          5 , "graduation"     , "https://www3.cde.ca.gov/researchfiles/cadashboard/graddownload2021dassonly.txt" ,
          5 , "graduation"     , "https://www3.cde.ca.gov/researchfiles/cadashboard/graddownload2020dassonly.txt"
)

# Extra lines when reading from local files:
# 4,          "ELA",     "C:/Users/jknapp/Solano County Office of Education/Assessment Research and Evaluation - A.R.E. Library/California School Dashboard/2024/Dashboard Files/ELA_2024.txt",
# 4,          "Math",    "C:/Users/jknapp/Solano County Office of Education/Assessment Research and Evaluation - A.R.E. Library/California School Dashboard/2024/Dashboard Files/MATH_2024.txt",
# 4,          "ELPI",    "C:/Users/jknapp/Solano County Office of Education/Assessment Research and Evaluation - A.R.E. Library/California School Dashboard/2024/Dashboard Files/ELPI_2024.txt",
# 8,    "college/career","C:/Users/jknapp/Solano County Office of Education/Assessment Research and Evaluation - A.R.E. Library/California School Dashboard/2024/Dashboard Files/CCI_2024.txt",
# 5,        "graduation","C:/Users/jknapp/Solano County Office of Education/Assessment Research and Evaluation - A.R.E. Library/California School Dashboard/2024/Dashboard Files/GRAD_2024.txt",
# 5,        "absenteeism","C:/Users/jknapp/Solano County Office of Education/Assessment Research and Evaluation - A.R.E. Library/California School Dashboard/2024/Dashboard Files/CHRONIC_2024.txt",
# 6,        "suspension", "C:/Users/jknapp/Solano County Office of Education/Assessment Research and Evaluation - A.R.E. Library/California School Dashboard/2024/Dashboard Files/SUSPENSION_2024.txt",
# 4,        "science", "C:/Users/jknapp/Solano County Office of Education/Assessment Research and Evaluation - A.R.E. Library/California School Dashboard/2024/Dashboard Files/SCIENCE_2024.txt",

# This function loads dashboard files and cleans them up for joining.
load_dashboard_file <- function(file_url, d_indicator, priority) {
  dash <- NULL
  year <- str_sub(file_url, -8, -5)
  print(file_url)
  if (d_indicator == "ELA" | d_indicator == "Math") {
    dash <- vroom(
      file_url,
      col_types = c(coe_flag = "c", pairshare_method = "c")
    )
  } else if (d_indicator == "ELPI") {
    dash <- vroom(file_url, col_types = c(coe_flag = "c"))
  } else if (d_indicator == "absenteeism" | d_indicator == "suspension") {
    dash <- vroom(
      file_url,
      col_types = c(coe_flag = "c", certifyflag = "c", dataerrorflag = "c")
    )
  } else {
    dash <- vroom(file_url, col_types = c(coe_flag = "c"))
  }
  dash |>
    rename_with(str_to_lower) |>
    mutate(
      indicator = d_indicator,
      priority = priority
    )
}

# test_index <- 7
# d_test <- load_dashboard_file(dashboard_files$url[[test_index]],
#                               dashboard_files$indicator[[test_index]],
#                               dashboard_files$priority[[test_index]])

dashboard <- NULL
# Load the empty dashboard tibble with the records
# from the dashboard website.
for (i in 1:nrow(dashboard_files)) {
  dashboard <- bind_rows(
    dashboard,
    load_dashboard_file(
      dashboard_files$url[[i]],
      dashboard_files$indicator[[i]],
      dashboard_files$priority[[i]]
    )
  )
}

# Last run 10 Jan 2025
# write_csv(dashboard, "data/full_dashboard.csv")
dashboard <- read_csv("data/full_dashboard.csv")
assistance <- read_csv("data/assistance.csv")

# Add longer demographic names to the dashboard tibble.
# Join with assistance status.
dashboard_with_assistance <-
  dashboard |>
  select(
    cds,
    rtype,
    districtname,
    countyname,
    schoolname,
    charter_flag,
    studentgroup,
    currdenom,
    currnumer,
    currstatus,
    statuslevel,
    changelevel,
    reportingyear,
    indicator,
    priority,
    priorstatus,
    change,
    color,
    smalldenom,
    starts_with("currprate"),
    ends_with("prloss"),
    curr_prep
  ) |>
  mutate(
    student_group_long = case_match(
      studentgroup,
      "ALL" ~ "All students",
      "AA" ~ "Black/African American",
      "AI" ~ "American Indian or Alaska Native",
      "AS" ~ "Asian",
      "FI" ~ "Filipino",
      "HI" ~ "Hispanic",
      "PI" ~ "Pacific Islander",
      "WH" ~ "White",
      "MR" ~ "Multiple Races/Two or more",
      "EL" ~ "English Learner",
      "ELO" ~ "English Learners Only",
      "RFP" ~ "RFEPs Only",
      "EO" ~ "English Only",
      "SBA" ~ "Smarter Balanced Assessment",
      "CAA" ~ "CA Alternative Assessment",
      "SED" ~ "Socioeconomically Disadvantaged",
      "SWD" ~ "Students with Disabilities",
      "FOS" ~ "Foster Youth",
      "HOM" ~ "Homeless Youth",
      "TOM" ~ "Multiple Races/Two or more",
      "LTEL" ~ "Long-Term English Learner"
    ),
    countyname = case_match(
      rtype,
      "X" ~ "CA State Aggregate",
      .default = countyname
    ),
    schoolname = case_when(
      rtype == "D" & is.na(schoolname) ~ "District Aggregate",
      .default = schoolname
    )
  ) |>
  left_join(assistance, by = join_by(cds, studentgroup, reportingyear)) |>
  mutate(
    priority_eligible = case_when(
      (assistance == "A" &
        (priority == 4 | priority == 5 | priority == 6)) |
        (assistance == "B" &
          (priority == 4 | priority == 5)) |
        (assistance == "C" &
          (priority == 5 | priority == 6)) |
        (assistance == "D" &
          (priority == 4 | priority == 6)) |
        (assistance == "E" &
          (priority == 4 | priority == 8)) |
        (assistance == "F" &
          (priority == 5 | priority == 8)) |
        (assistance == "G" &
          (priority == 6 | priority == 8)) |
        (assistance == "H" &
          (priority == 4 | priority == 5 | priority == 8)) |
        (assistance == "I" &
          (priority == 4 | priority == 6 | priority == 8)) |
        (assistance == "J" &
          (priority == 5 | priority == 6 | priority == 8)) |
        (assistance == "K" &
          (priority == 4 | priority == 5 | priority == 6 | priority == 8)) ~
        T,
      .default = F
    )
  ) |>
  distinct()

priority_4 <-
  dashboard_with_assistance |>
  filter(priority == 4, priority_eligible == T) |>
  mutate(
    color = case_when(
      reportingyear == 2022 ~ statuslevel,
      reportingyear == 2024 & indicator == "science" ~ currstatus,
      .default = color
    )
  ) |>
  select(reportingyear, cds, student_group_long, indicator, color) |>
  pivot_wider(names_from = indicator, values_from = color) |>
  mutate(
    caaspp_eligible = case_when(
      ELA == 1 & Math == 1 ~ T,
      ELA == 2 & Math == 1 ~ T,
      ELA == 1 & Math == 2 ~ T,
      .default = F
    ),
    elpi_eligible = case_when(
      ELPI == 1 ~ T,
      .default = F
    )
  )

ca_dashboard <-
  dashboard_with_assistance |>
  left_join(
    priority_4,
    by = join_by(cds, reportingyear, student_group_long)
  ) |>
  mutate(
    indicator_eligible = case_when(
      priority == 8 & priority_eligible ~ T,
      priority != 4 & priority_eligible & color == "1" ~ T,
      priority != 4 &
        priority_eligible &
        reportingyear == 2022 &
        statuslevel == 1 ~
        T,
      priority == 4 & indicator == "ELA" & caaspp_eligible == T ~ T,
      priority == 4 & indicator == "Math" & caaspp_eligible == T ~ T,
      priority == 4 & indicator == "ELPI" & elpi_eligible == T ~ T,
      .default = F
    )
  ) |>
  select(-c(ELA, Math, ELPI, caaspp_eligible, elpi_eligible))

# |>
#   mutate(indicator_color = case_match(color,
#                                       0 ~ "#D3D3D3",
#                                       1 ~ "#FF6347",
#                                       2 ~ "#FFA500",
#                                       3 ~ "#FFFF00",
#                                       4 ~ "#3CB371",
#                                       5 ~ "#1E90FF",
#                                       .default = "#2F4F4F"))

small_dashboard <-
  ca_dashboard |>
  filter(countyname == "Solano" | countyname == "CA State Aggregate")

# Save the file in two different places
write_csv(ca_dashboard, "data/ca_dashboard.csv")
write_csv(small_dashboard, "data/solano_dashboard.csv")
# write_csv(ca_dashboard, file = "C:/Users/jknapp/Solano County Office of Education/Assessment Research and Evaluation - A.R.E. Library/Data/ca_dashboard.csv")
# write_csv(small_dashboard, file = "C:/Users/jknapp/Solano County Office of Education/Assessment Research and Evaluation - A.R.E. Library/Data/solano_dashboard.csv")

essa <- read_csv("data/essa.csv")

dashboard_essa <-
  ca_dashboard |>
  filter(countyname == "Solano") |>
  left_join(
    essa,
    join_by(
      cds,
      districtname,
      countyname,
      schoolname,
      student_group_long == studentGroupLong
    )
  )

write_csv(dashboard_essa, "data/dashboard_essa.csv")
