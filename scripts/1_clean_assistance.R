# Download, clean, and combine dashboard files
# Solano County Office of Education
# Nov 2023

library(tidyverse)
library(openxlsx)
library(DBI)
library(duckdb)

# Open connection once
# con <- dbConnect(duckdb(), dbdir = "path/to/ca_education.duckdb")

assistance_25_url <- "https://www.cde.ca.gov/fg/aa/lc/documents/assistancestatus25.xlsx"
assistance_25_charter_url <- "https://www.cde.ca.gov/fg/aa/lc/documents/charterassistance25.xlsx"
assistance_24_url <- "https://www.cde.ca.gov/fg/aa/lc/documents/assistancestatus24.xlsx"
assistance_24_charter_url <- "https://www.cde.ca.gov/fg/aa/lc/documents/charterassistance24.xlsx"
assistance_23_url <- "https://www.cde.ca.gov/fg/aa/lc/documents/assistancestatus23.xlsx"
assistance_23_charter_url <- "https://www.cde.ca.gov/fg/aa/lc/documents/charterassistance23.xlsx"
assistance_22_url <- "https://www.cde.ca.gov/fg/aa/lc/documents/assistancestatus22.xlsx"
assistance_19_url <- "https://www.cde.ca.gov/fg/aa/lc/documents/assistancestatus19-rev.xlsx"
assistance_18_url <- "https://www.cde.ca.gov/fg/aa/lc/documents/assistancestatus18.xlsx"
assistance_17_url <- "https://www.cde.ca.gov/fg/aa/lc/documents/assistancestatus2017.xlsx"

# assistance_25 <- read.xlsx(assistance_25_url, sheet = 4, startRow = 6) |>
#   select(
#     cds = CDS,
#     grades_offered = Gsoffered,
#     reportingyear = ReportYear,
#     assistance_status = AssistanceStatus2024,
#     ends_with("priorities")
#   )
# assistance_25_charter <- read.xlsx(
#   assistance_25_charter_url,
#   sheet = 4,
#   startRow = 6
# ) |>
#   select(
#     cds = CDS,
#     grades_offered = Gsoffered,
#     reportingyear = ReportingYear,
#     assistance_status = AssistanceStatus2024,
#     -starts_with("Met"),
#     ends_with("current")
#   ) |>
#   rename_with(~ gsub("current", "priorities", .x))

assistance_24 <- read.xlsx(assistance_24_url, sheet = 4, startRow = 6) |>
  select(
    cds = CDS,
    grades_offered = Gsoffered,
    reportingyear = ReportYear,
    assistance_status = AssistanceStatus2024,
    ends_with("priorities")
  )
assistance_24_charter <- read.xlsx(
  assistance_24_charter_url,
  sheet = 4,
  startRow = 6
) |>
  select(
    cds = CDS,
    grades_offered = Gsoffered,
    reportingyear = ReportingYear,
    assistance_status = AssistanceStatus2024,
    -starts_with("Met"),
    ends_with("current")
  ) |>
  rename_with(~ gsub("current", "priorities", .x))
assistance_23 <- read.xlsx(assistance_23_url, sheet = 4, startRow = 6) |>
  select(
    cds = CDS,
    grades_offered = Gsoffered,
    reportingyear = ReportYear,
    assistance_status = AssistanceStatus2023,
    ends_with("priorities")
  )
assistance_23_charter <- read.xlsx(
  assistance_23_charter_url,
  sheet = 4,
  startRow = 6
) |>
  select(
    cds = CDS,
    grades_offered = Gsoffered,
    reportingyear = ReportingYear,
    assistance_status = AssistanceStatus2023,
    -starts_with("Met"),
    ends_with("current")
  ) |>
  rename_with(~ gsub("current", "priorities", .x))
assistance_22 <- read.xlsx(assistance_22_url, sheet = 4, startRow = 6) |>
  select(
    cds = CDS,
    grades_offered = Gsoffered,
    reportingyear = ReportYear,
    assistance_status = AssistanceStatus2022,
    # met_method_1 = MetMethod1,
    ends_with("priorities")
  )
assistance_19 <- read.xlsx(assistance_19_url, sheet = 4, startRow = 6) |>
  select(
    cds = CDS,
    grades_offered = Gsoffered,
    assistance_status = AssistanceStatus2019,
    # met_method_1 = MetMethod1, met_method_2 = MetMethod2,
    # met_method_3 = MetMethod3,
    ends_with("priorities")
  ) |>
  mutate(reportingyear = "2019")
assistance_18 <- read.xlsx(assistance_18_url, sheet = 1, startRow = 5) |>
  select(
    cds = CDS,
    grades_offered = gsoffered,
    assistance_status = AssistanceStatus2018,
    # met_method_1 = MetMethod1, met_method_2 = MetMethod2,
    # met_method_3 = MetMethod3,
    ends_with("priorities")
  ) |>
  mutate(reportingyear = "2018")
assistance_17 <- read.xlsx(assistance_17_url, sheet = 4, startRow = 5) |>
  select(
    cds = CDS,
    grades_offered = gsoffered,
    assistance_status = Assistance_Status,
    # met_method_1 = MetMethod1, met_method_2 = MetMethod2,
    # met_method_3 = MetMethod3,
    ends_with("PRIORITIES")
  ) |>
  rename_with(~ gsub("PRIORITIES", "priorities", .x)) |>
  mutate(reportingyear = "2017")

# add 2017, 2018

assistance <-
  bind_rows(
    assistance_24,
    assistance_24_charter,
    assistance_23,
    assistance_23_charter,
    assistance_22,
    assistance_19,
    assistance_18,
    assistance_17
  ) |>
  pivot_longer(
    cols = ends_with("priorities"),
    names_to = "studentgroup",
    values_to = "assistance",
    names_pattern = "(.*)priorities"
  ) |>
  mutate(
    studentgroup = if_else(studentgroup == "TOM", "MR", studentgroup),
    studentgroup = str_remove_all(studentgroup, "_")
  ) |>
  drop_na(assistance)

# write_csv(assistance, "data/assistance.csv")

essa24_url <- "https://www.cde.ca.gov/sp/sw/t1/documents/essaassistance24.xlsx"
essa23_url <- "https://www.cde.ca.gov/sp/sw/t1/documents/essaassistance23.xlsx"
essa22_url <- "https://www.cde.ca.gov/sp/sw/t1/documents/essaassistance22.xlsx"
essa21_url <- "https://www.cde.ca.gov/sp/sw/t1/documents/essaassistance21.xlsx"
essa19_url <- "https://www.cde.ca.gov/sp/sw/t1/documents/essaassistance19.xlsx"
essa18_url <- "https://www.cde.ca.gov/sp/sw/t1/documents/scheligibilitystate.xlsx"

essa24 <- read.xlsx(essa24_url, sheet = 2, startRow = 3) |>
  select(
    -c(
      AssistanceStatus2018,
      AssistanceStatus2019,
      AssistanceStatus2020,
      AssistanceStatus2021,
      AssistanceStatus2022,
      AssistanceStatus2023,
      TitleI1718,
      TitleI1819,
      TitleI2122
    )
  ) |>
  mutate(
    EnrollmentCount = parse_number(EnrollmentCount),
    across(
      c(AA, AI, AS, EL, FI, FOS, HI, HOM, PI, SED, SWD, TOM, WH),
      parse_number
    )
  ) |>
  rename(AssistanceStatus = AssistanceStatus2024, TitleI = TitleI2223)

essa23 <- read.xlsx(essa23_url, sheet = 2, startRow = 3) |>
  select(
    -c(
      AssistanceStatus2018,
      AssistanceStatus2019,
      AssistanceStatus2020,
      AssistanceStatus2021,
      AssistanceStatus2022,
      TitleI1718,
      TitleI1819,
      TitleI2122
    )
  ) |>
  mutate(EnrollmentCount = parse_number(EnrollmentCount)) |>
  rename(AssistanceStatus = AssistanceStatus2023, TitleI = TitleI2223)

essa22 <- read.xlsx(essa22_url, sheet = 2, startRow = 3) |>
  select(
    -c(
      AssistanceStatus2018,
      AssistanceStatus2019,
      AssistanceStatus2020,
      AssistanceStatus2021,
      TitleI1718,
      TitleI1819
    )
  ) |>
  rename(AssistanceStatus = AssistanceStatus2022, TitleI = TitleI2122)

essa21 <- read.xlsx(essa21_url, sheet = 1, startRow = 3) |>
  select(
    -c(
      AssistanceStatus2018,
      AssistanceStatus2019,
      AssistanceStatus2020,
      TitleI1718
    )
  ) |>
  rename(AssistanceStatus = AssistanceStatus2021, TitleI = TitleI1819)

essa19 <- read.xlsx(essa19_url, sheet = 1, startRow = 3) |>
  select(-AssistanceStatus2018) |>
  rename(AssistanceStatus = AssistanceStatus2019, TitleI = TitleI1819)

essa18 <- read.xlsx(essa18_url, sheet = 1, startRow = 3) |>
  rename(TitleI = TitleIStatus1718) |>
  rename(
    cds = CDS,
    schoolname = Schoolname,
    districtname = Districtname,
    countyname = Countyname
  )

essa <-
  bind_rows(essa24, essa23, essa22, essa21, essa19, essa18) |>
  pivot_longer(
    cols = c(AA, AI, AS, EL, FI, FOS, HI, HOM, PI, SED, SWD, TOM, WH),
    names_to = "studentGroup",
    values_to = "ATSIsupport"
  )

atsi <-
  essa |>
  select(cds, studentGroup, ATSIsupport, ReportingYear) |>
  drop_na(ATSIsupport) |>
  pivot_wider(
    names_from = ReportingYear,
    names_prefix = "ATSI",
    values_from = ATSIsupport
  ) |>
  mutate(
    atsi_years = if_else(ATSI2024 == 1, ATSI2024 + ATSI2023 + ATSI2022, 0)
  ) |>
  select(-c(ATSI2024, ATSI2023, ATSI2022))

csi <-
  essa |>
  select(cds, AssistanceStatus, ReportingYear) |>
  distinct() |>
  drop_na() |>
  pivot_wider(
    names_from = ReportingYear,
    names_prefix = "CSI_",
    values_from = AssistanceStatus,
    values_fill = "No Status"
  ) |>
  mutate(
    csi_years = case_when(
      CSI_2023 %in% c("No Status", "ATSI", "General Assistance") ~ 0,
      CSI_2022 %in% c("No Status", "ATSI", "General Assistance") ~ 1,
      CSI_2021 %in% c("No Status", "ATSI", "General Assistance") ~ 2,
      CSI_2019 %in% c("No Status", "ATSI", "General Assistance") ~ 4,
      CSI_2018 %in% c("No Status", "ATSI", "General Assistance") ~ 5,
      .default = 6
    )
  )

essa_joined <-
  essa |>
  filter(ReportingYear == "2023") |>
  select(-c(AssistanceStatus, ReportingYear)) |>
  distinct() |>
  left_join(csi, by = "cds") |>
  left_join(atsi, by = c("cds", "studentGroup")) |>
  mutate(
    studentGroupLong = case_match(
      studentGroup,
      "AA" ~ "Black/African American",
      "AI" ~ "American Indian or Alaska Native",
      "AS" ~ "Asian",
      "FI" ~ "Filipino",
      "HI" ~ "Hispanic",
      "PI" ~ "Pacific Islander",
      "WH" ~ "White",
      "MR" ~ "Multiple Races/Two or more",
      "EL" ~ "English Learner",
      "LTEL" ~ "Long-Term English Learner",
      "SED" ~ "Socioeconomically Disadvantaged",
      "SWD" ~ "Students with Disabilities",
      "FOS" ~ "Foster Youth",
      "HOM" ~ "Homeless Youth",
      "TOM" ~ "Multiple Races/Two or more"
    )
  )

# write_csv(essa_joined, "data/essa.csv")
