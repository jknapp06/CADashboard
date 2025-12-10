# data-urls.R
# Central place for all source URLs and small constants.
# Load this first in refresh.R

dashboard_files <- tibble::tribble(
  ~priority , ~indicator       , ~url                                                                             ,
          4 , "ELA"            , "https://www3.cde.ca.gov/researchfiles/cadashboard/eladownload2025.txt"          ,
          4 , "Math"           , "https://www3.cde.ca.gov/researchfiles/cadashboard/mathdownload2025.txt"         ,
          4 , "science"        , "https://www3.cde.ca.gov/researchfiles/cadashboard/sciencedownload2025.txt"      ,
          4 , "ELPI"           , "https://www3.cde.ca.gov/researchfiles/cadashboard/elpidownload2025.txt"         ,
          8 , "college/career" , "https://www3.cde.ca.gov/researchfiles/cadashboard/ccidownload2025.txt"          ,
          5 , "graduation"     , "https://www3.cde.ca.gov/researchfiles/cadashboard/graddownload2025.txt"         ,
          5 , "absenteeism"    , "https://www3.cde.ca.gov/researchfiles/cadashboard/chronicdownload2025.txt"      ,
          6 , "suspension"     , "https://www3.cde.ca.gov/researchfiles/cadashboard/suspdownload2025.txt"         ,
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

# Teacher credentialing data
teacher_assignments_url <- "https://www3.cde.ca.gov/demo-downloads/tamo/tamo2324.txt"

# Assistance files (from your original script)
assistance_urls <- list(
  assistance_25 = "https://www.cde.ca.gov/fg/aa/lc/documents/assistancestatus25.xlsx",
  assistance_25_charter = "https://www.cde.ca.gov/fg/aa/lc/documents/charterassistance25.xlsx",
  assistance_24 = "https://www.cde.ca.gov/fg/aa/lc/documents/assistancestatus24.xlsx",
  assistance_24_charter = "https://www.cde.ca.gov/fg/aa/lc/documents/charterassistance24.xlsx",
  assistance_23 = "https://www.cde.ca.gov/fg/aa/lc/documents/assistancestatus23.xlsx",
  assistance_23_charter = "https://www.cde.ca.gov/fg/aa/lc/documents/charterassistance23.xlsx",
  assistance_22 = "https://www.cde.ca.gov/fg/aa/lc/documents/assistancestatus22.xlsx",
  assistance_19 = "https://www.cde.ca.gov/fg/aa/lc/documents/assistancestatus19-rev.xlsx",
  assistance_18 = "https://www.cde.ca.gov/fg/aa/lc/documents/assistancestatus18.xlsx",
  assistance_17 = "https://www.cde.ca.gov/fg/aa/lc/documents/assistancestatus2017.xlsx"
)

# ESSA files (from your original script)
essa_urls <- list(
  # essa25 = "https://www.cde.ca.gov/sp/sw/t1/documents/essaassistance25.xlsx",
  essa24 = "https://www.cde.ca.gov/sp/sw/t1/documents/essaassistance24.xlsx",
  essa23 = "https://www.cde.ca.gov/sp/sw/t1/documents/essaassistance23.xlsx",
  essa22 = "https://www.cde.ca.gov/sp/sw/t1/documents/essaassistance22.xlsx", # double-check if needed
  essa21 = "https://www.cde.ca.gov/sp/sw/t1/documents/essaassistance21.xlsx",
  essa19 = "https://www.cde.ca.gov/sp/sw/t1/documents/essaassistance19.xlsx"
  # essa18 = "https://www.cde.ca.gov/sp/sw/t1/documents/scheligibilitystate.xlsx"
)

# Local cache directory for downloaded raw files
cache_dir <- "data/cache"
dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
