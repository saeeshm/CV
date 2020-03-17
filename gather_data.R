# Name: Nick Strayer, Adapted for use by Saeesh Mangwani
# Date: 2020-03-02
# Description: Using google sheets to gather and store cv information in a number of different objects

# ==== Loading libraries ====
library(googlesheets4)

if(sheet_is_publicly_readable){
  # This tells google sheets to not try and authenticate. Note that this will only
  # work if your sheet has sharing set to "anyone with link can view"
  sheets_deauth()
} else {
  # My info is in a public sheet so there's no need to do authentication but if you want
  # to use a private sheet, then this is the way you need to do it.
  # designate project-specific cache so we can render Rmd without problems
  options(gargle_oauth_cache = ".secrets")
  
  # Need to run this once before knitting to cache an authentication token
  googlesheets4::sheets_auth()
}
  
# Saving each page of the google sheet in a different object, to make indexing through this information easier later on 
position_data <- read_sheet(positions_sheet_loc, sheet = "positions") %>% as_tibble()
skills        <- read_sheet(positions_sheet_loc, sheet = "language_skills") %>% as_tibble()
text_blocks   <- read_sheet(positions_sheet_loc, sheet = "text_blocks") %>% as_tibble()
contact_info  <- read_sheet(positions_sheet_loc, sheet = "contact_info", skip = 1) %>% as_tibble()

