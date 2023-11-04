library(httr2)
library(tidyverse)
library(sf)
library(lutz)
library(hms)

req <- request("https://api2.sota.org.uk/api/")

# get 1000 or so of the most recent spots
# https://api2.sota.org.uk/docs/index.html
# "The parameter limit is an integer which when unsigned 
# defines the number of spots to retrieve (max 200)
#  and when signed, the number of hours' worth of 
# Spots (max 72 hours)"
resp_spot <- req %>% 
  req_url_path_append("spots") %>% 
  req_url_path_append("-72") %>% 
  # Add query parameters 
  req_url_query(
    `filter` = "all") %>% 
  req_perform() %>% 
  resp_body_json() 

file_name_with_date_time <- 
  paste0("sota-spots-", Sys.time(), ".rds")

saveRDS(resp_spot,
        paste0("data/", file_name_with_date_time))
