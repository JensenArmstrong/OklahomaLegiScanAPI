library(tidyverse)
library(dplyr)
library(stringr)
library(DT)
library(data.table)
library(httr)
library(jsonlite)
library(rlist)
library(plyr)
library(tidytext)

#API SETUP
##Key: 20e254de011ae47a7db702e48e8b86e4
okleg <- httr::GET("https://api.legiscan.com/?key=20e254de011ae47a7db702e48e8b86e4&op=getMasterList&state=OK")
okleg <- httr::content(okleg)
okleg <- enframe(unlist(okleg))

#STRUCTURE DF
##Identify number of columns
rgx_split <- "\\."
n_cols_max <-
  okleg %>%
  pull(name) %>% 
  str_split(rgx_split) %>% 
  map_dbl(~length(.)) %>% 
  max()
nms_sep <- paste0("name", 1:n_cols_max)
##Split columns
okleg <- okleg %>% 
  separate(name, into = nms_sep, sep = rgx_split, fill = "right") 
##Rename columns
names(okleg) [1] <- "APIget"
names(okleg) [2] <- "id"
names(okleg) [3] <- "headers"
names(okleg) [4] <- "values"
##Pivot DF
okleg <- okleg[(-1),] #delete row 1 with pesky NAs
okleg <- as.data.table(okleg, na.rm = FALSE) #convert to data.table
okleg <- spread(okleg, headers, values) #long to wide format
##Rename NA values
okleg$session_id <- as.character(okleg$session_id)
okleg$session_id[is.na(okleg$session_id)] <- "1753"
okleg$session_name <- as.character(okleg$session_name)
okleg$session_name[is.na(okleg$session_name)] <- "2021 Regular Session"

#TRACK BILL PROGRESS
##Back up MasterList status periodically per LegiScan's recommendation
billTrack <- okleg %>% 
  select(APIget, number, status, last_action_date, change_hash)

#ASSESS BILLS
##Sample of 2021 Regular Session bill subjects
justiceBills <- okleg %>% 
  filter(str_detect(description, "Criminal|crime|offender|sentencing|sentence|
                                  violent|court|parole|probation|violation|
                                  abuse|assault|justice|criminal"))
electionBills <- okleg %>% 
  filter(str_detect(description, "Election|election|Elections|elections|
                                  electors|electoral"))
medicaidBills <- okleg %>% 
  filter(str_detect(description, "Medicaid|SoonerCare"))

#TEXT PROCESSING
billText <- paste(okleg$description, collapse = " ")
billText <- str_replace_all(okleg$description, pattern = '\"', replacement = "") # Remove slashes
billText <- str_replace_all(okleg$description, pattern = '\n', replacement = "") # Remove \n
billText <- str_replace_all(okleg$description, pattern = '\u0092', replacement = "'") #Replace with quote
billText <- str_replace_all(okleg$description, pattern = '\u0091', replacement = "'") #Replace with quote
billText <- data_frame(Text = billText) # tibble aka neater data frame
billWords <- billText %>% 
  unnest_tokens(output = word, input = Text) 
##Remove articles
billWords <- billWords %>%
  anti_join(stop_words)
##Count
billWordCount <- count(billWords) #sort descending

#