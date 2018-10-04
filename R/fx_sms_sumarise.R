
# Function - SMS Summarise ------------------------------------------------

fx_sms_summarise <- function(raw_sms) {

  library(tidyverse)
  library(scales)

  raw_sms %>%
    ungroup() %>%
    summarise(Contacts = n_distinct(Contact),
              Messages = length(Contact),
              Length = sum(MessageLength),
              MinDate = min(DateTime) %>% as_date(),
              MaxDate = max(DateTime) %>% as_date()) %>%
  mutate_at(vars(Contacts, Messages, Length), comma)

}

