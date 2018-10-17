
# SMS - Read XML ----------------------------------------------------------

fx_sms_read_xml <- function(xml_path) {

  library(tidyverse)
  library(anytime)
  library(lubridate)
  library(foreach)
  library(xml2)
  library(rlang)


  # Import XML --------------------------------------------------------------

  inform("Read XML")
  data_xml_raw <- read_xml(xml_path, options = "HUGE")


  # Parse XML ---------------------------------------------------------------
  # MMS needs to be parsed in parts because it has many children for each parent node

  inform("Parse SMS/MMS")
  xml_sms <- data_xml_raw %>% xml_find_all("//sms")
  xml_mms <- data_xml_raw %>% xml_find_all("//mms//parts")


  data_sms <-
    tibble(Contact = xml_sms %>% xml_attr("contact_name"),
           DateTime = xml_sms %>% xml_attr("readable_date"),
           MessageType = xml_sms %>% xml_attr("type"),
           Message = xml_sms %>% xml_attr("body"))



  data_mms <-
    foreach(i = seq_along(xml_mms), .combine = "rbind") %do% {

      child <- xml_mms[[i]] %>% xml_children()
      parent <- xml_mms[[i]] %>% xml_parent()

      Contact <- parent %>% xml_attr(attr = "contact_name")
      DateTime <- parent %>% xml_attr(attr = "readable_date")
      MessageType <- parent %>% xml_attr(attr = "msg_box")

      tibble(Contact,
             DateTime,
             MessageType,
             Message = child %>% xml_attr("text"),
             CT = child %>% xml_attr("ct")) %>%
        filter(CT == "text/plain") %>%
        select(-CT)

    }



  data_sms_new <-
    bind_rows(data_sms, data_mms) %>%
    mutate(DateTime = parse_datetime(DateTime, "%Y/%d/%m %H:%M:%S"),
           MessageType = if_else(MessageType == 1, "Received", "Sent"))



  # Clean New Data ----------------------------------------------------------
  # Master sms_old should be standardized, and shouldn't need additional prep
  # Groups are long, so we just use first names and string together with commas

  inform("Clean XML")
  data_sms_new_clean <-
    data_sms_new %>%
    distinct() %>%
    filter(!is.na(DateTime),
           !Contact %in% c("(Unknown)", "KP", "Resist Bot")) %>%
    mutate(Message =
             Message %>%
             str_trim() %>%
             str_squish() %>%
             str_replace_all("\n", " ") %>%
             str_remove_all('\uFFFD|\u200D|\r')) %>%
    replace_na(lst(Message = ".")) %>%
    arrange(desc(DateTime))


  data_group_key <-
    data_sms_new_clean %>%
    filter(str_detect(Contact, ",")) %>%
    distinct(Contact) %>%
    arrange(Contact) %>%
    rowid_to_column()


  data_group_fix <-
    data_group_key %>%
    separate_rows(Contact, sep = ", ") %>%
    mutate(Contact = word(Contact, 1)) %>%
    group_by(rowid) %>%
    summarise(Group = str_flatten(Contact, collapse = ", ")) %>%
    left_join(data_group_key, by = "rowid")


  data_group_fix %>%
    right_join(data_sms_new_clean, by = "Contact") %>%
    arrange(desc(DateTime)) %>%
    mutate(Contact = if_else(is.na(Group), Contact, Group),
           MessageLength = str_length(Message)) %>%
    select(-rowid, -Group) %>%
    distinct()

  inform("End Import XML")


}
