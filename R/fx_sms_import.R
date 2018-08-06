# SMS Analysis - Raw Data Prep --------------------------------------------

# HEY FUTURE KP, THE DATA QUALITY CHECK SECTION IS A MESS
# IT MOSTLY WORKS, BUT MAN IS IT NUTS


# Define Function ---------------------------------------------------------

fx_sms_import <- function(xml_path, prior_master_date) {


  # Environment -------------------------------------------------------------

  pacman::p_load(dplyr, tidyr, tibble, tidyr, purrr, rlang, readr, lubridate,
                 anytime, foreach, xml2, magrittr)

  # pacman::p_load(tidyverse, anytime, lubridate, foreach, xml2, magrittr)



  # Import Data -------------------------------------------------------------

  data_sms_old <- read_rds(str_c("data/", prior_master_date, "_master.rds"))
  data_xml_raw <- read_xml(xml_path, options = "HUGE")



  # Parse XML ---------------------------------------------------------------
  # MMS needs to be parsed in parts because it has many children for each parent node

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


  data_sms_new_clean_group <-
    data_group_fix %>%
    right_join(data_sms_new_clean, by = "Contact") %>%
    arrange(desc(DateTime)) %>%
    mutate(Contact = if_else(is.na(Group), Contact, Group)) %>%
    select(-rowid, -Group) %>%
    distinct()




  # Master Data -------------------------------------------------------------
  # Use fuzzy matching on datetime hour and message, by removing punctuation
  # Then compare messages for those that are similar
  # Choose to keep message that has proper punctuation

  data_sms_master <-
    bind_rows(data_sms_old, data_sms_new_clean_group) %>%
    arrange(desc(DateTime)) %>%
    ungroup() %>%
    distinct(Contact, DateTime, MessageType, Message)



  # Data Quality Check ------------------------------------------------------

  possible_dupes <-
    data_sms_master %>%
    count(Contact, DateTime, MessageType) %>%
    filter(n > 1) %>%
    pull(DateTime)



  create_fuzzy_matches <-
    data_sms_master %>%
    arrange(desc(DateTime)) %>%
    mutate(MessageLength = str_length(Message),
           Adjusted_Length = str_remove_all(Message, "[^[:alnum:][:blank:]?&/\\-]") %>% str_length(),
           Fuzz_Date = floor_date(DateTime, unit = "hour"),
           Fuzz_Message = str_remove_all(Message, "[^A-Za-z0-9]"),
           Fuzz_Msg_Len = str_length(Fuzz_Message))



  # create_fuzzy_matches %>% filter(DateTime %in% check_list) %>% View()


  find_dupes <-
    create_fuzzy_matches %>%
    group_by(Contact, MessageType, Fuzz_Date, Fuzz_Message, Fuzz_Msg_Len) %>%
    add_count()


  # find_dupes %>% filter(DateTime %in% check_list) %>% View()


  test_fuzzy_matches <-
    find_dupes %>%
    mutate(Selection = if_else(Adjusted_Length == max(Adjusted_Length) &
                                 MessageLength == max(MessageLength), 1, 0)) %>%
    ungroup()


  # test_fuzzy_matches %>% filter(DateTime %in% check_list) %>% View()
  # test_fuzzy_matches %>% ungroup() %>% filter(DateTime %in% check_list) %>% count(Fuzz_Message) %>% filter(nn > 1)
  # test_fuzzy_matches %>% filter(DateTime %in% check_list) %>% pull(Fuzz_Message) %>% n_distinct()
  # test_fuzzy_matches %>% filter(DateTime == as_datetime("2017-06-28 17:55:58")) %>% View()



  remove_dupes <-
    test_fuzzy_matches %>%
    filter(Selection == 1)



  # remove_dupes %>%
  # ungroup() %>%
  # filter(DateTime == as_datetime("2017-07-23 18:16:00")) %>%
  # filter(DateTime == as_datetime("2017-06-28 17:55:58")) %>%
  # filter(DateTime == as_datetime("2017-10-13 12:32:50")) %>%
  # View()


  # remove_dupes %>%
  #   ungroup() %>%
  #   filter(DateTime == as_datetime("2017-07-23 18:16:00")) %>%
  #   pull(Message) %>%
  #   str_remove_all("[^[:alnum:][:blank:]?&/\\-]")

  # mutate(weird_thing = if_else(str_detect(Message, "\\<U[^\\>]*\\>"), 1, 0)) %>% View()


  # remove_dupes %>% filter(DateTime %in% check_list) %>% View()


  data_sms_master_clean <-
    remove_dupes %>%
    distinct(Contact, DateTime, MessageType, Message, MessageLength)


  # check_2 <- data_sms_master_clean %>% count(Contact, DateTime, MessageType) %>% filter(n > 1) %>% pull(DateTime)

  # data_sms_master_clean %>%
  #   filter(DateTime %in% check_2) %>%
  #   View()

  # step_2 <-
  #   step_1 %>%
  #   group_by(Contact, Fuzz_Date, MessageType, Fuzz_Message) %>%
  #   mutate(Possible_Dupe_1 = n()) %>%
  #   group_by(Contact, DateTime, MessageType) %>%
  #   mutate(Possible_Dupe_2 = n()) %>%
  #   ungroup() %>%
  #   mutate(Possible_Dupe_F = Possible_Dupe_1 + Possible_Dupe_2)
  #
  #
  #
  # step_3 <-
  #   step_2 %>%
  #   group_by(Contact, DateTime, MessageType, Possible_Dupe_F) %>%
  #   mutate(Selected_Message = if_else(MessageLength == max(MessageLength), 1, 0))
  #
  #
  # step_3 %>% filter(DateTime %in% check_list) %>% View()
  #
  #
  # step_4 <-
  #   step_3 %>%
  #   ungroup() %>%
  #   filter(Selected_Message > 0)
  #
  #
  # step_4 %>% filter(DateTime %in% check_list) %>% View()
  #
  #
  # step_5 <-
  #   step_4 %>%
  #   select(Contact, DateTime, MessageType, Message, MessageLength)




  # data_sms_master_clean <-
  #   data_sms_master %>%
  #
  #   arrange(desc(DateTime)) %>%
  #   mutate(MessageLength = str_length(Message),
  #          Fuzz_Date = floor_date(DateTime, unit = "hour"),
  #          Fuzz_Message = str_remove_all(Message, "[[:punct:]]") %>% str_squish() %>% str_trim(),
  #          Fuzz_Msg_Len = str_length(Fuzz_Message)) %>%
  #
  #   group_by(Contact, Fuzz_Date, MessageType, Fuzz_Message) %>%
  #   mutate(Possible_Dupe_1 = n()) %>%
  #
  #   group_by(Contact, DateTime, MessageType) %>%
  #   mutate(Possible_Dupe_2 = n()) %>%
  #
  #   ungroup() %>%
  #   mutate(Possible_Dupe_F = Possible_Dupe_1 + Possible_Dupe_2) %>%
  #
  #   group_by(Contact, DateTime, MessageType, Possible_Dupe_F) %>%
  #   mutate(Selected_Message = if_else(MessageLength == max(MessageLength), 1, 0)) %>%
  #
  #   filter(Selected_Message > 0) %>%
  #
  #   ungroup() %>%
  #   select(Contact, DateTime, MessageType, Message, MessageLength)



  # Removing Identification -------------------------------------------------

  Anon <- read_csv("data/anon_id.csv")

  data_anon_id <-
    data_sms_master_clean %>%
    arrange(DateTime) %>%
    distinct(Contact) %>%
    rowid_to_column("ID") %>%
    left_join(Anon)

  data_sms_anon <-
    left_join(data_sms_master_clean, data_anon_id) %>%
    select(Contact = Landform, DateTime, MessageType, Message, MessageLength)



  # Export Data -------------------------------------------------------------
  # Grab the date from the xml file
  # bz rds compression is pretty fast

  export_filename <-
    xml_path %>%
    word(-1, sep = "/") %>%
    word(1) %>%
    str_remove("sms-")


  write_rds(data_sms_new, str_glue("data/{export_filename}_new.rds"), compress = "bz")
  write_rds(data_sms_anon, str_glue("data/{export_filename}_master_anon.rds"), compress = "bz")
  write_rds(data_sms_master_clean, str_glue("data/{export_filename}_master.rds"), compress = "bz")


  # End Function ------------------------------------------------------------

  inform(str_glue("SMS master data output at `data/{export_filename}_master.rds`"))



}
