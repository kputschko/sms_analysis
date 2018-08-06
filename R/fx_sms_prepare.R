# SMS Analysis - Prepare Data ------------------------------------------

# Define Function ---------------------------------------------------------

fx_sms_prepare <- function(master_date, use_anon = TRUE) {

  # Configure Environment ---------------------------------------------------

  pacman::p_load(tidyverse, lubridate, rlang)


  # Import Data -------------------------------------------------------------

  path_import <-
    case_when(
      is_true(use_anon) ~ str_glue("data/{master_date}_master_anon.rds") %>% as_character(),
      is_false(use_anon) ~ str_glue("data/{master_date}_master.rds")%>% as_character())


  data_sms_master <- read_rds(path_import)



  # Contact Summary ---------------------------------------------------------

  export_data_sms_summary <-
    data_sms_master %>%
    group_by(Contact, MessageType) %>%
    summarise(Message_Count = n(),
              Length_Sum = sum(MessageLength),
              Length_Avg = mean(MessageLength),
              Contact_First = min(DateTime) %>% date(),
              Contact_Last = max(DateTime) %>% date(),
              Contact_Days = n_distinct(date(DateTime))) %>%
    mutate(Messages_per_Day = Message_Count / Contact_Days)



  # Contact Rank ------------------------------------------------------------

  export_data_sms_rank <-
    export_data_sms_summary %>%
    group_by(Contact) %>%
    mutate_at(vars(Length_Avg, Messages_per_Day), mean) %>%
    mutate_at(vars(Message_Count, Length_Sum), sum) %>%
    mutate_at(vars(Contact_Days), max) %>%
    select(-MessageType, -Contact_First, -Contact_Last) %>%
    distinct() %>%
    ungroup() %>%
    mutate(Rank_Text_Count = dense_rank(desc(Message_Count)),
           Rank_Length_Sum = dense_rank(desc(Length_Sum)),
           Rank_Length_Avg = dense_rank(desc(Length_Avg)),
           Rank_Contact_Days = dense_rank(desc(Contact_Days)),
           Rank_Messages_per_Day = dense_rank(desc(Messages_per_Day)))



  # SMS Summary Function ----------------------------------------------------

  fx_sms_summary <-
    function(data, length = "MessageLength", contact = "Contact") {

      library(rlang)

      summarise(.data = data,
                Contact_Count = n_distinct(!! sym(contact)),
                Message_Count = n(),
                Length_Sum = sum(!!sym(length)),
                Length_Avg = mean(!!sym(length)),
                Length_Std = sd(!!sym(length)))

    }



  # Period Summary ----------------------------------------------------------

  data_sms_period <-
    data_sms_master %>%
    mutate(Hour = hour(DateTime),
           Day = date(DateTime),
           Weekday = wday(DateTime, label = TRUE, week_start = 1),
           Month = floor_date(DateTime, unit = "month") %>% date(),
           Year = floor_date(DateTime, unit = "year") %>% year())


  export_data_period_day  <- data_sms_period %>% group_by(Day, MessageType) %>% fx_sms_summary()
  export_data_period_contact_day  <- data_sms_period %>% group_by(Contact, Day, MessageType) %>% fx_sms_summary()


  # Are these needed?
  # export_data_period_hour <- data_sms_period %>% group_by(Hour) %>% fx_sms_summary()
  # export_data_period_wday <- data_sms_period %>% group_by(Weekday, Hour, MessageType) %>% fx_sms_summary()
  # export_data_period_month <- data_sms_period %>% group_by(Month, MessageType) %>% fx_sms_summary()
  # export_data_period_year <- data_sms_period %>% group_by(Year, MessageType) %>% fx_sms_summary()
  # export_data_period_contact_hour <- data_sms_period %>% group_by(Contact, Hour) %>% fx_sms_summary()
  # export_data_period_contact_wday <- data_sms_period %>% group_by(Contact, Weekday, Hour, MessageType) %>% fx_sms_summary()
  # export_data_period_contact_month <- data_sms_period %>% group_by(Contact, Month, MessageType) %>% fx_sms_summary()
  # export_data_period_contact_year <- data_sms_period %>% group_by(Contact, Year, MessageType) %>% fx_sms_summary()



  # Message Type ------------------------------------------------------------

  export_data_sms_type <-
    bind_rows(
      data_sms_master %>% group_by(MessageType) %>% fx_sms_summary(),
      data_sms_master %>% fx_sms_summary() %>% mutate(MessageType = "All"))



  # Contact Nest ----------------------------------------------------------
  # Is this necessary?

  # data_sms_nest <-
  #   data_sms_master %>%
  #   group_by(Contact) %>%
  #   nest(.key = messages) %>%
  #   mutate(daily = map(messages,
  #                      ~ .x %>%
  #                        mutate(DateTime = date(DateTime)) %>%
  #                        group_by(DateTime, MessageType) %>%
  #                        add_count() %>%
  #                        summarise(Message_Count = unique(n),
  #                                  Length_Sum = sum(MessageLength),
  #                                  Length_Avg = mean(MessageLength))),
  #          monthly = map(messages,
  #                        ~ .x %>%
  #                          mutate(DateTime = floor_date(DateTime, unit = "month") %>% date()) %>%
  #                          group_by(DateTime, MessageType) %>%
  #                          add_count() %>%
  #                          summarise(Message_Count = unique(n),
  #                                    Length_Sum = sum(MessageLength),
  #                                    Length_Avg = mean(MessageLength))),
  #          yearly = map(messages,
  #                       ~ .x %>%
  #                         mutate(DateTime = floor_date(DateTime, unit = "year") %>% year()) %>%
  #                         group_by(DateTime, MessageType) %>%
  #                         add_count() %>%
  #                         summarise(Message_Count = unique(n),
  #                                   Length_Sum = sum(MessageLength),
  #                                   Length_Avg = mean(MessageLength)))
  #   )




  # Initiate Text -----------------------------------------------------------

  export_data_sms_initial_hour <-
    data_sms_period %>%
    arrange(DateTime) %>%
    group_by(Contact, Day) %>%
    slice(1) %>%
    group_by(Contact, MessageType, Hour) %>%
    summarise(Message_Count = n(),
              Total_Length = sum(MessageLength))



  # export_data_sms_first_message <-
  #   data_sms_period %>%
  #   group_by(Contact, Day) %>%
  #   summarise(First_Message = first(MessageType, order_by = DateTime)) %>%
  #   count(Contact, First_Message) %>%
  #   ungroup() %>%
  #   complete(Contact, First_Message, fill = lst(n = 0)) %>%
  #   group_by(Contact) %>%
  #   mutate(Proportion = n / sum(n)) %>%
  #   rename(Count = n)



  # Sent / Rec Difference ---------------------------------------------------

  data_sms_dif <-
    export_data_period_contact_day %>%
    ungroup() %>%
    filter(!str_detect(Contact, ","))

  export_data_sms_dif_length <-
    data_sms_dif %>%
    select(Contact, Day, MessageType, Length_Sum) %>%
    spread(MessageType, Length_Sum) %>%
    replace_na(replace = list(Received = 0, Sent = 0)) %>%
    mutate(Length_Difference = Received - Sent) %>%
    group_by(Contact) %>%
    summarise_at(.vars = vars(Length_Difference),
                 .funs = c(days = "length", "median", "mean", "sd", "min", "max"))


  # export_data_sms_dif_count <-
  #   data_sms_dif %>%
  #   select(Contact, Day, MessageType, Message_Count) %>%
  #   spread(MessageType, Message_Count) %>%
  #   replace_na(replace = list(Received = 0, Sent = 0)) %>%
  #   mutate(Count_Difference = Received - Sent) %>%
  #   group_by(Contact) %>%
  #   summarise_at(.vars = vars(Count_Difference),
  #                .funs = c(days = "length", "median", "mean", "sd", "min", "max"))




  # Export Data -------------------------------------------------------------

  .export <-
    ls(pattern = "export_") %>%
    mget(inherits = TRUE) %>%
    enframe() %>%
    mutate(name = str_remove(name, "export_"))


  write_rds(.export,
            str_glue("data/{master_date}_summary.rds"),
            compress = "bz")


  # End Function ------------------------------------------------------------

  inform(str_glue("SMS summary data output at `data/{master_date}_summary.rds`"))

}

