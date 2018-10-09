# SMS Analysis - Prepare Data ------------------------------------------


# Define Function ---------------------------------------------------------

fx_sms_prepare <- function(data_master) {


  # Configure Environment ---------------------------------------------------

  pacman::p_load(tidyverse, lubridate, rlang, scales)

  .fx_sms_summary <-
    function(data, length = "MessageLength", contact = "Contact") {
      library(rlang)

      summarise(.data = data,
                Contact_Count = n_distinct(!! sym(contact)),
                Message_Count = n(),
                Length_Sum = sum(!!sym(length)),
                Length_Avg = mean(!!sym(length)),
                Length_Std = sd(!!sym(length)))

    }


  # Overview ----------------------------------------------------------------
  # Is this needed?

  # export_sms_overview <-
  #   bind_rows(data_master %>% group_by(MessageType) %>% .fx_sms_summary(),
  #             data_master %>% .fx_sms_summary() %>% mutate(MessageType = "All")) %>%
  #   mutate_at(vars(Message_Count, Length_Sum), comma)


  # Messages by Day ---------------------------------------------------------

  data_sms_period <-
    data_master %>%
    mutate(Hour = hour(DateTime),
           Day = date(DateTime),
           Weekday = wday(DateTime, label = TRUE, week_start = 1),
           Week = floor_date(DateTime, unit = "week"),
           Month = floor_date(DateTime, unit = "month") %>% date(),
           Year = floor_date(DateTime, unit = "year") %>% year())

  export_sms_day <-
    data_sms_period %>%
    group_by(Day) %>%
    .fx_sms_summary()

  export_sms_day_type <-
    data_sms_period %>%
    group_by(Day, MessageType) %>%
    .fx_sms_summary()

  export_sms_week_contact <-
    data_sms_period %>%
    group_by(Week, Contact) %>%
    summarise_at("MessageLength", funs("Minimum" = min, "Maximum" = max, "Median" = median, "Count" = length))

  # Contact Summary ---------------------------------------------------------

  export_sms_contact <-
    data_master %>%
    group_by(Contact) %>%
    summarise(Message_Count = n(),
              Length_Sum = sum(MessageLength),
              Length_Avg = mean(MessageLength),
              Contact_First = min(DateTime) %>% date(),
              Contact_Last = max(DateTime) %>% date(),
              Contact_Days = n_distinct(date(DateTime))) %>%
    mutate(Messages_per_Day = Message_Count / Contact_Days)

  export_sms_contact_day <-
    data_sms_period %>%
    group_by(Contact, Day) %>%
    summarise(Message_Count = n(),
              Length_Sum = sum(MessageLength),
              Length_Avg = mean(MessageLength))


  export_sms_contact_type <-
    data_master %>%
    group_by(Contact, MessageType) %>%
    summarise(Message_Count = n(),
              Length_Sum = sum(MessageLength),
              Length_Avg = mean(MessageLength),
              Contact_First = min(DateTime) %>% date(),
              Contact_Last = max(DateTime) %>% date(),
              Contact_Days = n_distinct(date(DateTime))) %>%
    mutate(Messages_per_Day = Message_Count / Contact_Days)



  # Contact Rank ------------------------------------------------------------

  export_sms_rank <-
    export_sms_contact %>%
    ungroup() %>%
    mutate(.Rank_Message_Count = dense_rank(desc(Message_Count)),
           .Rank_Length_Sum = dense_rank(desc(Length_Sum)),
           .Rank_Contact_Days = dense_rank(desc(Contact_Days)),
           .Rank_Messages_per_Day = dense_rank(desc(Messages_per_Day)),
           Rank_Score = .Rank_Message_Count + .Rank_Length_Sum + .Rank_Contact_Days + .Rank_Messages_per_Day) %>%
    select(-contains(".Rank"))


  # Period Summary ----------------------------------------------------------

  # data_sms_period <-
  #   data_master %>%
  #   mutate(Hour = hour(DateTime),
  #          Day = date(DateTime),
  #          Weekday = wday(DateTime, label = TRUE, week_start = 1),
  #          Week = floor_date(DateTime, unit = "week"),
  #          Month = floor_date(DateTime, unit = "month") %>% date(),
  #          Year = floor_date(DateTime, unit = "year") %>% year())


  # export_data_period_day  <- data_sms_period %>% group_by(Day, MessageType) %>% .fx_sms_summary()
  # export_data_period_contact_day  <- data_sms_period %>% group_by(Contact, Day, MessageType) %>% .fx_sms_summary()

  # export_data_period_week <- data_sms_period %>% group_by(Week, MessageType) %>% .fx_sms_summary()
  # export_data_period_contact_week <- data_sms_period %>% group_by(Contact, Week, MessageType) %>% .fx_sms_summary()

  # Are these needed?
  # export_data_period_hour <- data_sms_period %>% group_by(Hour) %>% .fx_sms_summary()
  # export_data_period_wday <- data_sms_period %>% group_by(Weekday, Hour, MessageType) %>% .fx_sms_summary()
  # export_data_period_month <- data_sms_period %>% group_by(Month, MessageType) %>% .fx_sms_summary()
  # export_data_period_year <- data_sms_period %>% group_by(Year, MessageType) %>% .fx_sms_summary()
  # export_data_period_contact_hour <- data_sms_period %>% group_by(Contact, Hour) %>% .fx_sms_summary()
  # export_data_period_contact_wday <- data_sms_period %>% group_by(Contact, Weekday, Hour, MessageType) %>% .fx_sms_summary()
  # export_data_period_contact_month <- data_sms_period %>% group_by(Contact, Month, MessageType) %>% .fx_sms_summary()
  # export_data_period_contact_year <- data_sms_period %>% group_by(Contact, Year, MessageType) %>% .fx_sms_summary()



  # Message Type ------------------------------------------------------------




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

  export_sms_initial_hour <-
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
  # Remove all group messages

  export_sms_diff <-
    data_sms_period %>%
    filter(!str_detect(Contact, ",")) %>%
    mutate(Length_Adj = if_else(MessageType == "Sent", -MessageLength, MessageLength)) %>%
    group_by(Contact, Day) %>%
    summarise(Length_Difference = sum(Length_Adj)) %>%
    group_by(Contact) %>%
    summarise(quantiles = list(quantile(Length_Difference) %>% enframe() %>% spread(name, value)),
              `Days of Contact` = length(Day)) %>%
    unnest(quantiles) %>%
    rename(Min = "0%", Max = "100%", Q1 = "25%", Median = "50%", Q3 = "75%") %>%
    arrange(Median) %>%
    mutate(Contact = as_factor(Contact),
           `Longer Messages` = if_else(Median > 0, "Mine", "Theirs"))


  # export_data_sms_dif_length <-
  #   data_sms_dif %>%
  #   select(Contact, Day, MessageType, Length_Sum) %>%
  #   spread(MessageType, Length_Sum) %>%
  #   replace_na(replace = list(Received = 0, Sent = 0)) %>%
  #   mutate(Length_Difference = Received - Sent) %>%
  #   group_by(Contact) %>%
  #   summarise_at(.vars = vars(Length_Difference),
  #                .funs = c(days = "length", "median", "mean", "sd", "min", "max"))


  # export_data_sms_dif_length <-
  #   data_sms_diff %>%
  #
  #
  #
  #   select(Contact, Day, MessageType, Length_Sum) %>%
  #   spread(MessageType, Length_Sum) %>%
  #   replace_na(replace = list(Received = 0, Sent = 0)) %>%
  #   mutate(Length_Difference = Received - Sent) %>%
  #   group_by(Contact) %>%
  #   summarise(quantiles = list(quantile(Length_Difference) %>% enframe() %>% spread(name, value)),
  #             `Days of Contact` = n()) %>%
  #   unnest(quantiles) %>%
  #   rename(Min = "0%", Max = "100%", Q1 = "25%", Median = "50%", Q3 = "75%")



  # Export Data -------------------------------------------------------------

    ls(pattern = "export_") %>%
    mget(inherits = TRUE) %>%
    enframe() %>%
    mutate(name = str_remove(name, "export_"))


  # write_rds(.export,
  #           str_glue("data/{master_date}_summary.rds"),
  #           compress = "bz")


  # End Function ------------------------------------------------------------

  # inform(str_glue("SMS summary data output at `data/{master_date}_summary.rds`"))

}

