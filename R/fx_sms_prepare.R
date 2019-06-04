# SMS Analysis - Prepare Data ------------------------------------------


# Define Function ---------------------------------------------------------

fx_sms_prepare <- function(data_master) {


  # Configure Environment ---------------------------------------------------

  pacman::p_load(tidyverse, lubridate, rlang, scales, tidytext)

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


  # Period Adjustment -------------------------------------------------------

  export_sms_period_adjustment <-
    export_sms_contact_day %>%
    ungroup() %>%
    mutate(Period = ifelse(Day >= max(Day) - days(90), "new", "historical")) %>%
    group_by(Contact, Period) %>%
    summarise(day_min = min(Day),
              day_max = max(Day),
              day_all = difftime(day_max, day_min, units = "days") %>% parse_number() %>% ifelse(. == 0, 1, .),
              day_contact = length(Day),
              day_proportion = day_contact / day_all,
              length_sum = sum(Length_Sum),
              day_length = length_sum / day_contact) %>%
    select(Period, Contact, day_proportion, day_length) %>%
    gather(measure, value, day_proportion:day_length) %>%
    unite(Label, Period, measure) %>%
    spread(Label, value, fill = 0) %>%
    mutate(change_length = new_day_length / historical_day_length,
           change_frequency = new_day_proportion / historical_day_proportion)


  # Initiate Text -----------------------------------------------------------

  export_sms_initial_hour <-
    data_sms_period %>%
    arrange(DateTime) %>%
    group_by(Contact, Day) %>%
    slice(1)


  # Sent / Rec Difference ---------------------------------------------------

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


  # Tidy Text ---------------------------------------------------------------

  export_sms_tidytext <-
    data_master %>%
    filter(Contact %in% export_sms_rank$Contact) %>%
    mutate(Message = if_else(str_detect(Message, "f27bd7bb"), "reddit", Message)) %>%
    mutate(Message = if_else(str_detect(Message, "open.spotify.com"), "spotify", Message)) %>%
    mutate(Message = if_else(str_detect(Message, "youtu"), "youtube", Message)) %>%
    arrange(DateTime) %>%
    rowid_to_column("Message_Number") %>%
    group_by(Contact, MessageType) %>%
    unnest_tokens(word, Message) %>%
    ungroup()


  # Export Data -------------------------------------------------------------

  ls(pattern = "export_") %>%
    mget(inherits = TRUE) %>%
    enframe() %>%
    mutate(name = str_remove(name, "export_"))

}



# Test --------------------------------------------------------------------

# data_master <- readRDS("C:/Users/exp01754/OneDrive/Data/sms_analysis/data/2018-10-15_master.rds")
# test_prep <- fx_sms_prepare(data_master)

