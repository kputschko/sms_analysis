

# SMS Analysis - Visualize Data -------------------------------------------
# Check the function for ideas on how to adjust for selected contact in Shiny

# Define Function ---------------------------------------------------------

fx_sms_visualise <- function(master_date) {

  # Configure Environment ---------------------------------------------------

  pacman::p_load(tidyverse, scales)
  source('fx_utility_functions.R')

  .plot_colors <- list(me = "#ef8a62", them = "#67a9cf")

  .plot_ggtheme <-
    theme_minimal() +
    theme(strip.text.y = element_text(angle = 180, face = "bold"),
          panel.background = element_rect(color = "gray"),
          panel.grid.major.x = element_line(color = "gray", linetype = 3),
          panel.grid.minor.x = element_line(color = "gray", linetype = 3),
          legend.title.align = 0.5)



  # Import Data -------------------------------------------------------------

  data_sms_summaries <-
    str_glue("data/{master_date}_summary.rds") %>%
    read_rds() %>%
    deframe()



  # Top Contacts ------------------------------------------------------------

  export_list_top_contacts <-
    data_sms_summaries %>%
    pluck("data_sms_rank") %>%
    top_n(n = 25, wt = -Rank_Contact_Days) %>%
    pull(Contact)



  # Visuals -----------------------------------------------------------------


  # Master ------------------------------------------------------------------


  # // Length Difference -------------------------------------------------------

  plot_data_dif_length <-
    data_sms_summaries %>%
    pluck("data_sms_dif_length") %>%
    filter(Contact %in% export_list_top_contacts) %>%
    arrange(mean) %>%
    mutate(Contact = Contact %>% as_factor(),
           color = if_else(mean > 0, "Me", "Them"))


  export_master_plot_dif_length <-
    plot_data_dif_length %>%
    ggplot(aes(x = Contact)) +
    geom_linerange(aes(ymin = mean - 0.5 * sd,
                       ymax = mean + 0.5 * sd,
                       color = color)) +
    geom_point(aes(y = mean,
                   size = days,
                   fill = color),
               color = "black",
               shape = 21) +
    geom_hline(aes(yintercept = 0)) +
    labs(x = NULL,
         y = "Mean Difference in Message Length",
         fill = "Whose Messages\nAre Longer?",
         color = NULL,
         size = "Days of Contact") +
    guides(color = FALSE) +
    coord_flip() +
    scale_fill_manual(values = c(.plot_colors$me, .plot_colors$them)) +
    scale_color_manual(values = c(.plot_colors$me, .plot_colors$them)) +
    ggtitle("Mean Difference in Message Length") +
    .plot_ggtheme



  # // Contact Timeline --------------------------------------------------------

  plot_data_timeline <-
    data_sms_summaries %>%
    pluck("data_period_contact_day") %>%
    group_by(Contact, Day) %>%
    summarise(Total_Length = sum(Length_Sum)) %>%
    ungroup() %>%
    filter(Contact %in% export_list_top_contacts) %>%
    arrange(Day) %>%
    mutate(Contact = factor(Contact))

  export_master_plot_timeline <-
    plot_data_timeline %>%
    ggplot(aes(x = Day,
               y = Contact,
               alpha = Total_Length)) +
    geom_point(shape = 22,
               fill = "gray",
               color = "black") +
    scale_alpha(breaks = seq(100, 1000, by = 100),
                range = c(0.10, 0.50)) +
    labs(x = NULL,
         y = NULL) +
    guides(alpha = FALSE) +
    ggtitle("Message Length by Day") +
    .plot_ggtheme




  # // Contact Scatter -------------------------------------------------------

  export_master_plot_scatter <-
    data_sms_summaries %>%
    pluck("data_period_contact_day") %>%
    group_by(Contact, Day) %>%
    summarise(Total_Length = sum(Length_Sum)) %>%

    ggplot(aes(x = Day,
               y = Total_Length)) +
    geom_point(alpha = 0.05) +
    labs(x = NULL, y = NULL) +
    ggtitle(label = "Message Length by Day") +
    scale_y_sqrt() +
    .plot_ggtheme


  # === For Interactive Use in Dashboard ===
  # .plot_contact <- NULL
  # .plot_contact <- "Mouse"
  # .plot_contact <- "Bat"
  # .plot_contact <- "Aardvark"
  #
  # .plot_master_contact_point <-
  #   geom_point(data = . %>% filter(Contact == .plot_contact), color = "red", alpha = 0.30)
  #
  # .plot_master_contact_smooth <-
  #   geom_smooth(data = . %>% filter(Contact == .plot_contact), span = 0.45, se = FALSE, color = "black")
  #
  #
  #
  # if (is.null(.plot_contact)) {
  #
  #   export_master_plot_scatter
  #
  # } else {
  #
  #   export_master_plot_scatter +
  #     .plot_master_contact_point +
  #     .plot_master_contact_smooth +
  #     ggtitle(label = "Message Length by Day",
  #             subtitle = str_c("Contact:", .plot_contact, sep = " "))
  #
  #
  # }




  # // Length Ranks ---------------------------------------------------------

  plot_data_top_bars <-
    data_sms_summaries %>%
    pluck("data_sms_summary") %>%
    filter(Contact %in% export_list_top_contacts) %>%
    arrange(Length_Sum) %>%
    mutate(Length_Label =
             Length_Sum %>%
             magrittr::divide_by(1000) %>%
             format() %>%
             str_c("k")) %>%
    ungroup() %>%
    mutate(Contact = Contact %>% factor() %>% fct_inorder())

  export_master_plot_top_bars <-
    plot_data_top_bars %>%
    ggplot(aes(x = Contact, y = Length_Sum)) +
    geom_linerange(aes(ymin = 0,
                       ymax = Length_Sum,
                       color = MessageType),
                   position = position_dodge(width = 0.50),
                   size = 1.5) +
    scale_y_continuous(labels = unit_format("k", scale = 1/1000, sep = "")) +
    scale_color_manual(values = c(.plot_colors$me, .plot_colors$them)) +
    labs(x = NULL, y = NULL, color = "Message Type") +
    ggtitle("Total Message Length") +
    coord_flip() +
    .plot_ggtheme


  export_master_plot_top_bars_prop <-
    plot_data_top_bars %>%
    ggplot(aes(x = Contact, y = Length_Sum, fill = MessageType)) +
    geom_col(color = "black", position = "fill") +
    coord_flip() +
    geom_hline(aes(yintercept = 0.50), linetype = "dashed") +
    labs(x = NULL, y = NULL, fill = "Message Type") +
    ggtitle("Proportion of Message Length") +
    scale_fill_manual(values = c(.plot_colors$me, .plot_colors$them)) +
    .plot_ggtheme



  # Single Contact ----------------------------------------------------------


  fx_sms_plot_initial_msg <- function(data, contact, type) {

    library(tidyverse)

    .plot_ggtheme <-
      theme_minimal() +
      theme(strip.text.y = element_text(angle = 180, face = "bold"),
            panel.background = element_rect(color = "gray"),
            panel.grid.major.x = element_line(color = "gray", linetype = 3),
            panel.grid.minor.x = element_line(color = "gray", linetype = 3),
            legend.title.align = 0.5)

    .plot_title <-
      if_else(type == "Sent",
              str_glue("When do I initate conversations with {contact}?") %>% as.character(),
              str_glue("When does {contact} initate conversations with me?") %>% as.character())

    data %>%
      ggplot(aes(y = Message_Count, x = Hour)) +
      geom_pointrange(aes(ymin = 0, ymax = Message_Count, size = Total_Length, alpha = Total_Length)) +
      labs(x = NULL, y = NULL) +
      guides(alpha = FALSE, size = FALSE) +
      scale_size_area(max_size = 1) +
      scale_alpha(range = c(0.25, 0.75)) +
      scale_x_continuous(breaks = seq(0, 24, by = 6)) +
      ggtitle(label = .plot_title) +
      .plot_ggtheme

  }



  # // Single - Summary -----------------------------------------------------

  single_general_summary <-
    data_sms_summaries %>%
    pluck("data_sms_summary") %>%
    filter(Contact %in% export_list_top_contacts) %>%
    group_by(Contact) %>%
    nest(.key = "General_Summary")




  # // Single - Initial Message ---------------------------------------------


  single_initial_msg_summary <-
    data_sms_summaries %>%
    pluck("data_sms_initial_hour") %>%
    filter(Contact %in% export_list_top_contacts) %>%
    group_by(Contact, MessageType) %>%
    summarise_at(vars(Message_Count, Total_Length), sum) %>%
    mutate(Avg_Length = Total_Length / Message_Count,
           Proportion = Message_Count / sum(Message_Count)) %>%
    group_by(Contact) %>%
    nest(.key = "Initial_Msg_Summary")


  single_initial_msg_hour_summary <-
    data_sms_summaries %>%
    pluck("data_sms_initial_hour") %>%
    filter(Contact %in% export_list_top_contacts) %>%
    group_by(Contact, Hour, MessageType) %>%
    summarise_if(is.integer, sum) %>%
    group_by(Contact, MessageType) %>%
    nest(.key = "Initial_Msg_Hour_Summary") %>%
    mutate(Plot_Initial_Msg = pmap(.l = list(Initial_Msg_Hour_Summary, Contact, MessageType),
                                   .f = fx_sms_plot_initial_msg)) %>%
    select(-Initial_Msg_Hour_Summary) %>%
    spread(MessageType, Plot_Initial_Msg) %>%
    rename(Plot_Initial_Msg_Received = Received,
           Plot_Initial_Msg_Sent = Sent)





  # single_plot_initial_hour_them <-
  #   data_sms_summaries %>%
  #   pluck("data_sms_initial_hour") %>%
  #   filter(MessageType == "Received",
  #          Contact == .plot_contact) %>%
  #   group_by(Hour) %>%
  #   summarise_if(is.integer, sum) %>%
  #
  #   ggplot(aes(y = Message_Count, x = Hour)) +
  #   geom_pointrange(aes(ymin = 0, ymax = Message_Count, size = Total_Length, alpha = Total_Length)) +
  #   labs(x = NULL, y = NULL) +
  #   guides(alpha = FALSE, size = FALSE) +
  #   scale_size_area(max_size = 1) +
  #   scale_alpha(range = c(0.25, 0.75)) +
  #   scale_x_continuous(breaks = seq(0, 24, by = 6)) +
  #   ggtitle(label = str_glue("When does {.plot_contact} initate conversations with me?")) +
  #   .plot_ggtheme
  #
  #
  # single_plot_initial_hour_me <-
  #   data_sms_summaries %>%
  #   pluck("data_sms_initial_hour") %>%
  #   filter(MessageType == "Sent",
  #          Contact == .plot_contact) %>%
  #   group_by(Hour) %>%
  #   summarise_if(is.integer, sum) %>%
  #
  #   ggplot(aes(y = Message_Count, x = Hour)) +
  #   geom_pointrange(aes(ymin = 0, ymax = Message_Count, size = Total_Length, alpha = Total_Length)) +
  #   labs(x = NULL, y = NULL) +
  #   guides(alpha = FALSE, size = FALSE) +
  #   scale_size_area(max_size = 1) +
  #   scale_alpha(range = c(0.25, 0.75)) +
  #   scale_x_continuous(breaks = seq(0, 24, by = 6)) +
  #   ggtitle(label = str_glue("When do I initate conversations with {.plot_contact}?")) +
  #   .plot_ggtheme



  # // Single - Daily ---------------------------------------------------------

  single_plot_daily <-
    data_sms_summaries %>%
    pluck("data_period_contact_day") %>%
    filter(Contact %in% export_list_top_contacts) %>%
    group_by(Contact) %>%
    nest() %>%
    mutate(Plot_Daily = map(data,
                            ~ .x %>%
                              ggplot(aes(x = Day, y = Length_Sum)) +
                              geom_smooth(se = FALSE, span = 0.40, color = "black") +
                              geom_point(aes(size = Length_Avg, fill = MessageType),
                                         shape = 22, color = "black", alpha = 0.20) +
                              scale_y_sqrt() +
                              scale_fill_manual(values = c(.plot_colors$me, .plot_colors$them)) +
                              labs(x = NULL,
                                   y = NULL,
                                   fill = "Message Type",
                                   size = "Average Length") +
                              ggtitle(label = "Daily Message Length") +
                              .plot_ggtheme

    )) %>%
    select(-data)




  # // Export - Contact Summary ---------------------------------------------

  export_contact_nest <-
    ls(pattern = "single_") %>%
    mget(inherits = TRUE) %>%
    reduce(left_join)



  # Me ----------------------------------------------------------------------

  # // Me - Summary ---------------------------------------------------------

  export_me_table_summary <- data_sms_summaries$data_sms_type


  # // Me - Initial Message -------------------------------------------------

  export_me_value_initial_message <-
    data_sms_summaries %>%
    pluck("data_sms_initial_hour") %>%
    group_by(MessageType) %>%
    summarise_at(vars(Message_Count, Total_Length), sum) %>%
    mutate(Avg_Length = Total_Length / Message_Count,
           Proportion = Message_Count / sum(Message_Count))


  export_me_plot_initial_hour <-
    data_sms_summaries %>%
    pluck("data_sms_initial_hour") %>%
    filter(MessageType == "Sent") %>%
    group_by(Hour) %>%
    summarise_if(is.integer, sum) %>%

    ggplot(aes(y = Message_Count, x = Hour)) +
    geom_pointrange(aes(ymin = 0, ymax = Message_Count, size = Total_Length, alpha = Total_Length)) +
    labs(x = NULL, y = NULL) +
    guides(alpha = FALSE, size = FALSE) +
    scale_size_area(max_size = 1) +
    scale_alpha(range = c(0.25, 0.75)) +
    scale_x_continuous(breaks = seq(0, 24, by = 6)) +
    ggtitle(label = "Conversations Initiated by the Hour") +
    .plot_ggtheme



  # // Me - Daily -----------------------------------------------------------

  export_me_plot_daily <-
    data_sms_summaries %>%
    pluck("data_period_day") %>%
    group_by(Day) %>%
    filter(MessageType == "Sent") %>%
    ggplot(aes(x = Day,
               y = Contact_Count)) +
    geom_point(aes(size = Length_Sum,
                   # alpha = Length_Avg),
                   # alpha = rev(Message_Count)),
                   alpha = Message_Count),
               position = position_jitter(height = 0.10, width = 0.20),
               color = "black",
               fill = .plot_colors["me"],
               shape = 22) +
    geom_smooth(span = 0.25,
                color = "black",
                size = 0.75,
                se = FALSE) +
    scale_alpha_continuous(range = c(0.10, 0.25), breaks = c(10, 20, 30, 40, 50), trans = "reverse") +
    ggtitle("Messages Sent") +
    labs(x = NULL,
         y = "Contacts per Day",
         alpha = "Total Messages",
         size = "Total Length") +
    .plot_ggtheme




  # Tests -------------------------------------------------------------------


  # // Test - Length Diff

  # Difference - Length
  # candlestick chart / diverging bar chart

  # library(echarts4r)
  # library(plotly)

  # plot_data_dif_length %>%
  #   e_charts(Contact) %>%
  #   e_bar(mean) %>%
  #   # e_scatter(mean, days) %>%
  #   # e_scatter(min) %>%
  #   # e_scatter(max) %>%
  #   e_flip_coords() %>%
  #   e_tooltip(trigger = "axis")


  # plot_data_dif_length %>%
  #   ggplot(aes(x = Contact, y = mean, fill = color)) +
  #   geom_col() +
  #   coord_flip() +
  #   theme_minimal()
  #
  # plot_data_dif_length %>%
  #   ggplot(aes(x = Contact, y = mean, fill = color, color = color)) +
  #   geom_pointrange(aes(ymin = 0, ymax = mean, size = days)) +
  #   theme_minimal() +
  #   geom_hline(aes(yintercept = 0)) +
  #   coord_flip() +
  #   scale_size_continuous(range = c(.1, 1.5))



  # geom_pointrange(aes(ymin = 0, ymax = mean, size = days)) +
  # scale_size_continuous(range = c(.1, 1.5))

  # plot_dif_length %>% plotly::ggplotly()




  # // Test - Single Contact
  # Select Contact
  # - T: Days of contact, how many messages, average length of message, total length
  # - P: Message length by day; scatter; fill is type; include avg. smooth line?
  # -- Label: Propensity of contact to initiate conversation
  # - P: Length by time of day, pointrange, alpha is avg. length

  # - At what time of day are they likely to initiate a conversation?
  # - When they do initiate, how long is their message?

  # data_sms_summaries %>% names()
  # .plot_contact



  # // Test - Me
  # - My Summary
  # - Length by Day
  # - Contacts per Day
  # - Propensity to initiate conversation
  # -- At what time of day am I likely to initiate a conversation?
  # -- When I initiate, how long is my message?


  # data_sms_summaries %>% names()



  # Exports -----------------------------------------------------------------

  .export_visual <-
    ls(pattern = "export") %>%
    mget(inherits = TRUE) %>%
    enframe() %>%
    mutate(name = str_remove(name, "export_")) %>%
    spread(name, value) %>%
    mutate(plot_colors = list(.plot_colors))

  write_rds(.export_visual,
            str_glue("data/{master_date}_visuals.rds"),
            compress = "bz")


  # End Function ------------------------------------------------------------

  print(str_glue("SMS visual data output at `data/{master_date}_visuals.rds`"))

}



# Test Function -----------------------------------------------------------

# fx_sms_visualise(master_date = "2018-05-08")
