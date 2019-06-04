
# Prepare Visuals for TidyText --------------------------------------------

fx_plot_tidytext <- function(data,
                             data_ranks,
                             message_contact = "Contact",
                             message_type = "MessageType",
                             message_text = "Message",
                             message_contact_filter = NULL,
                             stop_words_dictionary = "smart",
                             stop_words_additional = c("just", "like"),
                             save_plots = TRUE,
                             overwrite = FALSE) {

  pacman::p_load(tidyverse, tidytext, rlang, widyr, topicmodels)
  source("R/fx_sms_helpers.R")

  stop_words <- get_stopwords(source = stop_words_dictionary) %>% union(stop_words_additional)
  sentiment_labels <- get_sentiments(lexicon = "nrc")

  # Import
  if (data %>% is_character()) {data <- read_rds(data)}


  # Prepare
  rlang_message_contact <- sym(message_contact)
  rlang_message_type <- sym(message_type)
  rlang_message_text <- sym(message_text)

  rlang_filter <-
    if (message_contact_filter %>% is_null()) {NULL} else {
        str_glue("{message_contact} %in% message_contact_filter") %>% parse_expr()}

  data_tidytext <-
    data %>%
    arrange(DateTime) %>%
    rowid_to_column("Message_Number") %>%
    filter(!!! rlang_filter) %>%
    mutate(!!rlang_message := str_replace(!!rlang_message, "https://youtu.be/.{1,11}", "{youtube.com}")) %>%
    group_by(!!rlang_message_contact, !!rlang_message_type) %>%
    unnest_tokens(word, !!rlang_message_text) %>%
    ungroup()



  # Plot: LDA Groups
  data_tidytext %>%
    filter(!!rlang_message_type == "Received") %>%
    anti_join(stop_words) %>%
    count(!!rlang_message_contact, word) %>%
    cast_dtm(!!rlang_message_contact, word, n) %>%
    LDA(k = 7, control = list(seed = 42)) %>%
    tidy(matrix = "gamma")

  # lda_order_1 <-
  #   left_join(test_ranks %>% select(Contact, Rank_Score),
  #             tidy_lda %>% filter(gamma > 0.25),
  #             by = c("Contact" = "document")) %>%
  #   arrange(Rank_Score)

  lda_order_2 <-
    lda_order_1 %>%
    group_by(topic) %>%
    summarise(Topic_Score = mean(Rank_Score)) %>%
    arrange(Topic_Score) %>%
    mutate(order_topic = sequence(n())) %>%
    select(topic, order_topic) %>%
    print()

  lda_order_3 <-
    lda_order_2 %>%
    left_join(lda_order_1) %>%
    group_by(Contact) %>%
    top_n(n = 1, wt = order_topic) %>%
    arrange(-order_topic, -Rank_Score) %>%
    ungroup() %>%
    mutate(order_contact = sequence(n())) %>%
    select(Contact, order_topic, order_contact, topic) %>%
    print()


  lda_plot <-
    tidy_lda %>%
    left_join(lda_order_3 %>% select(-topic), by = c("document" = "Contact")) %>%
    ggplot() +
    aes(x = document %>% reorder(order_contact),
        y = topic %>% factor(levels = lda_order_3$topic %>% unique() %>% rev()),
        fill = factor(topic),
        alpha = gamma,
        size = gamma,
        label = document) +
    geom_point(shape = 21, color = "black") +
    coord_flip() +
    labs(x = NULL, y = NULL) +
    guides(size = "none", alpha = "none", color = "none", fill = "none") +
    theme_minimal()




  # Plot: Frequency Comparison, Sentiment Analysis, Co-Occurance
  tidytext_plots <-
    data_tidytext %>%
    nest(-!!rlang_message_contact, .key = "words_raw") %>%
    mutate(

      plot_freq =
        words_raw %>%
        map(~.x %>%
              count(!!rlang_message_type, word) %>%
              group_by(!!rlang_message_type) %>%
              mutate(freq = n / sum(n)) %>%
              anti_join(stop_words, by = "word") %>%
              select(-n) %>%
              spread(!!rlang_message_type, freq) %>%
              ggplot() +
              aes(Received, Sent) +
              geom_jitter(alpha = 0.1, size = 2.5, width = 0.25, height = 0.25) +
              geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
              scale_x_log10(labels = percent_format()) +
              scale_y_log10(labels = percent_format()) +
              geom_abline(color = "red") +
              .plot_theme),

      plot_coocurrance =
        words_raw %>%
        map(~.x %>%
              anti_join(stop_words, by = "word") %>%
              filter(!!rlang_message_type == "Received") %>%
              pairwise_count(word, Message_Number, sort = TRUE, upper = FALSE) %>%
              filter(n > 2) %>%
              top_n(50, n) %>%
              ungroup() %>%
              graph_from_data_frame() %>%
              ggraph(layout = "fr") +
              geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "cyan4") +
              geom_node_point(size = 5) +
              geom_node_text(aes(label = name), repel = TRUE,
                             point.padding = unit(0.2, "lines")) +
              theme_void()
        ),

      plot_sentiment =
        words_raw %>%
        map(~.x %>%
              add_count(!!rlang_message_type) %>%
              inner_join(sentiment_label, by = "word") %>%
              group_by(sentiment, !!rlang_message_type) %>%
              summarise(total_words = unique(n),
                        total_sentiment = length(n),
                        p_sentiment = total_sentiment / total_words) %>%
              ggplot() +
              aes(x = reorder(sentiment, p_sentiment, sum), y = p_sentiment, fill = MessageType) +
              geom_col() +
              coord_flip() +
              scale_y_continuous(labels = percent_format()) +
              .plot_theme
        )

      ) %>%
    select(-words_raw)



    # Output
  lst(rlang_filter)

}


# Test --------------------------------------------------------------------

# data <- "C:/Users/exp01754/OneDrive/Data/sms_analysis/data/2018-10-15_master.rds"
# message_contact_filter <- NULL

data <- readRDS("C:/Users/exp01754/OneDrive/Data/sms_analysis/data/2018-10-15_master.rds")
message_contact <- "Contact"
message_type <- "MessageType"
message_text <- "Message"
message_contact_filter <- test_ranks$Contact %>% levels()

fx_plot_tidytext(
  data,
  message_contact,
  message_type,
  message_text,
  message_contact_filter)
