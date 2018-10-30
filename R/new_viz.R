# Set Up ------------------------------------------------------------------


# |- Packages
pacman::p_load(tidyverse, rlang, shiny, shinydashboard, scales, plotly, DT, tidytext)

source("R/fx_sms_read_xml.R")
source("R/fx_sms_sumarise.R")
source("R/fx_sms_append.R")
source("R/fx_sms_prepare.R")

test <- readRDS("C:/Users/exp01754/OneDrive/Data/sms_analysis/data/2018-10-15_master.rds")
test_prep <- test %>% fx_sms_prepare()

test_ranks <-
  deframe(test_prep)$sms_rank %>%
  top_n(n = 25, wt = -Rank_Score) %>%
  arrange(Rank_Score) %>%
  mutate(Contact = as_factor(Contact))

.plot_theme <-
  theme_minimal() +
  theme(strip.text.y = element_text(angle = 0, face = "bold"),
        strip.text.x = element_text(face = "bold"),
        panel.background = element_rect(color = "gray"),
        panel.grid.major.x = element_line(color = "gray", linetype = 3),
        panel.grid.minor.x = element_line(color = "gray", linetype = 3),
        legend.title.align = 0.5)


.plot_theme_dark <-
  .plot_theme +
  theme(panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(linetype = "solid", color = "#707073"),
        rect = element_rect(fill = "#2a2a2b")
  )

.plot_colors <- list(Sent = "#f8766d", Received = "#00bfc4")


# Attempt 1 ---------------------------------------------------------------



# step_1 <-
#   data_sms_summaries %>%
#   pluck("data_period_contact_day") %>%
#   filter(!!! contact_filter_rlang) %>%
#   group_by(Contact, Day) %>%
#   summarise(Total_Length = sum(Length_Sum))
#
# step_2 <-
#   s1 %>%
#   mutate(week = floor_date(Day, unit = "week")) %>%
#   filter(!!! contact_filter_rlang) %>%
#   group_by(week) %>%
#   summarise(mean = mean(Total_Length))



# plot_ly() %>%
#   add_markers(data = step_1, x = ~Day, y = ~Total_Length, alpha = 0.70, color = "#fdae6b") %>%
#   add_lines(data = step_2, x = ~week, y = ~mean, hoverinfo = "none", line = list(shape = "spline", type = "hv", width = 2, color = "#3182bd")) %>%
#   layout(yaxis = list(type = "log"))



# gg to py ----------------------------------------------------------------

# plot_gg <-
#   ggplot() +
#   # geom_smooth(aes(x = Day, y = Total_Length), data = step_1, se = FALSE, span = 0.30) +
#   geom_point(aes(x = week, y = mean), data = step_2, color = "#de2d26", alpha = alpha_adj) +
#   theme_minimal()

# plot_gg <-
#   step_1 %>%
#   ggplot() +
#   aes(x = Day, y = Total_Length) +
#   geom_point(alpha = alpha_adj, shape = 15, color = "#666666") +
#   # geom_smooth(se = FALSE, span = 0.25) +
#   # geom_smooth(se = FALSE, span = 0.25, color = "#de2d26") +
#   geom_smooth(se = FALSE, span = 0.0001, color = "red") +
#   scale_y_log10() +
#   theme_minimal()


# plot_gg %>%
#   ggplotly()



# Another -----------------------------------------------------------------


# export_data_period_day %>%
# ggplot() +
# aes(x = Day, y = Length_Avg, weight = Message_Count, size = Message_Count) +
# geom_point(data = export_data_period_contact_day %>% filter(!!! contact_filter_rlang),
#            alpha = alpha_adj,
#            shape = 15) +
# geom_smooth(color = "red", se = FALSE) +
# geom_smooth(data = export_data_period_contact_day %>% filter(!!! contact_filter_rlang), se = FALSE) +
# scale_y_log10() +
# theme_minimal()

# Overall
base <-
  export_data_period_day %>%
  ggplot() +
  aes(x = Day, y = Length_Avg, weight = Message_Count, size = Message_Count) +
  geom_smooth(color = "black", se = FALSE)  +
  geom_smooth(color = "red", se = FALSE,
              data = export_data_period_contact_day %>% filter(!!! contact_filter_rlang)) +
  geom_point(data = export_data_period_contact_day %>% filter(!!! contact_filter_rlang),
             alpha = 0.10) +
  scale_y_log10()


base %>% ggplotly()

additional <-
  ggplot() +
  # geom_smooth(data = export_data_period_contact_week %>% filter(!!! contact_filter_rlang),
  #             mapping = aes(x = Week, y = Length_Avg, weight = Message_Count),
  #             color = "blue") +
  stat_smooth(data = export_data_period_contact_week %>% filter(!!! contact_filter_rlang),
              mapping = aes(x = Week, y = Length_Avg, weight = Message_Count),
              color = "blue") +
  geom_point(data = export_data_period_contact_week %>% filter(!!! contact_filter_rlang),
             mapping = aes(x = Week, y = Length_Avg, size = Message_Count),
             shape = 15,
             alpha = alpha_adj) +
  scale_y_log10()


base + additional

p2 %>% ggplotly()

# export_data_period_contact_week %>%
#   ggplot() +
#   aes(x = Week, y = Length_Avg, weight = Message_Count) +
#   geom_smooth()

stat_sm



# at 3 --------------------------------------------------------------------

overview <-
  data_sms_period %>%
  group_by(Day) %>%
  fx_sms_summary()

a3 <-
  overview %>%
  ggplot() +
  aes(x = Day, y = Length_Avg, weight = Message_Count, size = Contact_Count) +
  geom_point(alpha = 0.10, shape = 15) +
  geom_smooth(se = FALSE, span = 0.25, color = "red") +
  theme_minimal() +
  labs(x = NULL, y = "Length", title = "Daily Average Message Length")

test <- a3 %>% ggplotly()

test


# ridgeline ---------------------------------------------------------------

install.packages("ggridges")
library(ggridges)

ttt <-
  data_sms_dif %>%
  select(Contact, Day, MessageType, Length_Sum) %>%
  spread(MessageType, Length_Sum) %>%
  replace_na(replace = list(Received = 0, Sent = 0)) %>%
  mutate(Length_Difference = Received - Sent) %>%
  filter(Contact %in% export_list_top_contacts) %>%
  group_by(Contact) %>%
  summarise(quantiles = list(quantile(Length_Difference) %>% enframe() %>% spread(name, value)),
            Days = n()) %>%
  unnest(quantiles) %>%
  rename(Min = "0%", Max = "100%", Q1 = "25%", Median = "50%", Q3 = "75%") %>%
  arrange(Median) %>%
  mutate(Color = if_else(Median > 0, "Me", "Them"),
         Contact = fct_inorder(Contact)) %>%

  ggplot() +
  aes(x = Contact) +
  geom_linerange(aes(ymin = Q1,
                     ymax = Q3,
                     color = Color)) +
  geom_point(aes(y = Median,
                 size = Days,
                 fill = Color),
             color = "black",
             shape = 21) +
  geom_hline(aes(yintercept = 0)) +
  labs(x = NULL,
       y = "Median Length Difference",
       # fill = "Whose Messages\nAre Longer?",
       # size = "Days of Contact",
       color = NULL) +
  guides(color = FALSE, fill = FALSE, size = FALSE) +
  coord_flip() +
  scale_fill_manual(values = c(.plot_colors$me, .plot_colors$them)) +
  scale_color_manual(values = c(.plot_colors$me, .plot_colors$them)) +
  .plot_ggtheme

ttt %>% ggplotly()


ggplot() +

  # aes(x = Length_Difference, y = Contact, fill = ..x..) +
  # geom_density_ridges_gradient(rel_min_height = 0.0001) +
  # scale_fill_viridis_c() +

  aes(x = Length_Difference, y = Contact) +
  geom_density_ridges(rel_min_height = 0.0001, alpha = 0.25, fill = "blue", quantile_lines = TRUE, quantiles = 2) +
  geom_vline(xintercept = 0, color = "black") +
  .plot_ggtheme



# Check Beeline Swarm Plot ------------------------------------------------
#ggbeeswarm

# Length by Day -----------------------------------------------------------

# data_test <-
#   test %>%
#   mutate(Hour = hour(DateTime),
#          Day = date(DateTime),
#          Weekday = wday(DateTime, label = TRUE, week_start = 1),
#          Week = floor_date(DateTime, unit = "week"),
#          Month = floor_date(DateTime, unit = "month") %>% date(),
#          Year = floor_date(DateTime, unit = "year") %>% year()) %>%
#   group_by(Day) %>%
#   fx_sms_summary()


p <-
  deframe(test_prep)$sms_day %>%
  ggplot() +
  aes(x = Day, y = Length_Sum, size = Message_Count, color = Contact_Count) +
  geom_point(alpha = 0.60) +
  scale_color_viridis_c() +
  labs(y = "Message Length",
       x = NULL,
       color = "Contacts per Day",
       size = "Messages per Day") +
  .plot_theme

ggplotly(p)



# Range - Len/Day ---------------------------------------------------------

devtools::install_github("jbkunst/highcharter")
library(highcharter)

test_range <-
  test %>%
  mutate(Day = date(DateTime)) %>%
  group_by(Day) %>%
  summarise_at("MessageLength", funs(min, median, max, mean))

test_range %>%
  ggplot() +
  aes(x = Day, ymin = min, ymax = max, color = median) +
  geom_linerange() +
  scale_color_viridis_c() +
  .plot_theme

hchart(lala,
       type = "columnrange",
       hcaes(x = Day, low = min, high = max, color = median))


# Rankings ----------------------------------------------------------------

test_prep <- test %>% fx_sms_prepare()

# deframe(test_prep)$sms_rank %>%
#   top_n(n = 25, wt = -Rank_Score) %>%
#   hchart("treemap", hcaes(x = Contact, value = !!sym(top_value), color = Rank_Score))



test_ranks %>% hchart("column", hcaes(y = Message_Count, x = Contact))
test_ranks %>% hchart("column", hcaes(y = Length_Sum, x = Contact))
test_ranks %>% hchart("column", hcaes(y = Length_Avg, x = Contact))
test_ranks %>% hchart("column", hcaes(y = Contact_Days, x = Contact))
test_ranks %>% hchart("column", hcaes(y = Messages_per_Day, x = Contact))

test_ranks %>%
  ggplot() +
  aes(x = Contact, y = Message_Count) +
  geom_col() +
  coord_flip() +
  .plot_theme


top_value <-
  c("Message_Count", "Length_Sum", "Length_Avg", "Contact_Days", "Messages_per_Day") %>%
  sample(1)

top_value_display <- top_value %>% str_replace_all("_", " ")

plot_ly(data = test_ranks,
        x = ~ Contact,
        y = ~ get(top_value),
        text = ~str_c(top_value_display, comma(get(top_value)), sep = ": "),
        marker = list(line = list(color = "black", width = 1.5)),
        type = "bar") %>%
  layout(title = top_value_display,
         xaxis = list(title = ""),
         yaxis = list(title = ""))


# Difference --------------------------------------------------------------

# .plot_colors <- list(me = "#ef8a62", them = "#67a9cf")

test_plot_dif <-
  test_prep %>%
  deframe() %>%
  pluck("sms_diff") %>%
  filter(Contact %in% test_ranks$Contact) %>%

  ggplot() +
  aes(x = Contact) +
  geom_linerange(aes(ymin = Q1,
                     ymax = Q3,
                     color = `Longer Messages`)) +
  geom_point(aes(y = Median,
                 size = `Days of Contact`,
                 fill = `Longer Messages`),
             color = "black",
             shape = 21) +
  geom_hline(aes(yintercept = 0)) +
  labs(x = NULL, color = NULL,
       y = "Median Difference",
       title = "Whose Messages Are Longer?") +
  guides(color = FALSE, size = FALSE) +
  # scale_color_manual(values = c("Mine" = .plot_colors$Sent, "Theirs" = .plot_colors$Recieved)) +
  # scale_fill_manual(values = c("Mine" = .plot_colors$Sent, "Theirs" = .plot_colors$Received),
  #                   aesthetics = c("color", "fill")) +

  # scale_fill_manual(values = c(Mine = .plot_colors$Sent, Theirs = .plot_colors$Recieved)) +
  # scale_color_manual(values = c(Mine = .plot_colors$Sent, Theirs = .plot_colors$Recieved)) +
  # scale_fill_manual(values = .plot_colors) +
  # scale_color_manual(values = .plot_colors) +
  # scale_fill_manual(values = c(.plot_colors$me, .plot_colors$them)) +
  # scale_color_manual(values = c(.plot_colors$me, .plot_colors$them)) +
  .plot_theme +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor = element_line(linetype = 3),
        axis.text.x = element_text(angle = -35))

test_plot_dif %>% ggplotly()


# Length - Prop -----------------------------------------------------------
library(tidyverse)
test <- readRDS("C:/Users/exp01754/OneDrive/Data/sms_analysis/data/2018-08-06_master.rds")
test_prep <- test %>% fx_sms_prepare()
.plot_colors <- list(me = "#ef8a62", them = "#67a9cf")

deframe(test_prep)


test_ranks <-
  deframe(test_prep)$sms_rank %>%
  top_n(n = 25, wt = -Rank_Score) %>%
  arrange(Rank_Score) %>%
  mutate(Contact = as_factor(Contact))

test_lenprop <-
  deframe(test_prep)$sms_contact_type %>%
  filter(Contact %in% test_ranks$Contact)

test_lenprop %>%
  ggplot(aes(x = Contact, y = Length_Sum, fill = MessageType)) +
  geom_col(color = "black", position = "fill") +
  coord_flip() +
  geom_hline(aes(yintercept = 0.50), linetype = "dashed") +
  labs(x = NULL, y = NULL, fill = "Message Type") +
  ggtitle("Proportion of Message Length") +
  scale_fill_manual(values = c(.plot_colors$me, .plot_colors$them)) +
  .plot_theme


# Timelines ---------------------------------------------------------------

.plot_theme_dark <-
  .plot_theme +
  theme(panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(linetype = "solid", color = "#707073"),
        rect = element_rect(fill = "#2a2a2b")
  )

test_timeline <-
  deframe(test_prep)$sms_week_contact %>%
  # filter(Contact == "Christy McGraw") %>%
  filter(Contact == "Emily Kay Piellusch") %>%
  ggplot() +
  aes(x = Week,
      ymin = Minimum,
      ymax = Maximum,
      y = Median,
      n = Count,
      color = Median) +
  geom_linerange() +
  scale_color_viridis_c(direction = -1, option = "D", end = 1, begin = 0.30) +
  labs(y = NULL, x = NULL, color = "Median Message Length") +
  .plot_theme_dark


ggplotly(test_timeline)


# Tidy Text ---------------------------------------------------------------

anon <- read_csv("data/anon_id.csv")
stop_words <- get_stopwords(source = "snowball")

contact <- "Emily Kay Piellusch"
# contact <- "Patrick Campbell"
# contact <- "Mom"

test_nlp <-
  test %>%
  filter(Contact == contact) %>%
  arrange(DateTime) %>%
  rowid_to_column("Message_Number") %>%
  group_by(MessageType) %>%
  unnest_tokens(word, Message) %>%
  anti_join(stop_words) %>%
  count(MessageType, word, sort = TRUE) %>%
  mutate(proportion = n / sum(n)) %>%
  select(-n) %>%
  spread(MessageType, proportion) %>%

  ggplot() +
  aes(x = Received, y = Sent, color = abs(Sent - Received)) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75") +
  theme(legend.position = "none")

list_words <-
  test %>%
  filter(Contact == contact) %>%
  arrange(DateTime) %>%
  rowid_to_column("Message_Number") %>%
  group_by(MessageType) %>%
  unnest_tokens(word, Message) %>%
  anti_join(stop_words) %>%
  count(MessageType, word, sort = TRUE) %>%
  mutate(proportion = n / sum(n)) %>%
  select(-n) %>%
  spread(MessageType, proportion) %>%
  mutate(Difference = abs(Sent - Received)) %>%
  print()

bind_rows(list_words %>% top_n(wt = Difference, n = 200),
          list_words %>% top_n(wt = -Difference, n = 200)) %>%
  ggplot() +
  geom_abline(color = "gray40", lty = 2) +
  aes(x = Received, y = Sent, color = Difference, label = word) +
  geom_point(alpha = 0.80, size = 3, position = position_jitter(width = 0, height = 0)) +
  geom_text(check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75") +
  guides(color = "none")


list_words_2 <-
  test %>%
  filter(Contact == contact) %>%
  group_by(MessageType) %>%
  unnest_tokens(word, Message) %>%
  anti_join(stop_words) %>%
  count(word, MessageType) %>%
  group_by(word) %>%
  filter(sum(n) > 10) %>%
  ungroup() %>%
  mutate(test_number = map_lgl(word, ~ .x %>% as.numeric() %>% is.na())) %>%
  filter(test_number) %>%
  select(-test_number) %>%
  group_by(word) %>%
  mutate(total = sum(n)) %>%
  ungroup() %>%
  arrange(desc(total))

list_top_bottom <-
  bind_rows(list_words_2 %>% top_n(n = 400, wt = total),
            list_words_2 %>% top_n(n = 100, wt = -total))

list_spread <-
  list_top_bottom %>%
  mutate(prop = n / sum(n)) %>%
  select(-n) %>%
  spread(MessageType, prop, fill = 0) %>%
  mutate(Received = ifelse(Received == min(Received), Received + sd(Received)/1000, Received),
         Sent = ifelse(Sent == min(Sent), Sent + sd(Sent)/10000, Sent))


list_spread %>%
  ggplot() +
  aes(x = Received, y = Sent, color = abs(Sent - Received), label = word) +
  geom_point(alpha = 0.80, size = 2.5, position = position_jitter(width = 0, height = 0)) +
  geom_abline(color = "gray40", lty = 2) +
  geom_text(check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75") +
  guides(color = "none")



# * Sentiment Analysis ------------------------------------------------------
# Label messages to keep them in order
# We want sentiment per message, over time
# Maybe sentiment per day, over time

contact <- "Emily Kay Piellusch"
# contact <- "Teresa Malmquist"
# contact <- "Mom"
# contact <- "Patrick Campbell"

sentiment_score <- get_sentiments("afinn")
sentiment_label <- get_sentiments("nrc")
sentiment_bin   <- get_sentiments("bing")

sentiment_base <-
  test %>%
  filter(Contact == contact) %>%
  arrange(DateTime) %>%
  rowid_to_column("Message_Number") %>%
  group_by(MessageType) %>%
  unnest_tokens(word, Message)

sentiment_plot_label <-
  sentiment_base %>%
  inner_join(sentiment_label) %>%
  count(sentiment, MessageType, sort = TRUE) %>%
  group_by(MessageType) %>%
  mutate(p = n / nrow(sentiment_base)) %>%
  group_by(sentiment) %>%
  mutate(sentiment_total = sum(n)) %>%
  arrange(desc(sentiment_total)) %>%
  ungroup() %>%
  ggplot() +
  aes(x = as_factor(sentiment), y = p, fill = MessageType) +
  coord_flip() +
  scale_y_continuous(labels = percent_format())

# X% of words are non-neutral
# X uses more non-neutral words
sentiment_plot_label + geom_col()
sentiment_plot_label + geom_col(position = "fill") + geom_hline(yintercept = 0.50)



# Work in Progress

sentiment_base %>%
  ungroup() %>%
  inner_join(sentiment_bin) %>%
  count(Message_Number, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative) %>%
  ggplot() +
  aes(x = Message_Number, y = sentiment) +
  geom_col()

sentiment_table <-
  sentiment_base %>%
  inner_join(sentiment_score) %>%
  group_by(Message_Number) %>%
  summarise(sum_score = sum(score),
            avg_score = mean(score),
            n_score = length(score)) %>%
  print()

sentiment_table %>%
  ggplot() +
  aes(x = Message_Number, y = sum_score) +
  geom_col()



# * Term Frequency ----------------------------------------------------------

contact <- "Emily Kay Piellusch"
# contact <- "Teresa Malmquist"
# contact <- "Mom"
# contact <- "Patrick Campbell"
# contact <- "Jenny Nguyen"
# contact <- "Mike"
# contact <- "Michelle Ngo"
# contact <- "Amanda Rae Friend"
# contact <- "Christy McGraw"

# How do I define a document?
# - 1 Message?
# - 1 Day?
# - All Received?

# test %>%
#   filter(Contact == contact) %>%
#   arrange(DateTime) %>%
#   rowid_to_column("Message_Number") %>%
#   group_by(MessageType) %>%
#   unnest_tokens(word, Message) %>%
#   group_by(MessageType, Message_Number, word) %>%
#   summarise(term_freq = length(word)) %>%
#   group_by(MessageType, Message_Number) %>%
#   mutate(total_len = sum(term_freq)) %>%
#   ungroup() %>%
#   mutate(term_prop = term_freq / total_len)

# REPLACE ALL INSTANCES OF F27BD7BB with "reddit.com"

test %>%
  filter(Contact == contact) %>%
  mutate(Message = if_else(str_detect(Message, "f27bd7bb"), "reddit", Message)) %>%
  mutate(Message = if_else(str_detect(Message, "open.spotify.com"), "spotify", Message)) %>%
  mutate(Message = if_else(str_detect(Message, "youtu"), "youtube", Message)) %>%
  arrange(DateTime) %>%
  rowid_to_column("Message_Number") %>%
  group_by(MessageType) %>%
  unnest_tokens(word, Message) %>%
  count(MessageType, word) %>%
  bind_tf_idf(word, MessageType, n) %>%
  group_by(MessageType) %>%
  top_n(15, tf_idf) %>%
  arrange(desc(tf_idf)) %>%
  ggplot() + aes(y = tf_idf, x = as_factor(word)) +
  geom_col() +
  facet_wrap(vars(MessageType), scales = "free") +
  coord_flip()



# N-Gram ------------------------------------------------------------------


stop_words_1 <- get_stopwords(source = "snowball")
stop_words_2 <- get_stopwords(source = "stopwords-iso")
stop_words_3 <- get_stopwords(source = "smart")

sentiment_score <- get_sentiments("afinn")


contact <- "Emily Kay Piellusch"
# contact <- "Teresa Malmquist"
# contact <- "Mom"
# contact <- "Patrick Campbell"
# contact <- "Jenny Nguyen"
# contact <- "Mike"
# contact <- "Michelle Ngo"
# contact <- "Amanda Rae Friend"
# contact <- "Christy McGraw"


test_ngrams <-
  test %>%
  filter(Contact == contact) %>%
  mutate(Message = if_else(str_detect(Message, "f27bd7bb"), "reddit", Message)) %>%
  mutate(Message = if_else(str_detect(Message, "open.spotify.com"), "spotify", Message)) %>%
  mutate(Message = if_else(str_detect(Message, "youtu"), "youtube", Message)) %>%
  arrange(DateTime) %>%
  rowid_to_column("Message_Number") %>%
  group_by(MessageType) %>%
  unnest_tokens(bigram, Message, token = "ngrams", n = 2) %>%
  ungroup() %>%
  filter(!is.na(bigram))


test_ngrams %>%
  count(MessageType, bigram, sort = TRUE) %>%
  bind_tf_idf(bigram, MessageType, n) %>%
  group_by(MessageType) %>%
  top_n(15, tf_idf) %>%
  arrange(MessageType, desc(tf_idf))


test_ngrams_sep <-
  test_ngrams %>%
  separate(bigram, into = c("word_1", "word_2"), sep = " ") %>%
  count(MessageType, word_1, word_2, sort = TRUE)

pacman::p_load(ggraph, igraph)

test_ngrams_sep %>%
  select(-MessageType) %>%
  # filter(n > 20, word_1 == "i") %>%
  # filter(n > 20, word_1 == "not") %>%
  # filter(n > 3, word_1 == "don't") %>%
  # filter(n > 3, word_2 == "kev") %>%
  filter(n > 3, word_1 == "maggie") %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)



test_bigram_sentiment <-
  test_ngrams_sep %>%
  inner_join(sentiment_score, by = c("word_1" = "word")) %>%
  inner_join(sentiment_score, by = c("word_2" = "word"))


test_ngrams_sep %>% filter(word_1 == "i") %>% arrange(MessageType, -n) %>% View()
test_ngrams_sep %>% filter(word_1 == "don't") %>% arrange(MessageType, -n) %>% View()
test_ngrams_sep %>% filter(word_1 == "not") %>% arrange(MessageType, -n) %>% View()



# LDA ---------------------------------------------------------------------

pacman::p_load(tm, topicmodels)

stop_words <-
  # get_stopwords(source = "snowball") %>%
  get_stopwords(source = "smart") %>%
  add_row(word = c("just", "like"))

test_lda <-
  test %>%
  ungroup() %>%
  filter(MessageType == "Received",
         Contact %in% test_ranks$Contact) %>%
  mutate(Message = if_else(str_detect(Message, "f27bd7bb"), "reddit", Message)) %>%
  mutate(Message = if_else(str_detect(Message, "open.spotify.com"), "spotify", Message)) %>%
  mutate(Message = if_else(str_detect(Message, "youtu"), "youtube", Message)) %>%
  arrange(DateTime) %>%
  rowid_to_column("Message_Number") %>%
  group_by(Contact) %>%
  unnest_tokens(word, Message) %>%
  anti_join(stop_words) %>%
  count(Contact, word) %>%
  ungroup()

test_dtm <-
  test_lda %>%
  ungroup() %>%
  cast_dtm(Contact, word, n)

# 8 is messy, 7 is better, 6 is ok

test_lda_model <- test_dtm %>% LDA(k = 7, control = list(seed = 42))

test_lda_contact <- test_lda_model %>% tidy(matrix = "gamma") %>% group_by(topic)
test_lda_terms <- test_lda_model %>% tidy(matrix = "beta") %>% group_by(topic) %>% top_n(10, beta) %>% arrange(topic, -beta)


test_lda_terms %>%
  ggplot() +
  aes(x = reorder(term, beta), y = beta) +
  geom_col() +
  coord_flip() +
  facet_wrap(facets = "topic", scales = "free")

test_lda_contact %>%
  ggplot() +
  aes(x = document, y = topic, alpha = gamma, size = gamma, fill = factor(topic)) +
  geom_point(shape = 21, color = "black") +
  coord_flip() +
  theme_minimal()

### THIS ONE ###
### THIS ONE ###
### THIS ONE ###

lda_order_1 <-
  left_join(test_ranks %>% select(Contact, Rank_Score),
            test_lda_contact %>% filter(gamma > 0.25),
            by = c("Contact" = "document")) %>%
  arrange(Rank_Score)

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
  test_lda_contact %>%
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

lda_plot %>% ggplotly(tooltip = "label")


# test_ranks %>%
#   select(Contact, Rank_Score) %>%
#   left_join(test_lda_contact, by = c("Contact" = "document")) %>%
#   mutate(Contact_Score = gamma * Rank_Score) %>%
#   group_by(topic) %>%
#   summarise(Topic_Score = mean(Contact_Score)) %>%
#   arrange(Topic_Score) %>%
#   mutate(Topic_Label = factor(sequence(n()))) %>%
#   left_join(test_lda_contact, by = "topic") %>%
#   filter(gamma > 0.25) %>%
#   arrange(desc(Topic_Label)) %>%
#   mutate(Contact_Order = as_factor(document)) %>%
#   right_join(test_lda_contact) %>%
#   ggplot() +
#   aes(x = Contact_Order,
#       y = Topic_Label,
#       fill = Topic_Label,
#       alpha = gamma,
#       size = gamma) +
#   geom_point(shape = 21, color = "black") +
#   coord_flip() +
#   labs(x = NULL, y = NULL) +
#   guides(size = "none", alpha = "none", color = "none", fill = "none") +
#   theme_minimal()



# test_lda_contact %>%
#   # filter(gamma >= 0.25) %>%
#   ggplot() +
#   aes(x = factor(document, test_lda_ranks$Contact_Order %>% fct_unique()),
#       y = factor(topic, test_lda_ranks$Topic_Order %>% fct_unique()),
#       fill = factor(topic),
#       alpha = gamma,
#       size = gamma) +
#   geom_point(shape = 21, color = "black") +
#   coord_flip() +
#   labs(x = NULL, y = NULL) +
#   guides(size = "none", alpha = "none", color = "none", fill = "none") +
#   # scale_size_continuous(range = c(4, 8), limits = c(0, 1)) +
#   theme_minimal()
### THIS ONE ###
### THIS ONE ###
### THIS ONE ###
### THIS ONE ###



# test_lda_contact %>%
#   group_by(topic) %>%
#   filter(gamma >= 0.25) %>%
#   ggplot() +
#   aes(x = document, y = gamma) +
#   geom_col() +
#   coord_flip() +
#   facet_wrap(vars(topic), scales = "free")


# pacman::p_load(vegan)


# test_lda_contact %>%
#   ungroup() %>%
#   filter(topic %in% 1:2) %>%
#   mutate(score = topic * gamma) %>%
#   group_by(document) %>%
#   summarise(x = mean(score)) %>%
#   arrange(-x)


# test_lda_contact %>%
#   ungroup() %>%
#   # filter(topic %in% 1:2) %>%
#   mutate(score = topic * gamma) %>%
#   dist() %>%
#   cmdscale() %>%
#   ordiplot()


# t1 %>%
#   ggplot(aes(x = x, y = 1, label = document)) +
#   geom_point(position = position_jitter(height = 0.05))

# tlda_s <-
#   test_lda_contact %>%
#   ungroup() %>%
#   spread(topic, gamma)

# asdf <-
#   tlda_s %>%
#   select(-document) %>%
#   prcomp(rank. = 2) %>%
#   pluck("x") %>%
#   as_tibble() %>%
#   add_column(doc = tlda_s$document) %>%
#   ggplot() +
#   aes(PC1, PC2, label = doc) +
#   geom_point(position = position_jitter(width = 0.05, height = 0.05))
#
# asdf %>% ggplotly()




# Initial -----------------------------------------------------------------

# install.packages("ggridges")
# library(ggridges)



# test_prep[7,2][[1]][[1]] %>%
#   ungroup() %>%
#   filter(Contact == "Emily Kay Piellusch") %>%
#   select(-Contact) %>%
#   add_row(Hour = 0:23) %>%
#   complete(MessageType, Hour, fill = list(Message_Count = 0, Total_Length = 0)) %>%
#   filter(!is.na(MessageType)) %>%
#   mutate(prop = Message_Count / sum(Message_Count)) %>%
#
  # ggplot() + aes(x = Hour, weight = prop, fill = MessageType) + geom_density(alpha = 0.80) + .plot_theme
  # ggplot() + aes(x = Hour, y = prop, color = MessageType) + geom_smooth(alpha = 0.6, span = 0.50, se = FALSE)
  # ggplot() + aes(x = Hour, y = MessageType, height = Message_Count, fill = MessageType) + geom_density_ridges(stat = "identity", alpha = 0.75)

data_sms_period <- deframe(test_prep)$sms_initial_hour

test_data_initial <-
  data_sms_period %>%
  arrange(DateTime) %>%
  group_by(Day, Contact) %>%
  slice(1) %>%
  # filter(Contact == "Emily Kay Piellusch") %>%
  filter(Contact == "Mom") %>%
  # filter(Contact == "Patrick Campbell") %>%
  mutate(MessageType = if_else(MessageType == "Sent", "Me", "Them")) %>%
  arrange(desc(MessageType))


test_plot_initial <-
  test_data_initial %>%
  ggplot() +
  aes(x = Hour, fill = MessageType, color = MessageType) +
  geom_density(alpha = 0.50) +
  .plot_theme +
  labs(y = NULL, fill = NULL, x = NULL, color = NULL) +
  scale_x_continuous(breaks = seq(0, 24, by = 6)) +
  scale_fill_manual(values = c("Me" = .plot_colors$Sent, "Them" = .plot_colors$Received)) +
  theme(axis.text.y = element_blank())



test_plot_initial %>% ggplotly(tooltip = c("y", "x"))



# Color Test --------------------------------------------------------------

tibble(label = names(.plot_colors), x = 1:4, y = 1:4) %>%
  ggplot() +
  aes(x = x, y = y, color = label) +
  geom_point(size = 5) +
  theme_minimal() +
  scale_color_manual(values = .plot_colors)


# Most Dramatic Change ----------------------------------------------------
# D = number of days between first and last day of contact
# d = number of contact days
# p = d/D = proportion of days in contact

# L = sum of message length
# l = L / d = message length per day

# compare p between [data >= prev 90 days] / [data < prev 90 days]

test_changes <-
  deframe(test_prep)$sms_contact_day %>%
  filter(Contact %in% test_ranks$Contact) %>%
  ungroup() %>%
  mutate(Period = ifelse(Day >= max(Day) - days(90), "new", "historical")) %>%
  group_by(Period, Contact) %>%
  summarise(day_min = min(Day),
            day_max = max(Day),
            day_all = difftime(day_max, day_min, units = "days") %>% parse_number(),
            day_contact = length(Day),
            day_proportion = day_contact / day_all,
            length_sum = sum(Length_Sum),
            day_length = length_sum / day_contact) %>%
  arrange(Contact) %>%
  select(Period, Contact, day_proportion, day_length) %>%
  gather(measure, value, day_proportion:day_length) %>%
  unite(Label, Period, measure) %>%
  spread(Label, value) %>%
  replace_na(list(new_day_length = 0,
                  new_day_proportion = 0)) %>%
  mutate(change_length = new_day_length / historical_day_length,
         change_frequency = new_day_proportion / historical_day_proportion) %>%

  ggplot() +
  aes(x = change_length,
      y = change_frequency,
      text = str_glue("Contact: {Contact}\nLength Change: {change_length %>% number(accuracy = 0.01, suffix = 'x')}\nFrequency Change: {change_frequency %>% number(accuracy = 0.01, suffix = 'x')}")) +
  geom_jitter(width = 0.025, height = 0.025, color = "#3182bd") +
  geom_hline(yintercept = 1) +
  geom_vline(xintercept = 1) +
  labs(title = "Habit Changes in Last 90 Days",
       x = "Daily Message Length",
       y = "Daily Contact Frequency") +
  .plot_theme


test_changes %>% ggplotly(tooltip = "text")

test_dates <-
  test %>%
  mutate(Period = ifelse(DateTime >= max(DateTime) - days(90),
                         "New", "Historical")) %>%
  filter(Contact %in% test_ranks$Contact) %>%
  group_by(Period, Contact) %>%
  summarise()

  print()


  summarise(min = min(DateTime),
            max = max(DateTime),
            period_90 = max - days(90),
            days = n_distinct(as_date(DateTime))) %>%
  mutate_if(is.Date, as_date) %>%
  print()

period_old <-
  test %>%
  filter(Contact %in% test_ranks$Contact,
         DateTime < test_dates$period_90) %>%
  group_by(Contact) %>%
  summarise(Length_Sum = sum(MessageLength),
            Contact_Days = n_distinct(as_date(DateTime)))

test_change <-
  deframe(test_prep)$sms_contact %>%
  select(Contact, Length_Sum, Contact_Days) %>%
  inner_join(period_old, by = "Contact",
             suffix = c(".overall", ".old")) %>%
  mutate(dlen = Length_Sum.overall / Length_Sum.old,
         dday = Contact_Days.overall / Contact_Days.old)

tp <-
  test_change %>%
  ggplot() +
  aes(x = dlen, y = dday, label = Contact) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10()




