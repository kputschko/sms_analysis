
# Set Up ------------------------------------------------------------------

library(plotly)
library(rlang)

# contact_filter <- NULL
contact_filter <- "Canyon"
# contact_filter <- "Channel"
contact_filter_logic <- str_glue("Contact == '{contact_filter}'")

  if (is_empty(contact_filter_logic)) {
    contact_filter_rlang <- NULL
    alpha_adj <- 0.05
    span_adj  <- 0.01
  } else {
    contact_filter_rlang <- parse_quos(contact_filter_logic, env = caller_env())
    alpha_adj <- 0.40
    span_adj  <- 0.25}


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
