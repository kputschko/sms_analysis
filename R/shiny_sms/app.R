
# Shiny - Message Analysis ------------------------------------------------


# Packages
pacman::p_load(tidyverse, shiny, shinydashboard, scales, plotly, DT)


# Data
filepath_date <- "2018-08-06"
filepath_data <- getwd() %>% str_replace("R/shiny_sms", "data")

# When running in Shiny
data_summary <- str_glue("{filepath_data}/{filepath_date}_summary.rds") %>% read_rds() %>% deframe()
data_visuals <- str_glue("{filepath_data}/{filepath_date}_visuals.rds") %>% read_rds()

# When running in rstudio
# data_summary <- str_glue("data/{filepath_date}_summary.rds") %>% read_rds() %>% deframe()
# data_visuals <- str_glue("data/{filepath_date}_visuals.rds") %>% read_rds()


# Prepare Data ------------------------------------------------------------

overview <-
  data_summary %>%
  pluck("data_sms_type") %>%
  filter(MessageType == "All") %>%
  select(Contact_Count:Length_Sum) %>%
  mutate_all(comma) %>%
  bind_cols(
    data_summary %>%
      pluck("data_period_contact_day") %>%
      ungroup() %>%
      summarise_at(vars(Day), c(Date_Min = "min", Date_Max = "max"))
  )



# User Interface ----------------------------------------------------------

ui <- dashboardPage(

  dashboardHeader(
    title = "Message Analysis"
  ),

  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "ui_dashboard", icon = icon("dashboard")),
      menuItem("Contact",  tabName = "ui_contact", icon = icon("dashboard")),
      menuItem("Sent",     tabName = "ui_sent", icon = icon("dashboard"))
    )
  ),

  dashboardBody(

    tabItems(

      tabItem(tabName = "ui_dashboard",

              fluidRow(
                infoBox("Total Contacts",   value = overview$Contact_Count, fill = TRUE),
                infoBox("Total Messages",   value = overview$Message_Count, fill = TRUE),
                infoBox("Total Characters", value = overview$Length_Sum,    fill = TRUE)
              ),

              fluidRow(
                infoBox("Earliest Message", value = overview$Date_Min, fill = FALSE, width = 6),
                infoBox("Latest Message",   value = overview$Date_Max, fill = FALSE, width = 6)
              ),

              fluidRow(
                box(plotlyOutput("overview_daily_average"), title = "Average Message Length by Day", width = 12)
              ),

              fluidRow(
                box(DTOutput("overview_top_contacts"), title = "Top 25 Contacts", width = 4, height = 650),
                box(plotlyOutput("overview_length_difference"), title = "Whose Messages are Longer?", width = 8, height = 650)
              )


      ),

      tabItem(tabName = "ui_contact"),
      tabItem(tabName = "ui_sent")
    )


  )
)


# Server ------------------------------------------------------------------

server <- function(input, output) {

  output$overview_daily_average <-
    renderPlotly(data_visuals$overview_plot_daily_average[[1]])

  output$overview_length_difference <-
    renderPlotly(data_visuals$overview_plot_dif_length[[1]] %>% layout(height = 600))

  output$overview_top_contacts <-
    renderDT(expr = data_summary$data_sms_rank %>%
               filter(Contact %in% data_visuals$list_top_contacts[[1]]) %>%
               arrange(-Length_Sum) %>%
               select(-contains("Rank")) %>%
               datatable(
                 height = 600,
                 extensions = c('Scroller'),
                 options = list(dom = 't',
                                scrollY = 550,
                                scroller = TRUE,
                                scrollX = TRUE)) %>%
               formatRound(2:3, digits = 0) %>%
               formatRound(c(4, 6), digits = 2))


}


# Run App -----------------------------------------------------------------

shinyApp(ui = ui, server = server)



