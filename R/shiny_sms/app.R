
# Shiny - Message Analysis ------------------------------------------------


# Packages
pacman::p_load(tidyverse, shiny, shinydashboard, scales, plotly)


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
                infoBox("Earliest Message", value = overview$Date_Min, fill = FALSE, width = 3),
                infoBox("Latest Message",   value = overview$Date_Max, fill = FALSE, width = 3)
              ),

              fluidRow(
                box(plotlyOutput("overview_daily_average"), title = "Average Message Length by Day")
              ),

              fluidRow(
                # box(dataTableOutput("overview_3"), title = "Top Contacts", width = 3),
                box(tableOutput("overview_3"), title = "Top Contacts"),
                box(plotOutput("overview_2"), title = "Average Difference in Message Length")
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

  output$overview_2 <-
    renderPlot(data_visuals$overview_plot_dif_length[[1]])

  output$overview_3 <-
    # renderDataTable(data_summary$data_sms_rank %>% filter(Contact %in% data_visuals$list_top_contacts[[1]]))
    renderTable(data_summary$data_sms_rank %>% filter(Contact %in% data_visuals$list_top_contacts[[1]]))


}


# Run App -----------------------------------------------------------------

shinyApp(ui = ui, server = server)



