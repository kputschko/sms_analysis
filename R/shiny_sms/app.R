
# Shiny - Message Analysis ------------------------------------------------


# Packages
pacman::p_load(tidyverse, shiny, shinydashboard, scales)


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
      menuItem("Sent",     tabName = "ui_sent", icon = icon("dasboard"))
    )
  ),

  dashboardBody(

    tabItems(

      tabItem(tabName = "ui_dashboard",

              fluidRow(
                infoBox("Total Contacts",   value = overview$Contact_Count, fill = TRUE, width = 3),
                infoBox("Total Messages",   value = overview$Message_Count, fill = TRUE, width = 3),
                infoBox("Total Characters", value = overview$Length_Sum,    fill = TRUE, width = 3)
              ),

              fluidRow(
                infoBox("Earliest Message", value = overview$Date_Min, fill = FALSE, width = 4),
                infoBox("Latest Message",   value = overview$Date_Max, fill = FALSE, width = 4)
              ),

              fluidRow(
                box(plotOutput("overview_plot"),
                    title = "Message Length by Day")
              )


      ),

      tabItem(tabName = "ui_contact"),
      tabItem(tabName = "ui_sent")
    )


  )
)


# Server ------------------------------------------------------------------

server <- function(input, output) {

  output$overview_plot <-
    renderPlot(data_visuals$master_plot_scatter)

}


# Run App -----------------------------------------------------------------

shinyApp(ui = ui, server = server)



