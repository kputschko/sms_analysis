
# Shiny - Message Analysis ------------------------------------------------


# Environment -------------------------------------------------------------

# |- Packages ----
pacman::p_load(tidyverse, rlang, shiny, shinydashboard, scales, plotly, DT)

source("R/fx_sms_read_xml.R")
source("R/fx_sms_sumarise.R")


# |- Helpers ----
fx_sms_app_render_dt <- function(data, yheight = 300) {
  datatable(
    data = data,
    extensions = c('Scroller'),
    options = list(dom = 't',
                   scrollY = yheight,
                   scroller = TRUE,
                   scrollX = TRUE))

}


# Data
# filepath_date <- "2018-08-06"

# When running in Shiny
# data_summary <- str_glue("data/{filepath_date}_summary.rds") %>% read_rds() %>% deframe()
# data_visuals <- str_glue("data/{filepath_date}_visuals.rds") %>% read_rds()

# When running in rstudio
# data_summary <- str_glue("data/{filepath_date}_summary.rds") %>% read_rds() %>% deframe()
# data_visuals <- str_glue("data/{filepath_date}_visuals.rds") %>% read_rds()


# Prepare Data ------------------------------------------------------------

# overview <-
#   data_summary %>%
#   pluck("data_sms_type") %>%
#   filter(MessageType == "All") %>%
#   select(Contact_Count:Length_Sum) %>%
#   mutate_all(comma) %>%
#   bind_cols(
#     data_summary %>%
#       pluck("data_period_contact_day") %>%
#       ungroup() %>%
#       summarise_at(vars(Day), c(Date_Min = "min", Date_Max = "max"))
#   )



# User Interface ----------------------------------------------------------

ui <- dashboardPage(

  dashboardHeader(
    title = "Message Analysis"
  ),

  dashboardSidebar(
    sidebarMenu(
      menuItem("Message Overview",    tabName = "ui_overview", icon = icon("dashboard")),
      menuItem("Messages By Contact", tabName = "ui_contact",   icon = icon("dashboard")),
      menuItem("Messages Sent",       tabName = "ui_sent",      icon = icon("dashboard"))
    )
  ),

  dashboardBody(

    tabItems(

      # |- Overview ----
      tabItem(tabName = "ui_overview",

              fluidRow(

                box(width = 3,
                    uiOutput("overview_path_import_master"),
                    actionButton("overview_action_import_master", "Load Database"),
                    hr(),
                    uiOutput("overview_path_import_xml"),
                    actionButton("overview_action_import_xml", "Load Backup"),
                    hr(),
                    radioButtons("overview_import_anon",
                                 "Anonymize Data",
                                 choiceNames = c("Yes", "No"),
                                 choiceValues = c(TRUE, FALSE),
                                 inline = TRUE),
                    actionButton("overview_action_import_add", "Add Backup to Database")
                ),


                tabBox(width = 9, height = 300,

                       tabPanel("Database Summary",
                                valueBoxOutput("overview_old_contacts", width = 4),
                                valueBoxOutput("overview_old_messages", width = 4),
                                valueBoxOutput("overview_old_length",   width = 4),
                                infoBoxOutput("overview_old_mindate",   width = 6),
                                infoBoxOutput("overview_old_maxdate",   width = 6)
                       ),

                       tabPanel("Backup Summary",
                                valueBoxOutput("overview_new_contacts"),
                                valueBoxOutput("overview_new_messages"),
                                valueBoxOutput("overview_new_length"),
                                infoBoxOutput("overview_new_mindate"),
                                infoBoxOutput("overview_new_maxdate")
                       )

                )
              )


              # fluidRow(
              #   box(plotlyOutput("overview_daily_average"), title = "Average Message Length by Day", width = 12)
              # ),

              # fluidRow(
              #   box(DTOutput("overview_top_contacts"), title = "Top 25 Contacts", width = 4, height = 650),
              #   box(plotlyOutput("overview_length_difference"), title = "Whose Messages are Longer?", width = 8, height = 650)
              # )


      )
    ),

    # |- Contact ----
    tabItem(tabName = "ui_contact"),

    # |- Sent ----
    tabItem(tabName = "ui_sent")
  )
)



# Server ------------------------------------------------------------------

server <- function(input, output) {

  options(shiny.maxRequestSize = 100 * 1024 ^ 2)

  # | Overview ------------------------------------------------------------

  # output$overview_daily_average <-
  #   renderPlotly(data_visuals$overview_plot_daily_average[[1]])

  # output$overview_length_difference <-
  #   renderPlotly(data_visuals$overview_plot_dif_length[[1]] %>% layout(height = 600))

  # output$overview_top_contacts <-
  #   renderDT(expr = data_summary$data_sms_rank %>%
  #              filter(Contact %in% data_visuals$list_top_contacts[[1]]) %>%
  #              arrange(-Length_Sum) %>%
  #              select(-contains("Rank")) %>%
  #              datatable(
  #                height = 600,
  #                extensions = c('Scroller'),
  #                options = list(dom = 't',
  #                               scrollY = 550,
  #                               scroller = TRUE,
  #                               scrollX = TRUE)) %>%
  #              formatRound(2:3, digits = 0) %>%
  #              formatRound(c(4, 6), digits = 2))


  # || List Files
  path_rds_dir <- "data\\"
  path_xml_dir <- str_glue("{Sys.getenv('USERPROFILE')}\\Downloads\\sms_backup\\")


  # || Select Files
  output$overview_path_import_master <- renderUI(
    selectizeInput("path_rds_file",
                   multiple = TRUE,
                   selected = NULL,
                   label = "Master Database",
                   choices = path_rds_dir %>% dir(pattern = "master.rds") %>% sort(TRUE),
                   options = list(maxItems = 1, placeholder = "NULL"))
  )

  output$overview_path_import_xml <- renderUI(
    selectizeInput("path_xml_file",
                   multiple = TRUE,
                   selected = NULL,
                   label = "XML Backup",
                   choices = path_xml_dir %>% dir(pattern = ".xml") %>% sort(TRUE),
                   options = list(maxItems = 1, placeholder = "NULL"))
  )

  # || Load Files
  data_old <-
    eventReactive(input$overview_action_import_master,
                  path_rds_dir %>% str_c(input$path_rds_file) %>% read_rds())

  data_new <-
    eventReactive(input$overview_action_import_xml,
                  path_xml_dir %>% str_c(input$path_xml_file) %>% fx_sms_read_xml())


  # || Summarise Files
  data_old_summary <- reactive(data_old() %>% fx_sms_summarise())
  data_new_summary <- reactive(data_new() %>% fx_sms_summarise())


  # || Display Values
  output$overview_old_contacts <- renderValueBox(valueBox(subtitle = "Total Contacts", value = data_old_summary()$Contacts, color = "light-blue", icon = icon("user", lib = "glyphicon")))
  output$overview_old_messages <- renderValueBox(valueBox(subtitle = "Total Messages", value = data_old_summary()$Messages, color = "light-blue", icon = icon("envelope", lib = "glyphicon")))
  output$overview_old_length   <- renderValueBox(valueBox(subtitle = "Total Characters", value = data_old_summary()$Length, color = "light-blue", icon = icon("font", lib = "glyphicon")))
  output$overview_old_mindate  <- renderInfoBox(infoBox("First Message", value = data_old_summary()$MinDate, fill = TRUE, color = "light-blue", icon = icon("calendar", lib = "glyphicon")))
  output$overview_old_maxdate  <- renderInfoBox(infoBox("Last Message", value = data_old_summary()$MaxDate, fill = TRUE, color = "light-blue", icon = icon("calendar", lib = "glyphicon")))

  output$overview_new_contacts <- renderValueBox(valueBox(subtitle = "Total Contacts",   value = data_new_summary()$Contacts, color = "light-blue", icon = icon("user",     lib = "glyphicon")))
  output$overview_new_messages <- renderValueBox(valueBox(subtitle = "Total Messages",   value = data_new_summary()$Messages, color = "light-blue", icon = icon("envelope", lib = "glyphicon")))
  output$overview_new_length   <- renderValueBox(valueBox(subtitle = "Total Characters", value = data_new_summary()$Length,   color = "light-blue", icon = icon("font",     lib = "glyphicon")))
  output$overview_new_mindate  <- renderInfoBox(infoBox(  title = "First Message",       value = data_new_summary()$MinDate,  color = "light-blue", icon = icon("calendar", lib = "glyphicon"), fill = TRUE))
  output$overview_new_maxdate  <- renderInfoBox(infoBox(  title = "Last Message",        value = data_new_summary()$MaxDate,  color = "light-blue", icon = icon("calendar", lib = "glyphicon"), fill = TRUE))

}


# Run App -----------------------------------------------------------------

shinyApp(ui = ui, server = server)



