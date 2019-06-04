
# Shiny - Message Analysis ------------------------------------------------


# Environment -------------------------------------------------------------

# |- Packages ----
pacman::p_load(tidyverse, rlang, scales,
               tidytext, topicmodels, widyr,
               shiny, shinydashboard, plotly, DT)

source("R/fx_sms_read_xml.R")
source("R/fx_sms_sumarise.R")
source("R/fx_sms_append.R")
source("R/fx_sms_prepare.R")
source("R/fx_sms_helpers.R")



# User Interface ----------------------------------------------------------

ui <- dashboardPage(

  dashboardHeader(
    title = "Message Analysis"
  ),

  dashboardSidebar(
    sidebarMenu(
      menuItem("Message Overview",    tabName = "ui_overview", icon = icon("dashboard")),
      menuItem("Messages By Contact", tabName = "ui_contact",  icon = icon("dashboard")),
      menuItem("Messages Sent",       tabName = "ui_sent",     icon = icon("dashboard"))
    )
  ),

  dashboardBody(

    tabItems(

      # |- Overview ----
      tabItem(tabName = "ui_overview",

              fluidRow(

                box(width = 3, status = "primary",
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
                    actionButton("overview_action_append", "Add Backup to Database"),
                    actionButton("overview_action_export", "Export Database")
                ),


                tabBox(width = 9, height = 300,

                       tabPanel("Debug", verbatimTextOutput("debug")),

                       tabPanel("Database Summary",
                                valueBoxOutput("overview_master_contacts", width = 4),
                                valueBoxOutput("overview_master_messages", width = 4),
                                valueBoxOutput("overview_master_length",   width = 4),
                                infoBoxOutput("overview_master_mindate",   width = 6),
                                infoBoxOutput("overview_master_maxdate",   width = 6)
                       ),

                       tabPanel("Backup Summary",
                                valueBoxOutput("overview_new_contacts"),
                                valueBoxOutput("overview_new_messages"),
                                valueBoxOutput("overview_new_length"),
                                infoBoxOutput("overview_new_mindate"),
                                infoBoxOutput("overview_new_maxdate")
                       )
                )
              ),

              fluidRow(
                box(width = 12,
                    title = "Message Length by Day",
                    footer = "Color: Contacts per Day - Size: Messages per Day",
                    plotlyOutput("overview_scatter", height = 400)
                )
              )
      ),

      # |- Contact ----
      tabItem(tabName = "ui_contact",

              fluidRow(
                tabBox(title = strong("Top 25 Contacts"), side = "right", width = 12, selected = "Message Count",
                       tabPanel("Messages per Day", plotlyOutput("overview_bar_mpd")),
                       tabPanel("Days of Contact",  plotlyOutput("overview_bar_daycnt")),
                       tabPanel("Average Length",   plotlyOutput("overview_bar_avglen")),
                       tabPanel("Message Length",   plotlyOutput("overview_bar_length")),
                       tabPanel("Message Count",    plotlyOutput("overview_bar_count"))
                )
              ),

              fluidRow(
                box(width = 3,
                    title = strong("Select Contact"),
                    uiOutput("contact_list"),
                    DTOutput("contact_summary")),

                tabBox(width = 9,
                       tabPanel(title = "Contact Timeline", plotlyOutput("contact_timeline")),
                       tabPanel(title = "Initial Messages", plotlyOutput("contact_initial")),
                       tabPanel(title = "Word Sentiment - Frequency", plotlyOutput("contact_sentiment_freq"))
                )
              ),

              fluidRow(
                box(width = 8,
                    title = strong("Whose Messages Are Longer?"),
                    plotlyOutput("overview_plot_diff", height = 450),
                    footer = em("Size: Days of Contact")),
                box(width = 4,
                    title = strong("Changes in Message Length/Frequency in Last 90 Days"),
                    footer = em(HTML("Q1: Increase in both length and frequency<br>Q3: Decrease in both length and frequency")),
                    plotlyOutput("contact_adjustment", height = 400))

              )
      ),

      # |- Sent ----
      tabItem(tabName = "ui_sent")

    ) # close tabItems
  ) # close dashboardBody
) # close dashboardPage




# Server ------------------------------------------------------------------

server <- function(input, output) {

  # | Overview ------------------------------------------------------------

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
  data_master <-
    eventReactive(input$overview_action_import_master,
                  path_rds_dir %>% str_c(input$path_rds_file) %>% read_rds())

  data_new <-
    eventReactive(input$overview_action_import_xml,
                  path_xml_dir %>% str_c(input$path_xml_file) %>% fx_sms_read_xml())


  # || Summarise Files
  data_master_summary <- reactive(data_master() %>% fx_sms_summarise())
  data_new_summary <- reactive(data_new() %>% fx_sms_summarise())


  # || Display Values
  output$overview_master_contacts <- renderValueBox(valueBox(subtitle = "Total Contacts", value = data_master_summary()$Contacts, color = "light-blue", icon = icon("user", lib = "glyphicon")))
  output$overview_master_messages <- renderValueBox(valueBox(subtitle = "Total Messages", value = data_master_summary()$Messages, color = "light-blue", icon = icon("envelope", lib = "glyphicon")))
  output$overview_master_length   <- renderValueBox(valueBox(subtitle = "Total Characters", value = data_master_summary()$Length, color = "light-blue", icon = icon("font", lib = "glyphicon")))
  output$overview_master_mindate  <- renderInfoBox(infoBox("First Message", value = data_master_summary()$MinDate, fill = TRUE, color = "light-blue", icon = icon("calendar", lib = "glyphicon")))
  output$overview_master_maxdate  <- renderInfoBox(infoBox("Last Message", value = data_master_summary()$MaxDate, fill = TRUE, color = "light-blue", icon = icon("calendar", lib = "glyphicon")))

  output$overview_new_contacts <- renderValueBox(valueBox(subtitle = "Total Contacts",   value = data_new_summary()$Contacts, color = "light-blue", icon = icon("user",     lib = "glyphicon")))
  output$overview_new_messages <- renderValueBox(valueBox(subtitle = "Total Messages",   value = data_new_summary()$Messages, color = "light-blue", icon = icon("envelope", lib = "glyphicon")))
  output$overview_new_length   <- renderValueBox(valueBox(subtitle = "Total Characters", value = data_new_summary()$Length,   color = "light-blue", icon = icon("font",     lib = "glyphicon")))
  output$overview_new_mindate  <- renderInfoBox(infoBox(  title = "First Message",       value = data_new_summary()$MinDate,  color = "light-blue", icon = icon("calendar", lib = "glyphicon"), fill = TRUE))
  output$overview_new_maxdate  <- renderInfoBox(infoBox(  title = "Last Message",        value = data_new_summary()$MaxDate,  color = "light-blue", icon = icon("calendar", lib = "glyphicon"), fill = TRUE))


  # || Join Database + New
  data_append <-
    eventReactive(input$overview_action_append, {
      if (data_master() %>% is.data.frame() %>% is_false() || data_new() %>% is.data.frame() %>% is_false()) {
        abort("Please specify both a Database and a Backup file to be joined")
      } else {
        fx_sms_append(data_xml = data_new(), data_master = data_master())
      }

    })


  # || Export Data
  observeEvent(input$overview_action_export, {
    try(silent = TRUE, {

      export_backup_date <-
        input$path_xml_file %>%
        word(-1, sep = "/") %>%
        word(1) %>%
        str_remove("sms-")

      path_export_master <- str_glue("data/{export_backup_date}_master.rds")
      path_export_new    <- str_glue("data/{export_backup_date}_new.rds")

      data_append() %>% write_rds(path_export_master, compress = "bz")
      data_new() %>% write_rds(path_export_new, compress = "bz")
    })
  })


  # || Anonymous
  data_master_anon <- reactive({
    if (input$overview_import_anon %>% parse_expr() %>% is_false()) {
      data_master()
    } else {

      data_anon <-
        read_csv("data/anon_id.csv",
                 col_types = cols(id = col_integer(), animal = col_character()))

      data_master() %>%
        arrange(DateTime) %>%
        distinct(Contact) %>%
        rowid_to_column("id") %>%
        left_join(data_anon, by = "id") %>%
        left_join(data_master(), by = "Contact") %>%
        select(Contact = animal, DateTime, MessageType, Message, MessageLength)
    }
  })


  # |- Data Summaries ----
  data_summaries <- reactive(
    data_master_anon() %>%
      fx_sms_prepare() %>%
      deframe()
  )

  # |- Plot Overview Scatter ----
  output$overview_scatter <- renderPlotly({

    plot_overview_scatter <-
      data_summaries() %>%
      pluck("sms_day") %>%

      ggplot() +
      aes(x = Day, y = Length_Sum, size = Message_Count, color = Contact_Count) +
      geom_point(alpha = 0.75) +
      scale_color_viridis_c() +
      labs(y = NULL, x = NULL, color = NULL, size = NULL) +
      .plot_theme

    ggplotly(plot_overview_scatter)

  })


  data_top <- reactive({
    data_summaries() %>%
      pluck("sms_rank") %>%
      top_n(n = 25, wt = -Rank_Score) %>%
      arrange(Rank_Score) %>%
      select(-Rank_Score) %>%
      mutate_at("Contact", as_factor) %>%
      mutate(display = str_glue("Message Count: {comma(Message_Count)}\nLength Sum:{comma(Length_Sum)}\nAverage Length: {comma(Length_Avg)}\nDays of Contact: {comma(Contact_Days)}\nMessages per Day: {comma(Messages_per_Day)}"))
  })


  # |- Plot Overview Bar ----
  .fx_plot_bar <- function(data = data_top(), y) {
    plot_ly(data = data, x = ~ Contact, y = ~ get(y), text = ~display,
            marker = list(line = list(color = "black", width = 1.5)),
            type = "bar") %>%
      layout(title = NULL, xaxis = list(title = ""), yaxis = list(title = ""))
  }

  output$overview_bar_mpd    <- renderPlotly({.fx_plot_bar(y = "Messages_per_Day")})
  output$overview_bar_count  <- renderPlotly({.fx_plot_bar(y = "Message_Count")})
  output$overview_bar_length <- renderPlotly({.fx_plot_bar(y = "Length_Sum")})
  output$overview_bar_avglen <- renderPlotly({.fx_plot_bar(y = "Length_Avg")})
  output$overview_bar_daycnt <- renderPlotly({.fx_plot_bar(y = "Contact_Days")})


  # |- Plot Overview Difference ----
  output$overview_plot_diff <- renderPlotly({

    plot_overview_diff <-
      data_summaries() %>%
      pluck("sms_diff") %>%
      filter(Contact %in% data_top()$Contact) %>%
      ggplot() +
      aes(x = Contact) +
      geom_linerange(aes(ymin = Q1, ymax = Q3, color = `Longer Messages`)) +
      geom_point(aes(y = Median,
                     size = `Days of Contact`,
                     fill = `Longer Messages`),
                 color = "black",
                 shape = 21) +
      geom_hline(aes(yintercept = 0)) +
      labs(x = NULL, color = NULL, y = "Median Difference") +
      guides(color = FALSE, fill = FALSE, size = FALSE) +
      .plot_theme +
      theme(panel.grid.major.x = element_blank(),
            panel.grid.minor = element_line(linetype = 3),
            axis.text.x = element_text(angle = -35))

    plot_overview_diff %>% ggplotly()

  })


  # | Contact ---------------------------------------------------------------

  output$contact_list <- renderUI(
    selectizeInput("filter_contact",
                   label = NULL,
                   selected = NULL,
                   multiple = TRUE,
                   choices = data_top()$Contact,
                   options = list(maxItems = 1, placeholder = "Top 25 Contacts"))
  )


  output$contact_summary <- renderDT({

    if (input$filter_contact %>% is_null()) {
      return(NULL)
    } else {

      data_summaries() %>%
        pluck("sms_contact_type") %>%
        ungroup() %>%
        filter(Contact == input$filter_contact) %>%
        select(-Contact) %>%
        mutate_if(is.numeric, comma) %>%
        mutate_if(is.Date, as.character) %>%
        gather(Measure, Value, -MessageType) %>%
        spread(MessageType, Value) %>%
        fx_sms_app_render_dt()
    }
  })


  output$contact_timeline <- renderPlotly({

    if (input$filter_contact %>% is_null()) {
      return(NULL)
    } else {


      plot_timeline <-
        data_summaries() %>%
        pluck("sms_week_contact") %>%
        filter(Contact == input$filter_contact) %>%
        ggplot() +
        aes(x = Week,
            ymin = Minimum,
            ymax = Maximum,
            y = Median,
            n = Count,
            color = Median) +
        geom_linerange(size = 2) +
        scale_color_viridis_c(direction = -1, option = "D", end = 1, begin = 0.30) +
        labs(y = NULL, x = NULL, color = "Median\nMessage\nLength", title = "Range of Message Lengths") +
        .plot_theme_dark

      ggplotly(plot_timeline)

    }

  })


  output$contact_initial <- renderPlotly({
    if (input$filter_contact %>% is_null()) {
      return(NULL)
    } else {

      plot_initial <-
        data_summaries() %>%
        pluck("sms_initial_hour") %>%
        filter(Contact == input$filter_contact) %>%
        mutate(MessageType = if_else(MessageType == "Sent", "Me", "Them")) %>%

        ggplot() +
        aes(x = Hour, fill = MessageType, color = MessageType) +
        geom_density(alpha = 0.50) +
        labs(y = NULL, fill = NULL, color = NULL,
             x = "Time of Day",
             title = "Who Sends the First Message?") +
        scale_x_continuous(breaks = seq(0, 24, by = 6)) +
        scale_fill_manual(values = c("Me" = .plot_colors$Sent, "Them" = .plot_colors$Received)) +
        .plot_theme +
        theme(axis.text.y = element_blank())

      ggplotly(plot_initial, tooltip = c("x", "y"))

    }
  })


  # |- Contact: Sentiment ----
  contact_sentiment_plot <- reactive({
    if (input$filter_contact %>% is_null()) {return(NULL)} else {

      sentiment_data <-
        data_summaries() %>%
        pluck("sms_tidytext") %>%
        filter(Contact == input$filter_contact) %>%
        add_count(MessageType) %>%
        inner_join(sentiment_label) %>%
        group_by(sentiment, MessageType) %>%
        summarise(total_words = unique(n),
                  total_sentiment = length(n),
                  p_sentiment = total_sentiment / total_words) %>%
        ggplot() +
        aes(x = reorder(sentiment, p_sentiment, sum), y = p_sentiment, fill = MessageType) +
        coord_flip() +
        scale_y_continuous(labels = percent_format()) +
        .plot_theme
    }
  })

  output$contact_sentiment_freq <- renderPlotly({plot_sentiment_basic + geom_col()})
  output$contact_sentiment_prop <- renderPlotly({plot_sentiment_basic + geom_col(position = "fill") + geom_hline(yintercept = 0.50)})


  # |- Contact: 90d Adj ----
  output$contact_adjustment <- renderPlotly({

    plot_adjustment <-
      data_summaries() %>%
      pluck("sms_period_adjustment") %>%
      filter(Contact %in% data_top()$Contact) %>%
      ggplot() +
      aes(x = change_length,
          y = change_frequency,
          text = str_glue("Contact: {Contact}\nLength Change: {change_length %>% number(accuracy = 0.01, suffix = 'x')}\nFrequency Change: {change_frequency %>% number(accuracy = 0.01, suffix = 'x')}")) +
      geom_jitter(width = 0.025, height = 0.025, color = "#3182bd") +
      geom_hline(yintercept = 1) +
      geom_vline(xintercept = 1) +
      labs(x = "Daily Message Length",
           y = "Daily Contact Frequency") +
      .plot_theme


    plot_adjustment %>% ggplotly(tooltip = "text")


  })

  # | Sent ------------------------------------------------------------------




  # || Debug
  output$debug <- renderPrint({
    list(append_value = input$overview_action_append,
         append_table = try(nrow(data_append())),
         anon_value = input$overview_import_anon,
         anon_check = try(data_master_anon()$Contact[[1]]),
         import_check = try(input$path_rds_file),
         export_check = try(export_backup_date, silent = TRUE),
         select_conta = input$filter_contact,
         select_table = try(nrow(data_summaries() %>% pluck("sms_week_contact")))
    )
  })

}


# Run App -----------------------------------------------------------------

str_glue("Starting Shiny App at {Sys.time()}") %>% inform()


shinyApp(ui = ui, server = server)



