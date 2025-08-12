library(shiny)
library(shinydashboard)
library(dashboardthemes)
library(plotly)
library(DT)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(scales)
library(shinyjs)
library(shinyalert)
library(rsconnect)
options(shiny.maxRequestSize = 100*1024^2)

`%||%` <- function(a, b) {
  if (!is.null(a)) a else b
}

tags$head(
  tags$script(src = "https://html2canvas.hertzen.com/dist/html2canvas.min.js"),
  tags$script(HTML("
    Shiny.addCustomMessageHandler('downloadPageAsImage', function(message) {
      html2canvas(document.body).then(function(canvas) {
        var link = document.createElement('a');
        link.download = 'dashboard_snapshot.png';
        link.href = canvas.toDataURL();
        link.click();
      });
    });
  "))
)

detect_percentage_cols <- function(df) {
  sapply(df, function(x) {
    is.character(x) && all(ifelse(is.na(x), TRUE, grepl("%$", x)))
  })
}

nepali_formatter <- function(x) {
  sapply(x, function(y) {
    if (is.numeric(y) && abs(y) >= 1e9) paste0(round(y / 1e9, 2), " arba")
    else comma(y)
  })
}

# ===== UI =====
ui <- dashboardPage(
  dashboardHeader(
    title = tagList(
      tags$div(
        style = "display: flex; align-items: left;",
        tags$img(
          src = "nepal_life_logo.png",
          height = "80px",
          width = "110px"
        ),
         tags$span("Nepal Life Insurance Analytics Dashboard")
      )
    )
    
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Upload Data", tabName = "data", icon = icon("upload")),
      menuItem("Company Dashboard", tabName = "dashboard", icon = icon("building")),
      menuItem("Company Dashboard (Advanced)", tabName = "dashboard_adv", icon = icon("building")),
      menuItem("Metric Comparison", tabName = "comparison", icon = icon("chart-line")),
      menuItem("Between Companies", tabName = "between", icon = icon("project-diagram")),
      menuItem("Risk Analytics", icon = icon("exclamation-triangle"),
               menuSubItem("Individual Risk Dashboard", tabName = "risk_individual"),
               menuSubItem("Between Companies Risk Dashboard", tabName = "risk_between"),
               menuSubItem("Combined Risk View", tabName = "risk_combined"),
               menuSubItem("Interactive Risk Chart", tabName = "interactive_risk")
               
      ),
      menuItem("Notice Board", tabName = "data1", icon = icon("trash")),
      menuItem("Theme Selector", tabName = "Theme_Selector",icon = icon("palette")),
      menuItem("About Me", tabName = "info",icon = icon("info-circle"))#,
      #menuItem("Download Analysis Report", tabName = "download_report", icon = icon("palette"))
      
    ),
    sidebarMenu(
      tags$br(),
      tags$div(
        class = "sidebar-footer",
        tags$a(href = "your_twitter_link", icon("twitter")),
        tags$a(href = "your_facebook_link", icon("facebook")),
        tags$a(href = "https://www.linkedin.com/in/dipesh-guragain-186a4223a?utm_source=share&utm_campaign=share_via&utm_content=profile&utm_medium=android_app", icon("linkedin")),
        tags$a(href = "https://github.com/Dipesh9871", icon("github")),
        tags$a(href = "https://openai.com/index/chatgpt/", icon("robot"), " ChatGPT")
      )
      
    )
    
  ),
  dashboardBody(
    useShinyjs(),
    shinyDashboardThemes(theme = "blue_gradient"),
    
    tags$script(src = "https://html2canvas.hertzen.com/dist/html2canvas.min.js"),
    tags$script(HTML("
  Shiny.addCustomMessageHandler('capturePlotAsImage', function(message) {
    var plotElement = document.getElementById('risk_plot');
    html2canvas(plotElement).then(function(canvas) {
      var link = document.createElement('a');
      link.download = 'interactive_risk_plot.png';
      link.href = canvas.toDataURL();
      link.click();
    });
  });
")),
    tags$head(
      tags$style(HTML("
    .box { box-shadow: 2px 2px 6px #999; }
    .box-title { font-weight: bold; }
    ul { padding-left: 20px; }
    li { margin-bottom: 6px; }
  "))
    ),
    
    
    tags$head(
      tags$style(HTML("
        .main-sidebar {
          overflow: visible; /* ensure children like fixed footer are visible */
        }
        .sidebar-footer-fixed {
          position: fixed;
          bottom: 20px;
          right: 20px;
          z-index: 1000;
        }
        .sidebar-footer-fixed a {
          margin-left: 6px;
          color: #FFFFFF;
        }
      "))
    ),
    
    tags$head(tags$style(HTML("
      #running_message {
        background-color:#003366; 
        color:white; 
        font-weight:bold; 
        padding:5px; 
        font-size:16px;
        overflow: hidden;
        white-space: nowrap;
        box-sizing: border-box;
      }
      #running_message span {
        display: inline-block;
        padding-left: 100%;
        animation: scroll-left 15s linear infinite;
      }
      @keyframes scroll-left {
        0% { transform: translateX(0%); }
        100% { transform: translateX(-100%); }
      }
    "))),
    
    tags$div(id = "running_message", uiOutput("marquee_text")),
    
    
    tabItems(
      
      tabItem(tabName = "interactive_risk",
              fluidRow(
                box(title = "Controls", status = "primary", width = 4, collapsible = TRUE,
                    fileInput("file", "Upload Excel File", accept = ".xlsx"),
                    uiOutput("sheet_select"),
                    checkboxInput("normalize", "Normalize (Stack by %)", FALSE),
                    checkboxInput("detect_ratios", "Auto-overlay %/ratios", TRUE),
                    uiOutput("company_select_ui"),     # <- New UI for company selection
                    uiOutput("var_select_ui"),
                    textInput("custom_x", "X-axis Label", ""),
                    textInput("custom_y", "Y-axis Label", ""),
                    textInput("plot_title", "Chart Title", ""),
                    actionButton("fullscreen", "🔲 Toggle Fullscreen"),
                    actionButton("download_snapshot1", "📷 Download Snapshot (PNG)"),
                    verbatimTextOutput("summary")
                ),
                box(title = "Interactive Risk Plot", status = "success", width = 8,collapsible = TRUE,
                    plotlyOutput("risk_plot", height = "600px"))
              )
      ),
      
      
      tabItem(tabName = "data",
              fluidRow(
                box(title = "Upload Excel File", status = "primary", width = 6,collapsible = TRUE,
                    fileInput("file", "Choose Excel File", accept = ".xlsx"),
                    radioButtons("table_format", "Number Format in Table:",
                                 choices = c("Nepali Format" = "nepali", "Comma Only" = "comma"),
                                 selected = "nepali", inline = TRUE),
                    uiOutput("sheet_select"))
              ),
              
              DTOutput("raw_table")
      ),
      tabItem(
        tabName = "Theme_Selector",
        fluidPage(
          titlePanel(HTML('<h style="font-size: 18px; color: #ff0000;">SHINY THEMES</h4>')),
          
          shinythemes::themeSelector()
        )
      ),
      tabItem(tabName = "info",
              fluidRow(
                box(
                  title = HTML("<b style='color:#1f77b4;'>📌 Overview of the Application</b>"),
                  status = "primary", solidHeader = TRUE, width = 12,
                  HTML("
            <p style='font-size:16px;'>
              This Shiny application serves as an <b>interactive dashboard</b> for visualizing and comparing key insurance metrics 
              across various Nepali insurance companies. Developed with <code>R</code>, it integrates dynamic plotting, 
              risk analysis, and dashboard interactivity to provide users with insightful analytics.
            </p>
          ")
                )
              ),
              
              fluidRow(
                box(
                  title = HTML("<b style='color:#2ca02c;'>🛠️ Features & Functional Layout</b>"),
                  status = "success", solidHeader = TRUE, width = 12,
                  HTML("
            <ul style='font-size:16px;'>
              <li><b>Upload Data:</b> Import Excel files dynamically and choose sheet names.</li>
              <li><b>Company Dashboard (Basic & Advanced):</b> View individual charts and metrics per company.</li>
              <li><b>Metric & Risk Comparison:</b> Compare companies using different chart types (bar, box, pie, stacked, etc.).</li>
              <li><b>Interactive Controls:</b> Customize titles, axis labels, chart types, and themes.</li>
              <li><b>Snapshot Export:</b> Download high-quality PNG images of plots or entire dashboard sections.</li>
              <li><b>Notice Board:</b> Post, update, and delete custom messages with persistent CSV storage.</li>
              <li><b>Marquee Text:</b> Real-time scrolling message bar editable by the user.</li>
              <li><b>Theme Selector:</b> Toggle between light/dark/colored UI themes.</li>
            </ul>
          ")
                )
              ),
              
              fluidRow(
                box(
                  title = HTML("<b style='color:#d62728;'>🔍 Audit Trail Summary</b>"),
                  status = "danger", solidHeader = TRUE, width = 12,
                  HTML("
            <ul style='font-size:16px;'>
              <li><b>File Uploads:</b> Captured using <code>fileInput()</code> with data validated and previewed.</li>
              <li><b>Dynamic Plots:</b> Created using <code>ggplot2</code> + <code>plotly</code> for interactivity.</li>
              <li><b>Notice Logging:</b> Every post is recorded to CSV for audit and persistence.</li>
              <li><b>Snapshot Capture:</b> Uses JavaScript <code>html2canvas</code> to export visual content.</li>
              <li><b>Reactive Programming:</b> All inputs and outputs are modular and responsive to changes.</li>
            </ul>
          ")
                )
              ),
              
              fluidRow(
                box(
                  title = HTML("<b style='color:#9467bd;'>📌 Limitations & Considerations</b>"),
                  status = "warning", solidHeader = TRUE, width = 12,
                  HTML("
            <ul style='font-size:16px;'>
              
              <li>Snapshot export may be browser-sensitive depending on JavaScript behavior.</li>
              <li>Session logs are not fully persisted beyond notice updates.</li>
            </ul>
          ")
                )
              ),
              
              fluidRow(
                box(
                  title = HTML("<b style='color:#8c564b;'>🎯 Conclusion</b>"),
                  status = "info", solidHeader = TRUE, width = 12,
                  HTML("
            <p style='font-size:16px;'>
              This application provides a <b>user-friendly platform</b> for exploring Nepalese insurance data 
              with robust analytics and visualization. It helps users to make informed decisions, 
              backed by interactive dashboards and structured audit trails.
            </p>
          ")
                )
              )
      ),
      
      tabItem(tabName = "data1",
              
              fluidRow(
                box(title = "Update Running Message", status = "info", width = 6,collapsible = TRUE,
                    textInput("running_message_input", "Enter Scrolling Message:", "Welcome to Nepal Life Insurance Analytics Dashboard"),
                    actionButton("update_marquee", "Update Message")
                )
              ),
              fluidRow(
                box(title = "Notice Board", status = "danger", width = 12,collapsible = TRUE,
                    textInput("new_notice", "Add New Notice"),
                    actionButton("add_notice", "Add Notice"),
                    DTOutput("notice_table"),
                    actionButton("delete_notice", "Delete Selected Notice")
                )
              ),
              
      ),
      
      # ==== COMPANY DASHBOARD ====
      tabItem(tabName = "dashboard",
              fluidRow(
                box(title = "Select Company", status = "primary", width = 4,collapsible = TRUE,
                    radioButtons("table_format", "Number Format in Table:",
                                 choices = c("Nepali Format" = "nepali", "Comma Only" = "comma"),
                                 selected = "nepali", inline = TRUE),
                    uiOutput("company_select")),
                box(title = "Company Overview", status = "success", width = 8,collapsible = TRUE,
                    plotlyOutput("company_plot", height = "350px"))
              ),
              fluidRow(
                box(title = "Company Data Table", status = "warning", width = 12,collapsible = TRUE,
                    DTOutput("company_table"))
              )
      ),
      tabItem(tabName = "dashboard_adv",
              fluidRow(
                box(title = "Select Company", status = "primary", width = 4, collapsible = TRUE,
                    radioButtons("table_format", "Number Format in Table:",
                                 choices = c("Nepali Format" = "nepali", "Comma Only" = "comma"),
                                 selected = "nepali", inline = TRUE),
                    uiOutput("company_var_select")   
                ),
                box(title = "Company Overview", status = "success", width = 8, collapsible = TRUE,
                    plotlyOutput("company_plot_adv", height = "350px"))
              ),
              fluidRow(
                box(title = "Company Data Table", status = "warning", width = 12, collapsible = TRUE,
                    DTOutput("company_table"))
              )
      ),
      
      
      
      tabItem(tabName = "comparison",
              fluidRow(
                box(title = "Select Metric for Comparison", status = "primary", width = 12,collapsible = TRUE,
                    radioButtons("table_format", "Number Format in Table:",
                                 choices = c("Nepali Format" = "nepali", "Comma Only" = "comma"),
                                 selected = "nepali", inline = TRUE),
                    uiOutput("metric_select"),
                    plotlyOutput("comparison_plot", height = "400px"))
              ),
              fluidRow(
                box(title = "Data Table (Metric Comparison)", status = "info", width = 12,collapsible = TRUE,
                    DTOutput("comparison_table"))
              )
      ),
      
      
      tabItem(tabName = "between",
              fluidRow(
                box(title = "Select Metrics", status = "primary", width = 12,collapsible = TRUE,
                    radioButtons("table_format", "Number Format in Table:",
                                 choices = c("Nepali Format" = "nepali", "Comma Only" = "comma"),
                                 selected = "nepali", inline = TRUE),
                    uiOutput("multi_metric_select"),
                    plotlyOutput("between_plot", height = "450px"))
              ),
              fluidRow(
                box(title = "Between Company Table", status = "info", width = 12,collapsible = TRUE,
                    DTOutput("between_table"))
              )
      ),
      
      
      tabItem(tabName = "risk_individual",
              fluidRow(
                box(title = "Individual Risk Controls", status = "primary", width = 4,collapsible = TRUE,
                    selectInput("plot_type_ind", "Plot Type:",
                                c("Bar (Grouped)", "Bar (Stacked)", "Pie", "Box Plot")),
                    conditionalPanel(
                      condition = "input.plot_type_ind == 'Pie'",
                      uiOutput("company_pie_select")
                    ),
                    checkboxInput("normalize_ind", "Normalize by Total (Percent)", FALSE),
                    textInput("title_ind", "Plot Title", "Individual Risk Dashboard"),
                    textInput("xlab_ind", "X-axis Label", "Company"),
                    textInput("ylab_ind", "Y-axis Label", "Value (NPR)"),
                    radioButtons("table_format", "Number Format in Table:",
                                 choices = c("Nepali Format" = "nepali", "Comma Only" = "comma"),
                                 selected = "nepali", inline = TRUE)
                    
                ),
                box(title = "Individual Risk Visualization", status = "success", width = 8,collapsible = TRUE,
                    plotlyOutput("plot_ind", height = "500px"))
              ),
              fluidRow(
                box(title = "Individual Risk Data Table", status = "info", width = 12,collapsible = TRUE,
                    DTOutput("table_ind"))
              )
      ),
      
      tabItem(tabName = "risk_between",
              fluidRow(
                box(title = "Between Risk Controls", status = "primary", width = 4,collapsible = TRUE,
                    selectInput("plot_type_bet", "Plot Type:",
                                c("Bar (Grouped)", "Bar (Stacked)", "Pie", "Box Plot")),
                    conditionalPanel(
                      condition = "input.plot_type_bet == 'Pie'",
                      uiOutput("company_pie_select_bet")
                    )
                    ,
                    checkboxInput("normalize_bet", "Normalize by Total (Percent)", FALSE),
                    textInput("title_bet", "Plot Title", "Between Companies Risk Dashboard"),
                    textInput("xlab_bet", "X-axis Label", "Company"),
                    textInput("ylab_bet", "Y-axis Label", "Value (NPR)"),
                    radioButtons("table_format", "Number Format in Table:",
                                 choices = c("Nepali Format" = "nepali", "Comma Only" = "comma"),
                                 selected = "nepali", inline = TRUE)
                    
                ),
                box(title = "Between Risk Visualization", status = "success", width = 8,collapsible = TRUE,
                    plotlyOutput("plot_bet", height = "500px"))
              ),
              fluidRow(
                box(title = "Between Risk Data Table", status = "info", width = 12,collapsible = TRUE,
                    DTOutput("table_bet"))
              )
      ),
      tabItem(
        tabName = "download_report",
        h2("Download Full Analysis Report"),
        downloadButton("download_report", "Download Report (HTML)")
      )
      ,
      
      tabItem(tabName = "risk_combined",
              fluidRow(
                box(title = "Combined Risk Controls", status = "primary", width = 4,collapsible = TRUE,
                    selectInput("plot_type_comb", "Plot Type:",
                                c("Bar (Grouped)", "Bar (Stacked)", "Pie", "Box Plot")),
                    checkboxInput("normalize_comb", "Normalize by Total (Percent)", FALSE),
                    textInput("title_comb", "Plot Title", "Combined Risk View"),
                    textInput("xlab_comb", "X-axis Label", "Company"),
                    textInput("ylab_comb", "Y-axis Label", "Value (NPR)"),
                    radioButtons("table_format", "Number Format in Table:",
                                 choices = c("Nepali Format" = "nepali", "Comma Only" = "comma"),
                                 selected = "nepali", inline = TRUE)
                    
                ),
                box(title = "Combined Risk Visualization", status = "success", width = 8,collapsible = TRUE,
                    plotlyOutput("plot_comb", height = "500px"))
              ),
              fluidRow(
                box(title = "Combined Risk Data Table", status = "info", width = 12,collapsible = TRUE,
                    DTOutput("table_comb"))
              )
      )
    )
  )
)


server <- function(input, output, session) {
  data_list <- reactiveVal(list())
  active_data <- reactiveVal(NULL)
  
  
  marquee_message <- reactiveVal("Welcome to Nepal Life Insurance Analytics Dashboard |||नेपाल लाइफद्वारा रु. १० अर्ब प्रथम बीमा शुल्क आर्जन गर्दै रेकर्ड कायम")
  
  observeEvent(input$update_marquee, {
    marquee_message(input$running_message_input)
  })
  
  output$marquee_text <- renderUI({
    HTML(paste0("<span>📢 ", marquee_message(), "</span>"))
  })
  
  notices <- reactiveVal(data.frame(Notice = character(), stringsAsFactors = FALSE))
  
  notice_path <- "data/notices.csv"
  if (!dir.exists("data")) dir.create("data")
  
  if (file.exists(notice_path)) {
    notices(read.csv(notice_path, stringsAsFactors = FALSE))
  } else {
    write.csv(data.frame(Notice = character()), notice_path, row.names = FALSE)
    notices(data.frame(Notice = character()))
  }
  
  
  
  observeEvent(input$add_notice, {
    req(input$new_notice != "")
    new_data <- data.frame(Notice = input$new_notice, stringsAsFactors = FALSE)
    notices(rbind(notices(), new_data))
    updateTextInput(session, "new_notice", value = "")
  })
  
  output$notice_table <- renderDT({
    datatable(notices(), selection = "single", options = list(dom = 't'))
  })
  
  observeEvent(input$delete_notice, {
    selected <- input$notice_table_rows_selected
    if (length(selected)) {
      current <- notices()
      notices(current[-selected, , drop = FALSE])
    }
  })
  
  observe({
    write.csv(notices(), notice_path, row.names = FALSE)
  })
  
  observeEvent(input$download_snapshot, {
    session$sendCustomMessage("capturePlotAsImage", list())
  })
  
  observeEvent(input$file, {
    req(input$file)
    sheets <- excel_sheets(input$file$datapath)
    sheet_data <- lapply(sheets, function(s) read_excel(input$file$datapath, sheet = s))
    names(sheet_data) <- sheets
    data_list(sheet_data)
    updateSelectInput(session, "sheet", choices = sheets)
  })
  
  output$sheet_select <- renderUI({
    req(length(data_list()) > 0)
    selectInput("sheet", "Select Sheet:", choices = names(data_list()))
  })
  
  observeEvent(input$sheet, {
    req(data_list(), input$sheet)
    active_data(data_list()[[input$sheet]])
  })
  
  output$var_select_ui <- renderUI({
    req(active_data())
    df <- active_data()
    numeric_vars <- names(df)[sapply(df, is.numeric)]
    checkboxGroupInput("selected_vars", "Select Variables to Display:", choices = numeric_vars)
  })
  
  output$summary <- renderPrint({
    req(input$selected_vars, active_data())
    summary(active_data()[, input$selected_vars, drop = FALSE])
  })
  
  output$risk_plot <- renderPlotly({
    req(input$selected_vars, active_data())
    
    df <- active_data()
    
    
    company_col <- names(df)[1]
    df$Category <- df[[company_col]]
    
    
    df <- df[, c("Category", input$selected_vars), drop = FALSE]
    
    
    df_long <- pivot_longer(df, -Category, names_to = "Variable", values_to = "Value")
    
    
    is_percent <- detect_percentage_cols(df)
    percent_cols <- names(which(is_percent))
    overlay_data <- NULL
    
    
    if (any(percent_cols %in% input$selected_vars) && input$detect_ratios) {
      overlay_data <- df_long %>%
        filter(Variable %in% percent_cols) %>%
        mutate(Value = as.numeric(str_remove(Value, "%")) / 100)
    }
    
    
    if (input$normalize) {
      df_long <- df_long %>%
        group_by(Category) %>%
        mutate(Value = Value / sum(Value, na.rm = TRUE)) %>%
        ungroup()
    }
    
    
    base_plot <- ggplot(df_long, aes(x = Category, y = Value, fill = Variable, text = paste0(Variable, ": ", nepali_formatter(Value)))) +
      geom_bar(stat = "identity", position = ifelse(input$normalize, "fill", "stack")) +
      labs(x = input$custom_x, y = input$custom_y, title = input$plot_title, fill = "Variable") +
      scale_y_continuous(labels = if (input$normalize) percent else nepali_formatter) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
    
    if (!is.null(overlay_data)) {
      base_plot <- base_plot +
        geom_line(data = overlay_data, aes(x = Category, y = Value, group = Variable, color = Variable), inherit.aes = FALSE, size = 1.2) +
        geom_point(data = overlay_data, aes(x = Category, y = Value, color = Variable), inherit.aes = FALSE)
    }
    
    
    ggplotly(base_plot, tooltip = "text") %>%
      animation_opts(1000, redraw = TRUE)
  })
  
  
  output$download_plot <- downloadHandler(
    filename = function() paste0("Risk_Chart_", Sys.Date(), ".png"),
    content = function(file) {
      req(input$selected_vars, active_data())
      
      df <- active_data()
      company_col <- names(df)[1]
      df$Category <- df[[company_col]]
      
      df <- df[, c("Category", input$selected_vars), drop = FALSE]
      df_long <- pivot_longer(df, -Category, names_to = "Variable", values_to = "Value")
      
      if (input$normalize) {
        df_long <- df_long %>%
          group_by(Category) %>%
          mutate(Value = Value / sum(Value, na.rm = TRUE)) %>%
          ungroup()
      }
      
      
      p <- ggplot(df_long, aes(x = Category, y = Value, fill = Variable)) +
        geom_bar(stat = "identity", position = ifelse(input$normalize, "fill", "stack")) +
        labs(x = input$custom_x, y = input$custom_y, title = input$plot_title, fill = "Variable") +
        scale_y_continuous(labels = if (input$normalize) percent else nepali_formatter) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))  
      
      
      ggsave(file, plot = p, width = 10, height = 6)
    }
  )
  
  
  observeEvent(input$fullscreen, {
    session$sendCustomMessage("toggleFullscreen", list())
  })
  
  #output$raw_table <- renderDT({ req(active_data()); datatable(active_data()) })
  output$raw_table <- renderDT({
    df <- active_data()
    fmt <- input$table_format %||% "nepali"
    if (fmt == "nepali") {
      df[] <- lapply(df, nepali_formatter)
    } else {
      df[] <- lapply(df, comma_formatter)
    }
    datatable(df, options = list(pageLength = 10))
  })
  
  
  output$company_select <- renderUI({ req(active_data()); selectInput("company", "Choose Company:", choices = unique(active_data()[[1]])) })
  #  output$company_plot <- renderPlotly({
  #    req(active_data(), input$company)
  #    df <- active_data() %>% filter(!!sym(names(active_data())[1]) == input$company)
  #    num_cols <- df %>% select(where(is.numeric))
  #    plot_ly(x = names(num_cols), y = as.numeric(num_cols[1, ]), type = "bar", name = input$company,text = ~round(as.numeric(num_cols[1, ]), 1), textposition = "outside")
  #  })
  
  output$company_plot <- renderPlotly({
    req(active_data(), input$company)
    df <- active_data() %>%
      filter(!!sym(names(active_data())[1]) == input$company)
    num_cols <- df %>% select(where(is.numeric))
    
    x_vars <- names(num_cols)
    y_vals  <- as.numeric(num_cols[1, ])
    plot_ly(
      x            = x_vars,
      y            = y_vals,
      type         = "bar",
      name         = input$company,
      text         = comma_formatter(y_vals),      
      textposition = "outside",
      hoverinfo    = "x+y+text"
    ) %>%
      layout(
        xaxis = list(title = "Variable"),
        yaxis = list(title = input$custom_y %||% "Value")
      )
  })
  
  
  output$company_var_select <- renderUI({
    req(active_data())
    df <- active_data()
    company_choices <- unique(df[[1]])
    
    
    num_vars <- names(df)[sapply(df, is.numeric)]
    
    tagList(
      selectInput("company", "Choose Company:", choices = company_choices),
      selectInput("selected_vars_adv", "Select Variables to Display:", 
                  choices = num_vars, selected = num_vars, multiple = TRUE)
    )
  })
  
  output$company_plot_adv <- renderPlotly({
    req(active_data(), input$company, input$selected_vars_adv)
    
    df <- active_data() %>%
      filter(!!sym(names(active_data())[1]) == input$company)
    
    
    selected_data <- df[, input$selected_vars_adv, drop = FALSE]
    
    x_vars <- names(selected_data)
    y_vals <- as.numeric(selected_data[1, ])
    
    plot_ly(
      x            = x_vars,
      y            = y_vals,
      type         = "bar",
      name         = input$company,
      text         = comma_formatter(y_vals),      
      textposition = "outside",
      hoverinfo    = "x+y+text"
    ) %>%
      layout(
        xaxis = list(title = "Variable"),
        yaxis = list(title = input$custom_y %||% "Value")
      )
  })
  
  
  
  #output$company_table <- renderDT({ req(active_data(), input$company); datatable(active_data() %>% filter(!!sym(names(active_data())[1]) == input$company)) })
  
  output$company_table <- renderDT({
    req(active_data(), input$company)
    
    df <- active_data()
    selected_column <- names(df)[1]  
    df <- df %>% filter(!!sym(selected_column) == input$company)
    
    fmt <- input$table_format %||% "nepali"
    if (fmt == "nepali") {
      df[] <- lapply(df, nepali_formatter)
    } else {
      df[] <- lapply(df, comma_formatter)
    }
    
    datatable(df, options = list(pageLength = 10))
  })
  
  
  output$metric_select <- renderUI({
    req(active_data())
    selectInput("metric", "Metric:", choices = names(active_data())[sapply(active_data(), is.numeric)])
  })
  
  # output$comparison_plot <- renderPlotly({
  #   req(active_data(), input$metric)
  #   df <- active_data()
  #   validate(
  #     need(input$metric %in% names(df), "Selected metric not found in dataset")
  #   )
  #   plot_ly(df, x = ~df[[1]], y = ~df[[input$metric]], type = "bar", color = ~df[[1]])
  # })
  
  output$comparison_plot <- renderPlotly({
    req(active_data(), input$metric)
    df <- active_data()
    
    validate(
      need(input$metric %in% names(df), "Selected metric not found in dataset")
    )
    
    x_vals <- df[[1]]  
    y_vals <- df[[input$metric]]
    
    plot_ly(
      x = ~x_vals,
      y = ~y_vals,
      type = "bar",
      color = ~x_vals,
      text = ~comma_formatter(y_vals),       
      textposition = "outside",
      hoverinfo = "x+y+text"                  
    ) %>%
      layout(
        xaxis = list(title = names(df)[1]),
        yaxis = list(title = input$metric)
      )
  })
  
  
  # output$comparison_table <- renderDT({ req(active_data()); datatable(active_data()) })
  output$comparison_table<- renderDT({
    df <- active_data()
    fmt <- input$table_format %||% "nepali"
    if (fmt == "nepali") {
      df[] <- lapply(df, nepali_formatter)
    } else {
      df[] <- lapply(df, comma_formatter)
    }
    datatable(df, options = list(pageLength = 10))
  })
  
  output$multi_metric_select <- renderUI({ req(active_data()); selectizeInput("metrics", "Metrics:", choices = names(active_data())[sapply(active_data(), is.numeric)], multiple = TRUE) })
  #  output$between_plot <- renderPlotly({
  #    req(active_data(), input$metrics)
  #    df <- active_data() %>% select(all_of(c(names(active_data())[1], input$metrics))) %>% pivot_longer(-1, names_to = "Metric", values_to = "Value")
  #    plot_ly(df, x = ~df[[1]], y = ~Value, color = ~Metric, type = "bar", barmode = "group",text = ~round(Value, 1), textposition = "outside")
  #  })
  
  output$between_plot <- renderPlotly({
    req(active_data(), input$metrics)
    
    df <- active_data() %>%
      select(all_of(c(names(active_data())[1], input$metrics))) %>%
      pivot_longer(-1, names_to = "Metric", values_to = "Value")
    
    x_vals <- df[[1]]
    
    plot_ly(
      data = df,
      x = ~x_vals,
      y = ~Value,
      color = ~Metric,
      type = "bar",
      barmode = "group",
      text = ~comma_formatter(Value),       
      textposition = "outside",
      hoverinfo = "x+y+text"                   
    ) %>%
      layout(
        xaxis = list(title = names(df)[1]),
        yaxis = list(title = "Value")
      )
  })
  
  
  
  
  #output$between_table <- renderDT({ req(active_data()); datatable(active_data()) })
  output$between_table<- renderDT({
    df <- active_data()
    fmt <- input$table_format %||% "nepali"
    if (fmt == "nepali") {
      df[] <- lapply(df, nepali_formatter)
    } else {
      df[] <- lapply(df, comma_formatter)
    }
    datatable(df, options = list(pageLength = 10))
  })
  
  
  makeRisk <- function(df, normalize, plot_type, title, xlab, ylab, company = NULL) {
    df_long <- df %>% pivot_longer(-1, names_to = "Category", values_to = "Value")
    if (!is.null(company)) {
      df_long <- df_long %>% filter(df_long[[1]] %in% company)
    }
    if (normalize) {
      df_long <- df_long %>% group_by(Category) %>% mutate(Value = Value / sum(Value) * 100) %>% ungroup()
    }
    if (plot_type %in% c("Bar (Grouped)", "Bar (Stacked)")) {
      bmode <- ifelse(plot_type == "Bar (Stacked)", "stack", "group")
      plot_ly(df_long, x = ~Category, y = ~Value, color = ~df_long[[1]], type = "bar", barmode = bmode,
              text = ~nepali_formatter(Value), textposition = "outside") %>%
        layout(title = title, xaxis = list(title = xlab), yaxis = list(title = ylab))
    } else if (plot_type == "Pie") {
      df_sum <- df_long %>% group_by(Category) %>% summarise(Value = sum(Value, na.rm = TRUE))
      plot_ly(df_sum, labels = ~Category, values = ~Value, type = "pie", textinfo = "label+percent") %>%
        layout(title = title)
    } else {
      plot_ly(df_long, x = ~Category, y = ~Value, type = "box", color = ~df_long[[1]], text = ~nepali_formatter(Value)) %>%
        layout(title = title, xaxis = list(title = xlab), yaxis = list(title = ylab))
    }
  }
  
  
  observe({
    req(active_data())
    
    selected_company_ind <- input$company_for_pie %||% NULL
    selected_company_bet <- input$company_for_pie_bet %||% NULL
    
    output$plot_ind <- renderPlotly({
      makeRisk(active_data(), input$normalize_ind, input$plot_type_ind,
               input$title_ind, input$xlab_ind, input$ylab_ind, company = selected_company_ind)
    })
    
    
    
    output$table_ind <- renderDT({
      df <- active_data()
      fmt <- input$table_format %||% "nepali"
      if (fmt == "nepali") {
        df[] <- lapply(df, nepali_formatter)
      } else {
        df[] <- lapply(df, comma_formatter)
      }
      datatable(df, options = list(pageLength = 10))
    })
    
    output$plot_bet <- renderPlotly({
      makeRisk(active_data(), input$normalize_bet, input$plot_type_bet,
               input$title_bet, input$xlab_bet, input$ylab_bet, company = selected_company_bet)
    })
    
    
    
    output$table_bet <- renderDT({
      df <- active_data()
      fmt <- input$table_format %||% "nepali"
      if (fmt == "nepali") {
        df[] <- lapply(df, nepali_formatter)
      } else {
        df[] <- lapply(df, comma_formatter)
      }
      datatable(df, options = list(pageLength = 10))
    })
    
    output$plot_comb <- renderPlotly({
      makeRisk(active_data(), input$normalize_comb, input$plot_type_comb,
               input$title_comb, input$xlab_comb, input$ylab_comb)
    })
    
    
    
    output$table_comb <- renderDT({
      df <- active_data()
      fmt <- input$table_format %||% "nepali"
      if (fmt == "nepali") {
        df[] <- lapply(df, nepali_formatter)
      } else {
        df[] <- lapply(df, comma_formatter)
      }
      datatable(df, options = list(pageLength = 10))
    })
  })
  comma_formatter <- function(x) {
    sapply(x, function(y) {
      if (is.na(y)) return(NA)
      if (!is.numeric(y)) return(as.character(y))
      formatC(y, format = "f", big.mark = ",", digits = 0)
    })
  }
  
  nepali_formatter <- function(x) {
    sapply(x, function(y) {
      if (is.na(y)) return(NA)
      if (!is.numeric(y)) return(as.character(y))
      if (abs(y) >= 1e9) return(paste0(round(y / 1e9, 2), " arba"))
      else if (abs(y) >= 1e5) return(paste0(round(y / 1e5, 2), " lakh"))
      else return(formatC(y, format = "f", big.mark = ",", digits = 0))
    })
  }
  
  
  output$company_pie_select <- renderUI({
    req(active_data())
    selectInput("company_for_pie", "Select Company (for Pie Chart):",
                choices = unique(active_data()[[1]]), selected = NULL)
  })
  
  output$company_pie_select_bet <- renderUI({
    req(active_data())
    selectizeInput("company_for_pie_bet", "Select Companies (for Pie Chart):",
                   choices = unique(active_data()[[1]]), multiple = TRUE, selected = NULL)
  })
  
  observe({
    shinyalert(
      title = "📢 Milestone 📢",
      text = "<img src='nepal_life_arba.png' width='100%'/",
      html = TRUE,
      size = "m",
      closeOnClickOutside = TRUE
    )
  })
}

shinyApp(ui, server)
