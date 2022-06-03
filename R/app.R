#' Launch the KarstID shiny application
#' 
#' An implementation of common analyses of karst spring hydrographs
#' through a Shiny application. It includes recession curves, statistical, 
#' classified discharges and simple correlational and spectral analyses. The 
#' application also allows performing a classification of the hydrological 
#' functioning and comparing the results to a database of 78 karst systems.
#' 
#' @param ... No argument are needed to launch the application.
#'
#' @export
#' @import shiny
#' @import waiter
#' @import data.table
#' @import ggplot2
#' @importFrom magrittr %>%
#' @importFrom stats qnorm quantile sd
#' @importFrom utils write.table

KarstID <- function(...) {
  
options(shiny.maxRequestSize = 100*1024^2) # increase the upload limit file size to 100
  
file_format <- c("text/csv","text/comma-separated-values, text/plain", ".csv")
delim <- c("Tabulation", "Semicolon", "Comma", "Space")
day_hour <- c("Day", "Hour")
dec_mark <- c("Point", "Comma")
date_format <- "%Y-%m-%d"

ui <- fluidPage(
  shinyjs::useShinyjs(),
  shinyFeedback::useShinyFeedback(),
  waiter::useWaiter(),
  waiter::waiterPreloader(),
  
  navbarPage(
    "KarstID",
    id = "menu",
    
    tabPanel(
      "Data import",
      
      sidebarLayout(
        
        sidebarPanel(
          
          fileInput("import", "Import dataset", accept = file_format),
          
          fluidRow(
            
            column(6,
                   textInput("name", "Name")),
            column(6,
                   selectInput("time_step", "Time step", choices = day_hour))
          ),
          
          fluidRow(
            
            column(6,
                   numericInput("skip_row", "Skip row", value = 0, min = 0, step = 1)),
            column(6,
                   numericInput("sheet", "Sheet", value = 1, min = 1, step = 1))
          ),
          
          
          fluidRow(
            
            column(6,
                   radioButtons("dec_mark", "Decimal mark", choices = dec_mark)),
            column(6,
                   radioButtons("delim", "Delimiter", choices = delim))
          ),
          
          checkboxInput("header", "Header", value = TRUE),
          shinyjs::hidden(checkboxInput("data_mean", "Compute and use daily mean", value = FALSE)),
          textInput("date_format", "Date format", value = date_format) %>% 
            shinyhelper::helper(type = "inline",
                                title = "Date Format",
                                colour = "grey",
                                content = c("<b>%d</b> - Day as a number (0-31)",
                                            "<b>%m</b> - Month (00-12)",
                                            "<b>%y</b> - 2-digit year",
                                            "<b>%Y</b> - 4-digit year",
                                            "<b>%H</b> - Decimal hour (24 hour)",
                                            "<b>%M</b> - Decimal minute (0-59)",
                                            "<b>%S</b> - Decimal second (0-59)",
                                            "<b>%a</b> - Abbreviated weekday (e.g. Mon)",
                                            "<b>%A</b> - Unabbreviated weekday (e.g. Monday)",
                                            "<b>%b</b> - Abbreviated month (e.g. Jan)",
                                            "<b>%B</b> - Abbreviated month (e.g. January)",
                                            "<br>",
                                            "Most common formats:",
                                            "- Date: %Y-%m-%d",
                                            "- Datetime: %Y-%m-%d %H:%M:%S"),
                                size = "m",
                                buttonLabel = "OK"),
          numericInput("max_gap", "Max interpolation gap size", value = 5, min = 0, step = 1),
          checkboxInput("keep_na", "Keep NA values", value = TRUE),
          actionButton("load_import", "Load dataset"),
          actionButton("load_default", "Load test dataset"),
          shinyjs::disabled(downloadButton("download_dataset", "Download dataset"))
        ),
        
        mainPanel(
          
          plotOutput("import_plot"),
          DT::DTOutput("stats_indicator"),
          br(),
          shinyjs::hidden(downloadButton("dl_stats", "Download results"))
        )
      )
    ),
    
    tabPanel(
      "Recession curves analysis",
      
      fluidRow(
        
        column(8,
               plotOutput("rc_plot", brush = "rc_brush"),
               
               fluidRow(
                 
                 column(4,
                        align = "center",
                        uiOutput("ui_rc_slider"),
                        fluidRow(actionButton("zoom_rc", "Zoom"),
                                 actionButton("reset_rc", "Reset"),
                                 actionButton("add_rc", "Add"),
                                 actionButton("delete_rc", "Delete"))),
                 column(4,
                        align = "center",
                        br(), br(),
                        shinyjs::hidden(downloadButton("dl_rc", "Download selected recession")),
                        br(), br(),
                        shinyjs::hidden(downloadButton("dl_rt", "Download table"))),
                 column(4,
                        fileInput("ul_rc", "Upload KarstID recession workspace", accept = file_format),
                        shinyjs::hidden(downloadButton("dl_hydrofile", "Save KarstID recession workspace")))
                 
               ),
               
               br(), br(),
               DT::DTOutput("dt_recap")
        ),
        
        column(4,
               plotOutput("rc_model_plot", click = "rc_model_bp"),
               uiOutput("ui_napeak"),
               shinyjs::hidden(uiOutput("ui_bp_value")),
               shinyjs::hidden(actionButton("save_param", "Save indicators")),
               shinyjs::hidden(actionButton("clear_param", "Clear selection")),
               br(), br(),
               verbatimTextOutput("model_perf")
        )
      )
    ),
    
    tabPanel(
      "Simple correlational and spectral analyses",
      
      column(6, plotOutput("acf_plot")),
      column(6, plotOutput("spf_plot")),
      
      fluidRow(
        
        column(4,
               uiOutput("ui_acspf_slider")),
        
        column(4,
               br(),
               verbatimTextOutput("display_acspf")),
        
        column(3,
               offset = 1,
               br(), br(), 
               shinyjs::hidden(downloadButton("dl_acspf", "Download results")))
      )
    ),
    
    tabPanel(
      "Analysis of classified discharges",
      
      column(6, 
             plotOutput("fdc_plot_normal"),
             br(),
             shinyjs::hidden(downloadButton("dl_fdc_normal", "Download results"))),
      
      column(6, 
             plotOutput("fdc_plot_mangin"),
             br(),
             shinyjs::hidden(checkboxInput("fdc_mangin_log", "Logarithmic scale")),
             shinyjs::hidden(downloadButton("dl_fdc_mangin", "Download results"))),
    ),
    
    tabPanel(
      "Classification",
      
      fluidRow(
        
        column(5,
               imageOutput("classif_img", inline = TRUE)),
        
        column(5,
               offset = 1,
               
               fluidRow(
                 
                 column(4,
                        br(),
                        tags$h3("Indicators:"),
                        tagAppendAttributes(tags$h5(textOutput("indicator_txt")), # allow \n in text
                                            style = "white-space:pre-wrap;")),
                 
                 column(8,
                        br(),
                        tags$h3("Distance to class:"),
                        tagAppendAttributes(tags$h5(htmlOutput("class_distance")), # allow \n in text
                                            style = "white-space:pre-wrap;"))
                 
                 ),
               
               fluidRow(br(),
                        textOutput("classif_txt"))
        )
      ),
      
      hr(),
      
      fluidRow(
        
        column(5,
               plotly::plotlyOutput("scatter_classif_plot", height = "600px")),
        
        column(7,
               DT::DTOutput("dt_classif"))
        
        )
      ),
    
    tags$script(
      HTML("var header = $('.navbar > .container-fluid');
                              header.append('<div style=\"float:right; padding-top: 8px\"><button id=\"about\" type=\"button\" class=\"btn action-button\">About</button></div>')")
    ),
    
    tags$script(
      HTML("var header = $('.navbar > .container-fluid');
header.append('<div style=\"float:right\"><a href=\"http://karma-project.org/\"><img src=\"extdata/KARMA_logo.png\" style=\"float:right; height:43px; padding-top:8px; padding-right:5px;\"></a> </div>');
    console.log(header)")
    ),
    
    tags$script(
      HTML("var header = $('.navbar > .container-fluid');
header.append('<div style=\"float:right\"><a href=\"https://sokarst.org/\"><img src=\"extdata/SNOKARST_logo.png\" style=\"float:right; height:43px; padding-top:8px; padding-right:5px;\"></a> </div>');
    console.log(header)")
    )
    
  )
)

server <- function(input, output, session) {
  
  shinyhelper::observe_helpers(withMathJax = TRUE)
  
  # about popup
  
  observeEvent(input$about, {
    about_popup()
  })
  
  
  # last tab memory
  
  tab <- reactiveValues(last = "Data import",
                        current = "Data import")
  
  observeEvent(input$menu, {
    tab$last <- tab$current
    tab$current <- input$menu
  })
  
  # shinyjs --------------------------------------------------------------------   
  
  # hide download button if no dataset
  observe({
    if (!is.null(df$df)) {
      shinyjs::show("dl_stats")
      shinyjs::show("dl_rc")
      shinyjs::show("dl_rt")
      shinyjs::show("dl_hydrofile")
      shinyjs::show("dl_acspf")
      shinyjs::show("dl_fdc_normal")
      shinyjs::show("dl_fdc_mangin")
      shinyjs::show("fdc_mangin_log")
    } else {
      shinyjs::hide("dl_stats")
      shinyjs::hide("dl_rc")
      shinyjs::hide("dl_rt")
      shinyjs::hide("dl_hydrofile")
      shinyjs::hide("dl_acspf")
      shinyjs::hide("dl_fdc_normal")
      shinyjs::hide("dl_fdc_mangin")
      shinyjs::hide("fdc_mangin_log")
    }
  })

  # hide data mean if time step is daily
  observeEvent(input$time_step, {
    if (input$time_step == "Day")
      shinyjs::hide("data_mean")
    else
      shinyjs::show("data_mean")
  })
  
  # hide recession model widget if no selection is selected
  observeEvent(input$dt_recap_rows_selected,
               ignoreNULL = FALSE, {
                 if (!is.null(input$dt_recap_rows_selected)) {
                   shinyjs::show("ui_bp_value")
                   shinyjs::show("save_param")
                   shinyjs::show("clear_param")
                 } else {
                   shinyjs::hide("ui_bp_value")
                   shinyjs::hide("save_param")
                   shinyjs::hide("clear_param")
                 }
               })
  
  # import data ----------------------------------------------------------------
  
  delim <- reactive({
    switch(input$delim,
           "Tabulation" = "\t",
           "Semicolon" = ";",
           "Comma" = ",",
           "Space" = " ")
  })
  
  dec_mark <- reactive({
    switch(input$dec_mark,
           "Point" = ".",
           "Comma" = ",")
  })
  
  time_step <- reactive({
    switch(input$time_step,
           "Day" = FALSE,
           "Hour" = TRUE)
  })
  
  data_mean <- reactive({
    if (input$data_mean) "day" else "default"
  })
  
  data_mean_num <- eventReactive(c(input$load_import, input$load_default), {
    if (input$data_mean == FALSE) {
      if (input$time_step == "Day") 1 else 24
    } else {
      1
    }
  })
  
  df <- reactiveValues()
  
  df_interp <- reactive({
    req(df$df)
    df$df
    })
  
  dt_stats <- reactive({
    q <- df_interp()$discharge
    dt <-  data.frame(Mean = mean(q, na.rm = TRUE),
                      Min = min(q, na.rm = TRUE),
                      Max = max(q, na.rm = TRUE),
                      Sd = sd(q, na.rm = TRUE),
                      Q10 = quantile(q, 0.1, na.rm = TRUE),
                      Q90 = quantile(q, 0.9, na.rm = TRUE)) %>% 
      dplyr::mutate(CV = (Sd / Mean) * 100,
                    SVC = Q90 / Q10) %>% 
      dplyr::mutate(dplyr::across(dplyr::everything(), round, 2)) %>% 
      # add unit with HTML and escape = FALSE
      # CAREFUL, if any changes in the number/order of variables,
      # you must change the names() in the output$dl_stats
      dplyr::rename("Mean<br>(m<sup>3</sup>.s<sup>-1</sup>)" = Mean,
                    "Min<br>(m<sup>3</sup>.s<sup>-1</sup>)" = Min,
                    "Max<br>(m<sup>3</sup>.s<sup>-1</sup>)" = Max,
                    "SD<br>(m<sup>3</sup>.s<sup>-1</sup>)" = Sd,
                    "Q10<br>(m<sup>3</sup>.s<sup>-1</sup>)" = Q10,
                    "Q90<br>(m<sup>3</sup>.s<sup>-1</sup>)" = Q90,
                    "CV<br>(%)" = CV) %>% 
      dplyr::mutate(`Number of NAs` = length(which(is.na(q))))
  })
  
  observeEvent(input$load_default, {
    df$df <- default_dataset
    
    if (df_rc$count > 0) {
      df_rc$recap <- df_rc$recap %>% dplyr::slice(0)
      df_rc$list <- list()
      df_rc$count <- 0
      DT::replaceData(dt_recap_proxy, df_rc$recap, rownames = FALSE)
    }
    
    shinyjs::enable("download_dataset") # enable download 
  })
  
  observeEvent(input$load_import, {
    req(!is.null(input$import))
    shinyFeedback::feedbackWarning("max_gap",
                                   input$max_gap > 10,
                                   "It is recommended to be cautious regarding the interpolation of a high number of consecutive NA values (>10), as it increases the probability of irrelevant estimations.")

    notif$acsp <- TRUE # reset acsp notif
    
    df$df <- import_data(input$import$datapath, 
                         mean = data_mean(),
                         delim = delim(), 
                         skip = input$skip_row,
                         header = input$header, 
                         na = c("", "NA"), 
                         decimal_mark = dec_mark(), 
                         date_time = time_step(),
                         date_format = input$date_format, 
                         maxgap = input$max_gap, 
                         no_NA = !input$keep_na,
                         sheet = input$sheet)
    
    if (df_rc$count > 0) {
      df_rc$recap <- df_rc$recap %>% dplyr::slice(0)
      df_rc$list <- list()
      df_rc$count <- 0
      DT::replaceData(dt_recap_proxy, df_rc$recap, rownames = FALSE)
    }
    
    shinyjs::enable("download_dataset") # enable download 
    
    miss_date <- any(is.na(df$df$date)) # check date error
    shinyFeedback::feedbackDanger("date_format", 
                                  miss_date, 
                                  "Date format error. Consider reformatting date format or check your date input for eventual NAs.")
    
    req(miss_date, cancelOutput = TRUE)
    df$df <- NULL # if date error reset table
  })
  
  output$import_plot <- renderPlot(
    ggplot(df_interp(), aes(date, discharge)) +
      geom_line(size = 0.8) +
      theme_bw() +
      xlab("Date") +
      ylab(expression("Discharge" ~(m^3~.s^-1))) +
      theme(axis.title = element_text(size = 16, color = "#2d2d2d"),
            axis.text = element_text(size = 14, color = "#2d2d2d"))
  )
  
  output$stats_indicator <- DT::renderDT({
    DT::datatable(dt_stats(),
                  rownames = FALSE,
                  selection = "none",
                  escape = FALSE,
                  options = list(dom = "t"))
  })
  
  output$download_dataset <- downloadHandler(
    filename = paste0(input$name, "_dataset.txt"),
    content = function(filename) {
      write.table(df_interp(), filename, sep = "\t", row.names = FALSE)
    },
  )
  
  output$dl_stats <- downloadHandler(
    filename = paste0(input$name, "_statistics.txt"),
    content = function(filename) {
      dt <- dt_stats()
      names(dt) <- c("Mean", "Min", "Max", "SD", "Q10", "Q90", "CV", "SVC", "Number of NAs")
      write.table(dt, filename, sep = "\t", row.names = FALSE)
    }
  )  
  
  # manual recession selection -------------------------------------------------
  
  df_filtered <- reactive({
    req(input$rc_slider) # avoid error due to plot loading faster than slider
    dplyr::filter(df_interp(), dplyr::between(date, input$rc_slider[1], input$rc_slider[2]))
  })
  
  rc_brush <- reactive({
    brushedPoints(df_filtered(), input$rc_brush)
  })
  
  rc_length <- reactive({
    nrow(rc_brush())
  })
  
  df_rc <- reactiveValues(list = list(),
                          save = list(), # duplicate rc for NA peak values
                          recap = data.frame("num" = integer(),
                                             "start" = as.Date(character()),
                                             "end" = as.Date(character()),
                                             "breakpoint" = integer(),
                                             "k" = double(),
                                             "i" = double(),
                                             "alpha" = double()),
                          count = 0)
  
  observeEvent(input$zoom_rc, {
    update_slider(session, "rc_slider", rc_brush())
    session$resetBrush("rc_brush")
  })
  
  observeEvent(input$reset_rc, {
    update_slider(session, "rc_slider", df_interp())
    session$resetBrush("rc_brush")
  })
  
  observeEvent(input$dt_recap_rows_selected, ignoreInit = TRUE, {
    freezeReactiveValue(input, "bp_value") # avoid rc_model_plot flicker when switching recessions
    updateCheckboxInput(session, "napeak", value = napeak$list[[input$dt_recap_rows_selected]])
    if (is.na(df_rc$recap[input$dt_recap_rows_selected, "breakpoint"])) {
      updateNumericInput(session,  "bp_value", value = "")
    } else {
      updateNumericInput(session, "bp_value", value = df_rc$recap[input$dt_recap_rows_selected, "breakpoint"])
    }
  })
  
  observeEvent(input$add_rc, {
    req(input$rc_brush)
    df_rc$count <- df_rc$count + 1
    df_rc$list[[df_rc$count]] <- rc_brush() %>% dplyr::mutate(t = 0:(rc_length() - 1))
    df_rc$recap[nrow(df_rc$recap) + 1,] <- list(df_rc$count,
                                                min(rc_brush()$date),
                                                max(rc_brush()$date),
                                                NA, # bp
                                                NA, # k
                                                NA, # i 
                                                NA) # alpha
    session$resetBrush("rc_brush")
    DT::replaceData(dt_recap_proxy,
                    df_rc$recap,
                    rownames = FALSE,
                    clearSelection = "none",
                    resetPaging = FALSE)
    
    df_rc$save[[df_rc$count]] <- df_rc$list[[df_rc$count]]
    napeak$list[[df_rc$count]] <- FALSE
  })
  
  observeEvent(input$delete_rc, ignoreNULL = FALSE, ignoreInit = TRUE, {
    req(input$dt_recap_rows_selected)
    df_rc$recap <- df_rc$recap[-input$dt_recap_rows_selected,]
    df_rc$list[[input$dt_recap_rows_selected]] <- NULL
    df_rc$recap$num[df_rc$recap$num > input$dt_recap_rows_selected] <- df_rc$recap$num[df_rc$recap$num > input$dt_recap_rows_selected] - 1
    df_rc$count <- df_rc$count - 1
    
    DT::replaceData(dt_recap_proxy, 
                    df_rc$recap, 
                    rownames = FALSE, 
                    resetPaging = FALSE)
    
    df_rc$save[[input$dt_recap_rows_selected]] <- NULL
    napeak$list[[input$dt_recap_rows_selected]] <- NULL
  })
  
  output$dl_rc <- downloadHandler(
    filename = paste0(input$name, "_recession_list_export.txt"),
    content = function(filename) {
      # the lapply function add a "num" column to differentiate each recession
      rc_list <- rbindlist(lapply(seq_along(df_rc$list), 
                                  function(i) dplyr::mutate(df_rc$list[[i]], num = i)
      )
      )
      write.table(rc_list, filename, sep = "\t", row.names = FALSE)
    }
  )
  
  output$dl_rt <- downloadHandler(
    filename = paste0(input$name, "_recession_table_export.txt"),
    content = function(filename) {
      write.table(df_rc$recap, filename, sep = "\t", row.names = FALSE)
    }
  )
  
  output$dl_hydrofile <- downloadHandler(
    filename = paste0(input$name, "_KarstID_export.rds"),
    content = function(filename) {
      rc_export <- list(df_rc$list, df_rc$recap, napeak$list, df_rc$save)
      saveRDS(rc_export, filename)
    }
  )
  
  observeEvent(input$ul_rc, {
    rc_import <- readRDS(input$ul_rc$datapath)
    df_rc$list <- rc_import[[1]]
    df_rc$recap <- rc_import[[2]]
    napeak$list <- rc_import[[3]]
    df_rc$save <- rc_import[[4]]
    df_rc$count <- max(df_rc$recap$num, na.rm = TRUE)
    DT::replaceData(dt_recap_proxy, 
                    df_rc$recap, 
                    rownames = FALSE, 
                    resetPaging = FALSE)
  })
  
  output$ui_rc_slider <- renderUI({
    sliderInput(
      "rc_slider",
      "Select a time interval",
      min = min(df_interp()$date),
      max = max(df_interp()$date),
      value = c(min(df_interp()$date), max(df_interp()$date)),
      timeFormat = "%Y-%m",
    )
  })
  
  output$rc_plot <- renderPlot({
    req(input$rc_slider)
    plot_all_rc(
      df_filtered(),
      df_rc$list,
      input$rc_slider[1],
      input$rc_slider[2],
      input$dt_recap_rows_selected
    )
  }) %>% bindCache(df_filtered(), df_rc$list, input$dt_recap_rows_selected, input$rc_slider)
  
  output$dt_recap <- DT::renderDT({
    DT::datatable(isolate(df_rc$recap),
                  selection = list(mode = "single"),
                  rownames = FALSE,
                  options = list(dom = "tp"))
  })
  
  dt_recap_proxy <- DT::dataTableProxy("dt_recap")
  
  # recession model ---------------------------------------------------------
  
  napeak <- reactiveValues(list = list())
  
  qmean <- reactive(mean(df_interp()$discharge, na.rm = TRUE))
  
  vtransit <- reactive(qmean() * 86400 * 365)
  
  selected_recession <- reactive({
    # long format to allow input$rc_model_bp on rc_model_plot when model is up
    data.table::melt(as.data.table(df_rc$list[[input$dt_recap_rows_selected]]), id.vars = "t", measure.vars = "discharge")
  }) %>% 
    bindCache(df_rc$list[[input$dt_recap_rows_selected]]) %>% 
    bindEvent(input$dt_recap_rows_selected, input$napeak)
  
  mangin_model <- reactive({
    model_mangin(selected_recession(), input$bp_value, vtransit(), isolate(data_mean_num()))
  }) %>% 
    bindCache(selected_recession(), input$bp_value, vtransit(), isolate(data_mean_num())) %>% 
    bindEvent(input$bp_value, input$dt_recap_rows_selected, input$napeak) # can work with eventReactive instead of caching
  
  observeEvent(input$napeak, {
    req(input$dt_recap_rows_selected)
    napeak$list[input$dt_recap_rows_selected] <- input$napeak
    if (input$napeak) {
      df_rc$list[[input$dt_recap_rows_selected]] <- rm_peak(df_rc$list[[input$dt_recap_rows_selected]])
    }
    if (!input$napeak) {
      df_rc$list[[input$dt_recap_rows_selected]] <- df_rc$save[[input$dt_recap_rows_selected]]
    }
    
    shinyFeedback::hideFeedback("bp_value")
    
    max_bp_value <- max_bp_value(df_rc$list[[input$dt_recap_rows_selected]]$discharge)
    is_possible <- !(input$bp_value < 2 | 
                       input$bp_value >= max_bp_value)
    shinyFeedback::feedbackWarning("bp_value", 
                                   !is_possible, 
                                   paste0("Breakpoint must be numeric, greater than 1 and lower than ",
                                          max_bp_value, 
                                          "."))
  })
  
  observeEvent(input$rc_model_bp, {
    shinyFeedback::hideFeedback("bp_value")
    
    mangin_breakpoint <- nearPoints(selected_recession(), input$rc_model_bp, maxpoints = 1, threshold = 500)
    
    
    max_bp_value <- max_bp_value(df_rc$list[[input$dt_recap_rows_selected]]$discharge)
    is_possible <- !(input$bp_value < 2 | 
                       input$bp_value >= max_bp_value)
    shinyFeedback::feedbackWarning("bp_value", 
                                   !is_possible, 
                                   paste0("Breakpoint must be numeric, greater than 1 and lower than ",
                                          max_bp_value, 
                                          "."))
    
    
    updateNumericInput(session, "bp_value", value = mangin_breakpoint$t)
  })
  
  observeEvent(input$bp_value, {
    req(input$dt_recap_rows_selected)
    shinyFeedback::hideFeedback("bp_value")
    req(is.numeric(input$bp_value))
    max_bp_value <- max_bp_value(df_rc$list[[input$dt_recap_rows_selected]]$discharge)
    is_possible <- !(input$bp_value < 2 | 
                       input$bp_value >= max_bp_value)
    shinyFeedback::feedbackWarning("bp_value", 
                                   !is_possible, 
                                   paste0("Breakpoint must be numeric, greater than 1 and lower than ",
                                          max_bp_value, 
                                          "."))
  })
  
  observeEvent(input$save_param, {
    shinyFeedback::hideFeedback("bp_value")
    
    max_bp_value <- max_bp_value(df_rc$list[[input$dt_recap_rows_selected]]$discharge)
    is_possible <- !(input$bp_value < 2 | 
                       input$bp_value >= max_bp_value |
                       !is.numeric(input$bp_value))
    shinyFeedback::feedbackDanger("bp_value", 
                                  !is_possible, 
                                  paste0("Breakpoint must be numeric, greater than 1 and lower than ",
                                         max_bp_value, 
                                         "."))
    
    req(is_possible, cancelOutput = TRUE) 
    df_rc$recap[input$dt_recap_rows_selected, "breakpoint"] <- input$bp_value
    df_rc$recap[input$dt_recap_rows_selected, "k"] <- mangin_model()$k
    df_rc$recap[input$dt_recap_rows_selected, "i"] <- mangin_model()$i
    df_rc$recap[input$dt_recap_rows_selected, "alpha"] <- mangin_model()$alpha
    DT::replaceData(
      dt_recap_proxy,
      df_rc$recap,
      rownames = FALSE,
      clearSelection = "none",
      resetPaging = FALSE)
  })
  
  observeEvent(
    input$clear_param,
    {
      freezeReactiveValue(input, "bp_value")
      shinyFeedback::hideFeedback("bp_value")
      
      df_rc$recap[input$dt_recap_rows_selected, "breakpoint"] <- NA
      df_rc$recap[input$dt_recap_rows_selected, "k"] <- NA
      df_rc$recap[input$dt_recap_rows_selected, "i"] <- NA
      df_rc$recap[input$dt_recap_rows_selected, "alpha"] <- NA
      updateNumericInput(session, "bp_value", value = "")
      DT::replaceData(
        dt_recap_proxy,
        df_rc$recap,
        rownames = FALSE,
        clearSelection = "none",
        resetPaging = FALSE)
    })
  
  output$ui_napeak <- renderUI({
    req(input$dt_recap_rows_selected)
    checkboxInput(
      "napeak",
      "Remove spikes in the recession curve",
      # isolate to avoid infinite loop when d-click input$napeak
      value = FALSE
    )
  })
  
  output$ui_bp_value <- renderUI({
    numericInput(
      "bp_value", 
      "Breakpoint value",
      min = 0,
      max = 100000,
      value = "",
      step = 1)
  })
  
  output$rc_model_plot <- renderPlot({ 
    req(input$dt_recap_rows_selected, length(input$bp_value) > 0)
    plot_rc_model(selected_recession(), mangin_model()[["recession"]], input$bp_value)
  })
  
  output$model_perf <- renderText({
    req(input$dt_recap_rows_selected, input$bp_value)
    results <- mangin_model()$recession
    rmse <- rmse(results$discharge, results$sim_discharge)
    paste0("RMSE = ", round(rmse, 4), " m3/s")
  })
  
  # Simple correlational and spectral analyses -------------------------------------

  ascp_results <- reactive({
    req(isolate(input$menu) == "Simple correlational and spectral analyses")
    
    if (any(is.na(df_interp()$discharge))) {
      req(!notif$acsp)
      acsp_waiter$show()
      no_na_df <- fill_gap(df$df, maxgap = isolate(input$max_gap), no_NA = TRUE)
      acsp <- acspf(no_na_df$discharge, 
                    max_lag = input$acspf_slider, 
                    timestep = isolate(data_mean_num()))
      return(acsp)
    }
    
    if (!any(is.na(df_interp()$discharge))) {
      acsp_waiter$show()
      acsp <- acspf(df_interp()$discharge, 
                    max_lag = input$acspf_slider, 
                    timestep = isolate(data_mean_num()))
      return(acsp)
    }
  }) %>% 
    bindCache(df$df, df_interp(), notif$acsp, input$acspf_slider, isolate(input$max_gap), isolate(data_mean_num()))
  
  observeEvent(input$menu, {
    req(input$menu == "Simple correlational and spectral analyses")
    req(any(is.na(df_interp()$discharge)))
    req(notif$acsp)
    showModal(
      modalDialog(
        paste0("Do you want to perform simple correlational and spectral analyses on the longest non-NA part of the discharge time series?",
               " Current max gap \ninput is ",
               input$max_gap,
               ". Consider unchecking the `keep NA values` box and reload dataset in the import tab."),
        title = "Presence of one or several NA values in the discharge time series",
        footer = tagList(
          actionButton("perform_acspf", "Perform analysis"),
          actionButton("cancel_acspf", "Cancel")
        ),
        fade = FALSE
      )
    )
  })
  
  observeEvent(input$perform_acspf, {
    notif$acsp <- FALSE
    removeModal()
  })
  
  observeEvent(input$cancel_acspf, {
    removeModal()
    updateTabsetPanel(session, "menu", selected = tab$last)
  })
  
  max_cutting <- eventReactive(c(input$load_import, input$load_default), {
    freezeReactiveValue(input, "acspf_slider")
    round((nrow(df_interp()) / 3) / data_mean_num())
  })
  
  output$ui_acspf_slider <- renderUI({
    sliderInput(
      "acspf_slider", 
      "Define cutting point in days (m)", 
      value = 125,
      min = 2, 
      max = max_cutting(),
      step = 1)
  })
  
  output$dl_acspf <- downloadHandler(
    filename = paste0(input$name, "_simple_correlational_and_spectral_export.txt"),
    content = function(filename) {
      acspf_tab <- data.frame(k = ascp_results()$k,
                              rk = ascp_results()$rk,
                              f = c(NA, ascp_results()$f),
                              sf = c(NA, ascp_results()$sf))
      write.table(acspf_tab, filename, sep = "\t", row.names = FALSE)
    }
  )
  
  output$acf_plot <- renderPlot({
    plot_acf(ascp_results()$k, ascp_results()$rk)
  })
  
  output$spf_plot <- renderPlot({
    plot_spf(ascp_results()$f, ascp_results()$sf)
  }) 
  
  output$display_acspf <- renderText({
    if (!is.na(ascp_results()$mem_ef))
      mem_ef_txt <- paste0("Memory Effect = ", round(ascp_results()$mem_ef, 2), " days \n")
    else
      mem_ef_txt <- paste0("Memory Effect cannot be calculated. \n")
    
    if (!is.na(ascp_results()$reg_time))
      reg_time_txt <- paste0("Regulation Time = ", round(ascp_results()$reg_time, 2), " days")
    else
      reg_time_txt <- paste0("Regulation Time cannot be calculated.")
  
    paste0(mem_ef_txt, 
           reg_time_txt)
  })
  
  # analysis of classified discharges ---------------------------------------
  
  fdc_df_normal <- reactive({
    req(isolate(input$menu) == "Analysis of classified discharges")
    fdc_normal(df_interp()$discharge)
  })
  
  fdc_df_mangin <- reactive({
    req(isolate(input$menu) == "Analysis of classified discharges")
    fdc_mangin(df_interp()$discharge)
  })
  
  output$fdc_plot_normal <- renderPlot({
    plot_fdc(fdc_df_normal(), method = "normal")
  })
  
  output$fdc_plot_mangin <- renderPlot({
    plot_fdc(fdc_df_mangin(), method = "mangin", xlog = input$fdc_mangin_log)
  })
  
  output$dl_fdc_normal <- downloadHandler(
    filename = paste0(input$name, "_classified_discharges_normal_export.txt"),
    content = function(filename) {
      write.table(fdc_df_normal(), filename, sep = "\t", row.names = FALSE)
    }
  )  
  
  output$dl_fdc_mangin <- downloadHandler(
    filename = paste0(input$name, "_classified_discharges_mangin_export.txt"),
    content = function(filename) {
      write.table(fdc_df_mangin(), filename, sep = "\t", row.names = FALSE)
    }
  )
  # classification ----------------------------------------------------------

  kmax <- reactive({
    if (length(df_rc$recap$k) > 0 & sum(!is.na(df_rc$recap$k)) > 0) {
      max(df_rc$recap$k, na.rm = TRUE)
    } else {
      NA
    }
    })
  
  alphamean <- reactive({
    if (length(df_rc$recap$alpha) > 0 & sum(!is.na(df_rc$recap$alpha)) > 0) {
      mean(df_rc$recap$alpha, na.rm = TRUE)
    } else {
      NA
    }
    })
  
  ir <- reactive({
    if (length(df_rc$recap$i) > 0 & sum(!is.na(df_rc$recap$i)) > 0) {
      max(df_rc$recap$i, na.rm = TRUE) - min(df_rc$recap$i, na.rm = TRUE)
    } else {
      NA
    }
    })
  
  class <- reactive(class_system(kmax(), alphamean(), ir()))
  
  dist_to_system <- reactive({
    classif_bdd[, Distance := calc_syst_distance(alpha_mean, alphamean(),
                                                 k_max, kmax(),
                                                 IR, ir())]
    classif_bdd <- classif_bdd[order(Distance)] %>% 
      # add unit with HTML and escape = FALSE
      dplyr::rename("Distance<br>(%)" = Distance,
                    "k<sub>max</sub>" = k_max,
                    "&#x3B1;<sub>mean</sub><br>(day<sup>-1</sup>)" = alpha_mean,
                    "Mean<br>(m<sup>3</sup>.s<sup>-1</sup>)" = mean,
                    "CV<br>(%)" = cv,
                    "SVC" = svc,
                    "ME<br>(day)" = ME,
                    "RT<br>(day)" = RT)
      
  })
  
  observeEvent(class(), {
    req(notif$plotly_trace)
    plotly::plotlyProxy("scatter_classif_plot", session) %>% 
      plotly::plotlyProxyInvoke("deleteTraces", c(78, 79)) 
    
    notif$plotly_trace <- FALSE
  })
  
  observeEvent(
    c(input$dt_classif_rows_selected),
    ignoreNULL = FALSE,
    ignoreInit = TRUE, {
      row_df <- nrow(classif_data_plot) # for counting traces
      
      # class style
      class_system <- classif_data_plot$System
      selected_system <- dist_to_system()$System[input$dt_classif_rows_selected]
      color_class <- ifelse(class_system %in% selected_system, 
                            "#fff700",
                            "black")
      width_class <- ifelse(class_system %in% selected_system, 
                            2, 
                            1.3)
      
      # m style
      m_system <- m$System
      selected_system <- dist_to_system()$System[input$dt_classif_rows_selected]
      color_m <- ifelse(m_system %in% selected_system, 
                        "#fff700",
                        "#8c8c8c")
      
      plotly::plotlyProxy("scatter_classif_plot", session) %>% 
        plotly::plotlyProxyInvoke("restyle",
                          list(marker.line.color = color_class,
                               marker.line.width = width_class),
                          as.list(seq(0, row_df - 1, 1))) %>% 
        plotly::plotlyProxyInvoke("restyle",
                          list(line = list(color = color_m)),
                          row_df)
    })
  
  observeEvent(input$menu, {
    req(input$menu == "Classification")
    req(!is.na(class()))
    
    if (length(which(!is.na(df_rc$recap$i))) < 2 & kmax() >= 0 & alphamean() >= 0) {
      req(notif$classif_ir)
      notif$classif_ir <- FALSE
      show_popup("It is advised to select at least two recession curves for a relevant IR indicator.")
    }
    
    if ((kmax() < 0 | alphamean() < 0) & length(which(!is.na(df_rc$recap$i))) >= 2) {
      req(notif$classif_k)
      notif$classif_k <- FALSE
      show_popup("kmax or alphamean should be greater than 0.")
    }
    
    if (length(which(!is.na(df_rc$recap$i))) < 2 & (kmax() < 0 | alphamean() < 0)) {
      req(notif$classif_k_ir)
      notif$classif_k_ir <- FALSE
      show_popup("It is advised to select at least two recession curves for a relevant IR indicator, and kmax or alphamean should be greater than 0.")
    }
    
    req(!notif$plotly_trace)
    color <- dplyr::recode(class(),
                           `C1` = "#000000",
                           `C2` = "#009E73",
                           `C3` = "#e79f00",
                           `C4` = "#9ad0f3",
                           `C5` = "#0072B2",
                           `C6` = "#D55E00")
    plotly::plotlyProxy("scatter_classif_plot", session) %>% 
      plotly::plotlyProxyInvoke("addTraces",
                        list(x = c(kmax(), kmax()),
                             y = c(alphamean(), alphamean()), 
                             z = c(ir(), ir()),
                             inherit = FALSE,
                             mode = "markers",
                             marker = list(color = color, line = list(color = "red", width = 3)),
                             showlegend = FALSE,
                             type = "scatter3d")) %>% 
      plotly::plotlyProxyInvoke("addTraces",
                        list(x = c(kmax(), kmax()),
                             y = c(alphamean(), alphamean()), 
                             z = c(0, ir()), 
                             mode = "lines",
                             showlegend = FALSE,
                             inherit = FALSE,
                             line = list(width = 5, color = "red"),
                             type = "scatter3d"))
    
    notif$plotly_trace <- TRUE
  })
  
  output$classif_img <- renderImage(deleteFile = FALSE, {
    filename <- class_filename(class())
    list(src = filename, width = "100%", height = "auto")
  })
  
  output$classif_txt <- renderText({
    name <- ifelse(input$name == "", "The system", input$name)
    if (!is.na(class())) carac_system(name, class())
  })
  
  output$indicator_txt <- renderText({
    if (is.na(kmax()) & is.na(alphamean()) & is.na(ir()))
      paste0("Indicators cannot be calculated: \n" ,
             "- No recession curves have been selected \n",
             "- Mangin's model has not been applied \n",
             "- Recession indicators were not saved")
    else
      paste0("k max = ", round(kmax(), 3), "\n", 
             "alpha mean = ", round(alphamean(), 3), "\n",
             "IR = ", round(ir(), 3))
  })
  
  output$class_distance <- renderText({
    dist <- calc_class_distance(class(), kmax(), alphamean(), ir())
    paste0("C1 : ", msg_dist(dist[1]), "\n", 
           "C2 : ", msg_dist(dist[2]), "\n",
           "C3 : ", msg_dist(dist[3]), "\n",
           "C4 : ", msg_dist(dist[4]), "\n",
           "C5 : ", msg_dist(dist[5]), "\n",
           "C6 : ", msg_dist(dist[6]))
  })
  
  output$dt_classif <- DT::renderDT({
    DT::datatable(dist_to_system(),
                  rownames = FALSE,
                  escape = FALSE,
                  options = list(dom = "ftp", pageLength = 10),
                  selection = "multiple")
  })
  
  output$scatter_classif_plot <- plotly::renderPlotly({
    plotly::plot_ly() %>% 
      plotly::add_trace(data = classif_data_plot,
                x = ~k_max, 
                y = ~alpha_mean, 
                z = ~IR,
                color = ~System,
                colors = ~color,
                mode = "markers",
                showlegend = FALSE,
                type = "scatter3d") %>% 
      plotly::add_trace(data = m, 
                x = ~k_max, 
                y = ~alpha_mean, 
                z = ~IR, 
                mode = "lines",
                showlegend = FALSE,
                line = list(width = 1.5),
                type = "scatter3d") %>% 
      plotly::layout(
        legend = list(x = 1, 
                      y = 0.5, 
                      font = list(size = 20),
                      itemsizing = "constant"),
        scene = list(
          hovermode = FALSE,
          xaxis = list(type = "log",
                       title = "k<sub>max</sub>",
                       showspikes = FALSE,
                       dtick = 1),
          yaxis = list(type = "log",
                       title = paste0(intToUtf8(0x03B1L), "<sub>mean</sub>"),
                       showspikes = FALSE,
                       dtick = 1),
          zaxis = list(title = "IR",
                       showspikes = FALSE)))
  })

  # notif -------------------------------------------------------------------

  notif <- reactiveValues(acsp = TRUE, # avoid showing notif again if user decides to perform analysis
                          classif_k = TRUE,
                          classif_ir = TRUE,
                          classif_k_ir = TRUE,
                          plotly_trace = FALSE) 
  
  # waiter ------------------------------------------------------------------
  
  acsp_waiter <- Waiter$new(id = c("acf_plot", "spf_plot"),
                                    html = spin_3(), 
                                    color = transparent(.7))
  
  # pre load outputs in background ------------------------------------------
  
  outputOptions(output, "ui_rc_slider", suspendWhenHidden = FALSE, priority = 10)
  outputOptions(output, "dt_recap", suspendWhenHidden = FALSE, priority = 5)
  outputOptions(output, "scatter_classif_plot", suspendWhenHidden = FALSE, priority = 2)
  outputOptions(output, "dt_classif", suspendWhenHidden = FALSE, priority = 1)
  
}

shinyApp(ui, server, ...)

}
