# Load required libraries
library(shiny)
library(ggplot2)
library(DT)
library(zip)
library(readr)

ui <- fluidPage(
  titlePanel("Bootstrap Hypothesis Testing"),
  
  sidebarLayout(
    sidebarPanel(
      radioButtons("test_mode", "Test Mode:",
                   choices = c("Univariate" = "univariate", "Bivariate" = "bivariate")),
      
      radioButtons("input_type", "Input Method:",
                   choices = c("Manual Entry" = "manual", "Upload CSV File" = "file")),
      
      # Manual entry
      conditionalPanel(
        condition = "input.input_type == 'manual' && input.test_mode == 'univariate'",
        textAreaInput("manual_data", "Enter numeric values (X, comma-separated):", "-0.56, -0.23, 1.56, 0.07, 0.13, 1.72, 0.46, -1.27, -0.69, -0.45, 
                      1.22, 0.36, 0.4, 0.11, -0.56, 1.79, 0.5, -1.97, 0.7, -0.47, -1.07, 
                      -0.22, -1.03, -0.73, -0.63, -1.69, 0.84, 0.15, -1.14, 1.25, 0.43, 
                      -0.3, 0.9, 0.88, 0.82, 0.69, 0.55, -0.06, -0.31, -0.38, -0.69, -0.21, 
                      -1.27, 2.17, 1.21, -1.12, -0.4, -0.47, 0.78, -0.08, 0.25, -0.03, -0.04, 
                      1.37, -0.23, 1.52, -1.55, 0.58, 0.12, 0.22, 0.38, -0.5, -0.33, -1.02, -1.07, 
                      0.3, 0.45, 0.05, 0.92, 2.05, -0.49, -2.31, 1.01, -0.71, -0.69, 1.03, -0.28, -1.22, 
                      0.18, -0.14, 0.01, 0.39, -0.37, 0.64, -0.22, 0.33, 1.1, 0.44, -0.33, 1.15, 0.99, 0.55, 
                      0.24, -0.63, 1.36, -0.6, 2.19, 1.53, -0.24, -1.03")
      ),
      
      conditionalPanel(
        condition = "input.input_type == 'manual' && input.test_mode == 'bivariate'",
        textAreaInput("manual_data_x", "Enter values for Group X (comma-separated):", "-0.56, -0.23, 1.56, 0.07, 0.13, 1.72, 0.46, -1.27, -0.69, -0.45, 
                      1.22, 0.36, 0.4, 0.11, -0.56, 1.79, 0.5, -1.97, 0.7, -0.47, -1.07, 
                      -0.22, -1.03, -0.73, -0.63, -1.69, 0.84, 0.15, -1.14, 1.25, 0.43, 
                      -0.3, 0.9, 0.88, 0.82, 0.69, 0.55, -0.06, -0.31, -0.38, -0.69, -0.21, 
                      -1.27, 2.17, 1.21, -1.12, -0.4, -0.47, 0.78, -0.08, 0.25, -0.03, -0.04, 
                      1.37, -0.23, 1.52, -1.55, 0.58, 0.12, 0.22, 0.38, -0.5, -0.33, -1.02, -1.07, 
                      0.3, 0.45, 0.05, 0.92, 2.05, -0.49, -2.31, 1.01, -0.71, -0.69, 1.03, -0.28, -1.22, 
                      0.18, -0.14, 0.01, 0.39, -0.37, 0.64, -0.22, 0.33, 1.1, 0.44, -0.33, 1.15, 0.99, 0.55, 
                      0.24, -0.63, 1.36, -0.6, 2.19, 1.53, -0.24, -1.03"),
        textAreaInput("manual_data_y", "Enter values for Group Y (comma-separated):", "1.29, 2.26, 1.75, 1.65, 1.05, 1.95, 1.22, 0.33, 1.62, 
                      2.92, 1.42, 2.61, 0.38, 1.94, 2.52, 2.3, 2.11, 1.36, 1.15, 0.98, 2.12, 1.05, 1.51, 
                      1.74, 3.84, 1.35, 2.24, 2.08, 1.04, 1.93, 3.44, 2.45, 2.04, 1.58, -0.05, 3.13, 
                      0.54, 2.74, 3.91, 0.56, 2.7, 1.74, 0.43, 0.49, 0.4, 1.47, 0.54, 2.69, 4.1, 
                      0.71, 2.79, 2.77, 2.33, 0.99, 1.88, 1.72, 2.56, 1.63, 2.98, 1.63, 3.05, 
                      0.95, 0.74, 5.24, 1.58, 2.3, 2.64, 1.52, 2.52, 2.37, 1.78, 2.07, 1.97, 
                      4.13, 1.26, 0.9, 2.04, 2.31, 2.44, 1.54, 0.94, 3.26, 1.65, 1.13, 1.76, 
                      1.8, 3.11, 2.08, 2.75, 1.5, 2.21, 1.68, 2.09, 1.1, 0.69, 4, 2.6, 0.75, 1.39, 0.81")
      ),
      
      # File upload
      conditionalPanel(
        condition = "input.input_type == 'file'",
        fileInput("data_file", "Upload CSV File", accept = ".csv"),
        uiOutput("file_column_ui")
      ),
      
      conditionalPanel(
        condition = "input.test_mode == 'bivariate'",
        radioButtons(
          "paired",
          "Are the samples paired?",
          choices = c("No" = FALSE, "Yes" = TRUE),
          selected = FALSE
        )
      ),
      
      
      
      numericInput("null_value", "Null Hypothesis Value (H0):", value = 0),
      selectInput("alternative", "Alternative Hypothesis:",
                  choices = c("two.sided", "greater", "less")),
      numericInput("alpha", "Significance Level (alpha):", value = 0.05, min = 0, max = 1),
      numericInput("num_bootstrap", "Number of Bootstrap Samples:", value = 1000, min = 100),
      selectInput("test_statistic", "Test Statistic:",
                  choices = c("Mean", "Median", "Variance")),
      numericInput("seed", "Random Seed:", value = 123)
      
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("1. Data Overview",
                 tabsetPanel(
                   tabPanel("Summary Tables",
                            
                            h4("Data Preview"),
                            DT::dataTableOutput("data_preview"),
                            br(),
                            
                            h4("Summary Table"),
                            h5("X"),
                            DTOutput("summary_x"),
                            downloadButton("download_summary_x", "Download Summary X"),
                            br(), br(),
                            
                            conditionalPanel(
                              condition = "input.test_mode == 'bivariate'",
                              h5("Y"),
                              DTOutput("summary_y"),
                              downloadButton("download_summary_y", "Download Summary Y")
                            )
                   ),
                   
                   tabPanel("EDA Plots",
                            uiOutput("eda_plots_ui")
                            
                            ,
                            
                            br(),
                            downloadButton("download_plot", "Download All EDA Plots (ZIP)")
                   )
                 )
        )
        ,
        
        tabPanel("2. Hypothesis Testing",
                 actionButton("run_test", "Run Bootstrap Test"),
                 verbatimTextOutput("test_results"),
                 plotOutput("bootstrap_plot"),
                 plotOutput("ci_plot"),
                 br(), br(),
                 downloadButton("download_ht_plots_zip", "Download All Hypothesis Plots (ZIP)"),
                 br(), br(),
                 downloadButton("download_data", "Download Bootstrap Replicates")
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  get_input_data <- reactive({
    req(input$input_type, input$test_mode)
    
    parse_numeric <- function(x) {
      x <- as.numeric(trimws(x))
      x <- x[!is.na(x)]
      validate(need(length(x) >= 2, "Data must contain at least two numeric values."))
      return(x)
    }
    
    if (input$input_type == "manual") {
      if (input$test_mode == "univariate") {
        x <- parse_numeric(unlist(strsplit(input$manual_data, ",")))
        return(list(x = x, y = NULL))
      } else {
        x <- parse_numeric(unlist(strsplit(input$manual_data_x, ",")))
        y <- parse_numeric(unlist(strsplit(input$manual_data_y, ",")))
        return(list(x = x, y = y))
      }
      
    } else {
      req(input$data_file)
      df <- read.csv(input$data_file$datapath)
      
      if (input$test_mode == "univariate") {
        req(input$file_column)
        x <- parse_numeric(df[[input$file_column]])
        return(list(x = x, y = NULL))
      } else {
        req(input$file_column_x, input$file_column_y)
        x <- parse_numeric(df[[input$file_column_x]])
        y <- parse_numeric(df[[input$file_column_y]])
        return(list(x = x, y = y))
      }
    }
  })
  
  
  
  x_data <- reactive({
    get_input_data()$x
  })
  
  y_data <- reactive({
    req(input$test_mode == "bivariate")
    get_input_data()$y
  })
  
  
  # UI for selecting column if file is uploaded
  output$file_column_ui <- renderUI({
    req(input$data_file)
    df <- read_csv(input$data_file$datapath, show_col_types = FALSE)
    
    if (input$test_mode == "bivariate") {
      tagList(
        selectInput("file_column_x", "Select Column for X:", choices = names(df)),
        selectInput("file_column_y", "Select Column for Y:", choices = names(df))
      )
    } else {
      selectInput("file_column", "Select Column:", choices = names(df))
    }
  })
  
  output$data_preview <- DT::renderDT({
    data <- get_input_data()
    
    # Univariate ??? single column
    if (is.null(data$y)) {
      df <- data.frame(X = data$x)
    } else {
      # Bivariate ??? two columns
      min_len <- min(length(data$x), length(data$y))
      
      df <- data.frame(
        X = data$x[1:min_len],
        Y = data$y[1:min_len]
      )
    }
    
    DT::datatable(
      df,
      options = list(scrollX = TRUE, searching = FALSE)
    )
  })
  
  # Placeholder for bootstrap test output
  test_output <- reactiveVal()
  
  
  summary_stats <- function(x) {
    data.frame(
      Mean = round(mean(x, na.rm = TRUE),2),
      SD = round(sd(x, na.rm = TRUE),2),
      Min = round(min(x, na.rm = TRUE),2),
      Q1 = round(quantile(x, 0.25, na.rm = TRUE),2),
      Median = round(median(x, na.rm = TRUE),2),
      Q3 = round(quantile(x, 0.75, na.rm = TRUE),2),
      Max = round(max(x, na.rm = TRUE),2)
    )
  }
  
  output$summary_x <- renderDT({
    req(input$test_mode == "bivariate" || input$test_mode == "univariate")
    data <- get_input_data()
    req(!is.null(data$x))
    datatable(summary_stats(data$x), options = list(dom = 't'),rownames = FALSE)
  })
  
  output$summary_y <- renderDT({
    req(input$test_mode == "bivariate")
    data <- get_input_data()
    req(!is.null(data$y))
    datatable(summary_stats(data$y), options = list(dom = 't'),rownames = FALSE)
  })
  
  
  output$download_summary_x <- downloadHandler(
    filename = function() {
      paste0("summary_x_", Sys.Date(), ".csv")
    },
    content = function(file) {
      data <- get_input_data()
      write.csv(summary_stats(data$x), file, row.names = FALSE)
    }
  )
  
  output$download_summary_y <- downloadHandler(
    filename = function() {
      paste0("summary_y_", Sys.Date(), ".csv")
    },
    content = function(file) {
      data <- get_input_data()
      write.csv(summary_stats(data$y), file, row.names = FALSE)
    }
  )
  
  
  
  
  observeEvent(input$run_test, {
    req(input$test_mode)
    set.seed(input$seed)
    
    if (input$test_mode == "univariate") {
      x <- x_data()
      
      out <- bootstrap_test_univariate(
        x = x,
        null_value = input$null_value,
        alternative = input$alternative,
        alpha = input$alpha,
        num_bootstrap = input$num_bootstrap,
        test_statistic = input$test_statistic,
        seed = input$seed
      )
    } else {
      x <- x_data()
      y <- y_data()
      
      out <- bootstrap_test_diff(
        x = x,
        y = y,
        null_value = input$null_value,
        alternative = input$alternative,
        alpha = input$alpha,
        num_bootstrap = input$num_bootstrap,
        test_statistic = input$test_statistic,
        seed = input$seed,
        paired = input$paired
      )
    }
    
    test_output(out)
  })
  
  # Display test results
  
  output$eda_plots_ui <- renderUI({
    if (input$test_mode == "univariate") {
      tagList(
        h4("Histograms"),
        fluidRow(
          column(6, offset = 3, plotOutput("hist_x"))
        ),
        
        h4("Boxplots"),
        fluidRow(
          column(6, offset = 3, plotOutput("box_x"))
        )
      )
    } else {
      tagList(
        h4("Histograms"),
        fluidRow(
          column(6, plotOutput("hist_x")),
          column(6, plotOutput("hist_y"))
        ),
        
        h4("Boxplots"),
        fluidRow(
          column(6, plotOutput("box_x")),
          column(6, plotOutput("box_y"))
        )
      )
    }
  })
  
  output$hist_x <- renderPlot({
    data <- get_input_data()
    req(data$x)
    ggplot(data.frame(x = data$x), aes(x = x)) +
      geom_histogram(bins = 20, fill = "steelblue", color = "white") +
      ggtitle("Histogram - X") +
      theme_minimal()
  })
  
  output$hist_y <- renderPlot({
    data <- get_input_data()
    req(data$y)
    ggplot(data.frame(y = data$y), aes(x = y)) +
      geom_histogram(bins = 20, fill = "darkorange", color = "white") +
      ggtitle("Histogram - Y") +
      theme_minimal()
  })
  
  output$box_x <- renderPlot({
    data <- get_input_data()
    req(data$x)
    ggplot(data.frame(x = data$x), aes(y = x)) +
      geom_boxplot(fill = "skyblue") +
      ggtitle("Boxplot - X") +
      theme_minimal()
  })
  
  output$box_y <- renderPlot({
    data <- get_input_data()
    req(data$y)
    ggplot(data.frame(y = data$y), aes(y = y)) +
      geom_boxplot(fill = "salmon") +
      ggtitle("Boxplot - Y") +
      theme_minimal()
  })
  
  
  
  
  output$download_plot <- downloadHandler(
    filename = function() {
      paste0("EDA_Plots_", Sys.Date(), ".zip")
    },
    content = function(file) {
      
      tmpdir <- tempdir()
      
      # Remove old PNGs (use full paths)
      old_pngs <- list.files(tmpdir, pattern = "\\.png$", full.names = TRUE)
      if (length(old_pngs) > 0) file.remove(old_pngs)
      
      data <- get_input_data()
      
      # File paths
      hist_x_path <- file.path(tmpdir, "hist_x.png")
      hist_y_path <- file.path(tmpdir, "hist_y.png")
      box_x_path  <- file.path(tmpdir, "box_x.png")
      box_y_path  <- file.path(tmpdir, "box_y.png")
      
      # Generate plots
      ggsave(hist_x_path,
             ggplot(data.frame(x = data$x), aes(x = x)) +
               geom_histogram(fill = "steelblue", color = "white", bins = 20) +
               ggtitle("Histogram - X") + theme_minimal(),
             bg = "white")
      
      if (!is.null(data$y)) {
        ggsave(hist_y_path,
               ggplot(data.frame(y = data$y), aes(x = y)) +
                 geom_histogram(fill = "darkorange", color = "white", bins = 20) +
                 ggtitle("Histogram - Y") + theme_minimal(),
               bg = "white")
      }
      
      ggsave(box_x_path,
             ggplot(data.frame(x = data$x), aes(y = x)) +
               geom_boxplot(fill = "skyblue") +
               ggtitle("Boxplot - X") + theme_minimal(),
             bg = "white")
      
      if (!is.null(data$y)) {
        ggsave(box_y_path,
               ggplot(data.frame(y = data$y), aes(y = y)) +
                 geom_boxplot(fill = "salmon") +
                 ggtitle("Boxplot - Y") + theme_minimal(),
               bg = "white")
      }
      
      # Collect only existing files
      files_to_zip <- c(hist_x_path, box_x_path)
      
      if (!is.null(data$y)) {
        files_to_zip <- c(files_to_zip, hist_y_path, box_y_path)
      }
      
      files_to_zip <- files_to_zip[file.exists(files_to_zip)]
      
      # Zip using zipr
      zip::zipr(
        zipfile = file,
        files = files_to_zip
      )
    },
    contentType = "application/zip"
  )
  
  
  
  
  
  
  output$test_results <- renderPrint({
    req(test_output())
    out <- test_output()
    
    cat("Method:", out$method, "\n")
    cat("Alternative Hypothesis:", out$alternative, "\n\n")
    
    cat("Test Statistic:", out$statistic, "\n")
    cat("Null Value:", out$null_value, "\n")
    cat("Significance Level (alpha):", out$significance_level, "\n\n")
    cat("Number of Bootstrap Samples (B):", out$B, "\n")
    
    
    cat("P-value:", out$p_value, "\n")
    cat("Confidence Interval:", paste0("[", round(out$conf_int[1], 4), ", ", round(out$conf_int[2], 4), "]"), "\n")
  })
  
  # Download Bootstrap Replicates
  output$download_data <- downloadHandler(
    filename = function() {
      paste("bootstrap_samples_", Sys.Date(), ".txt", sep = "")
    },
    content = function(file) {
      req(test_output())
      write.table(test_output()$bootstrap_samples, file, row.names = FALSE, col.names = FALSE)
    }
  )
  
  bootstrap_plot_reactive <- reactive({
    req(test_output())
    df <- data.frame(stat = test_output()$bootstrap_samples)
    
    ggplot(df, aes(x = stat)) +
      geom_histogram(color = "black", fill = "skyblue", bins = 30) +
      labs(title = "Bootstrap Distribution", 
           x = input$test_statistic, 
           y = "Frequency") +
      theme_minimal()
  })
  
  ci_plot_reactive <- reactive({
    req(test_output())
    df <- data.frame(stat = test_output()$bootstrap_samples)
    ci <- test_output()$conf_int
    null_val <- input$null_value
    
    ggplot(df, aes(x = stat)) +
      geom_histogram(aes(y = after_stat(density)), bins = 30,
                     fill = "lightgreen", color = "black") +
      geom_vline(xintercept = ci, color = "red", linetype = "dashed", linewidth = 1.2) +
      geom_vline(xintercept = null_val, color = "blue", linetype = "dotted", linewidth = 1.2) +
      labs(title = "Bootstrap Confidence Interval",
           x = input$test_statistic,
           y = "Density") +
      theme_minimal()
  })
  
  
  # Plot: Histogram of bootstrap samples
  output$bootstrap_plot <- renderPlot({
    bootstrap_plot_reactive()
  })
  
  # Plot: CI overlay
  output$ci_plot <- renderPlot({
    ci_plot_reactive()
  })
  
  
  
  output$download_ht_plots_zip <- downloadHandler(
    filename = function() {
      paste0("hypothesis_testing_plots_", Sys.Date(), ".zip")
    },
    content = function(file) {
      temp_dir <- tempdir()
      
      # Save bootstrap plot
      ggsave(file.path(temp_dir, "bootstrap_plot.png"),
             plot = bootstrap_plot_reactive(),
             width = 7, height = 5, dpi = 300, bg = "white")
      
      # Save CI plot
      ggsave(file.path(temp_dir, "ci_plot.png"),
             plot = ci_plot_reactive(),
             width = 7, height = 5, dpi = 300, bg = "white")
      
      # Build ZIP
      zip::zipr(zipfile = file,
                files = c(file.path(temp_dir, "bootstrap_plot.png"),
                          file.path(temp_dir, "ci_plot.png")))
    }
  )
  
}

# Run the app
shinyApp(ui = ui, server = server)
