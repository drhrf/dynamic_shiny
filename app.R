# Load necessary libraries
library(shiny)
library(readxl)  # For reading Excel files
library(DT)      # For interactive data tables
library(ggplot2) # For advanced plotting
library(plotly)  # For interactive plots
library(dplyr)   # For data manipulation
library(corrplot) # For correlation plot

# Define UI
ui <- fluidPage(
  titlePanel("Análise de dados da Clínica-escola"),
  
  # Tabset for organizing different functionalities
  tabsetPanel(
    tabPanel("Dados",
             sidebarLayout(
               sidebarPanel(
                 actionButton("load_button", "Carregue os dados"),
                 textOutput("status")
               ),
               mainPanel(
                 DTOutput("data_table")
               )
             )),
    
    tabPanel("Gráficos",
             sidebarLayout(
               sidebarPanel(
                 h4("Plot Customization"),
                 selectInput("x_variable", "Select X Variable:", choices = NULL),
                 selectInput("y_variable", "Select Y Variable:", choices = NULL),
                 selectInput("plot_type", "Select Plot Type:",
                             choices = c("Scatter Plot" = "scatter",
                                         "Bar Plot (Two Categorical Variables)" = "bar_two_cat",
                                         "Box Plot (Numerical by Categorical)" = "boxplot")),
                 actionButton("plot_button", "Generate Plot"),
                 hr(),
                 h4("Download Plot"),
                 numericInput("plot_width", "Plot Width (inches):", value = 7),
                 numericInput("plot_height", "Plot Height (inches):", value = 5),
                 numericInput("plot_res", "Resolution (dpi):", value = 300),
                 downloadButton("download_plot", "Download Plot")
               ),
               mainPanel(
                 plotlyOutput("eda_plot", height = "500px")
               )
             )),
    
    tabPanel("Estatística descritiva",
             sidebarLayout(
               sidebarPanel(
                 selectInput("summary_variable", "Select Variable:", choices = NULL),
                 selectInput("group_variable", "Group By (Optional):", choices = NULL),
                 actionButton("summary_button", "Generate Summary")
               ),
               mainPanel(
                 verbatimTextOutput("summary_output")
               )
             ))
  )
)

# Define server logic
server <- function(input, output, session) {
  # Reactive values to store data and status
  values <- reactiveValues(data = NULL, message = NULL, plot = NULL)
  
  # Load dataset from the provided GitHub link on button click
  observeEvent(input$load_button, {
    tryCatch({
      # Load the dataset from the GitHub link
      file_url <- "https://raw.githubusercontent.com/drhrf/dynamic_shiny/refs/heads/main/Dados_clinica_escola.xlsx"
      temp_file <- tempfile(fileext = ".xlsx")
      
      # Download the file to a temporary location
      download.file(file_url, destfile = temp_file, mode = "wb")
      
      # Read the dataset from the downloaded file
      values$data <- read_excel(temp_file, sheet = "Data")
      
      # Update the selectInput choices for variable selection
      updateSelectInput(session, "x_variable", choices = names(values$data))
      updateSelectInput(session, "y_variable", choices = names(values$data))
      updateSelectInput(session, "summary_variable", choices = names(values$data))
      updateSelectInput(session, "group_variable", choices = c("None", names(values$data)))
      
      # Display success message
      output$status <- renderText("File successfully loaded and read!")
      
    }, error = function(e) {
      # Display any errors that occur during loading
      output$status <- renderText(paste("An error occurred:", e$message))
    })
  })
  
  # Render the filtered dataset for exploration in the Data Overview tab
  output$data_table <- renderDT({
    req(values$data)  # Ensure data is available
    datatable(values$data, options = list(pageLength = 10, searchHighlight = TRUE))
  })
  
  # Generate EDA plots based on user selections using ggplot2 or plotly
  observeEvent(input$plot_button, {
    req(values$data)  # Ensure data is loaded before plotting
    req(input$x_variable)  # Ensure X variable is selected
    req(input$y_variable)  # Ensure Y variable is selected
    
    plot_data <- values$data
    x_var <- input$x_variable
    y_var <- input$y_variable
    
    # Generate ggplot based on plot type
    p <- NULL
    if (input$plot_type == "scatter") {
      # Scatter Plot for two numerical variables
      if (is.numeric(plot_data[[x_var]]) && is.numeric(plot_data[[y_var]])) {
        p <- ggplot(plot_data, aes_string(x = x_var, y = y_var)) +
          geom_point(color = "blue") +
          labs(title = paste("Scatter Plot of", y_var, "vs", x_var),
               x = x_var, y = y_var) +
          theme_minimal()
      } else {
        output$status <- renderText("Both selected variables must be numeric for Scatter Plot.")
      }
    } else if (input$plot_type == "bar_two_cat") {
      # Bar Plot for two categorical variables
      if ((is.factor(plot_data[[x_var]]) || is.character(plot_data[[x_var]])) &&
          (is.factor(plot_data[[y_var]]) || is.character(plot_data[[y_var]]))) {
        p <- ggplot(plot_data, aes_string(x = x_var, fill = y_var)) +
          geom_bar(position = "dodge") +
          labs(title = paste("Bar Plot of", y_var, "by", x_var),
               x = x_var, y = "Count") +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
      } else {
        output$status <- renderText("Both selected variables must be categorical for Bar Plot.")
      }
    } else if (input$plot_type == "boxplot") {
      # Box Plot for a numerical variable by a categorical variable
      if (is.numeric(plot_data[[y_var]]) && (is.factor(plot_data[[x_var]]) || is.character(plot_data[[x_var]]))) {
        p <- ggplot(plot_data, aes_string(x = x_var, y = y_var, fill = x_var)) +
          geom_boxplot() +
          labs(title = paste("Box Plot of", y_var, "by", x_var),
               x = x_var, y = y_var) +
          theme_minimal()
      } else {
        output$status <- renderText("Selected X variable must be categorical and Y variable must be numeric for Box Plot.")
      }
    }
    
    # Render Plotly plot from ggplot
    if (!is.null(p)) {
      output$eda_plot <- renderPlotly({
        ggplotly(p)
      })
      # Save the plot to values for download as ggplot
      values$plot <- p
    }
  })
  
  # Download handler for plot
  output$download_plot <- downloadHandler(
    filename = function() {
      paste("plot_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      ggsave(file, plot = values$plot, width = input$plot_width, height = input$plot_height, dpi = input$plot_res)
    }
  )
  
  # Generate descriptive statistics for the selected variable
  observeEvent(input$summary_button, {
    req(values$data)  # Ensure data is loaded before summarizing
    req(input$summary_variable)  # Ensure a variable is selected
    
    output$summary_output <- renderPrint({
      plot_data <- values$data
      selected_var <- input$summary_variable
      group_var <- input$group_variable
      
      if (group_var == "None") {
        # Univariate summary statistics
        if (is.numeric(plot_data[[selected_var]])) {
          summary_stats <- plot_data %>%
            summarise(
              Mean = mean(.data[[selected_var]], na.rm = TRUE),
              Median = median(.data[[selected_var]], na.rm = TRUE),
              SD = sd(.data[[selected_var]], na.rm = TRUE),
              Min = min(.data[[selected_var]], na.rm = TRUE),
              Max = max(.data[[selected_var]], na.rm = TRUE),
              `NA Count` = sum(is.na(.data[[selected_var]]))
            )
          print(summary_stats)
        } else if (is.factor(plot_data[[selected_var]]) || is.character(plot_data[[selected_var]])) {
          summary_stats <- plot_data %>%
            group_by(.data[[selected_var]]) %>%
            summarise(Count = n()) %>%
            arrange(desc(Count))
          print(summary_stats)
        }
      } else {
        # Summary statistics grouped by another variable
        if (is.numeric(plot_data[[selected_var]]) && (is.factor(plot_data[[group_var]]) || is.character(plot_data[[group_var]]))) {
          summary_stats <- plot_data %>%
            group_by(.data[[group_var]]) %>%
            summarise(
              Mean = mean(.data[[selected_var]], na.rm = TRUE),
              Median = median(.data[[selected_var]], na.rm = TRUE),
              SD = sd(.data[[selected_var]], na.rm = TRUE),
              Min = min(.data[[selected_var]], na.rm = TRUE),
              Max = max(.data[[selected_var]], na.rm = TRUE),
              `NA Count` = sum(is.na(.data[[selected_var]]))
            )
          print(summary_stats)
        } else {
          print("Selected variable combination is not supported for summary.")
        }
      }
    })
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
