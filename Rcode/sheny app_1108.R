# Define the required packages
packages <- c("shiny", "dplyr", "readxl", "EGAnet", "igraph", 
              "qgraph", "DT", "haven", "ggplot2", "jtools", 
              "ggrain", "shadowtext", "coin")

# Check for missing packages and install them if necessary
installed_packages <- packages %in% rownames(installed.packages())
if (any(!installed_packages)) {
  install.packages(packages[!installed_packages], dependencies = TRUE)
}

# Load the packages
lapply(packages, library, character.only = TRUE)

# Define file paths
data_dir <- "C:/Users/Pei-Chin/Documents/GitHub/World-Mobility/data"
output_dir <- "C:/Users/Pei-Chin/Documents/GitHub/World-Mobility/output"

# Load GSS data (.sav file)
gss_data <- read_sav(file.path(data_dir, "GSS7218_R3.sav"))

# Load variable classification data (.xlsx file)
labels_data <- read_excel(file.path(data_dir, "Revised_info_modified0708 - youngjae suggestions_modified.xlsx"))

# Load pre-existing US Immigration Data
usImmigrationData <- read.csv(file.path(data_dir, "USImmigration.csv"))

# Load pre-existing US GDP Data
usGdpData <- read.csv(file.path(data_dir, "FREDGDP.csv"))
usGdpData$DATE <- as.Date(usGdpData$DATE, format = "%Y-%m-%d")
usGdpData$Year <- as.integer(format(usGdpData$DATE, "%Y"))
usGdpData <- usGdpData %>%
  group_by(Year) %>%
  summarise(GDP = mean(GDP, na.rm = TRUE))

# Calculate mean by year, ignoring NAs
data_by_year <- gss_data %>%
  group_by(YEAR) %>%
  summarise_all(mean, na.rm = TRUE)

# Count missing values per column
nan_count_per_column <- sapply(data_by_year, function(x) sum(is.na(x)))

# Add missing value counts to label data
labels_data <- labels_data %>%
  mutate(missing_count = nan_count_per_column[match(variable, names(nan_count_per_column))])

# Shiny app UI
ui <- fluidPage(
  titlePanel("GSS Data Classification, EGA Analysis, and Cluster Selection"),
  
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("filterType", "Choose Variable Types:",
                         choices = c("Likert Scale" = "Likert", 
                                     "Binary" = "Binary", 
                                     "Continuous" = "Continuous", 
                                     "Multichoice" = "Multichoice",
                                     "Administration" = "Administration")),
      sliderInput("missingThreshold", "Select Missing Values Threshold:",
                  min = 0, max = max(nan_count_per_column, na.rm = TRUE), value = 5),
      actionButton("filterButton", "Filter Data"),
      actionButton("runEGA", "Run EGA Analysis"),
      uiOutput("clusterSelectUI"),
      uiOutput("variableSelectUI"),
      actionButton("plotButton", "Generate Plots")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Filtered Data", DTOutput("filteredTable")),
        tabPanel("EGA Network", plotOutput("egaPlot")),
        tabPanel("Clustered Variables", DTOutput("clusteredTable")),
        tabPanel("Histogram Plot", plotOutput("histogramPlot")),
        tabPanel("Raincloud Plot", plotOutput("raincloudPlot")),
        tabPanel("Debug Data", DTOutput("debugTable"))
      )
    )
  )
)

# Shiny app server logic
server <- function(input, output, session) {
  
  calculateStatistics <- function(data, classified_data) {
    partial_r2_values <- numeric(nrow(classified_data))
    beta_values <- numeric(nrow(classified_data))
    se_values <- numeric(nrow(classified_data))
    t_values <- numeric(nrow(classified_data))
    
    for (i in seq_along(classified_data$variable)) {
      current_variable <- classified_data$variable[i]
      model <- lm(as.formula(paste(current_variable, "~ YEAR")), data = data)
      
      t_value <- summary(model)$coefficients["YEAR", "t value"]
      partial_r2 <- (sqrt(t_value^2 / (t_value^2 + model$df.residual)))^2
      
      partial_r2_values[i] <- partial_r2
      beta_values[i] <- summary(model)$coefficients["YEAR", "Estimate"]
      se_values[i] <- summary(model)$coefficients["YEAR", "Std. Error"]
      t_values[i] <- t_value
    }
    
    classified_data$Partial_R2 <- partial_r2_values
    classified_data$beta_value <- beta_values
    classified_data$se_value <- se_values
    classified_data$t_value <- t_values
    
    return(classified_data)
  }
  
  gss_new <- reactive({
    gss_new_data <- gss_data %>%
      left_join(usImmigrationData, by = c("YEAR" = "Year")) %>%
      rename("Immigration" = "Number") %>%
      left_join(usGdpData, by = c("YEAR" = "Year")) %>%
      arrange(YEAR)
    return(gss_new_data)
  })
  
  filteredData <- eventReactive(input$filterButton, {
    data <- labels_data
    data <- data %>% filter(
      (`Likert Scale Variables` == 1 & "Likert" %in% input$filterType) |
        (`Binary Variables` == 1 & "Binary" %in% input$filterType) |
        (`Continuous Variables` == 1 & "Continuous" %in% input$filterType) |
        (`Multichoice variables` == 1 & "Multichoice" %in% input$filterType) |
        (`Administration variable` == 1 & "Administration" %in% input$filterType)
    )
    data <- data %>% filter(missing_count <= input$missingThreshold)
    return(data)
  })
  
  output$filteredTable <- renderDT({
    filteredData()
  })
  
  observeEvent(input$filterButton, {
    structured_data <- filteredData()
    file_path <- file.path(output_dir, "Filtered_GSS_Data.csv")
    
    if (nrow(structured_data) > 0) {
      write.csv(structured_data, file_path, row.names = FALSE)
      showModal(modalDialog(
        title = "Success",
        paste("File saved successfully to", file_path)
      ))
    } else {
      showModal(modalDialog(
        title = "Error",
        "No data available to save."
      ))
    }
  })
  
  egaResults <- eventReactive(input$runEGA, {
    selected_vars <- filteredData()$variable
    req(length(selected_vars) > 0)
    
    GSS_Processed <<- gss_data %>% select(all_of(selected_vars))
    standardized_data <- scale(GSS_Processed)
    ega.tmfg <- EGA(standardized_data, model = "TMFG")
    cluster_assignments <- ega.tmfg$wc
    unique_clusters <- unique(cluster_assignments)
    cluster_info <- data.frame(variable = selected_vars, cluster = cluster_assignments)
    
    output$clusterSelectUI <- renderUI({
      selectInput("selectedCluster", "Select Cluster:", choices = unique_clusters)
    })
    
    list(ega = ega.tmfg, cluster_info = cluster_info)
  })
  
  observeEvent(input$selectedCluster, {
    req(egaResults())
    selected_cluster <- input$selectedCluster
    cluster_info <- egaResults()$cluster_info
    cluster_vars <- cluster_info %>% filter(cluster == selected_cluster) %>% pull(variable)
    
    output$variableSelectUI <- renderUI({
      selectizeInput("interestedVars", "Select Interested Variables:", 
                     choices = cluster_vars, multiple = TRUE)
    })
  })
  
  classifiedVariables <- reactive({
    req(input$selectedCluster, input$interestedVars)
    
    selected_cluster <- input$selectedCluster
    interested_vars <- input$interestedVars
    cluster_info <- egaResults()$cluster_info
    
    proximate_vars <- setdiff(cluster_info %>% filter(cluster == selected_cluster) %>% pull(variable), interested_vars)
    distal_vars <- cluster_info %>% filter(cluster != selected_cluster) %>% pull(variable)
    
    classified_data <- data.frame(
      variable = c(interested_vars, proximate_vars, distal_vars),
      category = c(rep("Interested", length(interested_vars)),
                   rep("Proximate", length(proximate_vars)),
                   rep("Distal", length(distal_vars)))
    )
    
    selected_columns <- classified_data$variable
    gss_selected <- gss_data %>% select(YEAR, ID, all_of(selected_columns))
    stats_results <- calculateStatistics(gss_selected, classified_data)
    
    return(stats_results)
  })
  
  output$egaPlot <- renderPlot({
    req(egaResults())
    ega.tmfg <- egaResults()$ega
    qgraph(ega.tmfg$network, layout = "spring", groups = as.factor(ega.tmfg$wc), 
           legend = TRUE, nodeNames = ega.tmfg$items, legend.cex = 0.4)
  })
}

# Run Shiny app
shinyApp(ui = ui, server = server)
