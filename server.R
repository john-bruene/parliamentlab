library(shiny)
library(shinythemes)
library(DT)
library(ggplot2)
library(car)
library(nortest)
library(tseries)
library(RcmdrMisc)
library(lmtest)
library(dplyr)
library(cluster)  
library(clusterCrit)  
library(readr)
library(sortable)
library(plotly)
library(readxl)
library(umap)
library(dbscan)
library(FactoMineR)
library(factoextra)
library(cowplot)
library(packcircles)
library(viridis)
library(ggiraph)
library(fmsb) 
library(shinydashboard)
library(sf)
library(reactable)
library(tidyr)
library(sparkline)
library(htmltools)
library(waffle)
library(scales)
library(showtext)
library(reshape2)      
library(NbClust)       
library(gridExtra)     


# UMAP score tables and the voted-docs index are preprocessed into compact,
# gzip-compressed .rds files by scripts/convert_data_to_rds.R. The index
# columns (`Unnamed: 0`, `X.1`, `X`) have already been dropped and the
# coord1D / coord2D_red sign flips for P7-P9 are baked in; see the
# conversion script for details. Regenerate the .rds files whenever the
# source CSV / XLSX change.
P6 <- readRDS("data/P6_umap.rds")
P7 <- readRDS("data/P7_umap.rds")
P8 <- readRDS("data/P8_umap.rds")
P9 <- readRDS("data/P9_umap.rds")

EP6_9_Voted <- readRDS("data/EP6_9_Voted.rds")


five_thirty <- read.csv("www/clustered_congress.csv")


#####---------------------------------------









shinyServer(function(input, output, session) {
  

  
  clusterPlotObject <- reactiveVal(NULL)
  finalClusterPlotObject <- reactiveVal(NULL)

  
  
  generateClusterPlot <- function(data, clustering_columns, method_name, num_clusters) {
    # Standardtitel
    plot_title <- paste(method_name, "Clustering with", num_clusters, "Clusters")
    
    # Zusätzlicher Titeltext für HDBSCAN
    if (method_name == "HDBSCAN") {
      plot_title <- paste(method_name, "Clustering with minPts =", num_clusters)
    }
    
    # Plot erstellen
    p <- ggplot(data, aes_string(x = clustering_columns[1], y = clustering_columns[2], color = "Cluster")) +
      geom_point() +
      labs(title = plot_title) +
      theme_minimal() +
      theme(plot.title = element_text(size = 16, face = "bold"))
    
    # Zusätzliche Anpassung für MCA
    if (selected_mapping() == "MCA") {
      p <- p + coord_cartesian(ylim = c(NA, 1.2))
    }
    
    return(p)
  }
  
  
  
  output$downloadPlots <- downloadHandler(
    filename = function() {
      paste("plots.pdf")
    },
    content = function(file) {
      req(clusterPlotObject())
      req(finalClusterPlotObject())
      
      # Punkte im Koordinatensystem und Schriftgrößen anpassen
      clusterPlot <- clusterPlotObject() + 
        geom_point(size = 0.5) +  # Punktgröße im Plot
        theme(
          plot.title = element_text(size = 20, hjust = 0.5),   # Titel kleiner und zentriert
          axis.title = element_text(size = 16),                # Achsentitel kleiner
          axis.text = element_text(size = 14),                 # Achsenbeschriftungen kleiner
          legend.title = element_text(size = 16),              # Legendentitel kleiner
          legend.text = element_text(size = 14),               # Legendentext kleiner
          legend.key.size = unit(0.8, "cm")                   # Schlüsselgröße der Legende
        )
      
      finalClusterPlot <- finalClusterPlotObject() + 
        geom_point(size = 0.5) +  # Punktgröße im Plot
        theme(
          plot.title = element_text(size = 20, hjust = 0.5),   # Titel kleiner und zentriert
          axis.title = element_text(size = 16),                # Achsentitel kleiner
          axis.text = element_text(size = 14),                 # Achsenbeschriftungen kleiner
          legend.title = element_text(size = 16),              # Legendentitel kleiner
          legend.text = element_text(size =14),               # Legendentext kleiner
          legend.key.size = unit(0.8, "cm")                   # Schlüsselgröße der Legende
        )
      
      # PDF exportieren
      pdf(file, width = 16.54, height = 5.84, pointsize = 3)
      grid.arrange(finalClusterPlot, clusterPlot, ncol = 2)
      dev.off()
    }
  )
  


  
  
  
  # Function to get party colors, left-right order, and axis labels based on legislative period
  get_party_settings <- reactive({
    period <- input$selectedP  # Use input$selectedP within the reactive context
    
    # Define left-right order for each period
    left_right_orders <- list(
      "P6" = c(
        "Confederal Group of the European United Left - Nordic Green Left" = 1,
        "Socialist Group in the European Parliament" = 2,
        "Group of the Greens/European Free Alliance" = 3,
        "Group of the Alliance of Liberals and Democrats for Europe" = 4,
        "Group of the European People's Party (Christian Democrats) and European Democrats" = 5,
        "Union for Europe of the Nations Group" = 6,
        "Independence/Democracy Group" = 7,
        "Non-attached Members" = 8
      ),
      "P7" = c(
        "Confederal Group of the European United Left - Nordic Green Left" = 1,
        "Group of the Progressive Alliance of Socialists and Democrats in the European Parliament" = 2,
        "Group of the Greens/European Free Alliance" = 3,
        "Group of the Alliance of Liberals and Democrats for Europe" = 4,
        "Group of the European People's Party (Christian Democrats)" = 5,
        "European Conservatives and Reformists Group" = 6,
        "Europe of Freedom and Democracy Group" = 7,
        "Non-attached Members" = 8
      ),
      "P8" = c(
        "Confederal Group of the European United Left - Nordic Green Left" = 1,
        "Group of the Progressive Alliance of Socialists and Democrats in the European Parliament" = 2,
        "Group of the Greens/European Free Alliance" = 3,
        "Group of the Alliance of Liberals and Democrats for Europe" = 4,
        "Group of the European People's Party (Christian Democrats)" = 5,
        "European Conservatives and Reformists Group" = 6,
        "Europe of Freedom and Direct Democracy Group" = 7,
        "Europe of Nations and Freedom Group" = 8,
        "Non-attached Members" = 9
      ),
      "P9" = c(
        "The Left" = 1,
        "Socialists_Democrats" = 2,
        "Greens_EFA" = 3,
        "REG" = 4,
        "EPP" = 5,
        "ECR" = 6,
        "IDG" = 7,
        "NI" = 8,
        "Non-attached Members" = 9
      )
    )
    
    # Define party colors for each period
    party_colors_list <- list(
      "P6" = c(
        "Group of the European People's Party (Christian Democrats) and European Democrats" = "blue",
        "Confederal Group of the European United Left - Nordic Green Left" = "darkred",
        "Non-attached Members" = "grey",
        "Union for Europe of the Nations Group" = "brown",
        "Group of the Alliance of Liberals and Democrats for Europe" = "gold",
        "Group of the Greens/European Free Alliance" = "green",
        "Socialist Group in the European Parliament" = "red",
        "Independence/Democracy Group" = "lightblue"
      ),
      "P7" = c(
        "Group of the Alliance of Liberals and Democrats for Europe" = "gold",
        "Group of the European People's Party (Christian Democrats)" = "blue",
        "Europe of freedom and democracy Group" = "lightblue",
        "Non-attached Members" = "grey", 
        "Group of the Greens/European Free Alliance" = "green",
        "European Conservatives and Reformists Group" = "darkblue",
        "Group of the Progressive Alliance of Socialists and Democrats in the European Parliament" = "red",
        "Confederal Group of the European United Left - Nordic Green Left" = "darkred"
      ),
      "P8" = c(
        "Group of the Alliance of Liberals and Democrats for Europe" = "gold",
        "Non-attached Members" = "grey",
        "Group of the European People's Party (Christian Democrats)" = "blue",
        "European Conservatives and Reformists Group" = "darkblue",
        "Group of the Progressive Alliance of Socialists and Democrats in the European Parliament" = "red",
        "Group of the Greens/European Free Alliance" = "green",
        "Europe of Freedom and Direct Democracy Group" = "lightblue",
        "Europe of Nations and Freedom Group" = "purple",
        "Confederal Group of the European United Left - Nordic Green Left" = "darkred"
      ),
      "P9" = c(
        "REG" = "gold",
        "Socialists_Democrats" = "red",
        "EPP" = "blue",
        "Greens_EFA" = "green",
        "ECR" = "darkblue",
        "NI" = "grey",
        "The Left" = "darkred",
        "IDG" = "purple",
        "Non-attached Members" = "lightblue"
      )
    )
    
    # Define axis labels for each period
    axis_labels_list <- list(
      "P6" = c(
        "Confederal Group of the European United Left - Nordic Green Left" = "European United Left/Nordic Green Left",
        "Socialist Group in the European Parliament" = "Socialists",
        "Group of the Greens/European Free Alliance" = "Greens/EFA",
        "Group of the Alliance of Liberals and Democrats for Europe" = "Liberals & Democrats",
        "Group of the European People's Party (Christian Democrats) and European Democrats" = "EPP & ED",
        "Union for Europe of the Nations Group" = "Union for Europe of the Nations",
        "Independence/Democracy Group" = "Independence/Democracy",
        "Non-attached Members" = "Non-attached"
      ),
      "P7" = c(
        "Confederal Group of the European United Left - Nordic Green Left" = "European United Left/Nordic Green Left",
        "Group of the Progressive Alliance of Socialists and Democrats in the European Parliament" = "Socialists & Democrats",
        "Group of the Greens/European Free Alliance" = "Greens/EFA",
        "Group of the Alliance of Liberals and Democrats for Europe" = "Liberals & Democrats",
        "Group of the European People's Party (Christian Democrats)" = "EPP",
        "European Conservatives and Reformists Group" = "ECR",
        "Europe of Freedom and Democracy Group" = "Europe of Freedom & Democracy",
        "Non-attached Members" = "Non-attached"
      ),
      "P8" = c(
        "Confederal Group of the European United Left - Nordic Green Left" = "European United Left/Nordic Green Left",
        "Group of the Progressive Alliance of Socialists and Democrats in the European Parliament" = "Socialists & Democrats",
        "Group of the Greens/European Free Alliance" = "Greens/EFA",
        "Group of the Alliance of Liberals and Democrats for Europe" = "Liberals & Democrats",
        "Group of the European People's Party (Christian Democrats)" = "EPP",
        "European Conservatives and Reformists Group" = "ECR",
        "Europe of Freedom and Direct Democracy Group" = "Freedom & Direct Democracy",
        "Europe of Nations and Freedom Group" = "Nations & Freedom",
        "Non-attached Members" = "Non-attached"
      ),
      "P9" = c(
        "The Left" = "The Left",
        "Socialists_Democrats" = "Socialists & Democrats",
        "Greens_EFA" = "Greens/EFA",
        "REG" = "Liberals & Democrats",
        "EPP" = "EPP",
        "ECR" = "ECR",
        "IDG" = "ID",
        "NI" = "Non-attached",
        "Non-attached Members" = "Non-attached"
      )
    )
    
    # Retrieve settings for the specified period
    left_right <- left_right_orders[[period]]
    party_colors <- party_colors_list[[period]]
    axis_labels <- axis_labels_list[[period]]
    
    list(left_right_order = left_right, party_colors = party_colors, axis_labels = axis_labels)
  })
  
  
  
  #reactive part for polls
  data_react <- reactive({
    if (input$selectedP == "P9") {
      data_parliament <- P9
    } else if (input$selectedP == "P8") {
      data_parliament <- P8
    } else if (input$selectedP == "P7") {
      data_parliament <- P7
    } else if (input$selectedP == "P6") {
      data_parliament <- P6
    } 
    return(data_parliament)
  })
   
  # output$RawData <- DT::renderDataTable(
  #   DT::datatable({
  #     P9_reduced
  #   },
  #   options = list(
  #     pageLength = 10,  # Standardmäßig 10 Zeilen pro Seite anzeigen
  #     initComplete = JS(
  #       "function(settings, json) {",
  #       "$(this.api().table().header()).css({'background-color': 'moccasin', 'color': '1c1b1b'});",
  #       "}"
  #     ),  # JavaScript, um das Aussehen des Tabellenkopfes zu ändern
  #     columnDefs = list(list(className = 'dt-center', targets = "_all"))  # Alle Spalten zentrieren
  #   ),
  #   filter = "top",  # Filteroptionen oben in der Tabelle anzeigen
  #   selection = 'multiple',  # Mehrfachauswahl ermöglichen
  #   style = 'bootstrap',  # Bootstrap-Stil verwenden
  #   class = 'cell-border stripe',  # CSS-Klassen für Tabellenstil
  #   rownames = FALSE,  # Keine Zeilennamen anzeigen
  #   colnames = c("FullName", "Country", "Party", "EPG", "birthdate", "birthplace", "Age_At_Start", "Winning_Score", "Attendance_Score", "loyalty_score", "Activity_Index")  # Spaltennamen definieren
  #   )
  # )
  # 
  
  
##
  
  
  ####################
  # Initialize datasets with your data (assuming they are dynamically loaded via data_react)
  datasets <- reactiveValues(
    eurowatch = NULL,
    parltrack_bio = NULL,
    parltrack_activity = NULL,
    mergedData = NULL,
    cleanedData = NULL,
    transformedData = NULL
  )
  
  # Update datasets whenever the selected Legislature changes
  observe({
    data <- data_react()
    req(data)  # Ensure data is available
    
    # Create reduced datasets based on the selected data_parliament
    reduced_data_Eurowatch <- data %>% select("full", "Country", "Party", "EPG", starts_with("X"))
    reduced_data_Activity <- data %>% select("full", "CRE", "WDECL", "COMPARL", "REPORT", "REPORT_SHADOW", "COMPARL_SHADOW", "MOTION", "OQ", "WEXP", "WQ", "MINT", "IMOTION", "PRUNACT")
    reduced_data_Parl <- data %>% select("full", "title", "Photo", "birthdate", "birthplace", "Gender", "CV")
    
    # Directly assign the reduced datasets to reactiveValues
    datasets$eurowatch <- reduced_data_Eurowatch
    datasets$parltrack_bio <- reduced_data_Parl
    datasets$parltrack_activity <- reduced_data_Activity
  })
  
  
  observeEvent(input$mergeDatasets, {
    selectedDatasets <- input$datasetsToMerge
    
    # Initialize mergedData as NULL
    mergedData <- NULL
    
    # Check if 'full' column exists in each dataset before attempting to merge
    if ("Eurowatch (Simon Hix)" %in% selectedDatasets && 
        "full" %in% colnames(datasets$eurowatch) &&
        "Parltrack (Biographical)" %in% selectedDatasets && 
        "full" %in% colnames(datasets$parltrack_bio) &&
        "Parltrack (Activity)" %in% selectedDatasets && 
        "full" %in% colnames(datasets$parltrack_activity)) {
      
      # Merge all three datasets
      mergedData <- merge(datasets$eurowatch, datasets$parltrack_bio, by = "full", all.x = TRUE)
      mergedData <- merge(mergedData, datasets$parltrack_activity, by = "full", all.x = TRUE)
      
    } else if ("Eurowatch (Simon Hix)" %in% selectedDatasets && 
               "Parltrack (Biographical)" %in% selectedDatasets &&
               "full" %in% colnames(datasets$eurowatch) &&
               "full" %in% colnames(datasets$parltrack_bio)) {
      
      # Merge Eurowatch and Biographical datasets
      mergedData <- merge(datasets$eurowatch, datasets$parltrack_bio, by = "full", all.x = TRUE)
      
    } else if ("Eurowatch (Simon Hix)" %in% selectedDatasets && 
               "Parltrack (Activity)" %in% selectedDatasets &&
               "full" %in% colnames(datasets$eurowatch) &&
               "full" %in% colnames(datasets$parltrack_activity)) {
      
      # Merge Eurowatch and Activity datasets
      mergedData <- merge(datasets$eurowatch, datasets$parltrack_activity, by = "full", all.x = TRUE)
      
    } else if ("Parltrack (Biographical)" %in% selectedDatasets && 
               "Parltrack (Activity)" %in% selectedDatasets &&
               "full" %in% colnames(datasets$parltrack_bio) &&
               "full" %in% colnames(datasets$parltrack_activity)) {
      
      # Merge Biographical and Activity datasets
      mergedData <- merge(datasets$parltrack_bio, datasets$parltrack_activity, by = "full", all.x = TRUE)
      
    } else if ("Eurowatch (Simon Hix)" %in% selectedDatasets && "full" %in% colnames(datasets$eurowatch)) {
      # Only Eurowatch dataset selected
      mergedData <- datasets$eurowatch
      
    } else if ("Parltrack (Biographical)" %in% selectedDatasets && "full" %in% colnames(datasets$parltrack_bio)) {
      # Only Biographical dataset selected
      mergedData <- datasets$parltrack_bio
      
    } else if ("Parltrack (Activity)" %in% selectedDatasets && "full" %in% colnames(datasets$parltrack_activity)) {
      # Only Activity dataset selected
      mergedData <- datasets$parltrack_activity
      
    } else {
      # Display an error notification if 'full' column is missing in any selected dataset
      showNotification("The 'full' column is missing in one or more selected datasets. Please check the data.", type = "error")
      return(NULL)
    }
    
    # Update reactive values with the merged data
    datasets$mergedData <- mergedData
    datasets$cleanedData <- mergedData
  })
  
  # Display merged data in a table
  output$mergedDataPreview <- DT::renderDataTable({
    req(datasets$mergedData)  # Ensure mergedData is available
    
    # Safely select specific columns
    selected_columns <- datasets$mergedData %>% 
      select(any_of(c("full", "title", "Country", "Party", "EPG", "X1", "X2", "X3", "title", "Photo", "birthdate", "birthplace", "Gender", "CV", "CRE", "WDECL", "COMPARL", "REPORT", "REPORT_SHADOW", "COMPARL_SHADOW", "MOTION", "OQ", "WEXP", "WQ", "MINT", "IMOTION", "PRUNACT")))
    
    # Render the filtered dataset
    selected_columns
  })
  
  
  
  
  ### Data Cleaning ###
  # Dynamically load column names from merged data
  ### Load dataset variables dynamically after merging ###

  # UI for replacing NA with 0 (dynamically generated)
  output$naReplaceChoices <- renderUI({
    if (!is.null(datasets$mergedData)) {
      # Specify the default columns that should be pre-selected
      default_columns <- c("CRE", "WDECL", "COMPARL", "REPORT", "REPORT_SHADOW", "COMPARL_SHADOW", "MOTION", "OQ", "WEXP", "WQ", "MINT", "IMOTION", "PRUNACT")
      
      checkboxGroupInput(
        "replaceNAColumns", 
        "Replace NA with 0 in:", 
        choices = c("full", "Country", "Party", "EPG", "X1", "X2", "X3", "title", "Photo", "birthdate", "birthplace", "Gender", "CV", "CRE", "WDECL", "COMPARL", "REPORT", "REPORT_SHADOW", "COMPARL_SHADOW", "MOTION", "OQ", "WEXP", "WQ", "MINT", "IMOTION", "PRUNACT"),
        selected = default_columns
      )
    } else {
      p("No dataset available for NA replacement. Please merge the datasets first.")
    }
  })
  
  # Replace NAs with 0 in selected columns
  observeEvent(input$replaceNAWithZero, {
    if (!is.null(datasets$cleanedData) && !is.null(input$replaceNAColumns)) {
      columns_to_replace <- input$replaceNAColumns
      
      # Replace NAs with 0 in the selected columns
      datasets$cleanedData <- datasets$cleanedData %>%
        dplyr::mutate_at(vars(one_of(columns_to_replace)), ~replace(., is.na(.), 0))
      
      # Update the cleaned data preview after replacement
      output$cleanedDataPreview <- DT::renderDataTable({
        selected_columns <- datasets$cleanedData %>% 
          select("full", "Country", "Party", "EPG", "X1", "X2", "X3", "Photo", "birthdate", "birthplace", "Gender", "CRE", "WDECL", "COMPARL", "REPORT", "REPORT_SHADOW", "COMPARL_SHADOW", "MOTION", "OQ", "WEXP", "WQ", "MINT", "IMOTION", "PRUNACT")
        selected_columns
      })
    }
  })
  
  # Data Cleaning: Handle missing values based on selected columns
  observeEvent(input$cleanData, {
    if (!is.null(datasets$mergedData) && !is.null(input$cleanVars)) {
      datasets$cleanedData <- datasets$mergedData %>%
        dplyr::filter_at(vars(one_of(input$cleanVars)), all_vars(!is.na(.)))
    }
  })
  
  # Apply NA Rules (for rows and columns)
  observeEvent(input$applyNARules, {
    if (!is.null(datasets$cleanedData)) {
      # Define NA thresholds for rows and columns
      na_limit_rows <- input$naThreshold * ncol(datasets$cleanedData)
      na_limit_cols <- input$naThreshold * nrow(datasets$cleanedData)
      
      # Filter rows and columns based on NA thresholds
      datasets$cleanedData <- datasets$cleanedData[rowSums(is.na(datasets$cleanedData)) <= na_limit_rows, ]
      datasets$cleanedData <- datasets$cleanedData[, colSums(is.na(datasets$cleanedData)) <= na_limit_cols]
    }
  })
  
  # Display cleaned data in a table (specific columns)
  output$cleanedDataPreview <- DT::renderDataTable({
    req(datasets$cleanedData)  # Ensure cleanedData is available
    
    selected_columns <- datasets$cleanedData %>% 
      select(any_of(c("full", "title", "Country", "Party", "EPG", "X1", "X2", "X3", "title", "Photo", "birthdate", "birthplace", "Gender", "CV", "CRE", "WDECL", "COMPARL", "REPORT", "REPORT_SHADOW", "COMPARL_SHADOW", "MOTION", "OQ", "WEXP", "WQ", "MINT", "IMOTION", "PRUNACT")))
    selected_columns
  })
  
  # Plot missing values (only for columns with missing data)
  output$missingValuesPlot <- renderPlot({
    if (!is.null(datasets$cleanedData)) {
      missing_data <- is.na(datasets$cleanedData)
      missing_count <- colSums(missing_data)
      
      # Filter only columns with missing values
      columns_with_na <- missing_count[missing_count > 0]
      
      if (length(columns_with_na) > 0) {
        barplot(columns_with_na, las = 2, col = "lightblue", 
                main = "Missing Values per Variable", 
                ylab = "Number of Missing Values", xlab = "Variables")
      } else {
        plot.new()
        title(main = "No Missing Values in Selected Columns")
      }
    }
  })
  
  
  ##########################

      explanations <- list(
        "Loyalty Index" = div(
          h5(strong("Loyalty Score Formula:")),
          p("The loyalty score measures how often a politician votes in line with their political group."),
          withMathJax(p("$$ Loyalty~Score = \\frac{Votes~with~Group}{Total~Votes} $$")),
          p("You can adjust the weight for certain votes below."),
          checkboxInput("check_final_votes", "Only consider final votes", value = TRUE),
          checkboxInput("check_absent", "Ignore absent votes", value = TRUE),
          checkboxInput("check_abstain", "Count abstain as loyal", value = TRUE),
        ),
        
        "Attendance Rate" = div(
          h5(strong("Attendance Rate Formula:")),
          p("The attendance rate calculates how often a politician attends voting sessions."),
          withMathJax(p("$$ Attendance~Rate = \\frac{Attended~Sessions}{Total~Sessions} $$"))
        ),
        
        "Experience" = div(
          h5(strong("Experience Formula:")),
          p("Experience is calculated as the number of days a politician has served in the European parliament."),
          withMathJax(p("$$ Experience = Current~Legislative~Date - Active~since $$"))
        ),
        
        "Activity Index" = div(
          h5(strong("Activity Index Formula:")),
          p("The activity index measures the overall activity of a politician based on several factors (e.g., speeches, motions)."),
          withMathJax(p("$$ Activity~Index = \\sum (Activity~Types \\times Weight) $$"))
        ),
        
        "Winning Index" = div(
          h5(strong("Winning Score Formula:")),
          p("The winning score measures how often a politician votes with the winning side."),
          withMathJax(p("$$ Winning~Score = \\frac{Votes~with~Winning~Majority}{Total~Votes} $$")),
          checkboxInput("check_final_votes", "Only consider final votes", value = TRUE),
          checkboxInput("check_absent", "Ignore absent votes", value = TRUE)
        ),
        
        "Age" = div(
          h5(strong("Age Formula:")),
          p("The age of the politician is calculated based on the date they got into office and their birth year."),
          withMathJax(p("$$ Age = EnteringOffice~Year - Birth~Year $$")),
          checkboxInput("check_age", "Use Current Year instead", value = FALSE)
        ),
        
        "Topic Scores" = div(
          h5(strong("Topic Scores Formula:")),
          p("Topic scores represent how a politician votes on specific issues or topics. They are calculated as the proportion of votes in favor of a topic."),
          withMathJax(p("$$ Topic~Score = \\frac{Votes~on~Topic}{Total~Votes~on~Topic} $$")),
          p("You can chose the the topic scores you want to create"),
          checkboxGroupInput(
            "topicColumns", 
            "Chose topics:", 
            choices = c("Economic", "Social", "Foreign Policy", "Industry", "Education", "Healthcare", "Justice", "Agriculture", "Budget", "Gender", "Civil Libierties", "Trade", "Transport"),
            selected = c("Economic", "Social", "Foreign Policy", "Industry", "Education", "Healthcare") # These columns will be pre-selected
          )
        )
      )
      
  ### Apply Feature Engineering with Predefined Values Based on Legislative Period ###
  # List to store names of engineered features
  engineered_features <- reactiveVal(c())  # Initialize as empty
  
  ### Apply Feature Engineering with Predefined Values Based on Legislative Period ###
  observeEvent(input$applyFeatureEngineering, {
    selectedData <- data_react()  # Get the selected dataset based on legislative period
    
    if (!is.null(selectedData)) {
      engineeredData <- datasets$cleanedData  # Start with cleaned data
      
      # Match rows based on FullName
      matching_indices <- match(engineeredData$full, selectedData$full)
      
      # List to keep track of newly engineered feature names
      newly_engineered_features <- c()
      
      # Engineer features based on user selection without using user-defined weights/thresholds
      if ("Loyalty Index" %in% input$featureEngineeringOptions) {
        engineeredData$Loyalty_Index <- selectedData$loyalty_score[matching_indices]  # Add only matching rows
        newly_engineered_features <- c(newly_engineered_features, "Loyalty_Index")
      }
      
      if ("Attendance Rate" %in% input$featureEngineeringOptions) {
        engineeredData$Attendance_Rate <- selectedData$Attendance_Score[matching_indices]  # Add only matching rows
        newly_engineered_features <- c(newly_engineered_features, "Attendance_Rate")
      }
      
      if ("Experience" %in% input$featureEngineeringOptions) {
        engineeredData$Experience <- selectedData$Experience_at_Start[matching_indices]  # Add only matching rows
        newly_engineered_features <- c(newly_engineered_features, "Experience")
      }
      
      if ("Activity Index" %in% input$featureEngineeringOptions) {
        engineeredData$Activity_Index <- selectedData$Activity_Index[matching_indices]  # Add only matching rows
        newly_engineered_features <- c(newly_engineered_features, "Activity_Index")
      }
      
      if ("Winning Index" %in% input$featureEngineeringOptions) {
        engineeredData$Winning_Index <- selectedData$Winning_Score[matching_indices]  # Add only matching rows
        newly_engineered_features <- c(newly_engineered_features, "Winning_Index")
      }
      
      if ("Age" %in% input$featureEngineeringOptions) {
        engineeredData$Age <- selectedData$Age_At_Start[matching_indices]  # Add only matching rows
        newly_engineered_features <- c(newly_engineered_features, "Age")
      }
      
      if ("Topic Scores" %in% input$featureEngineeringOptions) {
        # Get selected topics from user input
        selected_topics <- input$topicColumns
        
        # Define the mapping between topic names and column names in selectedData
        topic_mapping <- list(
          "Economic" = "economic_votesScore",
          "Social" = "social_votesScore",
          "Foreign Policy" = "foreign_policy_votesScore",
          "Industry" = "industry_votesScore",
          "Education" = "education_votesScore",
          "Healthcare" = "health_votesScore",
          "Justice" = "law_votesScore",
          "Agriculture" = "agriculture_fisheries_votesScore",
          "Budget" = "budget_votesScore",
          "Gender" = "gender_votesScore",
          "Civil Liberties" = "civil_liberties_votesScore",
          "Trade" = "international_trade_votesScore",
          "Transport" = "transport_tourism_votesScore"
        )
        
        # Loop over selected topics and dynamically generate engineered features
        for (topic in selected_topics) {
          column_name <- topic_mapping[[topic]]  # Get corresponding column name
          
          # Check if the column exists in selectedData
          if (!is.null(column_name) && column_name %in% colnames(selectedData)) {
            feature_name <- paste0(topic, "_Score")  # Create feature name dynamically
            engineeredData[[feature_name]] <- selectedData[[column_name]][matching_indices]
            newly_engineered_features <- c(newly_engineered_features, feature_name)
          } else {
            warning(paste("Column", column_name, "does not exist in selectedData. Skipping", topic))
          }
        }
      }
      
      
      # Save the newly engineered data in reactive values
      datasets$engineeredData <- engineeredData
      engineered_features(newly_engineered_features)  # Update the list of engineered features
      
      # Preview the engineered features
      output$engineeredFeaturesPreview <- DT::renderDataTable({
        datasets$engineeredData
      })
      
      # Update the dropdown to show only engineered features, with Age as the default if present
      output$variableSelectorUI <- renderUI({
        selectInput(
          inputId = "selectedVariable",
          label = "Variable to Visualize:",
          choices = newly_engineered_features,  # Show only engineered features
          selected = if ("Age" %in% newly_engineered_features) "Age" else newly_engineered_features[1]  # Default to "Age" if available
        )
      })
      
      # Update the dropdown to show only engineered features, with Age as the default if present
      output$variableSelectorUI2 <- renderUI({
        selectInput(
          inputId = "selectedVariable2",
          label = "Variable to Visualize:",
          choices = newly_engineered_features,  # Show only engineered features
          selected = if ("Age" %in% newly_engineered_features) "Age" else newly_engineered_features[1]  # Default to "Age" if available
        )
      })
      
      
      
      # Dynamisch erzeugtes selectInput nur anzeigen, wenn Features vorhanden sind
      output$variableSelectorUI1 <- renderUI({
        # Check if there are engineered features
        if (!is.null(newly_engineered_features) && length(newly_engineered_features) > 0) {
          # Create the selectInput if features exist
          selectInput(
            inputId = "selectedVariable1",
            label = "Variable to Visualize:",
            choices = newly_engineered_features,  # Show only engineered features
            selected = if ("Age" %in% newly_engineered_features) "Age" else newly_engineered_features[1]  # Default to "Age" if available
          )
        } else {
          NULL  # Return NULL if no features are available, so nothing is rendered
        }
      })
    }
  })
  
  
  
  output$featureExplanation <- renderUI({
    selected_features <- input$featureEngineeringOptions
    
    if (!is.null(selected_features) && length(selected_features) > 0) {
      # Iterate over each selected feature and render its explanation
      lapply(selected_features, function(feature) {
        if (feature %in% names(explanations)) {
          explanations[[feature]]
        }
      })
    } else {
      p("Please select one or more features to view their explanations.")
    }
  })
  
  # General feature distribution plot in Feature Engineering
  output$featureDistributionPlot <- renderPlot({
    req(input$selectedVariable1)
    var_name <- input$selectedVariable1
    
    # Check if the selected feature exists and has valid numeric data
    feature_data <- datasets$engineeredData[[var_name]]
    if (!is.null(feature_data) && is.numeric(feature_data) && !all(is.na(feature_data))) {
      # Plot only if there is numeric, non-NA data
      hist(feature_data, main = paste("Distribution of", var_name), 
           xlab = var_name, col = "skyblue", border = "black")
    } else {
      # Create an empty plot if data is not valid for plotting
      plot.new()
      title(main = paste("Variable", var_name, "is not suitable for plotting (non-numeric or all NA values)"))
    }

    
  })
  

  
  ### Data Transformation ###
  # Define the bins and their UI outputs as in your previous code. 
  # Use `engineeredData` for data transformations instead of `mergedData`
  
  
  # Populate transformation bins dynamically based on engineered data
  observe({
    req(datasets$engineeredData)  # Ensure engineeredData is available
    
    # Only include engineered features
    engineered_vars <- engineered_features()  # Retrieve the names of engineered features
    
    # Populate initial transformation bins with engineered variables
    output$noChangesBinUI <- renderUI({
      rank_list(
        text = "Drag variables that require no changes here",
        labels = engineered_vars,  # Initially populate with all engineered features
        input_id = "noChangesBin",
        options = sortable_options(group = "transformGroup")
      )
    })
    
    output$standardScalingBinUI <- renderUI({
      rank_list(
        text = "Drag variables to apply Standard Scaling",
        labels = NULL,  # Initially empty
        input_id = "standardScalingBin",
        options = sortable_options(group = "transformGroup")
      )
    })
    
    output$minMaxScalingBinUI <- renderUI({
      rank_list(
        text = "Drag variables to apply Min-Max Scaling",
        labels = NULL,  # Initially empty
        input_id = "minMaxScalingBin",
        options = sortable_options(group = "transformGroup")
      )
    })
    
    output$logScalingBinUI <- renderUI({
      rank_list(
        text = "Drag variables to apply Log Scaling",
        labels = NULL,  # Initially empty
        input_id = "logScalingBin",
        options = sortable_options(group = "transformGroup")
      )
    })
    
    output$binningBinUI <- renderUI({
      rank_list(
        text = "Drag variables to apply Binning",
        labels = NULL,  # Initially empty
        input_id = "binningBin",
        options = sortable_options(group = "transformGroup")
      )
    })
  })
  
  # Apply Recommended Transformations
  observeEvent(input$applyRecommended, {
    # Define recommended transformations for each engineered variable
    recommendations <- list(
      "Attendance_Rate" = "noChangesBinUI",
      "Loyalty_Index" = "standardScalingBinUI",
      "Winning_Index" = "standardScalingBinUI",
      "Age" = "noChangesBinUI",
      "Experience" = "logScalingBinUI",
      "Activity_Index" = "standardScalingBinUI",
      "Economy_Score" = "minMaxScalingBinUI",
      "Social_Score" = "minMaxScalingBinUI",
      "Foreign_Policy_Score" = "minMaxScalingBinUI",
      "Industry_Score" = "minMaxScalingBinUI",
      "Education_Score" = "minMaxScalingBinUI",
      "Budget_Score" = "minMaxScalingBinUI",
      

      "Healthcare_Score" = "minMaxScalingBinUI",
      "Justice_Score" = "minMaxScalingBinUI",
      "Agriculture_Score" = "minMaxScalingBinUI",
      "Gender_Score" = "minMaxScalingBinUI",
      "Civil_Liberties_Score" = "minMaxScalingBinUI",
      "Trade_Score" = "minMaxScalingBinUI",
      "Transport_Score" = "minMaxScalingBinUI"
    )
    
    # Filter recommendations to include only engineered variables
    engineered_vars <- engineered_features()  # Get only the engineered features
    
    filtered_recommendations <- recommendations[names(recommendations) %in% engineered_vars]
    
    # Populate each bin based on filtered recommendations
    output$noChangesBinUI <- renderUI({
      rank_list(
        text = "Drag variables that require no changes here",
        labels = names(filtered_recommendations)[sapply(filtered_recommendations, function(x) x == "noChangesBinUI")],
        input_id = "noChangesBin",
        options = sortable_options(group = "transformGroup")
      )
    })
    
    output$standardScalingBinUI <- renderUI({
      rank_list(
        text = "Drag variables to apply Standard Scaling",
        labels = names(filtered_recommendations)[sapply(filtered_recommendations, function(x) x == "standardScalingBinUI")],
        input_id = "standardScalingBin",
        options = sortable_options(group = "transformGroup")
      )
    })
    
    output$minMaxScalingBinUI <- renderUI({
      rank_list(
        text = "Drag variables to apply Min-Max Scaling",
        labels = names(filtered_recommendations)[sapply(filtered_recommendations, function(x) x == "minMaxScalingBinUI")],
        input_id = "minMaxScalingBin",
        options = sortable_options(group = "transformGroup")
      )
    })
    
    output$logScalingBinUI <- renderUI({
      rank_list(
        text = "Drag variables to apply Log Scaling",
        labels = names(filtered_recommendations)[sapply(filtered_recommendations, function(x) x == "logScalingBinUI")],
        input_id = "logScalingBin",
        options = sortable_options(group = "transformGroup")
      )
    })
    
    output$binningBinUI <- renderUI({
      rank_list(
        text = "Drag variables to apply Binning",
        labels = names(filtered_recommendations)[sapply(filtered_recommendations, function(x) x == "binningBinUI")],
        input_id = "binningBin",
        options = sortable_options(group = "transformGroup")
      )
    })
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ### Apply Transformation to Engineered Features ###
  observeEvent(input$applyTransformation, {
    req(ncol(datasets$engineeredData) > 0)  # Ensure engineeredData exists
    
    transformedData <- datasets$engineeredData  # Start with engineered data
    engineered_vars <- engineered_features()  # Only work with engineered variables
    
    # Capture the variables in each transformation bin
    noChangeVars <- input$noChangesBin
    standardScalingVars <- input$standardScalingBin
    minMaxScalingVars <- input$minMaxScalingBin
    logScalingVars <- input$logScalingBin
    binningVars <- input$binningBin
    
    # Apply transformations only to engineered variables
    # Standard Scaling
    if (!is.null(standardScalingVars)) {
      for (var in standardScalingVars) {
        if (var %in% engineered_vars && !is.null(transformedData[[var]]) && is.numeric(transformedData[[var]])) {
          transformedData[[var]] <- scale(transformedData[[var]])
        }
      }
    }
    
    # Min-Max Scaling
    if (!is.null(minMaxScalingVars)) {
      for (var in minMaxScalingVars) {
        if (var %in% engineered_vars && !is.null(transformedData[[var]]) && is.numeric(transformedData[[var]])) {
          valid_data <- transformedData[[var]][is.finite(transformedData[[var]])]
          if (length(valid_data) > 1) {
            transformedData[[var]] <- (transformedData[[var]] - min(valid_data)) / (max(valid_data) - min(valid_data))
          }
        }
      }
    }
    
    # Log Scaling
    if (!is.null(logScalingVars)) {
      for (var in logScalingVars) {
        if (var %in% engineered_vars && !is.null(transformedData[[var]]) && is.numeric(transformedData[[var]]) && all(transformedData[[var]] >= 0, na.rm = TRUE)) {
          transformedData[[var]] <- log1p(transformedData[[var]])
        }
      }
    }
    
    # Binning with custom breaks for Attendance_Rate
    if (!is.null(binningVars)) {
      for (var in binningVars) {
        if (var %in% engineered_vars && !is.null(transformedData[[var]]) && is.numeric(transformedData[[var]])) {
          if (var == "Attendance_Rate") {
            # Define custom breaks for Attendance_Rate
            transformedData[[var]] <- cut(
              transformedData[[var]], 
              breaks = c(0, 0.5, 0.75, 0.9, 0.98, 1),  # Custom break points (adjust as needed)
              labels = c("Very Low (≤50 %)", "Low (≤75 %)", "Moderate (≤90 %)", "High (≤98 %)", "Very High (≤100%)"),  # Labels for each bin
              include.lowest = TRUE
            )
          } else {
            # Default binning for other variables
            transformedData[[var]] <- cut(
              transformedData[[var]], 
              breaks = 4,  # Default number of bins
              labels = FALSE, 
              include.lowest = TRUE
            )
          }
        }
      }
    }
    
    transformedData <- transformedData %>%
      mutate(across(where(is.numeric), ~ round(.x, 2)))
    
    # Save the transformed data in reactive values
    datasets$transformedData <- transformedData
    
    # Display the transformed data
    output$transformedDataPreview <- DT::renderDataTable({
      datasets$transformedData
    })
    
    # Show before and after histograms for selected variable
    output$beforeTransformPlot <- renderPlot({
      req(input$selectedVariable)
      var_name <- input$selectedVariable
      if (!is.null(datasets$engineeredData[[var_name]]) && is.numeric(datasets$engineeredData[[var_name]])) {
        hist(datasets$engineeredData[[var_name]], main = paste("Original Distribution of", var_name), 
             xlab = var_name, col = "lightblue", border = "black")
      } else {
        plot.new()
        title(main = paste("Variable", var_name, "is not numeric or has no data"))
      }
    })
    
    output$afterTransformPlot <- renderPlot({
      req(input$selectedVariable)
      var_name <- input$selectedVariable
      
      # Check if the selected variable exists in the transformed dataset
      if (!is.null(datasets$transformedData[[var_name]])) {
        if (is.numeric(datasets$transformedData[[var_name]])) {
          # Use a histogram for numeric data
          hist(datasets$transformedData[[var_name]], 
               main = paste("Transformed Distribution of", var_name), 
               xlab = var_name, col = "salmon", border = "black")
        } else if (is.factor(datasets$transformedData[[var_name]])) {
          # Use a bar plot for categorical data (e.g., binned Attendance_Rate)
          barplot(table(datasets$transformedData[[var_name]]), 
                  main = paste("Binned Distribution of", var_name), 
                  xlab = var_name, col = "salmon", border = "black")
        } else {
          plot.new()
          title(main = paste("Variable", var_name, "is not numeric or categorical"))
        }
      } else {
        plot.new()
        title(main = paste("Variable", var_name, "is not available in transformed data"))
      }
    })
    
  })
  
  ###################
  
 ######### FINAL
  
  
  # Check if transformed data exists
  output$transformedDataExists <- reactive({
    !is.null(datasets$transformedData) && ncol(datasets$transformedData) > 0
  })
  outputOptions(output, "transformedDataExists", suspendWhenHidden = FALSE)
  
  # Download button functionality for the transformed dataset
  output$downloadFinalData <- downloadHandler(
    filename = function() {
      paste("final_dataset.csv")
    },
    content = function(file) {
      req(datasets$transformedData)
      write.csv(datasets$transformedData, file, row.names = FALSE)
    }
  )
  
  output$summaryStats <- DT::renderDataTable({
    req(datasets$transformedData)
    
    # Check if the switch is ON for full data
    if (input$show_full_data) {
      # Show the full dataset
      datasets$transformedData
    } else {
      # Show only the first 10 rows and first 10 columns
      datasets$transformedData %>%
        select(1:min(10, ncol(.))) %>%  # Select first 10 columns or fewer if the dataset has fewer columns
        head(10)                         # Display first 10 rows
    }
  }, options = list(pageLength = 10, scrollX = TRUE))  # Add scrolling for large tables
  
  
  
  ###################
  
  ### EXPLORATORY ###
  
  
  ##### SUMMARY TABLE #####
  
  # RenderUI für Summary Table
  output$summaryTable <- renderUI({
    
    # Aggregating data für Summary mit Sortierung basierend auf EPG Reihenfolge im Boxplot
    req(input$selectedVariable2)  # Nur weiterfahren, wenn eine Variable ausgewählt ist
    
    # Holen der EPG-Kurzbezeichnungen aus den axis_labels
    data <- datasets$transformedData
    settings <- get_party_settings()
    axis_labels <- settings$axis_labels  # Kurzbezeichnungen der Parteien für den Boxplot
    
    # Datenaufbereitung mit Kurzbezeichnungen für die Tabelle
    summary_data <- data %>%
      filter(!is.na(EPG)) %>%
      mutate(
        selected_value = as.numeric(.data[[input$selectedVariable2]]),
        EPG_short = factor(EPG, levels = names(axis_labels), labels = axis_labels)  # Verwenden von Kurzbezeichnungen
      ) %>%
      group_by(EPG_short) %>%
      summarise(
        Avg_Selected_Variable = mean(selected_value, na.rm = TRUE),
        Num_MEPs = n()
      ) %>%
      arrange(match(EPG_short, levels(factor(axis_labels))))  # Sortieren nach Reihenfolge im Boxplot
    
    # Kompakte Tabelle rendern
    reactable(
      summary_data,
      columns = list(
        EPG_short = colDef(name = "Political Group", minWidth = 100,
                           style = list(color = "black", fontWeight = "bold"),
                           headerStyle = list(backgroundColor = "lightgrey")),
        Avg_Selected_Variable = colDef(name = paste("Avg.", input$selectedVariable2),
                                       format = colFormat(digits = 1),
                                       style = list(fontSize = "10px"),
                                       cell = function(value) {
                                         if (value > 0.8) {
                                           tags$div(style = "color: green; font-weight: bold", value)
                                         } else if (value > 0.6) {
                                           tags$div(style = "color: orange; font-weight: bold", value)
                                         } else {
                                           tags$div(style = "color: red; font-weight: bold", value)
                                         }
                                       }),
        Num_MEPs = colDef(name = "MEPs",
                          style = list(color = "black", fontWeight = "bold", fontSize = "12px"),
                          headerStyle = list(backgroundColor = "lightgrey"))
      ),
      bordered = TRUE,
      striped = TRUE,
      highlight = TRUE,
      defaultPageSize = 6,
      pagination = FALSE,
      theme = reactableTheme(
        backgroundColor = "white",
        borderColor = "black",
        highlightColor = "#e6f7ff",
        cellPadding = "5px"
      )
    )
  })
  
  ##### BOXPLOT #####
  
  # Extended Box Plot for EPG-specific analysis
  output$extendedBoxPlot <- renderPlot({
    req(input$selectedVariable2)
    
    # Use transformed data instead of data_react()
    data <- datasets$transformedData
    settings <- get_party_settings()
    
    # Retrieve color palette and axis labels for each EPG from the settings
    party_colors <- settings$party_colors
    axis_labels <- settings$axis_labels  # Use dynamic axis labels based on selected period
    
    # Sort EPG by mean values if the sortmean checkbox is selected
    if (input$sortmean) {
      mean_values <- data %>%
        group_by(EPG) %>%
        summarize(mean_values = mean(.data[[input$selectedVariable2]], na.rm = TRUE)) %>%
        arrange(desc(mean_values))  # Sort in descending order
      
      data$EPG <- factor(data$EPG, levels = mean_values$EPG)
    }
    
    # Create the boxplot for the selected variable
    ggplot(data = data, aes(x = EPG, y = .data[[input$selectedVariable2]], fill = EPG)) +
      geom_boxplot(color = "black") +
      labs(title = paste("Distribution of", input$selectedVariable2, "by Political Group (EPG)"),
           x = "Faction in the EP", y = input$selectedVariable2) +
      scale_fill_manual(values = party_colors) +
      scale_x_discrete(labels = axis_labels) +  # Use dynamic axis labels from settings
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 30, hjust = 1, size = 10),  # Rotate text
        legend.position = "none",  # Remove legend
        plot.title = element_text(size = 16, face = "bold")
      )
  })
  
  

  # Plot rendern und Daten verarbeiten
  output$countryMapPlot <- renderPlot({
    req(input$selectedVariable2)  # Sicherstellen, dass eine Variable ausgewählt ist
    
    # Laden der Eurostat-Geodaten und Vorbereitung
    SHP_0 <- readRDS("data/SHP_0.rds")
    
    # Zuordnungstabelle erstellen, um Ländernamen in geo-Codes umzuwandeln
    country_codes <- tibble(
      Country = c("Germany", "Portugal", "Luxembourg", "United Kingdom", "France", "Italy", "Netherlands",
                  "Denmark", "Ireland", "Spain", "Belgium", "Austria", "Finland", "Sweden", "Greece",
                  "Cyprus", "Lithuania", "Poland", "Hungary", "Latvia", "Estonia", "Slovakia", "Malta",
                  "Slovenia", "Czech Republic", "Romania", "Bulgaria"),
      geo = c("DE", "PT", "LU", "UK", "FR", "IT", "NL", "DK", "IE", "ES", "BE", "AT", "FI", "SE",
              "GR", "CY", "LT", "PL", "HU", "LV", "EE", "SK", "MT", "SI", "CZ", "RO", "BG")
    )
    
    # Zuordnung der EU28-Länder, inklusive UK
    EU28_with_UK <- country_codes %>%
      filter(geo %in% unique(SHP_0$NUTS_ID))  # Filtert nur vorhandene Länder aus SHP_0
    
    # Geodaten auf EU28 inklusive UK beschränken und transformieren
    SHP_28_with_UK <- SHP_0 %>%
      select(geo = NUTS_ID, geometry) %>%
      inner_join(EU28_with_UK, by = "geo") %>%
      st_as_sf() %>%
      st_transform(crs = 4326)
    
    # Daten vorbereiten: transformierte Daten aggregieren nach Land
    aggregated_data <- datasets$transformedData %>%
      group_by(Country) %>%
      summarize(mean_value = mean(.data[[input$selectedVariable2]], na.rm = TRUE)) %>%
      left_join(country_codes, by = "Country")  # Ländercodes hinzufügen
    
    # Zusammenführen der Geodaten mit den aggregierten Werten
    map_data <- SHP_28_with_UK %>%
      left_join(aggregated_data, by = "geo")
    
    # ggplot Karte erstellen
    ggplot(map_data) +
      geom_sf(aes(fill = mean_value), color = "white", size = 0.2) +
      scale_fill_viridis(
        name = input$selectedVariable2,
        option = "magma",
        direction = -1,
        begin = 0.2, end = 0.9,
        labels = scales::label_number(accuracy = 0.1)  # Genauere Beschriftung
      ) +
      labs(
        title = paste("Avg.", input$selectedVariable2, "per Country")
      ) +
      coord_sf(xlim = c(-10, 35), ylim = c(35, 70), expand = FALSE) +  # Zoom auf Europa
      theme_void() +
      theme(
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 15, hjust = 0.5),
        plot.caption = element_text(size = 10, hjust = 0),
        legend.position = "right",
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10)
      )
  })
  
  
  
  
  # Tabelle der Top 3 und Last 3 Länder
  output$tabletop <- renderReactable({
    req(input$selectedVariable2)
    
    # Aggregieren der Daten und Hinzufügen von Platzierungen
    aggregated_data <- datasets$transformedData %>%
      group_by(Country) %>%
      summarize(mean_value = mean(.data[[input$selectedVariable2]], na.rm = TRUE)) %>%
      arrange(desc(mean_value)) %>%
      mutate(Platzierung = row_number())
    
    # Auswahl der Top 3 und Last 3 in der gewünschten Reihenfolge
    top3 <- head(aggregated_data, 3) %>% arrange(Platzierung)  # Top 3 in aufsteigender Reihenfolge (1, 2, 3)
    last3 <- tail(aggregated_data, 3) %>% arrange(Platzierung)  # Last 3 ebenfalls in aufsteigender Reihenfolge (25, 26, 27)
    
    # Zusammenführen der Ergebnisse ohne weitere Sortierung
    result <- bind_rows(top3, last3)
    
    # Reactable Tabelle erstellen
    reactable(result,
              columns = list(
                Platzierung = colDef(
                  name = "Rank",
                  cell = function(value) {
                    if (value <= 3) {
                      tagList(icon("arrow-up", style = "color: #1a9641;"), paste(value))
                    } else {
                      tagList(icon("arrow-down", style = "color: #d73027;"), paste(value))
                    }
                  },
                  align = "center",
                  style = list(fontWeight = "bold")
                ),
                Country = colDef(
                  name = "Country",
                  style = list(fontWeight = "bold", color = "#333")
                ),
                mean_value = colDef(
                  name = paste(input$selectedVariable2),
                  format = colFormat(digits = 2),
                  style = function(value) {
                    color <- if (value >= mean(result$mean_value, na.rm = TRUE)) "#238b45" else "#d73027"
                    list(color = color, fontWeight = "bold")
                  },
                  align = "center"
                )
              ),
              bordered = TRUE,
              highlight = TRUE,
              striped = TRUE,
              style = list(fontFamily = "Arial, sans-serif", fontSize = "12px"),
              defaultPageSize = 6,
              theme = reactableTheme(
                headerStyle = list(
                  backgroundColor = "#f5f5f5",
                  fontWeight = "bold",
                  fontSize = "17px",
                  borderBottom = "2px solid #ddd"
                ),
                rowStyle = list(
                  fontSize = "15px"
                ),
                highlightColor = "#e8f4fa",
                borderColor = "#ddd"
              )
    )
  })
  


  
  
  ##############
  ### EXPLANATORY ###
  
  # Summary Statistics Output

  
  
  # DW-NOMINATE Plot
  observeEvent(input$runDWNom, {
    use_precomputed$dw_nom <- FALSE
    
    output$dwnomPlot <- renderPlot({
      # Step 1: Settings
      settings <- get_party_settings()
      party_colors <- settings$party_colors
      
      # Prepare data
      data <- data_react()

      dwnom_data <- data %>% filter(Attendance_Score > input$absenceThreshold * 0.01)
      
      # Assign and rename columns if needed
      dwnom_data$Name <- dwnom_data$FullName
      dwnom_data$Age <- dwnom_data$Age_At_Start
      dwnom_data$Experience <- dwnom_data$Experience_at_Start
      dwnom_data$Sex <- dwnom_data$Gender
      dwnom_data$Winning_Index <- dwnom_data$Winning_Score
      dwnom_data$Attendance_Index <- dwnom_data$Attendance_Score
      dwnom_data$Loyalty_Index <- dwnom_data$loyalty_score
      dwnom_data$Activity_Index <- dwnom_data$Activity_Index
      dwnom_data$Economy_Score <- dwnom_data$economic_votesScore
      dwnom_data$Social_Score <- dwnom_data$social_votesScore
      dwnom_data$Foreign_Policy_Score <- dwnom_data$foreign_policy_votesScore
      dwnom_data$Industry_Score <- dwnom_data$industry_votesScore
      dwnom_data$Education_Score <- dwnom_data$education_votesScore
      dwnom_data$Budget_Score <- dwnom_data$budget_votesScore
      
      datasets$dwnom_data <- dwnom_data %>%
        select(coord1D, coord2D, coord1D_red, coord2D_red, EPG, Name, Country, Party, Photo, Age, Experience, Sex, Winning_Index, Attendance_Index, Loyalty_Index, Activity_Index, Economy_Score, Social_Score, Foreign_Policy_Score, Industry_Score, Education_Score, Budget_Score)
      
      

      # Render DW-NOMINATE plot
      ggplot(dwnom_data, aes(x = coord1D, y = coord2D, color = EPG)) +
        geom_point() +
        labs(title = "DW-NOMINATE Plot", x = "DW1", y = "DW2") +
        scale_color_manual(values = party_colors) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 16, face = "bold"),
          legend.position = "none"
        )
    })
    
  })
  
  

  # Observe button to run MCA
  observeEvent(input$runMCA, {
    use_precomputed$mca <- FALSE
    
    output$mcaPlot <- renderPlot({
      withProgress(message = "Running MCA...", value = 0, {
      
      # Step 1: MCA Settings
      settings <- get_party_settings()
      party_colors <- settings$party_colors
      incProgress(0.1, detail = "Loading settings...")
      
      # Prepare data
      data <- data_react()
      legislature_map <- c("P6" = 6, "P7" = 7, "P8" = 8, "P9" = 9)
      
      # Filter data based on selected Parliament and relevant votes
      relevant_votes <- EP6_9_Voted %>%
        filter(Legislature == legislature_map[input$selectedP]) %>%
        {
          if (input$use_final_votes_only) {
            filter(., final_vote == 1)  # Filter for final votes if checkbox is selected
          } else {
            .
          }
        } %>%
        pull(Vote_ID)
      incProgress(0.2, detail = "Filtering data...")
      
      relevant_columns <- paste0("X", relevant_votes)
      mca_data <- data %>%
        select(all_of(relevant_columns), EPG, FullName, Country, Party, Photo, Age_At_Start, Experience_at_Start, Gender, Winning_Score, Attendance_Score, loyalty_score, Activity_Index, economic_votesScore, social_votesScore, foreign_policy_votesScore, industry_votesScore, education_votesScore, budget_votesScore, coord1D_red, coord2D_red) %>%
        filter(Attendance_Score > input$absenceThreshold * 0.01)
      
      # Convert all voting variables to factors
      mca_data <- mca_data %>%
        mutate(across(all_of(relevant_columns), as.factor))
      incProgress(0.4, detail = "Preparing MCA data...")
      
      # Dynamic MCA dimensions (cap at 10 for safety)
      n_components <- min(input$mca_ncp, 10)
      
      # Run MCA
      mca_results <- MCA(mca_data %>% select(all_of(relevant_columns)), ncp = n_components, graph = FALSE)
      
      
      
      # Variable contributions to dimensions
      contrib <- as.data.frame(mca_results$var$contrib[, 1:2])  # First two dimensions explicitly
      
      # Add variable names
      contrib$Variable <- rownames(contrib)
      rownames(contrib) <- NULL
      
      # Save contributions to datasets for export
      datasets$variable_contributions <- contrib
      

      
      incProgress(0.7, detail = "Running MCA analysis...")
      # Convert MCA output to data frame
      mca_coords <- as.data.frame(mca_results$ind$coord)
      
      # Dynamic column naming
      new_colnames <- character(n_components)
      for (i in 1:n_components) {
        if (input$use_final_votes_only) {
          new_colnames[i] <- paste0("MCA", i, "_red")
        } else {
          new_colnames[i] <- paste0("MCA", i)
        }
      }
      colnames(mca_coords) <- new_colnames
      
      # Add MCA results to mca_data dynamically
      for (i in 1:n_components) {
        column_name <- colnames(mca_coords)[i]
        mca_data[[column_name]] <- mca_coords[[i]]
      }
      
      # Add additional metadata for visualization
      mca_data <- mca_data %>%
        mutate(
          Name = FullName,
          Age = Age_At_Start,
          Experience = Experience_at_Start,
          Sex = Gender,
          Winning_Index = Winning_Score,
          Attendance_Index = Attendance_Score,
          Loyalty_Index = loyalty_score,
          Activity_Index = Activity_Index,
          Economy_Score = economic_votesScore,
          Social_Score = social_votesScore,
          Foreign_Policy_Score = foreign_policy_votesScore,
          Industry_Score = industry_votesScore,
          Education_Score = education_votesScore,
          Budget_Score = budget_votesScore
        )
      
      # Store MCA results for clustering
      datasets$mca_data <- mca_data
      
      # Determine which dimensions to plot
      MCA_x <- if (input$use_final_votes_only) "MCA1_red" else "MCA1"
      MCA_y <- if (input$use_final_votes_only) "MCA2_red" else "MCA2"
      
      # Render MCA plot
      p <- ggplot(mca_data, aes_string(x = MCA_x, y = MCA_y, color = "EPG")) +
        geom_point() +
        labs(title = "Multiple Correspondence Analysis (MCA) Plot", x = MCA_x, y = MCA_y) +
        scale_color_manual(values = party_colors) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 16, face = "bold"),
          legend.position = "none"  # Remove legend
        )

      p <- p + coord_cartesian(ylim = c(NA, 1.2))  # Upper bound set, lower bound unrestricted

      
      p
      })
    })
  })
    
  
  # Download Handler for MCA Contributions
  output$downloadContrib <- downloadHandler(
    filename = function() {
      paste("variable_contributions_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      # Write the MCA contributions (from datasets) to a CSV file
      write.csv(datasets$variable_contributions, file, row.names = FALSE)
    }
  )
  
  output$topicContributions <- renderTable({
    if (is.null(datasets$variable_contributions)) {
      return(data.frame(Message = "Variable contributions are not available yet."))
    }
    
    # Schritt 1: Extract Vote_ID from variable_contributions$Variable
    variable_contributions <- datasets$variable_contributions
    
    legislature_map <- c("P6" = 6, "P7" = 7, "P8" = 8, "P9" = 9)
    
    # Filter data based on selected Parliament and relevant votes
    EP6_9 <- EP6_9_Voted %>%
      filter(Legislature == legislature_map[input$selectedP])
    
    variable_contributions_processed <- variable_contributions %>%
      mutate(Vote_ID = as.integer(sub("X(\\d+)_.*", "\\1", Variable)))
    
    # Schritt 2: Sicherstellen, dass Vote_ID numerisch ist
    variable_contributions_processed <- variable_contributions_processed %>%
      mutate(Vote_ID = as.integer(Vote_ID))
    
    EP6_9_Voted_processed <- EP6_9 %>%
      mutate(Vote_ID = as.integer(Vote_ID))
    
    # Schritt 3: Dynamische Identifikation der Dimensionen (Dim1, Dim2, ...)
    dim_cols_pattern <- "^Dim\\s*\\d+$"
    dim_cols <- names(variable_contributions_processed)[grepl(dim_cols_pattern, names(variable_contributions_processed), ignore.case = TRUE)]
    
    if (length(dim_cols) == 0) {
      stop("Keine Dimensionsspalten in variable_contributions gefunden.")
    }
    
    # Schritt 4: Left Join der Datensätze und Auffüllen fehlender Beiträge mit 0
    merged_data <- EP6_9_Voted_processed %>%
      select(Vote_ID, main_policy_name) %>%
      left_join(variable_contributions_processed %>% select(Vote_ID, all_of(dim_cols)), by = "Vote_ID") %>%
      mutate(across(all_of(dim_cols), ~replace_na(.x, 0)))
    
    # Schritt 5: Aggregation der Beiträge pro Thema
    topic_contributions <- merged_data %>%
      group_by(main_policy_name) %>%
      summarise(across(all_of(dim_cols), sum, na.rm = TRUE)) %>%
      mutate(Total_Contribution = rowSums(across(all_of(dim_cols)))) %>%
      rename(`Vote Topic` = main_policy_name) # Rename column
    
    # Schritt 6: Add percentages and sort by Total_Contribution
    total_sum <- sum(topic_contributions$Total_Contribution)
    topic_contributions <- topic_contributions %>%
      mutate(Percentage = (Total_Contribution / total_sum) * 100) %>%
      arrange(desc(Total_Contribution)) %>%
      slice_head(n = 5) # Select top 5 rows
    
    # Select only the relevant columns for display
    topic_contributions <- topic_contributions %>%
      select(`Vote Topic`, `Dim 1`, `Dim 2`, Percentage) %>%
      mutate(Percentage = sprintf("%.2f%%", Percentage)) # Format percentages with 2 decimal places
    
    return(topic_contributions)
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  
  
  
  
  # Observe button to run UMAP
  observeEvent(input$runUMAP, {
    use_precomputed$umap <- FALSE
    
    output$umapPlot <- renderPlot({
      
      withProgress(message = "Running UMAP...", value = 0, {
      # Step 1: UMAP Settings
      settings <- get_party_settings()
      party_colors <- settings$party_colors
      incProgress(0.1, detail = "Loading settings...")
      
      # Prepare data
      data <- data_react()
      legislature_map <- c("P6" = 6, "P7" = 7, "P8" = 8, "P9" = 9)
      
      # Filter data based on selected Parliament and relevant votes
      relevant_votes <- EP6_9_Voted %>%
        filter(Legislature == legislature_map[input$selectedP]) %>%
        {
          if (input$use_final_votes_only) {
            filter(., final_vote == 1)  # Nur finale Votes, wenn Checkbox aktiviert
          } else {
            .
          }
        } %>%
        pull(Vote_ID)
      incProgress(0.2, detail = "Filtering data...")
      
      
      relevant_columns <- paste0("X", relevant_votes)
      umap_data <- data %>% select(all_of(relevant_columns), EPG, FullName, Country, Party, Photo, Age_At_Start, Experience_at_Start, Gender, Winning_Score, Attendance_Score, loyalty_score, Activity_Index, economic_votesScore, social_votesScore, foreign_policy_votesScore, industry_votesScore, education_votesScore, budget_votesScore, coord1D_red, coord2D_red)
      umap_data <- umap_data %>% filter(Attendance_Score > input$absenceThreshold*0.01)
      
      
      umap_data <- umap_data %>%
        mutate(across(all_of(relevant_columns), as.factor))
      incProgress(0.4, detail = "Preparing UMAP data...")
      

      # Berechnen der Gower-Distanzmatrix
      gower_dist <- daisy(umap_data %>% select(all_of(relevant_columns)), metric = "gower")
      incProgress(0.6, detail = "Calculating distance matrix...")
      
      # UMAP-Konfiguration
      # UMAP Configuration
      umap_config <- umap.defaults
      umap_config$input <- "dist"
      umap_config$n_neighbors <- input$umap_n_neighbors
      umap_config$min_dist <- input$umap_min_dist
      umap_config$spread <- input$umap_spread
      umap_config$n_components <- input$umap_n_components
      
      # Anwendung von UMAP auf die Distanzmatrix
      umap_results <- umap(as.matrix(gower_dist), config = umap_config)
      incProgress(0.8, detail = "Running UMAP analysis...")
      
      # Vorbereitung der Daten für die Visualisierung
      umap_results <- as.data.frame(umap_results$layout)
      

      # Cap n_components at 10
      n_components <- min(input$umap_n_components, 10)
      
      # Initialize column names
      new_colnames <- character(n_components)
      
      # Iterate through components and dynamically name the columns
      for (i in 1:n_components) {
        if (input$use_final_votes_only) {
          new_colnames[i] <- paste0("UMAP", i, "_red")
        } else {
          new_colnames[i] <- paste0("UMAP", i)
        }
      }
      
      # Assign the dynamically generated column names to umap_results
      colnames(umap_results) <- new_colnames
      
      # Add UMAP results to umap_data dynamically
      for (i in 1:n_components) {
        column_name <- colnames(umap_results)[i]
        umap_data[[column_name]] <- umap_results[[i]]
      }
      
      umap_data$EPG <- umap_data$EPG
      umap_data$Name <- umap_data$FullName
      umap_data$Age <- umap_data$Age_At_Start
      umap_data$Experience <- umap_data$Experience_at_Start
      umap_data$Sex <- umap_data$Gender
      umap_data$Winning_Index <- umap_data$Winning_Score
      umap_data$Attendance_Index <- umap_data$Attendance_Score
      umap_data$Loyalty_Index <- umap_data$loyalty_score
      umap_data$Activity_Index <- umap_data$Activity_Index
      umap_data$Economy_Score <- umap_data$economic_votesScore
      umap_data$Social_Score <- umap_data$social_votesScore
      umap_data$Foreign_Policy_Score <- umap_data$foreign_policy_votesScore
      umap_data$Industry_Score <- umap_data$industry_votesScore
      umap_data$Education_Score <- umap_data$education_votesScore
      umap_data$Budget_Score <- umap_data$budget_votesScore
      
      
      
      
      # Store UMAP results to be used in clustering
      datasets$umap_data <- umap_data
      
      # Use final vote coordinates if checkbox is selected
      UMAP_x <- if (input$use_final_votes_only) "UMAP1_red" else "UMAP1"
      UMAP_y <- if (input$use_final_votes_only) "UMAP2_red" else "UMAP2"
      
      
      # Visualization
      ggplot(umap_data, aes_string(x = UMAP_x, y = UMAP_y, color = "EPG")) +
        geom_point() +
        labs(title = "UMAP mit Gower-Distanz", x = UMAP_x, y = UMAP_y) +
        theme_minimal() +
        scale_color_manual(values = party_colors) +
        theme(
          plot.title = element_text(size = 16, face = "bold"),
          legend.position = "none"  # Remove legend
        )
      
      })
      
    })
    
    
  })
  
  
  # Initial flag for precomputed image mode
  use_precomputed <- reactiveValues(dw_nom = TRUE, mca = TRUE, umap = TRUE)
  
  # Display precomputed DW-NOMINATE plot or generate new one on button click
  output$dwNomPlotUI <- renderUI({
    if (use_precomputed$dw_nom) {
      output$dwnomPlot_pre <- renderPlot({
        settings <- get_party_settings()
        party_colors <- settings$party_colors
        
        # Prepare data
        data <- data_react()
        
        # Use final vote coordinates if checkbox is selected
        coord1 <- if (input$use_final_votes_only) "coord2D_red" else "coord1D"
        coord2 <- if (input$use_final_votes_only) "coord1D_red" else "coord2D"
        
        dwnom_data <- data %>% select(EPG, FullName, Country, Party, Photo, coord1D, coord2D, coord1D_red, coord2D_red, Age_At_Start, Experience_at_Start, Gender, Winning_Score, Attendance_Score, loyalty_score, Activity_Index, economic_votesScore, social_votesScore, foreign_policy_votesScore, industry_votesScore, education_votesScore, budget_votesScore)
        
        
        
        # Assign and rename columns if needed
        dwnom_data$Name <- dwnom_data$FullName
        dwnom_data$Age <- dwnom_data$Age_At_Start
        dwnom_data$Experience <- dwnom_data$Experience_at_Start
        dwnom_data$Sex <- dwnom_data$Gender
        dwnom_data$Winning_Index <- dwnom_data$Winning_Score
        dwnom_data$Attendance_Index <- dwnom_data$Attendance_Score
        dwnom_data$Loyalty_Index <- dwnom_data$loyalty_score
        dwnom_data$Activity_Index <- dwnom_data$Activity_Index
        dwnom_data$Economy_Score <- dwnom_data$economic_votesScore
        dwnom_data$Social_Score <- dwnom_data$social_votesScore
        dwnom_data$Foreign_Policy_Score <- dwnom_data$foreign_policy_votesScore
        dwnom_data$Industry_Score <- dwnom_data$industry_votesScore
        dwnom_data$Education_Score <- dwnom_data$education_votesScore
        dwnom_data$Budget_Score <- dwnom_data$budget_votesScore
        
        datasets$dwnom_data <- dwnom_data %>%
          select(coord1D, coord2D, coord1D_red, coord2D_red, EPG, Name, Country, Party, Photo, Age, Experience, Sex, Winning_Index, Attendance_Index, Loyalty_Index, Activity_Index, Economy_Score, Social_Score, Foreign_Policy_Score, Industry_Score, Education_Score, Budget_Score)
        
        
        # Rename columns if needed for consistency
        
        # Rename columns if needed for consistency
        dwnom_data <- dwnom_data %>%
          rename(DW1 = all_of(coord1), DW2 = all_of(coord2))
        

        # Render DW-NOMINATE plot
        ggplot(dwnom_data, aes(x = DW1, y = DW2, color = if (input$color_switch) "black" else EPG)) +
          geom_point() +
          labs(title = "DW-NOMINATE Plot", x = "DW1", y = "DW2") +
          scale_color_manual(values = if (input$color_switch) c("black" = "black") else party_colors) +
          theme_minimal() +
          theme(
            plot.title = element_text(size = 16, face = "bold"),
            legend.position = "none"
          )
      })
    } else {
      plotOutput("dwnomPlot")
    }
  })
  
  # Display precomputed MCA plot or generate new one on button click
  output$mcaPlotUI <- renderUI({
    if (use_precomputed$mca) {
      output$mcaPlot_pre <- renderPlot({
        settings <- get_party_settings()
        party_colors <- settings$party_colors
        
        # Prepare data
        data <- data_react()
        
        # Use final vote coordinates if checkbox is selected
        coord1 <- if (input$use_final_votes_only) "MCA1_red" else "MCA1"
        coord2 <- if (input$use_final_votes_only) "MCA2_red" else "MCA2"
        
        
        mca_data <- data %>% select(EPG, FullName, Country, Party, Photo, MCA1, MCA2, MCA1_red, MCA2_red , Age_At_Start, Experience_at_Start, Gender, Winning_Score, Attendance_Score, loyalty_score, Activity_Index, economic_votesScore, social_votesScore, foreign_policy_votesScore, industry_votesScore, education_votesScore, budget_votesScore, coord2D_red)
        
        
        # Assign and rename columns if needed
        mca_data$Name <- mca_data$FullName
        mca_data$Age <- mca_data$Age_At_Start
        mca_data$Experience <- mca_data$Experience_at_Start
        mca_data$Sex <- mca_data$Gender
        mca_data$Winning_Index <- mca_data$Winning_Score
        mca_data$Attendance_Index <- mca_data$Attendance_Score
        mca_data$Loyalty_Index <- mca_data$loyalty_score
        mca_data$Activity_Index <- mca_data$Activity_Index
        mca_data$Economy_Score <- mca_data$economic_votesScore
        mca_data$Social_Score <- mca_data$social_votesScore
        mca_data$Foreign_Policy_Score <- mca_data$foreign_policy_votesScore
        mca_data$Industry_Score <- mca_data$industry_votesScore
        mca_data$Education_Score <- mca_data$education_votesScore
        mca_data$Budget_Score <- mca_data$budget_votesScore
        
        datasets$mca_data <- mca_data %>%
          select(MCA1, MCA2, MCA1_red, MCA2_red, EPG, Name, Country, Party, Photo, Age, Experience, Sex, Winning_Index, Attendance_Index, Loyalty_Index, Activity_Index, Economy_Score, Social_Score, Foreign_Policy_Score, Industry_Score, Education_Score, Budget_Score, coord2D_red)
        
        mca_data <- mca_data %>%
          rename(MC1 = all_of(coord1), MC2 = all_of(coord2))
        
        # Filter out observations where MC1 or MC2 exceeds 1.5
        mca_data <- mca_data %>% filter(abs(MC1) <= 1.2, abs(MC2) <= 1.5)
        
        
        # Render MCA plot
        ggplot(mca_data, aes(x = MC1, y = MC2, color = if (input$color_switch) "black" else EPG)) +
          geom_point() +
          labs(title = "Multiple Correspondence Analysis (MCA) Plot", x = "MCA1", y = "MCA2") +
          scale_color_manual(values = if (input$color_switch) c("black" = "black") else party_colors) +
          theme_minimal() +
          theme(
            plot.title = element_text(size = 16, face = "bold"),
            legend.position = "none"
          )
      })
    } else {
      plotOutput("mcaPlot")
    }
  })
  
  # Display precomputed UMAP plot or generate new one on button click
  output$umapPlotUI <- renderUI({
    if (use_precomputed$umap) {
      output$umapPlot_pre <- renderPlot({
        settings <- get_party_settings()
        party_colors <- settings$party_colors
        
        # Prepare data
        data <- data_react()
        
        # Use final vote coordinates if checkbox is selected
        UMAP_x <- if (input$use_final_votes_only) "UMAP1_red" else "UMAP1"
        UMAP_y <- if (input$use_final_votes_only) "UMAP2_red" else "UMAP2"
        
        umap_data <- data %>% select(EPG, FullName, Country, Party, Photo, UMAP1, UMAP2, UMAP1_red, UMAP2_red, Age_At_Start, Experience_at_Start, Gender, Winning_Score, Attendance_Score, loyalty_score, Activity_Index, economic_votesScore, social_votesScore, foreign_policy_votesScore, industry_votesScore, education_votesScore, budget_votesScore, coord2D_red)
        
        # Assign and rename columns if needed
        umap_data$Name <- umap_data$FullName
        umap_data$Age <- umap_data$Age_At_Start
        umap_data$Experience <- umap_data$Experience_at_Start
        umap_data$Sex <- umap_data$Gender
        umap_data$Winning_Index <- umap_data$Winning_Score
        umap_data$Attendance_Index <- umap_data$Attendance_Score
        umap_data$Loyalty_Index <- umap_data$loyalty_score
        umap_data$Activity_Index <- umap_data$Activity_Index
        umap_data$Economy_Score <- umap_data$economic_votesScore
        umap_data$Social_Score <- umap_data$social_votesScore
        umap_data$Foreign_Policy_Score <- umap_data$foreign_policy_votesScore
        umap_data$Industry_Score <- umap_data$industry_votesScore
        umap_data$Education_Score <- umap_data$education_votesScore
        umap_data$Budget_Score <- umap_data$budget_votesScore
        
        datasets$umap_data <- umap_data %>%
          select(UMAP1, UMAP2, UMAP1_red, UMAP2_red, EPG, Name, Country, Party, Photo, Age, Experience, Sex, Winning_Index, Attendance_Index, Loyalty_Index, Activity_Index, Economy_Score, Social_Score, Foreign_Policy_Score, Industry_Score, Education_Score, Budget_Score, coord2D_red)
        
        
        
        
        
        # Rename columns if needed for consistency
        umap_data <- umap_data %>%
          rename(UMAP_x = all_of(UMAP_x), UMAP_y = all_of(UMAP_y))

        
        
        # Assuming input$color_switch is the toggle input
        ggplot(umap_data, aes(x = UMAP_x, y = UMAP_y, color = if (input$color_switch) "black" else EPG)) +
          geom_point() +
          labs(title = "UMAP Plot with Gower Distance", x = "UMAP1", y = "UMAP2") +
          scale_color_manual(values = if (input$color_switch) c("black" = "black") else party_colors) +
          theme_minimal() +
          theme(
            plot.title = element_text(size = 16, face = "bold"),
            legend.position = "none"
          )
        
      })
    } else {
      plotOutput("umapPlot")
    }
  })
  
  

  
  
  
  
  
  

  
  ###########################
  
  selected_mapping <- reactiveVal(NULL)
  
  # Define color and icon output based on checkbox selection
  observeEvent(input$select_dwnom, {
    if (input$select_dwnom) {
      output$icon_dwnom <- renderUI({ icon("crown", style = "color: gold; font-size: 3em;") })
      shinyjs::addClass(selector = ".dwnom_plot_row", class = "highlight")
      selected_mapping("DW-NOMINATE")
      
      output$finalClusterPlot <- renderPlot({
        settings <- get_party_settings()
        party_colors <- settings$party_colors
        data <- data_react()
        
        # Use reduced columns if the option is enabled
        col_x <- if (input$use_final_votes_only) "coord2D_red" else "coord1D"
        col_y <- if (input$use_final_votes_only) "coord1D_red" else "coord2D"
        
        # Render DW-NOMINATE plot
        p <- ggplot(data, aes_string(x = col_x, y = col_y, color = "EPG")) +
          geom_point() +
          labs(title = "DW-NOMINATE Plot", x = "DW1", y = "DW2") +
          scale_color_manual(values = party_colors) +
          theme_minimal() +
          theme(
            plot.title = element_text(size = 16, face = "bold"),
            legend.position = "none"  # Remove legend
          )
        
        finalClusterPlotObject(p)
        p
      })
      
    } else {
      output$icon_dwnom <- renderUI({ NULL })
      shinyjs::removeClass(selector = ".dwnom_plot_row", class = "highlight")
    }
  })
  
  observeEvent(input$select_umap, {
    if (input$select_umap) {
      output$icon_umap <- renderUI({ icon("crown", style = "color: gold; font-size: 3em;") })
      shinyjs::addClass(selector = ".umap_plot_row", class = "highlight")
      selected_mapping("UMAP")
      
      output$finalClusterPlot <- renderPlot({
        settings <- get_party_settings()
        party_colors <- settings$party_colors
        data <- get_selected_data()
        
        # Use reduced columns if the option is enabled
        col_x <- if (input$use_final_votes_only) "UMAP1_red" else "UMAP1"
        col_y <- if (input$use_final_votes_only) "UMAP2_red" else "UMAP2"
        
        # Render UMAP plot
        p <- ggplot(data, aes_string(x = col_x, y = col_y, color = "EPG")) +
          geom_point() +
          labs(title = "UMAP with Gower Distance", x = "UMAP1", y = "UMAP2") +
          scale_color_manual(values = party_colors) +
          theme_minimal() +
          theme(
            plot.title = element_text(size = 16, face = "bold"),
            legend.position = "none"  # Remove legend
          )
        
        finalClusterPlotObject(p)
        p
      })
      
    } else {
      output$icon_umap <- renderUI({ NULL })
      shinyjs::removeClass(selector = ".umap_plot_row", class = "highlight")
    }
  })
  
  observeEvent(input$select_mca, {
    if (input$select_mca) {
      output$icon_mca <- renderUI({ icon("crown", style = "color: gold; font-size: 3em;") })
      shinyjs::addClass(selector = ".mca_plot_row", class = "highlight")
      selected_mapping("MCA")
      
      output$finalClusterPlot <- renderPlot({
        settings <- get_party_settings()
        party_colors <- settings$party_colors
        data <- get_selected_data()
        
        # Use reduced columns if the option is enabled
        col_x <- if (input$use_final_votes_only) "MCA1_red" else "MCA1"
        col_y <- if (input$use_final_votes_only) "MCA2_red" else "MCA2"
        
        # Render MCA plot
        p <- ggplot(data, aes_string(x = col_x, y = col_y, color = "EPG")) +
          geom_point() +
          labs(title = "Multiple Correspondence Analysis (MCA) Plot", x = "MCA1", y = "MCA2") +
          scale_color_manual(values = party_colors) +
          theme_minimal() +
          theme(
            plot.title = element_text(size = 16, face = "bold"),
            legend.position = "none"  # Remove legend
          )
        
          p <- p + coord_cartesian(ylim = c(NA, 1.2))  # Upper bound set, lower bound unrestricted
        
          finalClusterPlotObject(p)
          p
      })
    } else {
      output$icon_mca <- renderUI({ NULL })
      shinyjs::removeClass(selector = ".mca_plot_row", class = "highlight")
    }
  })
  
  
  
  ############################
  
  
  # Filter data by absence threshold
  filter_mep_by_absence <- function(data, threshold) {
    data %>%
      rowwise() %>%
      mutate(absence_percentage = sum(c_across(starts_with("X")) == 0) / length(c_across(starts_with("X"))) * 100) %>%
      ungroup() %>%
      filter(absence_percentage <= threshold) %>%
      select(-absence_percentage)  # Remove the helper column
  }

  
  get_selected_data <- function() {
    mapping <- selected_mapping()
    req(mapping)  # Make sure mapping is not NULL
    
    if (mapping == "UMAP") {
      return(datasets$umap_data)
    } else if (mapping == "MCA") {
      return(datasets$mca_data)
    } else if (mapping == "DW-NOMINATE") {
      return(datasets$dwnom_data)
    } else {
      return(NULL)
    }
  }
  
  
  
  
  
  
  
  # Observe K-Means button and run clustering

  # Observe K-Means button and run clustering
  observeEvent(input$runKMeans, {
    
    clusteringCompleted(TRUE)
    
    set.seed(123)
    data <- get_selected_data()
    req(data)
    
    # Dynamically determine column prefix and suffix based on selected mapping
    if (selected_mapping() == "UMAP") {
      col_prefix <- "UMAP"
      col_suffix <- if (input$use_final_votes_only) "_red" else ""
      num_dimensions <- min(input$umap_n_components, 10)
      clustering_columns <- paste0(col_prefix, 1:num_dimensions, col_suffix)
    } else if (selected_mapping() == "MCA") {
      col_prefix <- "MCA"
      col_suffix <- if (input$use_final_votes_only) "_red" else ""
      num_dimensions <- min(input$mca_ncp, 10)
      clustering_columns <- paste0(col_prefix, 1:num_dimensions, col_suffix)
    } else if (selected_mapping() == "DW-NOMINATE") {
      # DW-NOMINATE is always 2D
      clustering_columns <- if (input$use_final_votes_only) {
        c("coord2D_red", "coord1D_red")
      } else {
        c("coord1D", "coord2D")
      }
      num_dimensions <- 2  # Always 2D for DW-NOMINATE
    }
    
    # Remove rows with NAs in clustering columns
    data <- data[complete.cases(data[, clustering_columns]), ]
    
    # Perform K-Means clustering
    kmeans_result <- kmeans(data[, clustering_columns], centers = input$kmeans_k)
    data$Cluster <- as.factor(kmeans_result$cluster)
    
    # Save the updated data back to datasets
    if (selected_mapping() == "UMAP") {
      datasets$umap_data <- data
    } else if (selected_mapping() == "MCA") {
      datasets$mca_data <- data
    } else if (selected_mapping() == "DW-NOMINATE") {
      datasets$dwnom_data <- data
    }
    
    # Generate the Elbow Plot after K-Means (only for 2D clustering)
    if (num_dimensions == 2) {
      output$elbowPlotK <- generate_elbow_plot(data, clustering_columns[1], clustering_columns[2], 10, input$kmeans_k)
    } else {
      output$elbowPlotK <- NULL  # Elbow plot not applicable for higher dimensions
    }
    
    # Cluster-Plot generieren und speichern
    cluster_plot <- generateClusterPlot(data, clustering_columns, "K-Means", input$kmeans_k)
    clusterPlotObject(cluster_plot)
    
    # Cluster-Plot rendern
    output$clusterPlot <- renderPlot({
      clusterPlotObject()
    })
    
    
  })
  
  

  
  
  # Helper function to generate Elbow Plot
  generate_elbow_plot <- function(data, col_x, col_y, max_clusters, current_k) {
    # Calculate the SSE for different cluster numbers
    sse <- sapply(1:max_clusters, function(k) {
      kmeans(data[, c(col_x, col_y)], centers = k, nstart = 10)$tot.withinss
    })
    
    # Render the elbow plot
    output$elbowPlot <- renderPlot({
      plot(1:max_clusters, sse, type = "b", pch = 19, col = "blue",
           xlab = "Number of Clusters", ylab = "Total Within Sum of Squares (SSE)",
           main = "Elbow Method for Optimal Clustering")
      abline(v = current_k, col = "red", lty = 2)
    })
  }
  
  
  # Observe PAM button and run clustering

  # Observe PAM button and run clustering
  observeEvent(input$runPAM, {
    
    clusteringCompleted(TRUE)
    
    set.seed(123)
    data <- get_selected_data()
    req(data)
    
    # Dynamically determine column prefix and suffix based on selected mapping
    if (selected_mapping() == "UMAP") {
      col_prefix <- "UMAP"
      col_suffix <- if (input$use_final_votes_only) "_red" else ""
      num_dimensions <- min(input$umap_n_components, 10)
      clustering_columns <- paste0(col_prefix, 1:num_dimensions, col_suffix)
    } else if (selected_mapping() == "MCA") {
      col_prefix <- "MCA"
      col_suffix <- if (input$use_final_votes_only) "_red" else ""
      num_dimensions <- min(input$mca_ncp, 10)
      clustering_columns <- paste0(col_prefix, 1:num_dimensions, col_suffix)
    } else if (selected_mapping() == "DW-NOMINATE") {
      # DW-NOMINATE is always 2D
      clustering_columns <- if (input$use_final_votes_only) {
        c("coord2D_red", "coord1D_red")
      } else {
        c("coord1D", "coord2D")
      }
      num_dimensions <- 2  # Always 2D for DW-NOMINATE
    }
    
    # Remove rows with NAs in clustering columns
    data <- data[complete.cases(data[, clustering_columns]), ]
    
    # Perform PAM clustering
    pam_result <- cluster::pam(data[, clustering_columns], k = input$pam_k)
    data$Cluster <- as.factor(pam_result$clustering)
    
    # Save the updated data back to datasets
    if (selected_mapping() == "UMAP") {
      datasets$umap_data <- data
    } else if (selected_mapping() == "MCA") {
      datasets$mca_data <- data
    } else if (selected_mapping() == "DW-NOMINATE") {
      datasets$dwnom_data <- data
    }
    
    # Generate the Elbow Plot after PAM (only for 2D clustering)
    if (num_dimensions == 2) {
      output$elbowPlotPAM <- generate_elbow_plot(data, clustering_columns[1], clustering_columns[2], 10, input$pam_k)
    } else {
      output$elbowPlotPAM <- NULL  # Elbow plot not applicable for higher dimensions
    }
    
    # Cluster-Plot generieren und speichern
    cluster_plot <- generateClusterPlot(data, clustering_columns, "PAM", input$pam_k)
    clusterPlotObject(cluster_plot)
    
    # Cluster-Plot rendern
    output$clusterPlot <- renderPlot({
      clusterPlotObject()
    })


  })
  
  
  
  
  # Observe HDBSCAN button and run clustering
  observeEvent(input$runHDBSCAN, {
    
    clusteringCompleted(TRUE)
    
    set.seed(123)
    data <- get_selected_data()
    req(data)
    
    # Dynamically determine column prefix and suffix based on selected mapping
    if (selected_mapping() == "UMAP") {
      col_prefix <- "UMAP"
      col_suffix <- if (input$use_final_votes_only) "_red" else ""
      num_dimensions <- min(input$umap_n_components, 10)
      clustering_columns <- paste0(col_prefix, 1:num_dimensions, col_suffix)
    } else if (selected_mapping() == "MCA") {
      col_prefix <- "MCA"
      col_suffix <- if (input$use_final_votes_only) "_red" else ""
      num_dimensions <- min(input$mca_ncp, 10)
      clustering_columns <- paste0(col_prefix, 1:num_dimensions, col_suffix)
    } else if (selected_mapping() == "DW-NOMINATE") {
      # DW-NOMINATE is always 2D
      clustering_columns <- if (input$use_final_votes_only) {
        c("coord2D_red", "coord1D_red")
      } else {
        c("coord1D", "coord2D")
      }
      num_dimensions <- 2  # Always 2D for DW-NOMINATE
    }
    
    # Remove rows with NAs in clustering columns
    data <- data[complete.cases(data[, clustering_columns]), ]
    
    # Perform HDBSCAN clustering
    hdbscan_result <- dbscan::hdbscan(data[, clustering_columns], minPts = input$hdbscan_minPts)
    data$Cluster <- as.factor(hdbscan_result$cluster)
    
    # Save updated data back to datasets
    if (selected_mapping() == "UMAP") {
      datasets$umap_data <- data
    } else if (selected_mapping() == "MCA") {
      datasets$mca_data <- data
    } else if (selected_mapping() == "DW-NOMINATE") {
      datasets$dwnom_data <- data
    }
    
    # Render cluster plot

    cluster_plot <- generateClusterPlot(data, clustering_columns, "HDBSCAN", input$hdbscan_minPts)
    clusterPlotObject(cluster_plot)
    
    # Cluster-Plot rendern
    output$clusterPlot <- renderPlot({
      clusterPlotObject()
    })
    
  })
  
  

  
  
  
  
  # Download Handler for exporting clustered data
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("clustered_data_", selected_mapping(), ".csv")
    },
    content = function(file) {
      data <- get_selected_data()  # Get the data based on the selected mapping
      req(data)  # Ensure data exists
      
      # Save the selected dataset to CSV
      write.csv(data, file, row.names = FALSE)
    }
  )
  

  
  # Server code for Cluster Metrics and Stability
    observeEvent(input$runMetrics, {
      set.seed(123)
      data <- get_selected_data()
      req(data)
      
      # Determine the correct column names based on selected mapping and final votes setting
      col_x <- if (selected_mapping() == "UMAP") {
        if (input$use_final_votes_only) "UMAP1_red" else "UMAP1"
      } else if (selected_mapping() == "MCA") {
        if (input$use_final_votes_only) "MCA1_red" else "MCA1"
      } else {
        if (input$use_final_votes_only) "coord1D_red" else "coord1D"
      }
      
      col_y <- if (selected_mapping() == "UMAP") {
        if (input$use_final_votes_only) "UMAP2_red" else "UMAP2"
      } else if (selected_mapping() == "MCA") {
        if (input$use_final_votes_only) "MCA2_red" else "MCA2"
      } else {
        if (input$use_final_votes_only) "coord2D_red" else "coord2D"
      }
      
      # Conditionally exclude the first cluster (assumed to be outliers)
      if (input$ignore_first_cluster) {
        data <- data[data$Cluster != 0, ]
      }
      
      # Prepare data for clustering metrics
      clustering_data <- data[, c(col_x, col_y)]
      clusters <- as.integer(data$Cluster)
      
      # Reactive expressions for metric calculations
      silhouette_score <- reactive({
        silhouette(clusters, dist(clustering_data))
      })
      
      db_index <- reactive({
        intCriteria(as.matrix(clustering_data), clusters, crit = "Davies_Bouldin")$davies_bouldin
      })
      
      ch_index <- reactive({
        intCriteria(as.matrix(clustering_data), clusters, crit = "Calinski_Harabasz")$calinski_harabasz
      })
      
      
      # Silhouette Plot
      if (input$enable_silhouette) {
        output$silhouettePlot <- renderPlot({
          avg_silhouette_score <- mean(silhouette_score()[, 'sil_width'])
          fviz_silhouette(silhouette_score()) +
            ggtitle("Silhouette Plot") +
            geom_hline(yintercept = 0.5, linetype = "dashed", color = "darkblue", size = 0.75) +
            annotate("text", x = 1, y = 0.55, label = "Good Separation Threshold", 
                     color = "darkblue", size = 4, hjust = 0, vjust = -0.5) +
            theme_minimal() +
            theme(plot.title = element_text(size = 16, face = "bold"), 
                  plot.caption = element_text(color = "red", face = "bold", size = 12),
                  legend.position = "right") +
            labs(caption = paste("Average Silhouette Score:", round(avg_silhouette_score, 3)))
        })
      }
      
      # Davies-Bouldin Index Plot
      if (input$enable_dbindex) {
        output$dbIndexPlot <- renderPlot({
          db_index_value <- db_index()
          color <- ifelse(db_index_value < 0.5, "green",
                          ifelse(db_index_value < 1.5, "yellow", "red"))
          
          ggplot() +
            geom_col(aes(x = "DB Index", y = db_index_value), fill = color, width = 0.4) +
            geom_hline(yintercept = 0.5, linetype = "dashed", color = "green") +
            geom_hline(yintercept = 1.5, linetype = "dashed", color = "yellow") +
            annotate("text", x = 1, y = db_index_value + 0.1, label = round(db_index_value, 3), size = 5) +
            coord_cartesian(ylim = c(0, max(2, db_index_value + 0.5))) +
            ggtitle("Davies-Bouldin Index") +
            ylab("Index Value") +
            theme_minimal() +
            theme(plot.title = element_text(size = 16, face = "bold"),
                  axis.title.x = element_blank(),
                  axis.text.x = element_blank(),
                  axis.ticks.x = element_blank())
        })
      }
      
      
      # Calinski
      if (input$enable_chindex) {
        output$chIndexPlot <- renderPlot({
          ch_index <- intCriteria(as.matrix(data[, c(col_x, col_y)]), as.integer(data$Cluster), crit = "Calinski_Harabasz")

          
          ch_index_value <- ch_index$calinski_harabasz
          color <- ifelse(ch_index_value > 3000, "green",
                          ifelse(ch_index_value > 1500, "yellow", "red"))
          
          ggplot() +
            geom_col(aes(x = "DB Index", y = ch_index_value), fill = color, width = 0.4) +
            geom_hline(yintercept = 0.5, linetype = "dashed", color = "green") +
            geom_hline(yintercept = 1.5, linetype = "dashed", color = "yellow") +
            annotate("text", x = 1, y = ch_index_value + 0.1, label = round(ch_index_value, 3), size = 5) +
            coord_cartesian(ylim = c(0, max(2, ch_index_value + 0.5))) +
            ggtitle("Calinski-Harabasz Index") +
            ylab("Index Value") +
            theme_minimal() +
            theme(plot.title = element_text(size = 16, face = "bold"),
                  axis.title.x = element_blank(),
                  axis.text.x = element_blank(),
                  axis.ticks.x = element_blank())
        })
      }

      

      
      # Elbow Plot
      if (input$enable_elbow) {
        output$elbowPlot <- renderPlot({
          
          # Calculate SSE for each number of clusters
          sse <- sapply(1:10, function(k) {
            kmeans(clustering_data, centers = k, nstart = 10)$tot.withinss
          })
          
          # Data for plot
          plot_df <- data.frame(Clusters = 1:10, SSE = sse)
          
          # Render Elbow Plot
          ggplot(plot_df, aes(x = Clusters, y = SSE)) +
            geom_point(color = "blue") +
            geom_line(color = "blue") +
            geom_vline(xintercept = length(unique(clusters)), color = "red", linetype = "dashed") +
            annotate("text", x = length(unique(clusters)) + 0.5, y = max(sse), 
                     label = paste("Selected K =", length(unique(clusters))), color = "red", hjust = 0) +
            ggtitle("Elbow Method for Optimal Clustering") +
            xlab("Number of Clusters") +
            ylab("Total Within-Cluster Sum of Squares (SSE)") +
            theme_minimal() +
            theme(plot.title = element_text(size = 16, face = "bold"))
        })
      }
      
      
      # Cluster Stability (Bootstrapping)
      if (input$enable_bootstrap) {
        output$stabilityPlot <- renderPlot({
          progress <- Progress$new(session, min = 1, max = input$num_bootstrap)
          on.exit(progress$close())
          
          progress$set(message = "Calculating cluster stability...", value = 1)
          
          # Replicate bootstrapping
          stability_results <- replicate(input$num_bootstrap, {
            # Sample with replacement
            sample_indices <- sample(1:nrow(clustering_data), replace = TRUE)
            sample_data <- clustering_data[sample_indices, ]
            sample_clusters <- clusters[sample_indices]
            
            # Perform clustering on the sample
            km <- kmeans(sample_data, centers = length(unique(clusters)), nstart = 10)
            
            # Calculate ARI with original cluster labels
            mclust::adjustedRandIndex(sample_clusters, km$cluster)
          })
          
          progress$set(value = input$num_bootstrap)
          
          # Calculate mean stability score
          stability_mean <- mean(stability_results)
          
          # Plot Stability Histogram
          ggplot(data.frame(ARI = stability_results), aes(x = ARI)) +
            geom_histogram(binwidth = 0.05, fill = "lightblue", color = "black") +
            geom_vline(xintercept = stability_mean, color = "red", linetype = "dashed") +
            annotate("text", x = stability_mean + 0.05, y = max(table(cut(stability_results, breaks = seq(0, 1, 0.05)))), 
                     label = paste("Mean ARI =", round(stability_mean, 3)), color = "red", hjust = 0) +
            ggtitle("Cluster Stability (Adjusted Rand Index)") +
            xlab("Adjusted Rand Index") +
            ylab("Frequency") +
            theme_minimal() +
            theme(plot.title = element_text(size = 16, face = "bold"))
        })
      }
      
    })

  
  ########################
  

  
  
  output$clusterPlot_538 <- renderPlot({
    set.seed(123)  # Set the random seed for reproducibility
    
    # Perform k-means clustering based on user-selected number of clusters
    cluster_data <- five_thirty %>%
      select(dw_nominate_dim1, dw_nominate_dim2) %>%
      na.omit()
    
    #rename dw_nominate_dim1 and dw_nominate_dim2 to "nominate_dim1" and "nominate_dim2"
    colnames(cluster_data) <- c("nominate_dim1", "nominate_dim2")
    
    km <- kmeans(cluster_data[, c("nominate_dim1", "nominate_dim2")], centers = input$clusters, nstart = 25)

    # Add cluster assignments to the data
    cluster_data$cluster <- as.factor(km$cluster)
    
    # Post-hoc labeling for clusters based on the selected number of clusters
    cluster_labels <- switch(as.character(input$clusters),
                             "2" = c("Group 1", "Group 2"),
                             "3" = c("Group 1", "Group 2", "Group 3"),
                             "4" = c("Group 1", "Group 2", "Group 3", "Group 4"),
                             "5" = c("Moderate Republicans", "Compromise Conservatives", "Old Guard Republicans", "Progressive Democrats", "Core Democrats"),
                             "6" = c("Moderate Republicans", "Compromise Conservatives", "Old Guard Republicans", "Progressive Democrats", "Core Democrats", "Moderate Democrats"),
                             "7" = c("Moderate Republicans", "Compromise Conservatives", "Old Guard Republicans", "Progressive Democrats", "Core Democrats", "Moderate Democrats", "Far-Right Establishment"),
                             "8" = c("Moderate Republicans", "Compromise Conservatives", "Old Guard Republicans", "Far-Right Establishment", "Far-Right Obstructionists", "Progressive Democrats", "Core Democrats", "Moderate Democrats"),
                             "9" = c("Moderate Republicans", "Compromise Conservatives", "Old Guard Republicans", "Far-Right Establishment", "Far-Right Obstructionists", "Progressive Democrats", "Core Democrats", "Moderate Democrats", "Freedom Caucus"),
                             "10" = c("Group 1", "Group 2", "Group 3", "Group 4", "Group 5", "Group 6", "Group 7", "Group 8", "Group 9", "Group 10"))
    
    # Ensure the cluster labels are assigned correctly
    cluster_data$cluster_label <- factor(cluster_data$cluster, 
                                         levels = 1:input$clusters, 
                                         labels = cluster_labels[1:input$clusters])
    
    # Check if red/blue intensity mode is enabled
    if (input$use_party_colors) {
      # Color based on nominate_dim1 (extremity mapping to red/blue)
      ggplot(cluster_data, aes(x = nominate_dim1, y = nominate_dim2, color = nominate_dim1)) +
        geom_point(size = 4) +
        scale_color_gradientn(colors = c("blue", "white", "red")) +  # Red for high values, blue for low
        labs(title = paste("Clustering with", input$clusters, "Groups"),
             x = "Nominate Dimension 1 (Liberal vs. Conservative)", 
             y = "Nominate Dimension 2 (Establishment vs. Anti-Establishment)",
             color = "Extremity") +  # Add label to the legend
        theme_minimal() +
        theme(legend.position = "bottom", legend.title = element_text(size = 12), legend.text = element_text(size = 8))
      
    } else {
      # Use the predefined colors
      group_colors <- c("Moderate Republicans" = "#fc8d62", 
                        "Compromise Conservatives" = "#66c2a5",
                        "Old Guard Republicans" = "#8da0cb", 
                        "Far-Right Establishment" = "#e78ac3",
                        "Far-Right Obstructionists" = "#a6d854",
                        "Progressive Democrats" = "#ffd92f",
                        "Core Democrats" = "#e5c494",
                        "Moderate Democrats" = "#b3b3b3",
                        "Freedom Caucus" = "#d53e4f",
                        "Group 1" = "#1f77b4", "Group 2" = "#ff7f0e", "Group 3" = "#2ca02c",
                        "Group 4" = "#d62728", "Group 5" = "#9467bd", "Group 6" = "#8c564b", 
                        "Group 7" = "#e377c2", "Group 8" = "#7f7f7f", "Group 9" = "#bcbd22", 
                        "Group 10" = "#17becf")
      
      ggplot(cluster_data, aes(x = nominate_dim1, y = nominate_dim2, color = cluster_label)) +
        geom_point(size = 4) +
        scale_color_manual(values = group_colors) +  # Custom colors for groups
        labs(title = paste("Clustering with", input$clusters, "Groups"),
             x = "Nominate Dimension 1 (Liberal vs. Conservative)", 
             y = "Nominate Dimension 2 (Establishment vs. Anti-Establishment)", 
             color = "Group") +  # Legend will be based on the cluster labels
        theme_minimal() +
        theme(legend.position = "bottom", legend.title = element_text(size = 12), legend.text = element_text(size = 8))
    }
  })

  
  
  
  output$clusterPie <- renderPlot({
    set.seed(123)  # Set the random seed for reproducibility
    
    
    # Perform k-means clustering based on user-selected number of clusters
    cluster_data <- five_thirty %>%
      select(dw_nominate_dim1, dw_nominate_dim2, party) %>%
      na.omit()
    
    #rename dw_nominate_dim1 and dw_nominate_dim2 to "nominate_dim1" and "nominate_dim2"
    colnames(cluster_data) <- c("nominate_dim1", "nominate_dim2", "party")
    
    km <- kmeans(cluster_data[, c("nominate_dim1", "nominate_dim2")], centers = input$clusters, nstart = 25)
    
    # Add cluster assignments to the data
    cluster_data$cluster <- as.factor(km$cluster)
    
    # Post-hoc labeling for clusters based on the selected number of clusters
    if (input$clusters <= 10) {
      cluster_labels <- switch(as.character(input$clusters),
                               "2" = c("Group 1", "Group 2"),
                               "3" = c("Group 1", "Group 2", "Group 3"),
                               "4" = c("Group 1", "Group 2", "Group 3", "Group 4"),
                               "5" = c("Moderate Republicans", "Compromise Conservatives", "Old Guard Republicans", "Progressive Democrats", "Core Democrats"),
                               "6" = c("Moderate Republicans", "Compromise Conservatives", "Old Guard Republicans", "Progressive Democrats", "Core Democrats", "Moderate Democrats"),
                               "7" = c("Moderate Republicans", "Compromise Conservatives", "Old Guard Republicans", "Progressive Democrats", "Core Democrats", "Moderate Democrats", "Far-Right Establishment"),
                               "8" = c("Moderate Republicans", "Compromise Conservatives", "Old Guard Republicans", "Far-Right Establishment", "Far-Right Obstructionists", "Progressive Democrats", "Core Democrats", "Moderate Democrats"),
                               "9" = c("Moderate Republicans", "Compromise Conservatives", "Old Guard Republicans", "Far-Right Establishment", "Far-Right Obstructionists", "Progressive Democrats", "Core Democrats", "Moderate Democrats", "Freedom Caucus"),
                               "10" = c("Group 1", "Group 2", "Group 3", "Group 4", "Group 5", "Group 6", "Group 7", "Group 8", "Group 9", "Group 10"))
    } else {
      # Generate generic labels if input$clusters > 10
      cluster_labels <- paste("Group", 1:input$clusters)
    }
    
    # Ensure the cluster labels are assigned correctly
    cluster_data$cluster_label <- factor(cluster_data$cluster, 
                                         levels = 1:input$clusters, 
                                         labels = cluster_labels)
    
    # Summarize the number of members in each cluster
    cluster_summary <- cluster_data %>%
      group_by(cluster_label, party) %>%  # Group by cluster and party
      summarise(Members = n())  # Summarize the count of members
    
    # Check if the user wants to use party colors (Red/Blue)
    if (input$use_party_colors) {
      # Party color scheme (Red for Republican, Blue for Democrat)
      party_colors <- c("D" = "blue", "R" = "red")
      
      # Create the pie chart based on party affiliation with custom line color
      ggplot(cluster_summary, aes(x = "", y = Members, fill = party)) +
        geom_bar(stat = "identity", width = 1, color = "black") +  # Add line color
        coord_polar("y") +
        labs(title = "Cluster Proportions by Party") +
        theme_minimal() +
        scale_fill_manual(values = party_colors) +
        theme(legend.position = "bottom")
      
    } else {
      # Map colors to different groups for visualization (standard cluster colors)
      group_colors <- c("Moderate Republicans" = "#fc8d62", 
                        "Compromise Conservatives" = "#66c2a5",
                        "Old Guard Republicans" = "#8da0cb", 
                        "Far-Right Establishment" = "#e78ac3",
                        "Far-Right Obstructionists" = "#a6d854",
                        "Progressive Democrats" = "#ffd92f",
                        "Core Democrats" = "#e5c494",
                        "Moderate Democrats" = "#b3b3b3",
                        "Freedom Caucus" = "#d53e4f",
                        "Group 1" = "#1f77b4", "Group 2" = "#ff7f0e", "Group 3" = "#2ca02c",
                        "Group 4" = "#d62728", "Group 5" = "#9467bd", "Group 6" = "#8c564b", 
                        "Group 7" = "#e377c2", "Group 8" = "#7f7f7f", "Group 9" = "#bcbd22", 
                        "Group 10" = "#17becf")
      
      # Handle additional groups if the number of clusters is more than 10
      if (input$clusters > 10) {
        additional_colors <- colorRampPalette(c("darkred", "darkblue"))(input$clusters - 10)
        group_colors <- c(group_colors, setNames(additional_colors, paste("Group", 11:input$clusters)))
      }
      
      # Create the pie chart based on cluster labels
      ggplot(cluster_summary, aes(x = "", y = Members, fill = cluster_label)) +
        geom_bar(stat = "identity", width = 1) +
        coord_polar("y") +
        labs(title = "Cluster Proportions by Cluster") +
        theme_minimal() +
        scale_fill_manual(values = group_colors) +
        theme(legend.position = "none")
    }
  })
  
  
  output$ReadMore <- renderUI({
    selected <- input$selectedMetric
    switch(selected,
           "wcss" = p("Within-Cluster Sum of Squares (WCSS) measures the variance within clusters."),
           "bcss" = p("Between-Cluster Sum of Squares (BCSS) measures the variance between clusters. "),
           "tss" = p("Total Sum of Squares (TSS) is the total variance, which is partitioned into within-cluster and between-cluster variance."),
           "silhouette" = p("Silhouette Score indicates how well points are clustered."),
           "ch_index" = p("Calinski-Harabasz Index measures the ratio of between-cluster and within-cluster dispersion."),
           "db_index" = p("Davies-Bouldin Index measures how well clusters are separated. Lower values indicate better separation.")
    )
  })
  
  # Assuming you have the k-means model km already computed
  output$metricResult <- renderText({
    selected <- input$selectedMetric
    
    set.seed(123)  # Set the random seed for reproducibility
    
    # Perform k-means clustering based on user-selected number of clusters
    cluster_data <- five_thirty %>%
      select(dw_nominate_dim1, dw_nominate_dim2) %>%
      na.omit()
    
    #rename dw_nominate_dim1 and dw_nominate_dim2 to "nominate_dim1" and "nominate_dim2"
    colnames(cluster_data) <- c("nominate_dim1", "nominate_dim2")
    
    km <- kmeans(cluster_data[, c("nominate_dim1", "nominate_dim2")], centers = input$clusters, nstart = 25)
    
    # Calculate WCSS (tot.withinss) and BCSS (betweenss)
    wcss <- km$tot.withinss
    bcss <- km$betweenss
    tss <- wcss + bcss
    
    # Silhouette score requires both cluster assignments and a distance matrix
    dist_matrix <- dist(cluster_data[, c("nominate_dim1", "nominate_dim2")])
    sil_scores <- silhouette(km$cluster, dist_matrix)
    sil_avg <- mean(sil_scores[, 3])  # Average silhouette score
    
    # Calinski-Harabasz and Davies-Bouldin Index
    crits <- intCriteria(as.matrix(cluster_data[, c("nominate_dim1", "nominate_dim2")]), km$cluster, c("Calinski_Harabasz", "Davies_Bouldin"))
    calinski_harabasz <- crits$calinski_harabasz
    davies_bouldin <- crits$davies_bouldin
    
    # Switch to select the appropriate metric and display the actual calculated value
    switch(selected,
           "wcss" = paste("WCSS: ", round(wcss, 2)),
           "bcss" = paste("BCSS: ", round(bcss, 2)),
           "tss" = paste("TSS: ", round(tss, 2)),
           "silhouette" = paste("Silhouette Score: ", round(sil_avg, 2)),
           "ch_index" = paste("Calinski-Harabasz Index: ", round(calinski_harabasz, 2)),
           "db_index" = paste("Davies-Bouldin Index: ", round(davies_bouldin, 2))
    )
  })
  
  # Render the conclusion text based on metric
  output$Conclusion <- renderText({
    selected <- input$selectedMetric
    
    # Provide interpretation guidance based on the metric
    switch(selected,
           "wcss" = "Lower WCSS values are preferred as they indicate compact clusters.",
           "bcss" = "Higher BCSS values are preferred as they indicate well-separated clusters.",
           "tss" = "Total variance (TSS) is partitioned into within-cluster and between-cluster variance.",
           "silhouette" = "Silhouette scores close to 1 indicate well-clustered points.",
           "ch_index" = "Higher Calinski-Harabasz Index indicates better-defined clusters.",
           "db_index" = "Lower Davies-Bouldin Index values indicate better clustering separation."
    )
  })
  
  ######### RESULTS ########
  
  # Render Plotly Parliament Plot
  # Interactive Parliament Plot Function
  # Modified Interactive Parliament Plot Function
  # Modified Interactive Parliament Plot Function
  create_interactive_parliament_plot <- function(fullData, filteredData, color_df, left_right_order) {
    
    # Data processing for parliament plot
    data <- fullData %>%
      select(EPG, Name, Photo, Age, Gender, Activity_Index) %>%
      mutate(seats = 1,
             seats_total = nrow(fullData),
             left_right = left_right_order[EPG]) %>%
      mutate(left_right = factor(left_right, levels = 1:8))
    
    # Rename columns
    colnames(data) <- c("party_name_short", "member_name", "photo", "Age", "Gender", "Activity_Index", "seats", "seats_total", "left_right")
    

    # Create the 'selected' variable using 'filteredData$Name' (ensure matching column names)
    data <- data %>%
      mutate(selected = ifelse(member_name %in% filteredData$full, TRUE, FALSE))
    
    # Handle NAs in 'member_name' (if any)
    data$selected[is.na(data$member_name)] <- FALSE
    
    # Calculate the number of seats in the parliament
    seats_total <- data %>% 
      distinct(seats_total) %>% 
      pull()
    
    # Determine the number of rows
    parl_rows_df <- tibble(
      seats = seq(0, 700, 100),
      parl_rows = seq(5, 12, 1)
    )
    
    parl_rows_nr <- parl_rows_df %>% 
      filter(seats < seats_total) %>% 
      filter(parl_rows == max(parl_rows)) %>% 
      pull(parl_rows) 
    
    # Arrange parties by left-right order
    data <- data %>% 
      arrange(left_right) %>% 
      mutate(cum_seats = cumsum(seats),
             seat_share = seats / seats_total * 100,
             seat_share_label = seat_share %>% round(1) %>% as.character() %>% paste0("%"),
             cum_seats_position = case_when(
               cum_seats == min(cum_seats) ~ cum_seats / 2,
               TRUE ~ lag(cum_seats) + (seats / 2)
             ),
             full_label = paste0(seats, " Seats", " (", seat_share_label, ")"))
    
    # Merge parliament data with colors and selection status
    parliament_data <- parliament_data(
      election_data = data,  
      parl_rows = parl_rows_nr,
      type = 'semicircle',
      party_seats = data$seats
    ) %>% 
      mutate(member_name = data$member_name,
             selected = data$selected) %>%
      left_join(color_df, by = "party_name_short")
    
    # Create parliament plot with adjusted alpha based on selection
    seats_parl <- ggplot(parliament_data, 
                         aes(x, y, color = party_name_short, 
                             text = paste("Member:", member_name, "<br>Party:", party_name_short), 
                             key = member_name)) +
      geom_parliament_seats(aes(alpha = selected), show.legend = FALSE) +
      theme_ggparliament(legend = FALSE) +
      theme(legend.position = "bottom") +
      guides(color = guide_legend(nrow = 2), alpha = "none") +
      scale_color_manual("Party", values = setNames(color_df$colour, color_df$party_name_short)) +
      scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = 0.2), guide = "none")
    
    # Convert ggplot to Plotly for interactivity
    interactive_parliament_plot <- ggplotly(seats_parl, tooltip = "text", width = 900, height = 600, source = "parliamentPlot")
    
    # Adjust the Plotly object to remove duplicate legend entries
    interactive_parliament_plot$x$data <- lapply(interactive_parliament_plot$x$data, function(trace) {
      # Only show legend for the first occurrence of each party
      if (grepl(",FALSE|,TRUE", trace$name)) {
        trace$showlegend <- FALSE
      }
      return(trace)
    })
    
    # Adjust the legend layout
    interactive_parliament_plot <- interactive_parliament_plot %>% 
      layout(
        legend = list(orientation = "h",   # Move the legend to the bottom
                      x = 0.5,             # Center the legend horizontally
                      y = -0.1,            # Move the legend slightly below the plot
                      xanchor = "center",  # Horizontal anchor
                      yanchor = "top")     # Vertical anchor
      ) %>% event_register("plotly_click")
    
    return(interactive_parliament_plot)
  }
  
  
  
  
  
  # Reactive expression for filtered data
  filteredData <- reactive({
    data <- datasets$transformedData
    
    # Initialize an unnamed list to hold filter expressions
    filter_exprs <- list()
    
    # Age filter
    age_default <- c(min(data$Age, na.rm = TRUE), max(data$Age, na.rm = TRUE))
    if (!all(input$ageRange == age_default)) {
      # Filter is adjusted, exclude NAs
      filter_exprs <- c(filter_exprs, list(expr((Age >= !!input$ageRange[1] & Age <= !!input$ageRange[2]) & !is.na(Age))))
    }
    
    # Activity Index filter
    activity_default <- c(min(data$Activity_Index, na.rm = TRUE), max(data$Activity_Index, na.rm = TRUE))
    if (!all(input$activityRange == activity_default)) {
      filter_exprs <- c(filter_exprs, list(expr((Activity_Index >= !!input$activityRange[1] & Activity_Index <= !!input$activityRange[2]) & !is.na(Activity_Index))))
    }
    
    # Gender filter
    gender_default <- c("M", "F")
    if (!setequal(input$gender, gender_default)) {
      filter_exprs <- c(filter_exprs, list(expr((Gender %in% !!input$gender) & !is.na(Gender))))
    }
    
    # Country filter
    if (input$country != "All") {
      filter_exprs <- c(filter_exprs, list(expr((Country == !!input$country) & !is.na(Country))))
    }
    
    # Apply all filters
    if (length(filter_exprs) > 0) {
      data <- data %>% filter(!!!filter_exprs)
    }
    # Else, no filters applied, keep data as is
    
    return(data)
  })
  
  
  
  # Initialize default values after data is loaded
  observe({
    data <- datasets$transformedData
    
    # Age Slider
    output$ageSliderUI <- renderUI({
      sliderInput("ageRange", "Age Range:",
                  min = min(data$Age, na.rm = TRUE),
                  max = max(data$Age, na.rm = TRUE),
                  value = c(min(data$Age, na.rm = TRUE), max(data$Age, na.rm = TRUE)))
    })
    
    # Activity Index Slider
    output$activitySliderUI <- renderUI({
      sliderInput("activityRange", "Activity Index:",
                  min = min(data$Activity_Index, na.rm = TRUE),
                  max = max(data$Activity_Index, na.rm = TRUE),
                  value = c(min(data$Activity_Index, na.rm = TRUE), max(data$Activity_Index, na.rm = TRUE)))
    })
    
    # Gender Checkbox Group
    output$genderCheckboxUI <- renderUI({
      checkboxGroupInput("gender", "Gender:",
                         choices = c("M", "F"),
                         selected = c("M", "F"))
    })
    
    # Country Select Input
    output$countrySelectUI <- renderUI({
      selectInput("country", "Country:",
                  choices = c("All", sort(unique(data$Country))),
                  selected = "All")
    })
  })
  
  # Reset filters
  observeEvent(input$resetFilters, {
    data <- datasets$transformedData
    
    # Reset Age Slider
    updateSliderInput(session, "ageRange",
                      min = min(data$Age, na.rm = TRUE),
                      max = max(data$Age, na.rm = TRUE),
                      value = c(min(data$Age, na.rm = TRUE), max(data$Age, na.rm = TRUE)))
    
    # Reset Activity Index Slider
    updateSliderInput(session, "activityRange",
                      min = min(data$Activity_Index, na.rm = TRUE),
                      max = max(data$Activity_Index, na.rm = TRUE),
                      value = c(min(data$Activity_Index, na.rm = TRUE), max(data$Activity_Index, na.rm = TRUE)))
    
    # Reset Gender Checkbox Group
    updateCheckboxGroupInput(session, "gender",
                             choices = c("M", "F"),
                             selected = c("M", "F"))
    
    # Reset Country Select Input
    updateSelectInput(session, "country",
                      choices = c("All", sort(unique(data$Country))),
                      selected = "All")
  })
  
  output$filterSummary <- renderPrint({
    total_rows <- nrow(datasets$transformedData)
    filtered_rows <- nrow(filteredData())
    na_counts <- sapply(datasets$transformedData[, c("Age", "Activity_Index", "Gender", "Country")], function(x) sum(is.na(x)))
    
    list(
      Total_Rows = total_rows,
      Filtered_Rows = filtered_rows,
      NAs_Included = na_counts,
      Age_Range = input$ageRange,
      Activity_Index_Range = input$activityRange,
      Gender = input$gender,
      Country = input$country
    )
  })
  
  
  
  
  # Render Parliament Plot
  output$parliamentPlot <- renderPlotly({
    
    req(datasets$transformedData)  # Ensure the data is available
    
    fullData <- datasets$transformedData
    fullData$Name <- fullData$full
    
    # Convert the party colors into a tibble
    settings <- get_party_settings()
    left_right_order <- settings$left_right_order
    
    color_data <- as_tibble(settings$party_colors, rownames = "EPG")
    colnames(color_data) <- c("party_name_short", "colour")
    
    # Call the modified plotting function with both datasets
    create_interactive_parliament_plot(fullData, filteredData(), color_data, left_right_order)
  })
  
  
  
  
  
  country_code_map <- c(
    "Luxembourg" = "lu",
    "Italy" = "it",
    "France" = "fr",
    "Germany" = "de",
    "Netherlands" = "nl",
    "Ireland" = "ie",
    "United Kingdom" = "gb",
    "Greece" = "gr",
    "Spain" = "es",
    "Belgium" = "be",
    "Portugal" = "pt",
    "Finland" = "fi",
    "Sweden" = "se",
    "Austria" = "at",
    "Slovenia" = "si",
    "Czech Republic" = "cz",
    "Lithuania" = "lt",
    "Poland" = "pl",
    "Hungary" = "hu",
    "Slovakia" = "sk",
    "Malta" = "mt",
    "Cyprus" = "cy",
    "Estonia" = "ee",
    "Latvia" = "lv",
    "Bulgaria" = "bg",
    "Romania" = "ro",
    "Croatia" = "hr"
  )
  
  
  
  # Display selected politician's details
  observeEvent(event_data("plotly_click", source = "parliamentPlot"), {
    click_data <- event_data("plotly_click", source = "parliamentPlot")
    
    data <- datasets$transformedData  # Use the selected dataset
    data$Name <- data$full
    
    if (!is.null(click_data)) {
      clicked_name <- click_data$key
      
      
      if (clicked_name %in% data$Name) {
        # Find the politician's details in the dataset
        politician_info <- data %>% filter(Name == clicked_name)
        
        # Lookup the ISO code using the country name
        country_code <- country_code_map[politician_info$Country]
        
        # Construct the relative path to the flag image
        flag_path <- if (!is.na(country_code)) {
          paste0("flags/", country_code, ".png")
        } else {
          NULL
        }
        
        # Debugging log for flag path
        cat("Flag Path:", flag_path, "\n")
        
        # Render the politician details in the sidebar
        output$politicianDetails <- renderUI({
          tagList(
            tags$h4(politician_info$Name),
            tags$img(src = politician_info$Photo, width = "60%", height = "auto"),  # Politician's photo
            tags$p(
              strong("EPG:"), politician_info$EPG
            ),
            tags$p(
              strong("Country:"),
              if (!is.null(flag_path)) {
                tags$img(src = flag_path, width = "20px", height = "15px")  # Render flag
              } else {
                "(Flag not available)"
              },
              " ", politician_info$Country
            ),
            tags$p(strong("Party:"), politician_info$Party),
            tags$p(strong("Age:"), politician_info$Age)
          )
        })
      } else {
        # If no match is found, clear the UI or display a message
        output$politicianDetails <- renderUI({
          tagList(
            tags$h4("No politician found"),
            tags$p("Click on a valid seat to view details.")
          )
        })
      }
    }
  })
  
  
  
  
  ##### RESULTS #####
  clusteringCompleted <- reactiveVal(FALSE)
  
  
  # Tab Panel for Step 5: Results
  output$resultsContent <- renderUI({
    if (!clusteringCompleted()) {
      # Placeholder content
      tagList(
        tags$div(
          style = "text-align: center; padding: 50px;",
          tags$h3("Clustering Not Completed"),
          tags$p("Please complete the clustering process to view the results."),
          tags$img(src = "placeholder-image.png", width = "600px", height = "auto", style = "border:2px solid black;"),
          br(),
          br()
        )
      )
    } else {
      # Actual Results content
      tagList(
        # Title Section with explanatory text
        fluidRow(
          column(width = 2),
          column(
            width = 8,
            style = "text-align:center; margin-top: 20px;",
            tags$img(src = "DALLE.webp", width = "100%", height = "auto", style = "border:1px solid #e2e2e2;"),
            tags$p(
              "Image generated with DALL·E (AI).", 
              style = "font-size: 12px; color: #666; margin-top: 5px;"
            ),  # Bildnachweis hinzugefügt
            h1(uiOutput("dynamicTitle"), style = "margin-top: 20px;"),
            p(
              paste("PUBLISHED", format(Sys.time(), "%B %d, %Y, at %I:%M %p")),
              class = "byline"
            )
          ),
          column(width = 2)
        ),
        
        
        # Subtitle or introductory paragraph
        fluidRow(
          column(
            width = 8, offset = 2,
            p("Explore the final clustering of Members of the European Parliament (MEPs). Each cluster represents a unique grouping based on voting behavior. Use the tools below to interact with the data.",
              class = "subtitle", style = "text-align:center;")
          )
        ),
        
        tags$div(class = "divider"),
        
        # European Political Group (EPG) Legend
        fluidRow(
          column(
            width = 8, align = "left",
            h4("Color a specific EPG"),
            # Dynamic UI output for EPG legend buttons
            uiOutput("epg_legend_buttons"),
            br(),
            actionButton("reset_colors", "Reset Highlighting", class = "btn btn-default")
          ),
          column(
            width = 4, align = "right",
            br(),
            br(),
            selectInput(
              inputId = "searchPolitician",
              label = "Search Politician:",
              choices = c("Choose a name" = "", NULL),  # No default selection
              selected = NULL
            ),
            checkboxInput("color_epg", label = HTML("<strong>Color by European Political Group (EPG)</strong>"), value = TRUE)
          )
        ),
        
        tags$div(class = "divider"),
        
        # Cluster plots section with responsive layout
        fluidRow(
          column(
            width = 10, offset = 1,
            h3("Cluster Visualizations"),
            uiOutput("clusterPlots"),  # Where the cluster plots will be dynamically displayed
            class = "cluster-plot"
          )
        ),
        
        tags$div(class = "divider"),
        
        # DW-NOMINATE 2nd Dimension Score Distribution
        fluidRow(
          column(
            width = 6,
            h3("DW-NOMINATE 1nd Dimension Score Distribution"),
            conditionalPanel(
              condition = "output.dwNominatePlot== null",
              p("The Plot will appear here after performing the clustering. Make sure to finish the clustering process."),
              style = "border:1px solid black; padding:10px; background-color:lightyellow;"),
            plotOutput("dwNominatePlot", width = "500px")
          ),
          column(
            width = 6,
            h4("Radar Chart Comparing Clusters"),
            plotlyOutput("clusterComparisonRadar")
          )
        ),
        
        tags$div(class = "divider"),
        
        # Overview of All Clusters
        fluidRow(
          column(
            width = 10, offset = 1,
            h3("Overview of All Clusters"),
            p("An aggregated view of all clusters and their characteristics.", style = "color: #666;")
          )
        ),
        
        # UI code using DT
        fluidRow(
          column(
            width = 12,
            reactableOutput("clusterComparisonTable")
          )
        ),
        
        br(),
        br()
      )
    }
  })
  
  # Tab Panel
  output$resultsTab <- renderUI({
    tabPanel(
      "Step 5: Results",
      uiOutput("resultsContent")
    )
  })
  
  
  
  
  
  #################
  
  
  output$dynamicTitle <- renderUI({
    data <- get_selected_data()
    
    # Filter clusters if the checkbox is checked
    if (input$ignore_first_cluster) {
      filtered_data <- data[data$Cluster != 0, ]
    } else {
      filtered_data <- data
    }
    
    # Count unique clusters
    num_clusters <- length(unique(filtered_data$Cluster))
    
    # Convert parliament_period from "P6", "P7", etc., to "6th", "7th", etc.
    parliament_period <- switch(
      input$selectedP,
      "P6" = "6th European Parliament",
      "P7" = "7th European Parliament",
      "P8" = "8th European Parliament",
      "P9" = "9th European Parliament"
    )
    
    h2(
      style = "color: #222222; font-weight: bold;",
      HTML(paste0("The ", num_clusters, 
                  "<span style='font-size: 0.8em; opacity: 0.6;'>-ish</span> Groups of Politicians in the ", parliament_period))
    )
  })
  
  
  
  
  
  selected_epg <- reactiveVal(NULL)

  
  
  observe({
    data <- get_selected_data()
    req(data)
    
    if (clusteringCompleted() && "Cluster" %in% colnames(data)) {
      clustered_politicians <- data %>%
        filter(!is.na(Cluster)) %>%
        pull(Name) %>%
        unique()
      
      # Keep the current selection if it's valid
      current_selection <- input$searchPolitician
      if (!is.null(current_selection) && current_selection %in% clustered_politicians) {
        selected_value <- current_selection
      } else {
        selected_value <- NULL  # Reset to NULL if invalid
      }
      
      # Update the input without resetting a valid selection
      updateSelectInput(
        session,
        inputId = "searchPolitician",
        choices = c("Choose a name" = "", clustered_politicians),
        selected = selected_value  # Retain current selection
      )
    }
  })
  
  
  
  # UI for dynamically generated EPG highlight buttons
  output$epg_legend_buttons <- renderUI({
    data <- get_selected_data()
    req(data)
    
    # Get unique EPGs and colors for the current dataset
    epgs <- unique(data$EPG)
    settings <- get_party_settings()
    epg_colors <- settings$party_colors
    
    # Generate buttons for each EPG dynamically
    fluidRow(
      lapply(epgs, function(epg) {
        epg_clean <- gsub("[^a-zA-Z0-9]", "_", epg)  # Clean the EPG name for input IDs
        column(width = 3,
               actionButton(inputId = paste0("highlight_", epg_clean), label = epg,
                            style = paste("background-color:", epg_colors[epg], "; color: white; width: 100%;"))
        )
      })
    )
  })
  
  # Observers for each dynamically created EPG button
  observe({
    data <- get_selected_data()
    req(data)
    data <- data[data$Cluster != 0, ]
    
    epgs <- unique(data$EPG)
    
    # Create an observer for each EPG button dynamically
    lapply(epgs, function(epg) {
      epg_clean <- gsub("[^a-zA-Z0-9]", "_", epg)  # Clean the EPG name for input IDs
      observeEvent(input[[paste0("highlight_", epg_clean)]], {
        if (input[[paste0("highlight_", epg_clean)]] > 0) {
          selected_epg(epg)
        }
      })
    })
  })
  
  # Observer to check for color reset
  observeEvent(input$reset_colors, {
    selected_epg(NULL)  # Reset the selected EPG
  })
  
  # Plot generation with conditional highlighting based on selected politician or EPG
  output$clusterPlots <- renderUI({
    data <- get_selected_data()
    req(data)  # Ensure data is available
    

    
    data <- data[data$Cluster != 0, ]
    
    # Load party colors from settings or use default
    settings <- get_party_settings()
    party_colors <- settings$party_colors
    
    # Get unique clusters and create UI output for each
    unique_clusters <- sort(unique(data$Cluster))  # Sorted for consistent ordering
    
    # Layout: display plots in a grid
    plot_outputs <- lapply(unique_clusters, function(cluster_id) {
      cluster_name <- as.character(cluster_id)
      
      div(
        class = "col-lg-4 col-md-6",
        girafeOutput(outputId = paste0("clusterPlot_", cluster_name), width = "100%", height = "300px")
      )
    })
    
    # Render the list of plots
    tagList(
      div(
        class = "container-fluid",
        fluidRow(
          do.call(tagList, plot_outputs)
        )
      )
    )
  })
  
  # Observing input to dynamically generate cluster plots with highlighting
  observe({
    data <- get_selected_data()
    req(data)
    
    # Check for 'Cluster' column and load party colors
    if (!"Cluster" %in% colnames(data)) {
      return(NULL)
    }
    settings <- get_party_settings()
    party_colors <- settings$party_colors
    
    selected_politician <- input$searchPolitician
    selected_epg_group <- selected_epg()
    color_by_epg <- input$color_epg
    
    data <- data[data$Cluster != 0, ]
    
    # Generate each plot based on each cluster
    unique_clusters <- sort(unique(data$Cluster))
    for (cluster_id in unique_clusters) {
      local({
        cluster_data <- subset(data, Cluster == cluster_id)
        cluster_name <- as.character(cluster_id)
        
        # Default color for all observations
        default_color <- "lightgrey"
        cluster_data$highlight_color <- default_color
        
        # Update the highlight column based on the selected politician or selected EPG
        cluster_data$highlight <- "Normal"  # Default state
        
        # Highlight individual politician if selected
        if (!is.null(selected_politician) && selected_politician != "") {
          cluster_data$highlight <- ifelse(cluster_data$Name == selected_politician, "Highlight", "Normal")
          cluster_data$highlight_color <- ifelse(cluster_data$Name == selected_politician, party_colors[cluster_data$EPG], default_color)
        }
        
        # Highlight EPG group if selected
        if (!is.null(selected_epg_group) && selected_epg_group != "") {
          cluster_data$highlight <- ifelse(cluster_data$EPG == selected_epg_group, "Highlight", cluster_data$highlight)
          
          # Set color only for selected EPG group if the "Color by EPG" option is off
          if (!isTruthy(color_by_epg)) {
            cluster_data$highlight_color <- ifelse(cluster_data$EPG == selected_epg_group, party_colors[cluster_data$EPG], default_color)
          }
        } else if (isTruthy(color_by_epg)) {
          # Apply EPG colors to all if "Color by EPG" is active
          cluster_data$highlight_color <- party_colors[cluster_data$EPG]
        }
        
        output[[paste0("clusterPlot_", cluster_name)]] <- renderGirafe({
          # Tooltips for MEPs
          cluster_data$text <- paste("MEP:", cluster_data$Name, "<br>EPG:", cluster_data$EPG, "<br>Cluster:", cluster_data$Cluster)
          
          # Generate circle layout with dynamic sizes
          packing <- circleProgressiveLayout(
            ifelse(cluster_data$highlight == "Highlight", 2, 1),  # Larger circles for highlights
            sizetype = 'area'
          )
          cluster_data <- cbind(cluster_data, packing)
          dat.gg <- circleLayoutVertices(packing, npoints = 50)
          
          # Align indices properly for `dat.gg` and `cluster_data`
          dat.gg$highlight_color <- cluster_data$highlight_color[dat.gg$id]
          dat.gg$alpha <- ifelse(cluster_data$highlight[dat.gg$id] == "Highlight", 1, 0.8)
          
          # Plot with conditional highlighting
          p <- ggplot() + 
            geom_polygon_interactive(
              data = dat.gg, 
              aes(x, y, group = id, fill = highlight_color, tooltip = cluster_data$text[id], 
                  data_id = id, alpha = alpha), 
              colour = ifelse(dat.gg$alpha == 1, "black", "black")  # Dynamic border colors
            ) +
            scale_fill_identity() +  # Use fill colors directly from the `highlight_color` column
            scale_alpha_identity() +  # Ensures alpha is handled as specified
            theme_void() +
            coord_equal() +
            labs(title = paste("Cluster", cluster_name)) +
            theme(
              plot.margin = unit(c(0, 0, 0, 0), "cm"),
              legend.position = "none"
            )
          
          # Render interactive plot
          girafe(ggobj = p, width_svg = 5, height_svg = 5)
        })
      })
    }
  })
  

  # DW-NOMINATE 2nd Dimension Score Distribution
  output$dwNominatePlot <- renderPlot({
    data <- get_selected_data()
    
    # Ensure that data is available
    req(data)
    
    # Validate that data has the required columns
    validate(
      need("Cluster" %in% names(data), "The 'Cluster' column is missing in the data."),
      need("coord2D_red" %in% names(data), "The 'coord2D_red' column is missing in the data.")
    )
    
    # Filter out rows where Cluster is 0 and validate non-emptiness
    data <- data[data$Cluster != 0, ]
    validate(
      need(nrow(data) > 0, "No data available for plotting after filtering.")
    )
    
    # Reverse the cluster order for the y-axis if desired
    data$Cluster <- factor(data$Cluster, levels = rev(sort(unique(data$Cluster))))
    
    # Calculate the median DW-NOMINATE 2nd dimension score for each cluster
    median_data <- data %>%
      group_by(Cluster) %>%
      summarize(median_coord2D = median(coord2D_red, na.rm = TRUE), .groups = "drop")
    
    ggplot(data, aes(x = coord2D_red, y = Cluster, color = Cluster)) +
      geom_point(size = 1.5, alpha = 0.7) +
      geom_point(data = median_data, aes(x = median_coord2D, y = Cluster), color = "black", size = 3) +
      geom_text(data = median_data, aes(x = median_coord2D, y = Cluster, label = round(median_coord2D, 2)), 
                vjust = -0.5, color = "black") +
      scale_y_discrete(name = "Clusters") +
      scale_x_continuous(name = "DW-NOMINATE 1nd Dimension (Conservatism-Liberalism)") +
      labs(title = "Political Left-Right-Distribution") +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 18, face = "bold"),
        plot.subtitle = element_text(size = 14),
        axis.title = element_text(size = 14)
      )
  })
  
  
  #######  EACH CLUSTER PLOT #######
  
  # Create a reactive expression to store clustering results
  clustering_results <- reactive({
    data <- get_selected_data()
    req(data)
    
    # Ensure clusters exist
    if (!"Cluster" %in% colnames(data)) {
      showNotification("Please run a clustering algorithm to generate clusters.", type = "warning")
      return(NULL)
    }
    
    data <- data[data$Cluster != 0, ]
    
    data
  })
  


  # Server code using reactable with enhanced waffle chart and percentages in the legend
  output$clusterComparisonTable <- renderReactable({
    data <- clustering_results()
    req(data)
    
    data <- data[data$Cluster != 0, ]
    
    # Get the short names and colors based on the selected legislative period
    period <- input$selectedP
    
    settings <- get_party_settings()
    short_names <- settings$axis_labels
    epg_colors <- settings$party_colors
    names(epg_colors) <- names(short_names)
    
    # Prepare cluster_overview data frame
    cluster_overview <- data %>%
      group_by(Cluster) %>%
      summarise(
        Cluster_Size = n(),
        Avg_Age = round(mean(Age, na.rm = TRUE), 2),
        Avg_Experience = round(mean(Experience, na.rm = TRUE), 2),
        Avg_Attendance = round(mean(Attendance_Index, na.rm = TRUE), 2),
        Avg_Loyalty = round(mean(Loyalty_Index, na.rm = TRUE), 2),
        Avg_Activity = round(mean(Activity_Index, na.rm = TRUE), 2),
        Avg_Winning = round(mean(Winning_Index, na.rm = TRUE), 2),
        Dominant_EPG = short_names[names(sort(table(EPG), decreasing = TRUE))[1]],
        Age_Distribution = list(Age),
        DW_Nominate_Distribution = list(coord2D_red)
      ) %>%
      arrange(Cluster)
    
    # Create the reactable
    reactable(
      cluster_overview,
      searchable = TRUE,
      filterable = TRUE,
      pagination = TRUE,
      defaultPageSize = 10,
      highlight = TRUE,
      bordered = TRUE,
      striped = TRUE,
      fullWidth = TRUE,
      columns = list(
        Cluster = colDef(
          name = "Cluster",
          align = "center",
          cell = function(value) {
            tooltip <- paste("Cluster", value)
            div(title = tooltip, value)
          }
        ),
        Cluster_Size = colDef(name = "Cluster Size", align = "center"),
        Avg_Age = colDef(name = "Average Age", align = "center"),
        Avg_Experience = colDef(name = "Average Experience", align = "center"),
        Avg_Attendance = colDef(name = "Average Attendance", align = "center"),
        Avg_Loyalty = colDef(name = "Average Loyalty", align = "center"),
        Avg_Activity = colDef(name = "Average Activity", align = "center"),
        Avg_Winning = colDef(name = "Average Winning", align = "center"),
        Dominant_EPG = colDef(name = "Dominant EPG", align = "center"),
        Age_Distribution = colDef(
          name = "Age Distribution",
          align = "center",
          cell = function(values) {
            sparkline(values, type = "box")
          }
        ),
        DW_Nominate_Distribution = colDef(
          name = "DW-NOMINATE Score Distribution",
          align = "center",
          width = 180,
          cell = function(values) {
            # Create a sparkline for DW-NOMINATE distribution
            sparkline(values, type = "bar", chartRangeMin = -1, chartRangeMax = 1)
          }
        )
      ),
      details = function(index) {
        # Get the cluster ID
        cluster_id <- cluster_overview$Cluster[index]
        # Filter data for the selected cluster
        cluster_data <- data %>% filter(Cluster == cluster_id)
        
        # --- EPG Composition Waffle Chart ---
        # Prepare EPG distribution data
        epg_distribution <- cluster_data %>%
          group_by(EPG) %>%
          summarise(Count = n()) %>%
          mutate(
            EPG_Full = short_names[EPG],
            Percentage = (Count / sum(Count)) * 100
          )
        
        # Prepare data for waffle chart
        epg_counts <- epg_distribution$Count
        # Add percentages to the legend labels
        legend_labels <- paste0(epg_distribution$EPG_Full, " (", round(epg_distribution$Percentage, 1), "%)")
        names(epg_counts) <- legend_labels
        
        # Define the colors for the EPGs
        epg_colors_full <- epg_colors[epg_distribution$EPG]
        names(epg_colors_full) <- legend_labels
        
        # Render the waffle chart
        output[[paste0("epgWaffle_", cluster_id)]] <- renderPlot({
          # Create the waffle chart
          waffle_chart <- waffle(
            parts = epg_counts,
            rows = 10,
            colors = epg_colors_full,
            title = paste("EPG Composition in Cluster", cluster_id),
            legend_pos = "bottom"
          )
          
          # Customize the chart
          waffle_chart <- waffle_chart +
            theme_minimal() +
            theme(
              plot.title = element_text(size = 16, face = "bold"),
              legend.title = element_blank(),
              legend.text = element_text(size = 10),
              axis.text = element_blank(),
              axis.title = element_blank(),
              panel.grid = element_blank(),
              plot.background = element_rect(fill = "white", color = NA)
            )
          
          print(waffle_chart)
        })
        
        # --- Top Activists Table ---
        # Prepare top activists data
        top_activists <- cluster_data %>%
          arrange(desc(Activity_Index)) %>%
          head(5) %>%
          select(Name, Party, Country, Activity_Index)
        
        output[[paste0("topActivists_", cluster_id)]] <- renderTable({
          top_activists
        }, striped = TRUE, bordered = TRUE, hover = TRUE)
        
        # Return the HTML content
        htmltools::div(
          style = "padding: 16px;",
          htmltools::h5(paste("Detailed Information for Cluster", cluster_id)),
          htmltools::div(
            style = "display: flex; flex-wrap: wrap;",
            # EPG waffle chart
            htmltools::div(
              style = "flex: 1; min-width: 300px; padding: 10px;",
              h6("Political Group Composition:"),
              plotOutput(outputId = paste0("epgWaffle_", cluster_id), height = "300px")
            ),
            # Top activists table
            htmltools::div(
              style = "flex: 1; min-width: 300px; padding: 10px;",
              h6("Top 5 Activists by Activity Index:"),
              tableOutput(outputId = paste0("topActivists_", cluster_id))
            )
          )
        )
      },
      theme = reactableTheme(
        headerStyle = list(background = "#f7f7f7"),
        borderColor = "#dfe2e5"
      )
    )
  })
  

  
  # Server code for Interactive Radar Chart Comparing Clusters with plotly
  output$clusterComparisonRadar <- renderPlotly({
    data <- clustering_results()
    req(data)
    
    data <- data[data$Cluster != 0, ]
    
    # Prepare data for radar chart
    radar_data <- data %>%
      group_by(Cluster) %>%
      summarise(
        Economy_Score = mean(Economy_Score, na.rm = TRUE),
        Social_Score = mean(Social_Score, na.rm = TRUE),
        Foreign_Policy_Score = mean(Foreign_Policy_Score, na.rm = TRUE),
        Industry_Score = mean(Industry_Score, na.rm = TRUE),
        Education_Score = mean(Education_Score, na.rm = TRUE),
        Budget_Score = mean(Budget_Score, na.rm = TRUE)
      )
    
    # Normalize the data to a 0-1 range
    radar_data <- radar_data %>%
      mutate(across(-Cluster, ~ scales::rescale(., to = c(0, 1))))
    
    # Convert data to long format for plotly radar plot
    radar_data_long <- radar_data %>%
      pivot_longer(-Cluster, names_to = "Score", values_to = "Value")
    
    # Plot radar chart with plotly
    plot_ly(type = 'scatterpolar', mode = 'lines+markers') %>%
      add_trace(
        data = radar_data_long,
        r = ~Value,
        theta = ~Score,
        color = ~Cluster,
        fill = 'toself',
        hoverinfo = 'text',
        text = ~paste("Cluster:", Cluster, "<br>Score:", Score, "<br>Value:", round(Value, 2))
      ) %>%
      layout(
        polar = list(
          radialaxis = list(range = c(0, 1), tickvals = seq(0, 1, 0.2)),
          angularaxis = list(tickfont = list(size = 12))
        ),
        showlegend = TRUE,
        legend = list(title = list(text = "Cluster"), orientation = "h", x = 0.5, xanchor = "center", y = -0.2)
      )
  })
  
  


  
  
})
