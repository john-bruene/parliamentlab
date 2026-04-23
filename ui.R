library(shiny)
library(shinythemes)
library(DT)
library(ggplot2)
library(car)
library(nortest)
library(tseries)
library(RcmdrMisc)
library(lmtest)
library(sortable)
library(plotly)
library(shinyBS)
library(shinyjs) 
library(fontawesome)
library(reactable)
library(bslib)
library(shinyWidgets)


# Define EPG colors
epg_colors <- c(
  "EPP" = "#1f78b4",  # European People's Party
  "S&D" = "#e31a1c",  # Progressive Alliance of Socialists and Democrats
  "Renew" = "#33a02c",  # Renew Europe
  "Greens" = "#b2df8a",  # Greens/EFA
  "ID" = "#ff7f00",  # Identity and Democracy
  "ECR" = "#6a3d9a",  # European Conservatives and Reformists
  "GUE/NGL" = "#fb9a99",  # European United Left–Nordic Green Left
  "Non-Inscrits" = "#b15928"  # Non-attached members
)



shinyUI(fluidPage(
  theme = shinytheme("lumen"),  # Apply the Lumen theme
  
  useShinyjs(),  # Enable shinyjs for interactivity
  
  # Custom CSS for the highlight effect
  tags$style(HTML("
    .highlight {
      background-color: #d4f7d4 !important;
      border-radius: 10px;
      padding: 5px;
    }
  ")),
                  

  titlePanel("Silent Parties: A Cluster Analysis of Voting Behavior in the European Parliament"),
    navbarPage("Content",
               tabPanel(icon("home"),
                        
                        fluidRow(column(
                          
                          br(),
                          tags$img(src="UniGoettingen_logo.png",width="200px",height="auto"),width=2),
                                 column(
                                   
                                   br(),
                                   p("Welcome to this interactive application, designed to explore and analyze the clustering of voting behaviors within the European Parliament. 
    This tool provides insights into how Members of the European Parliament (MEPs) form latent voting blocs based on their voting patterns. 
    Using advanced dimensionality reduction techniques like UMAP, MCA, and DW-NOMINATE, as well as clustering methods such as K-Means, PAM, and HDBSCAN, 
    this application allows you to investigate the ideological alignments and coalitions that emerge within the EU legislative body.", 
                                     strong("Don't worry if you're new to these techniques!"), "This app provides an intuitive interface to guide you through each step, 
    offering visuals and interpretations to help you understand the complex relationships between MEPs.", 
                                     style="text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                                   br(),
                                   
                                   p("The data used in this application are publicly accessible through the", em("VoteWatch Europe"), "and", em("PollTrack.eu"), "databases. 
    These data sources contain voting records and biographical information for MEPs, covering multiple EU legislative sessions from 2004 to 2022. 
    By leveraging this comprehensive dataset, you can gain insights into how political groups, alliances, and individual MEPs align on critical issues across 
    different policy areas, including economics, foreign policy, social issues, and more.", 
                                     style="text-align:justify;color:black;background-color:papayawhip;padding:15px;border-radius:10px"),
                                   
                                   width=8
                                 ),
                                 
                                 column(
                                     br(),
                                     tags$img(src="European_Parliament_logo.png",width="200px",height="auto"),
                                     br(),
                                     br(),
                                     p("For more information please check the",em("European Parliament"),"page clicking",
                                     br(),
                                     a(href="https://data.europarl.europa.eu/en/home", "Here",target="_blank"),style="text-align:center;color:black"),
                                   
                                        width=2)),

                        # 
                        hr(),
                        tags$style(".fa-clipboard-list {color:#E87722}"),
                        h3(p("How to Use this App", icon("clipboard-list", lib = "font-awesome"), style = "color:black;text-align:center")),
                    
                        tags$img(src="explanation.png", 
                                 width="1000px", 
                                 height="auto", 
                                 style="display: block; margin-left: auto; margin-right: auto;"),

                        
                        #     fluidRow(column(DT::dataTableOutput("RawData"),
                        #                     width = 12)),
                        hr(),
                        

                        
                        # Header Text
                        div(
                          tags$h3("How This Project Came to Be", style = "text-align:center; color:black; padding-top:20px;"),
                          br(),
                          tags$p("This application allows you to explore the complexities of MEP voting behavior through various analytical lenses. My hope is that it reveals some of the nuances within the European Parliament and inspires further exploration.", 
                                 style = "text-align:center; color:black; background-color:lavender; padding:10px; border-radius:10px;")
                        ),
                        
                        # Flowchart-style progress with icons, adjusted with flex layout
                        div(
                          style = "display: flex; justify-content: center; gap: 20px; flex-wrap: wrap; padding-top: 20px;",
                          
                          # Each step as a flex item
                          div(style = "text-align: center; width: 200px;",
                              icon("lightbulb", "fa-3x", style = "color:#FFD700;"),
                              tags$h4("Inspiration"),
                              tags$p("The idea to analyze voting behavior in the European Parliament started with a simple question: Are there hidden patterns in the way MEPs vote?")
                          ),
                          
                          div(style = "text-align: center; width: 200px;",
                              icon("database", "fa-3x", style = "color:#6495ED;"),
                              tags$h4("Data Collection"),
                              tags$p("Using publicly available data on MEPs' voting records, I gathered detailed datasets spanning multiple legislative periods.")
                          ),
                          
                          div(style = "text-align: center; width: 200px;",
                              icon("cogs", "fa-3x", style = "color:#FFA07A;"),
                              tags$h4("Data Processing"),
                              tags$p("Data cleaning, filtering, and encoding helped prepare the dataset for deeper analysis, ensuring accuracy in every step.")
                          ),
                          
                          div(style = "text-align: center; width: 200px;",
                              icon("chart-line", "fa-3x", style = "color:#32CD32;"),
                              tags$h4("Exploratory Analysis"),
                              tags$p("With visualizations and statistical summaries, I began uncovering initial patterns and trends in the voting data.")
                          )
                        ),
                        
                        # Second row
                        div(
                          style = "display: flex; justify-content: center; gap: 20px; flex-wrap: wrap; padding-top: 20px;",
                          
                          div(style = "text-align: center; width: 200px;",
                              icon("project-diagram", "fa-3x", style = "color:#FF69B4;"),
                              tags$h4("Dimensionality Reduction"),
                              tags$p("Using UMAP, MCA, and DW-NOMINATE, I transformed high-dimensional voting data into a compact format for clustering.")
                          ),
                          
                          div(style = "text-align: center; width: 200px;",
                              icon("sitemap", "fa-3x", style = "color:#FF8C00;"),
                              tags$h4("Clustering"),
                              tags$p("Applying K-Means, PAM, and HDBSCAN, I identified clusters representing potential political groups or alliances.")
                          ),
                          
                          div(style = "text-align: center; width: 200px;",
                              icon("check-circle", "fa-3x", style = "color:#4682B4;"),
                              tags$h4("Validation"),
                              tags$p("Metrics like silhouette score and stability analysis validated the clusters' reliability, ensuring meaningful insights.")
                          )
                        ),
                        

                        
                        hr(),
                        p(
                          em("Submitted in partial fulfillment of the requirements", br("for the degree of Master of Science")),
                          br(),
                          em("Developed by"),
                          br("John F. Brüne"),
                          style = "text-align:center; font-family: times"
                        )
                        ),
               tabPanel("Step 1: Introduction",
                        
                        fluidRow(column(width=2),
                          column(
                                  h4("Introduction - Defining the Problem",style="color:black;text-align:center"),
                                  width=8,style="background-color:lavender;border-radius: 10px")
                                  ),
                        br(),
                        fluidRow(column(width=2, icon("hand-point-right", "fa-5x"), align = "center"),
                                 column(
                                   p("In May 2024 the U.S. website FiveThirtyEight conducted an analysis to explore patterns in the voting behavior of U.S. House members. 
             They used the K-Means clustering algorithm to categorize representatives into 8 distinct clusters based on their voting records. 
             However, this example serves as a reminder of the challenges of clustering unlabelled data: there is no single 'correct' number of clusters. 
             Depending on how we choose to analyze the data, we could find 4, 8, or even 10 clusters, without a definitive answer as to which is most accurate.", 
                                     style = "color:black;text-align:justify"),
                                   a(href = "https://projects.fivethirtyeight.com/types-democrats-republicans-house-2024/", 
                                     tags$img(src = "538_Overview_1.png", width = "100%", height = "auto", style = "border:1px solid black"),
                                     target = "_blank"),
                                   
                                   p("In the following tabs, this study will turn to similar data from the European Parliament, aiming to explore the same clustering challenge and investigate how representatives form latent voting blocs based on their behavior.",
                                     style = "color:black;text-align:justify; background-color:lightyellow; padding:10px"),


                                   width = 8, style = "background-color:lavender;border-radius: 10px")
                        ),
                        br(),

                        hr(),
                        tags$style(".fa-chart-pie {color:#E87722}"),
                        h3(p(em("Graphical tests "),icon("chart-pie",lib = "font-awesome"),style="color:black;text-align:center")),
                        tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: coral; border-top: 1px coral; border-bottom: 1px coral;border-left: 1px coral}")),
                        tags$style(HTML(".js-irs-0 .irs-max, .js-irs-0 .irs-min {background:papayawhip}")),
                        
                        br(),
                        sidebarLayout(
                                      sidebarPanel(
                                        
                                        sliderInput("clusters", 
                                                    "Chose the number of Clusters:", 
                                                    min = 2, 
                                                    max = 10, 
                                                    value = 8),
                                        
                                        #sliderInput("nstart", "Number of Random Starts (nstart):", 
                                         #           min = 1, max = 50, value = 25),  # Slider for nstart value

                                        # Checkbox for red-blue intensity coloring
                                        checkboxInput("use_party_colors", "Use Party Colors (Red/Blue)", value = FALSE), 
                                        br(),
                                        
                                        
                                        p("Remember this is the 538s result:",style="color:black;text-align:center"),
                                        a(href="https://projects.fivethirtyeight.com/types-democrats-republicans-house-2024/", tags$img(src="538_Example.png",width = "90%", height = "auto",style="border:1px solid black"),
                                              target="_blank"),
                                        br(),
                                        br(),
                                        tags$style(".fa-wikipedia-w {color:black}"),
                                        p("Read more about k-means clustering here → ", a(href="https://en.wikipedia.org/wiki/K-means_clustering", icon("wikipedia-w"),target="_blank"),style="color:black;text-align:center")
                                      
                                        
                                        
                                      ),
                                      mainPanel(
                                        fluidRow(
                                          column(width = 1),  # Add an empty column on the left to center the yellowish box
                                          column(
                                            withMathJax(),
                                            p('The K-Means algorithm aims to minimize the following objective function:', 
                                              style = "color:black;text-align:justify"),
                                            
                                            p('$$ \\text{argmin} \\sum_{i=1}^{k} \\sum_{x_j \\in C_i} || x_j - \\mu_i ||^2 $$', 
                                              style = "color:black;border:1px solid black;background-color:white"),
                                            
                                            p("Here, \\(k\\) represents the number of clusters, \\(x_j\\) are the data points (voting records), and \\(\\mu_i\\) represents the centroid of each cluster \\(C_i\\). 
        The algorithm iteratively adjusts these centroids to minimize within-cluster variance, but the selection of \\(k\\) remains subjective. 
        The clustering of unlabelled data is inherently flexible — meaning that one dataset could yield different numbers of clusters depending on interpretation and goals.",
                                              style = "color:black;text-align:justify"),
                                            
                                            width = 10, 
                                            style = "background-color:papayawhip;border-radius: 10px"
                                          ),
                                          column(width = 1)  # Add an empty column on the right to balance the space
                                        ),
                                        
                                        br(),
                                        
                                        fluidRow(
                                          column(br(), plotOutput("clusterPie"), br(), width = 4, style = "border:1px solid black"),
                                          column(br(), plotOutput("clusterPlot_538"), br(), width = 8, style = "border:1px solid black;border-left:none")
                                        )
                                      )
                        ),
                        hr(),
                        tags$style(".glyphicon-folder-open {color:#E87722}"),
                        h3(p(em("Analytical tests  "),icon("folder-open",lib = "glyphicon"),style="color:black;text-align:center")),
                        br(),
                        sidebarLayout(
                          sidebarPanel(
                            selectInput("selectedMetric", p("Please select the metric you want to view:", style="color:black; text-align:center"),
                                        choices = list(
                                          "Silhouette Score" = "silhouette",
                                          "Calinski-Harabasz Index" = "ch_index",
                                          "Davies-Bouldin Index" = "db_index",
                                          "Total Sum of Squares (TSS)" = "tss",
                                          "Within-Cluster Sum of Squares (WCSS)" = "wcss",
                                          "Between-Cluster Sum of Squares (BCSS)" = "bcss"

                                        )),
                            uiOutput("ReadMore")
                          ),
                          mainPanel(
                            fluidRow(
                              tags$head(tags$style("#metricResult{height: 155px; border: 1px solid black; background-color: lavender}")),
                              column(verbatimTextOutput("metricResult"), br(), width = 6),
                              
                              column(br(),
                                     p("Remember, the interpretation of each metric is important in determining clustering quality.", style="color:black"),
                                     br(),
                                     textOutput("Conclusion"),
                                     br(), width = 6, style="background-color:lavender;border-left:8px solid blue")
                            ),
                            br(),
                            br()
                          ))
                        ),
               
               tabPanel("Step 2: Data Preparation",
                        
                        # Title Section
                        fluidRow(
                          column(width = 2),
                          column(
                            h4(p("Data Preparation", style = "color:black;text-align:center")),
                            width = 8, style = "background-color:lavender; border-radius: 10px"
                          )
                        ),
                        
            

                        # Quote Section
                        br(),
                        fluidRow(
                          column(width = 2),
                          column(
                            p("\"In God we trust. All others must bring data.\"", style = "color:black; font-style:italic; text-align:center; font-size:16px"),
                            p("~ W. Edwards Deming", style = "color:black; text-align:center; font-weight:bold"),
                            width = 8, style = "background-color:papayawhip"
                          )
                        ),
                        
                        # Introduction Section
                        br(),
                        fluidRow(
                          column(width = 2, icon("hand-point-right", "fa-5x"), align = "center"),
                          column(
                            p("Data preparation is a critical phase in any analysis, ensuring that the data is clean, standardized, and ready for modeling. In this section, we outline the key steps we took to prepare the voting data from the European Parliament for clustering. These steps are designed to create a robust dataset that accurately represents the voting patterns and ideological positions of the Members of the European Parliament (MEPs).", style = "color:black; text-align:justify; padding:10px"),
                            width = 8, style = "background-color:lavender; border-radius: 10px; padding:10px"
                          )
                        ),
                        
                        # Data Preparation Steps
                        br(),
                        fluidRow(
                          column(width = 2),
                          column(
                            h4(p("1. Data Collecting", style = "color:black")),
                            tags$img(src = "voting.png", width = "auto", height = "150px", style = "border: 1px solid black; margin-bottom:10px"),
                            tags$img(src = "plus.png", width = "auto", height = "80px", 
                                     style = "margin: 10px 20px;"),  # 10px top/bottom, 20px 
                            tags$img(src = "parltrack_ac.png", width = "auto", height = "150px", style = "border: 1px solid black; margin-bottom:10px"),
                            p("The dataset was carefully collected from publicly available voting records of the European Parliament. Each record contains information on votes cast by Members of the European Parliament (MEPs) across various issues, allowing us to analyze their political alignment and engagement. This initial step ensures we have a comprehensive dataset for meaningful analysis.", style = "color:black; text-align:justify"),
                            width = 8, style = "background-color:lavender; border-radius: 10px; padding:10px"
                          )
                        ),
                        
                        br(),
                        fluidRow(
                          column(width = 2),
                          column(
                            h4(p("2. Data Cleaning", style = "color:black")),
                            p("To ensure data quality, we applied a complete case analysis approach, removing any records with missing values. By doing so, we can rely on each observation in the dataset as a full representation of each MEP's voting behavior, leading to more accurate and reliable clustering results.", style = "color:black; text-align:justify"),
                            width = 8, style = "background-color:lavender; border-radius: 10px; padding:10px"
                          )
                        ),
                        
                        br(),
                        fluidRow(
                          column(width = 2),
                          column(
                            h4(p("3. Feature Engineering", style = "color:black")),
                            p("In this step, we used our raw data to create more meaningful variables.", style = "color:black; text-align:justify"),
                            width = 8, style = "background-color:lavender; border-radius: 10px; padding:10px"
                          )
                        ),
                        
                        br(),
                        fluidRow(
                          column(width = 2),
                          column(
                            h4(p("4. Data Transformation", style = "color:black")),
                            p("To prepare the dataset for clustering, we standardized each feature, setting the mean to zero and the standard deviation to one. This transformation ensures that all variables contribute equally, enhancing the clustering algorithm's ability to detect patterns across different dimensions of voting behavior.", style = "color:black; text-align:justify"),
                            width = 8, style = "background-color:lavender; border-radius: 10px; padding:10px"
                          )
                        ),
                        
                        
                        hr(),
                        tags$style(HTML("
                                        
                                          .tabbable > .nav > li[class=active]    > a {background-color: #BFF7BB; color:black}
                                        
                                        ")),
                        tabsetPanel(
                          tabPanel("Data Merging",
                                   sidebarLayout(
                                     sidebarPanel(
                                       h4("Merging Options"),
                                      
                                       
                                       br(),
                                       
                                       # Period selection input
                                       selectInput("selectedP", 
                                                   p("Please select the legislative period you want to work with:", style="color:black; text-align:center"),
                                                   choices = list(
                                                     "6th: 2004 - 2009" = "P6",
                                                     "7th: 2009 - 2014" = "P7",
                                                     "8th: 2014 - 2019" = "P8",
                                                     "9th: 2019 - 2024" = "P9"
                                                   )
                                       ),
                                       
                                       p("Choose one or more datasets to combine. Each dataset offers unique information:"),
                                       tags$ul(
                                         tags$li(tags$b("Eurowatch (Simon Hix):"), " Contains ideological scores and voting records, providing insights into the 
                       ideological stance and voting alignment of each MEP."),
                                         tags$li(tags$b("Parltrack (Biographical):"), " Offers biographical data such as age, gender, and country affiliation, 
                       giving a deeper understanding of MEP backgrounds."),
                                         tags$li(tags$b("Parltrack (Activity):"), " Includes data on attendance and activity levels, showing engagement and voting frequency.")
                                       ),
                                       
                                       checkboxGroupInput("datasetsToMerge", "Select Datasets:", 
                                                          choices = c("Eurowatch (Simon Hix)", "Parltrack (Biographical)", "Parltrack (Activity)"), 
                                                          selected = c("Eurowatch (Simon Hix)", "Parltrack (Biographical)", "Parltrack (Activity)")),
                                       
                                       # Button to trigger merging
                                       actionButton("mergeDatasets", "Generate Dataset", class = "btn-success"),
                                       
                                       br(),
                                       
                                       # Preview message
                                       p("After selecting the datasets and legislative period, click 'Merge Datasets' to combine the data sources. A preview of the merged data will appear below, allowing you to check that all relevant features are included.")
                                     ),
                                     
                                     mainPanel(
                                       h4("Merged Data Preview"),
                                       
                                       # Placeholder message if data has not been merged yet
                                       conditionalPanel(
                                         condition = "output.mergedDataPreview == null",
                                         p("The merged data preview will appear here after merging."),
                                         style = "border:1px solid black; padding:10px; background-color:lightyellow;"),
                                       
                                       # Data table output for preview
                                       DT::dataTableOutput("mergedDataPreview"),
                                       
                                       br()
                                     )
                                   )
                          ),
                          
                          
                          tabPanel("Data Cleaning",
                                   sidebarLayout(
                                     sidebarPanel(
                                       h4("Cleaning Options"),
                                       actionButton("replaceNAWithZero", "Replace NAs with 0", class = "btn-success"),
                                       actionButton("applyNARules", "Apply NA Rules", class = "btn-success"),

                                       # Add checkbox group for replacing NA with 0
                                       p("Select variables where NA values should be replaced with 0:"),
                                       uiOutput("naReplaceChoices"),  # New UI for replacing NAs in specific columns
                                       
                                       
                                       br(),
                                       sliderInput("naThreshold", "Remove Variables and Observations with more than ...% of data missing:", min = 0, max = 1, value = 0.30, step = 0.05)
                                       
                                       
                                     ),
                                     mainPanel(
                                       h4("Data Preview"),
                                       plotOutput("missingValuesPlot"),
                                       br(),
                                       DT::dataTableOutput("cleanedDataPreview"),
                                       br()
                                     )
                                   )
                          ),
                          
                          tabPanel("Feature Engineering",
                                   sidebarLayout(
                                     sidebarPanel(
                                       h4("Feature Engineering Options"),
                                       p("Select the features you would like to engineer and review the description for each feature."),
                                       
                                       # Apply feature engineering button
                                       actionButton("applyFeatureEngineering", "Apply Feature Engineering", class = "btn-success"),
                                       
                                       
                                       # Feature selection checkboxes
                                       checkboxGroupInput(
                                         "featureEngineeringOptions", 
                                         "Select Features to Engineer:", 
                                         choices = c("Attendance Rate", "Loyalty Index", "Winning Index", "Age", "Experience", "Activity Index", "Topic Scores"),
                                         selected = c("Attendance Rate", "Loyalty Index", "Winning Index", "Age", "Experience", "Activity Index", "Topic Scores")
                                       ),
                                       
                                     ),
                                     
                                     mainPanel(
                                       h4("Feature Engineering Results"),
                                       
                                       # Display for explanations and visualization side-by-side
                                       fluidRow(
                                         column(
                                           width = 5,
                                           h5("Feature Explanation"),
                                           uiOutput("featureExplanation"),  # Dynamic content based on selected features
                                           style = "border:1px solid #ddd; padding:10px; border-radius:5px; background-color:#f9f9f9;"
                                         ),
                                         column(
                                           width = 7,
                                           h5("Distribution Plot"),
                                           p("Visualize the distribution of the newly engineered features below to ensure they are properly calculated."),
                                           uiOutput("variableSelectorUI1"),
                                           br(),
                                           plotOutput("featureDistributionPlot"),  # Plot for feature distribution
                                           style = "border:1px solid #ddd; padding:10px; border-radius:5px; background-color:#f9f9f9; border-left:none"
                                         )
                                       ),
                                       br()
                                     )
                                   )
                          ),
                          
                          
                          
                          tabPanel("Data Transformation",
                                   sidebarLayout(
                                     sidebarPanel(
                                       h4("Transformation Settings"),
                                       p("Choose a variable to visualize its distribution before and after transformation."),
                                       
                                       # Dynamically populated dropdown for selecting the variable to visualize
                                       uiOutput("variableSelectorUI"),
                                       br(),
                                       
                                       # Button for applying recommended transformations
                                       actionButton("applyRecommended", "Show Recommended Transformations", 
                                                    class = "btn-primary"),
                                       
                                       
                                       br(),
                                       br(),
                                       
                                       # Apply button
                                       actionButton("applyTransformation", "Apply Transformation", class = "btn-success"),
                                       
                                       br(),
                                       br(),
                                       
                                       p("Drag variables into the appropriate transformation bin to specify how each variable should be transformed."),
                                       

                                         # No Changes bin
                                         div(
                                           h5(strong("No Changes")),
                                           p("Keep the original values without any transformation.", style = "font-size: 12px; color: #555;"),
                                           uiOutput("noChangesBinUI")
                                         ),
                                         br(),
                                         
                                         # Standard Scaling bin
                                         div(
                                           h5(strong("Standard Scaling")),
                                           p("Scale variables to have a mean of 0 and standard deviation of 1.", style = "font-size: 12px; color: #555;"),
                                           uiOutput("standardScalingBinUI")
                                         ),
                                         br(),
                                         
                                         # Min-Max Scaling bin
                                         div(
                                           h5(strong("Min-Max Scaling")),
                                           p("Scale variables to be between 0 and 1.", style = "font-size: 12px; color: #555;"),
                                           uiOutput("minMaxScalingBinUI")
                                         ),
                                         br(),
                                         
                                         # Log Scaling bin
                                         div(
                                           h5(strong("Log Scaling")),
                                           p("Apply logarithmic transformation to reduce skewness. Use with non-negative variables.", style = "font-size: 12px; color: #555;"),
                                           uiOutput("logScalingBinUI")
                                         ),
                                         br(),
                                         
                                         # Binning bin
                                         div(
                                           h5(strong("Binning")),
                                           p("Group continuous variables into discrete bins.", style = "font-size: 12px; color: #555;"),
                                           uiOutput("binningBinUI")
                                         ),
                                         br()
                                         
                                       

                                     ),
                                     
                                     mainPanel(
                                       fluidRow(
                                         br(),
                                         column(width = 1),  # Center the explanatory text box
                                         column(
                                           withMathJax(),
                                           p('Select a variable from the sidebar to see its distribution before and after transformation.'),
                                           p("Transformations can reveal hidden patterns or make data more suitable for modeling. Here’s an example visualization comparing the original and transformed data distributions.", 
                                             style = "color:black;text-align:justify"),
                                           width = 10, 
                                           style = "background-color:papayawhip;border-radius: 10px; padding: 10px"
                                         ),
                                         column(width = 1)  # Balance space on the right
                                       ),
                                       
                                       br(),
                                       
                                       # Side-by-side plots for "before" and "after" transformations
                                       fluidRow(
                                         column(
                                           plotOutput("beforeTransformPlot", height = "300px"),  # Plot for original data
                                           tags$h5("Original Distribution", style = "text-align:center; margin-top: 10px;"),
                                           width = 6, 
                                           style = "border:1px solid black; padding: 10px"
                                         ),
                                         column(
                                           plotOutput("afterTransformPlot", height = "300px"),  # Plot for transformed data
                                           tags$h5("Transformed Distribution", style = "text-align:center; margin-top: 10px;"),
                                           width = 6, 
                                           style = "border:1px solid black; border-left:none; padding: 10px"
                                         )
                                       ),
                                       br()
                                     )
                                   )
                          ),
                          
                          
                          
                          
                          tabPanel("Final Dataset",
                                   sidebarLayout(
                                     sidebarPanel(
                                       h4("Final Dataset Options"),
                                       
                                       conditionalPanel(
                                         condition = "output.transformedDataExists",  # Check if transformed data is ready
                                         input_switch(
                                           id = "show_full_data",
                                           label = "Show Full Data (takes 1-2 minutes to load)",
                                           value = FALSE
                                         ),
                                         br(),
                                         p("The final dataset is ready for download. Use the button below to save the transformed data for further analysis."),
                                         downloadButton("downloadFinalData", "Download Final Dataset")
                                       ),
                                       
                                       # Message if transformed data is not ready
                                       conditionalPanel(
                                         condition = "!output.transformedDataExists",
                                         p(strong("Please complete data transformations first.")),
                                         br(),
                                         br()
                                       )
                                     ),
                                     
                                     mainPanel(
                                       conditionalPanel(
                                         condition = "output.transformedDataExists",  # Only show summaries if transformed data exists
                                         h4("Final Dataset Summary"),
                                         fluidRow(
                                           column(width = 12, DT::dataTableOutput("summaryStats"))  # Summary statistics table
                                         ),
                                         br(),
                                         fluidRow(
                                         )
                                       )
                                     )
                                   )
                          )
                          
                          
                        )
                        
               ),
               
               tabPanel(
                 "Step 3: Exploration",
                 
                 # Header with detailed introduction
                 fluidRow(
                   column(width = 2),
                   column(
                     h4("Exploratory Analysis"),
                     p("In this step, we delve into the key patterns and relationships within the voting behavior data of the European Parliament. 
         Exploratory analysis is essential for understanding how MEPs (Members of the European Parliament) align on different issues, 
         which factions tend to vote together, and how individual MEP characteristics might correlate with voting behavior. 
         This preliminary step will help us determine relevant variables for clustering and ultimately guide our grouping decisions."),
                     p("Use the options below to select variables of interest and generate visualizations that reveal underlying structures in the data. 
         For instance, you might explore how different political groups or countries align on specific voting topics, or examine the attendance and activity levels of MEPs. 
         Additionally, interactive maps and summary tables are provided to give you a comprehensive view of the data."),
                     width = 8, style = "background-color:lavender; border-radius: 10px; padding:15px;"
                   )
                 ),
                 
                 hr(),
                 br(),
                 
                 sidebarLayout(
                   sidebarPanel(
                     
                     h4("Select Variables for Exploration"),
                     p("Choose the variables you would like to analyze."),

                     
                     # Variable selection
                     uiOutput("variableSelectorUI2"),
                     
                     # Summary option
                     checkboxInput("sortmean", "Sort by mean", value = TRUE),
                     
                     br(),
                     
                     hr(),
                   ),
                   
                   mainPanel(
                     h3("Exploratory Plots and Statistics"),
                     
                     p("The exploratory plots and statistics below provide insights into the distribution and relationships within the selected variables. 
         Use these visualizations to identify notable voting patterns, similarities, and differences among MEPs. You may find that certain groups consistently 
         align on votes, while others show greater variability."),
                     
                     hr(),
                     
                     # UI für den Summary Table in der Hauptübersicht hinzufügen
                     fluidRow(
                       column(width = 7,
                              conditionalPanel(
                                condition = "output.extendedBoxPlot == null",
                                p("Please finish the Data Preparation first."),
                                style = "padding:10px; background-color:lightyellow;"),
                              plotOutput("extendedBoxPlot"),
                              style = "border:1px solid black; padding:10px; background-color:lightyellow;"
                              ),
                       column(
                         width = 5,
                         conditionalPanel(
                           condition = "output.extendedBoxPlot == null",
                           p("Please finish the Data Preparation first."),
                           style = "padding:10px; background-color:lightyellow;"),
                         uiOutput("summaryTable"),
                         style = "border:1px solid black; padding:10px; background-color:lightyellow;"
                       )
                     ),
                     
                     # Detailed explanation for the box plot
                     h4("Interpretation of Box Plot"),
                     p("The box plot visualizes the distribution of the selected variable(s) across different categories, such as political groups (EPGs) or countries. 
         Each box represents the spread of data for one category, showing the median, interquartile range, and potential outliers. 
         This helps in identifying which groups have more homogeneous voting behavior or attendance levels and which ones display greater variation."),
                     p("For instance, a narrow box with minimal outliers indicates that a group's members tend to behave similarly on the selected measure, 
         while a wider box or multiple outliers may suggest diverse voting patterns within that group."),
                     
                     br(),
                     hr(),
                     br(),
                     
                     # Display tables and maps with added explanations
                     fluidRow(
                       column(
                         width = 7, 
                         br(),
                         h4("Country Map"),
                         p("The map visualizes the selected measure (such as attendance or voting alignment) by country. 
             This can help you observe geographical patterns in the data, such as higher engagement in certain regions or countries with MEPs 
             who tend to vote together."),
                         plotOutput("countryMapPlot"), 
                         br(), 
                         style = "border:1px solid black; padding:10px; background-color:lightyellow;"
                       ),
                       column(
                         width = 5, 
                         br(),
                         h4("Summary Table"),
                         p("This table provides a summary of the selected variable(s) for each political group or country. Use this table to identify differences across groups, 
             such as variations in average attendance scores or voting alignments. Sorting by mean can be useful for highlighting key trends and outliers."),
                         reactableOutput("tabletop"), 
                         br(), 
                         style = "border:1px solid black; padding:10px; background-color:lightyellow;"
                       )

                     ),
                     br()


                     
                     # Sidebar panel for politician details and parliament selection
                   )
                 ),
                 hr(),
                 tags$style(".glyphicon-user {color:#E87722}"),
                 h3(p(em("Parliament View"),icon("user",lib = "glyphicon"),style="color:black;text-align:center")),
                 br(),
                 sidebarLayout(

                   sidebarPanel(
                     h4("Politician Details"),
                     uiOutput("politicianDetails"),
                     hr(),
                     shinyBS::bsCollapse(
                       id = "filtersAccordion",  # Unique ID for the collapse panel
                       open = "Filter Politicians",  # Default open panel
                       shinyBS::bsCollapsePanel(
                         title = "Filter Politicians",
                         h4("Demographics"),
                         uiOutput("ageSliderUI"),
                         uiOutput("genderCheckboxUI"),
                         uiOutput("countrySelectUI"),
                         hr(),
                         h4("Performance"),
                         uiOutput("activitySliderUI"),
                         actionButton("resetFilters", "Reset Filters"),
                         hr(),
                         verbatimTextOutput("filterSummary")
                       )
                     ),
                     width = 3
                   ),
               
                 
                   
                     
                     # Main panel for displaying the parliament plot
                     mainPanel(
                       conditionalPanel(
                         condition = "output.extendedBoxPlot == null",
                         p("Please finish the Data Preparation first."),
                         style = "padding:10px; background-color:lightyellow;"),
                       plotlyOutput("parliamentPlot")  # Parliament plot takes up the main panel
                     )
                     
                     
                     
                   )
               ),
               
               
               
               tabPanel("Step 4: Clustering",
                        
                 
                        fluidRow(
                          column(width = 2),
                          column(
                            h4(p("Dimension Reduction Techniques - From General to Specific", style = "color:black;text-align:center")),
                            width = 8, style = "background-color:lavender;border-radius: 10px"
                          )
                        ),
                        br(),
                        
                        fluidRow(
                          column(width = 2),  # Empty column for spacing
                          column(
                            p("This flowchart outlines the process from data collection to analysis, highlighting the importance of dimensionality reduction. 
       High-dimensional data, such as voting records across multiple issues, poses challenges that make dimensionality reduction an essential step.",
                              style = "color:black;text-align:justify"),
                            
                            a(
                              tags$img(src = "pipeline5.png", width = "100%", height = "auto"),
                              target = "_blank"
                            ),
                            
                            p("Dimensionality reduction is necessary for several reasons:", style = "color:black;text-align:justify"),
                            tags$ul(
                              tags$li(tags$b("Visualization:"), " Reduces the complexity of high-dimensional data to enable easy visualization in two or three dimensions."),
                              tags$li(tags$b("Pattern Discovery:"), " Simplifies data to highlight underlying patterns that may be hidden in higher dimensions."),
                              tags$li(tags$b("Computational Efficiency:"), " Reduces the computational cost of clustering and other algorithms by simplifying the input data."),
                              tags$li(tags$b("Noise Reduction:"), " Focuses on the most informative dimensions, removing less relevant variability.")
                            ),
                            
                            p("This tab focuses on reducing the dimensions of voting data to identify clusters of politicians with similar voting behavior. 
       The scatterplot of raw data illustrates the difficulty of interpreting patterns in the original dimensions. The MCA scatterplot demonstrates how dimensionality reduction 
       simplifies and clarifies these patterns, enabling better identification of clusters.",
                              style = "color:black;text-align:justify; background-color:lightyellow; padding:10px"),
                            
                            width = 8, style = "background-color:lavender;border-radius: 10px; padding:15px"
                          ),
                          column(width = 2)  # Empty column for spacing
                        ),
                        
                        
                        
                        br(),
                        
                        fluidRow(
                          column(width = 2, icon("hand-point-right", "fa-5x"), align = "center"),
                          column(
                            width = 8,
                            h4("Dimensionality Reduction Techniques", style = "color:black;text-align:center"),
                            
                            p("In this step, we employ three different dimensionality reduction techniques—UMAP, MCA, and DW-NOMINATE—to progressively reveal patterns in the voting data. 
       Each method has distinct strengths, and the process moves from a broad representation to a more targeted political spectrum approach. These mappings help us 
       visualize latent voting patterns within the European Parliament.",
                              style = "color:black;text-align:justify"),
                            
                            br(),
                            
                            # DW-NOMINATE Section
                            h4("1. DW-NOMINATE - A Political Spectrum Analysis", style = "color:black"),
                            p("DW-NOMINATE is a widely used method in political science to map representatives onto a multidimensional ideological spectrum. 
       It provides a detailed understanding of MEP alignments by placing them within an ideological context based on their voting behavior. 
       This approach reveals ideological trends and bloc formations within the Parliament, helping us go beyond general clusters to locate each MEP’s position 
       on specific political dimensions.",
                              style = "color:black;text-align:justify;background-color:lightyellow;padding:10px;border:1px solid black;border-radius:5px"),
                            
                            br(),
                            
                            # MCA Section
                            h4("2. MCA - A Categorical Perspective", style = "color:black"),
                            p("Multiple Correspondence Analysis (MCA) refines our understanding by focusing on relationships between categorical voting behaviors. 
       This method is particularly helpful for data structured around discrete votes, as it identifies nuanced alignments between MEPs based on their voting responses. 
       MCA creates a focused clustering structure tailored to parliamentary voting patterns.",
                              style = "color:black;text-align:justify;background-color:lightyellow;padding:10px;border:1px solid black;border-radius:5px"),
                            
                            br(),
                            
                            # UMAP Section
                            h4("3. UMAP - An Overview", style = "color:black"),
                            p("UMAP (Uniform Manifold Approximation and Projection) is a flexible tool for reducing high-dimensional data into a simpler, lower-dimensional space. 
       It provides an initial view into possible voting blocs within the Parliament, creating a visual representation of MEP voting data that highlights clusters 
       without predefining any structure.",
                              style = "color:black;text-align:justify;background-color:lightyellow;padding:10px;border:1px solid black;border-radius:5px"),
                            
                            style = "background-color:papayawhip;border-radius:10px;padding:15px"
                          ),
                          
                          # Add the arrow in a separate column
                          column(
                            width = 2,
                            align = "center",
                            div(
                              br(),
                              br(),
                              br(),
                              p(strong("More Specific"), style = "color:black; text-align:center; font-size: 12px; margin-bottom:5px"),
                              tags$img(src = "arrow.png", width = "110px", height = "auto"),
                              p(strong("More General"), style = "color:black; text-align:center; font-size: 12px; margin-top:5px")
                            )
                          )
                        ),
                        br(),
                        
                   
                        
                        # 1. Panel: Clustering Methoden
                        tabsetPanel(
                          tabPanel("Mapping Methods",
                                   br(),
                                   
                                   fluidRow(column(width = 2),
                                            column(
                                              h4(p("Decision for a dimension reduction technique", style = "color:black;text-align:center")),
                                              width = 8, style = "background-color:lavender; border-radius: 10px")),
                                   br(),
                                   
                                   sidebarLayout(
                                     sidebarPanel(
                                       
                                       h4("Settings"),
                                       hr(),
                 
                                       # Legislature Selection
                                       selectInput("selectedP", 
                                                   p("Select Parliament:", style = "color:black; text-align:center; font-weight:bold;"),
                                                   choices = list(
                                                     "6th: 2004 - 2009" = "P6",
                                                     "7th: 2009 - 2014" = "P7",
                                                     "8th: 2014 - 2019" = "P8",
                                                     "9th: 2019 - 2024" = "P9"
                                                   ),
                                                   selected = "P7"),
                                       bsTooltip("selectedP", 
                                                 "Select the legislature you want to analyze from the dropdown menu.",
                                                 "right"),
                                       
                                       # Final Vote Checkbox
                                       checkboxInput("use_final_votes_only", 
                                                     p("Use Only Final Votes", style = "font-weight:bold; color:black;"), 
                                                     value = FALSE),
                                       bsTooltip("use_final_votes_only", 
                                                 "Check this option to analyze only the final votes instead of intermediate votes.",
                                                 "right"),
                                       checkboxInput("color_switch", "Without coloring", FALSE), # Switch für Farben
                                       
                                       hr(),
                                       
                                       # Collapsible Panels for Advanced Settings
                                       shinyBS::bsCollapse(
                                         id = "advanced_settings",
                                         open = "DWNOMINATE Settings",  # Keeps the first section open by default
                                         
                                         shinyBS::bsCollapsePanel(
                                           title = "DWNOMINATE Settings",
                                           actionButton("runDWNom", "Run DWNOMINATE"),
                                           bsTooltip("runDWNom", 
                                                     "Run the DWNOMINATE algorithm for dimensionality reduction.",
                                                     "right")
                                         ),
                                         
                                         shinyBS::bsCollapsePanel(
                                           title = "MCA Settings",
                                           sliderInput("absenceThreshold", 
                                                       "Minimum Attendance (% of absolute votes):", 
                                                       min = 0, max = 100, value = 80),
                                           bsTooltip("absenceThreshold", 
                                                     "Set the minimum attendance percentage for votes to be included in the analysis.", 
                                                     "right"),

                                           sliderInput("mca_ncp", "Number of Components", value = 2, min = 2, max = 10),
                                           bsTooltip("mca_ncp", 
                                                     "Choose the number of components for MCA.",
                                                     "right"),
                                           
                                           actionButton("runMCA", "Run MCA"),
                                           bsTooltip("runMCA", 
                                                     "Run the MCA algorithm for dimensionality reduction.",
                                                     "right"),

                                           hr(),
                                           downloadButton("downloadContrib", "Download Variable Contributions"),
                                           br(),
                                           br(),
                                           tableOutput("topicContributions")
                                         ),
                                         
                                         shinyBS::bsCollapsePanel(
                                           title = "UMAP Settings",
                                           sliderInput("absenceThreshold_umap", 
                                                       "Minimum Attendance (% of absolute votes):", 
                                                       min = 0, max = 100, value = 80),
                                           bsTooltip("absenceThreshold_umap", 
                                                     "Set the minimum attendance percentage for votes to be included in the analysis.", 
                                                     "right"),
                                           
                                           sliderInput("umap_n_neighbors", "Number of Neighbors", min = 5, max = 100, value = 40, step = 5),
                                           bsTooltip("umap_n_neighbors", 
                                                     "Adjust the number of neighbors for UMAP (local connectivity).",
                                                     "right"),
                                           
                                           sliderInput("umap_min_dist", "Minimum Distance", min = 0.0, max = 1.0, value = 0.5, step = 0.1),
                                           bsTooltip("umap_min_dist", 
                                                     "Adjust the minimum distance for UMAP (clustering tightness).",
                                                     "right"),
                                           
                                           sliderInput("umap_spread", "Spread", min = 0.1, max = 3.0, value = 1, step = 0.1),
                                           bsTooltip("umap_spread", 
                                                     "Adjust the spread parameter for UMAP (distribution of points).",
                                                     "right"),
                                           
                                           numericInput("umap_n_components", "Number of Components", value = 2, min = 2, max = 10),
                                           bsTooltip("umap_n_components", 
                                                     "Choose the number of dimensions for UMAP output.",
                                                     "right"),
                                           
                                           actionButton("runUMAP", "Run UMAP"),
                                           bsTooltip("runUMAP", 
                                                     "Run the UMAP algorithm for dimensionality reduction.",
                                                     "right")
                                         )
                                       ),
                                       
                                       br(),
                                       br()
                                     )
                                     ,
                                     

                                     mainPanel(
                                       h3(p('Mapping Output', style = "color:salmon; text-align:center")),
                                       hr(),
                                       
                                       # Explanatory Text with Arrow
                                       tags$div(
                                         style = "text-align:right; margin-bottom:15px;",
                                         "Use the checkboxes in this column to select a mapping method for clustering.",
                                         tags$div(
                                           style = "position:relative; display:inline-block; text-align:right;",
                                           tags$span(
                                             style = "font-size:24px; color:gray; margin-top:-10px;",
                                             "⬇"  # Unicode down arrow
                                           )
                                         )
                                       ),
                                       
                                       # DW-NOMINATE Plot Section
                                       fluidRow(
                                         class = "dwnom_plot_row",  # Add the class for highlighting
                                         column(
                                           width = 10,
                                           uiOutput("dwNomPlotUI")
                                         ),
                                         column(
                                           width = 2,
                                           checkboxInput("select_dwnom", label = NULL, value = FALSE, width = "100%"),
                                           uiOutput("icon_dwnom")  # Placeholder for icon
                                         ), style = "border:1px solid black"
                                       ),
                                       hr(),
                                       
                                       # MCA Plot Section
                                       fluidRow(
                                         class = "mca_plot_row",  # Add the class for highlighting
                                         column(
                                           width = 10,
                                           uiOutput("mcaPlotUI")
                                         ),
                                         column(
                                           width = 2,
                                           checkboxInput("select_mca", label = NULL, value = FALSE, width = "100%"),
                                           uiOutput("icon_mca")  # Placeholder for icon
                                         ), style = "border:1px solid black"
                                       ),
                                       hr(),
                                       
                                       # UMAP Plot Section
                                       fluidRow(
                                         class = "umap_plot_row",  # Add the class for highlighting
                                         column(
                                           width = 10,
                                           uiOutput("umapPlotUI")
                                         ),
                                         column(
                                           width = 2,
                                           checkboxInput("select_umap", label = NULL, value = FALSE, width = "100%"),
                                           uiOutput("icon_umap")  # Placeholder for icon
                                         ), style = "border:1px solid black"
                                       ),
                                       br(),

                                       br()
                                     )
                                     
                                     
                                     
                                   )
                          
                          
                          ),
                          
                          
                          tabPanel("Clustering Methods",
                                   
                                   br(),
                                   
                                   fluidRow(column(width = 2),
                                            column(
                                              h4(p("Clustering Methods", style = "color:black;text-align:center")),
                                              width = 8, style = "background-color:lavender; border-radius: 10px")
                                   ),
                                   
                                   br(),
                                   
                                   sidebarLayout(
                                     sidebarPanel(
                                       
                                       h4("Clustering Methods"),
                                       p("Choose a clustering method and adjust its settings. Each method offers a unique way to group MEPs based on voting patterns."),
                                       
                                       tabsetPanel(
                                         tabPanel("K-Means",
                                                  sliderInput("kmeans_k", "Number of Clusters (k):", min = 2, max = 10, value = 6),
                                                  actionButton("runKMeans", "Run K-Means", class = "btn-success"),
                                                  hr(),
                                                  h4("Optimal Clustering (Elbow Method)"),
                                                  plotOutput("elbowPlotK")
                                         ),
                                         tabPanel("PAM",
                                                  sliderInput("pam_k", "Number of Clusters (k):", min = 2, max = 10, value = 6),
                                                  actionButton("runPAM", "Run PAM", class = "btn-success"),
                                                  hr(),
                                                  h4("Optimal Clustering (Elbow Method)"),
                                                  plotOutput("elbowPlotPAM")
                                                  
                                         ),
                                         tabPanel("HDBSCAN",
                                                  sliderInput("hdbscan_minPts", "Minimum Points for Cluster:", min = 5, max = 50, value = 20),
                                                  actionButton("runHDBSCAN", "Run HDBSCAN", class = "btn-success")
                                         )
                                       )
                                     ),
                                     
                                     mainPanel(
                                       h3(p('Clustering Output', style = "color:salmon; text-align:center")),
                                       
                                       h4("Try different clustering techniques"),
                                       p("Visualize the clusters formed by each method in the UMAP-reduced space. The left plot shows the initial dimension reduction, while the right plot displays the cluster assignments. Adjust clustering parameters to explore different groupings."),
                                       
                                       fluidRow(
                                         column(width = 6, plotOutput("finalClusterPlot")),
                                         column(width = 6,                               conditionalPanel(
                                           condition = "output.clusterPlot == null",
                                           p("Please run a clustering method first."),
                                           style = "padding:10px; background-color:lightyellow;"), plotOutput("clusterPlot")),
                                         style = "border:1px solid black"
                                       ),
                                       br(),
                                       downloadButton("downloadPlots", "Download plots (PDF)"),
                                       br(),
                                       br()

                                       
                                     )
                                   )
                          ),
                          
                          
                          # UI-Teil
                          tabPanel(
                            "Cluster Metrics and Stability",
                            br(),
                            
                            fluidRow(column(width = 2),
                                     column(
                                       h4(p("Compare the metrics for different clustering settings", style = "color:black;text-align:center")),
                                       width = 8, style = "background-color:lavender; border-radius: 10px")
                            ),
                            br(),
                            sidebarLayout(
                              sidebarPanel(
                                h4("Choose Metrics"),
                                checkboxInput("enable_silhouette", "Show Silhouette Score", value = TRUE),
                                checkboxInput("enable_dbindex", "Show Davies-Bouldin Index", value = TRUE),
                                checkboxInput("enable_chindex", "Show Calinski-Harabasz Index", value = TRUE),
                                checkboxInput("ignore_first_cluster", "(Only HDBSCAN) Disregard Cluster 0 (Outliers)", value = TRUE),
                                actionButton("runMetrics", "Calculate Metrics",  class = "btn-success"),
                                br(),
                                hr(),
                                downloadButton("downloadData", "Download Clustered Data"), width = 3
                              ),
                              

                              
                              mainPanel(
                                h3("Cluster Evaluation Metrics", style = "color:salmon; text-align:center"),
                                # Explanatory Text
                                p("The following metrics help evaluate the quality of your clustering results. A combination of these metrics provides a comprehensive assessment."),
                                
                                  fluidRow(
                                    column(br(), bsTooltip("silhouettePlot", "Measures how similar an object is to its own cluster compared to other clusters."), plotOutput("silhouettePlot"),br(),width=4,style="border:1px solid black"),
                                    column(br(), bsTooltip("dbIndexPlot", "Evaluates intra-cluster similarity and inter-cluster differences. Lower values are better."), plotOutput("dbIndexPlot"),br(),width=4,style="border: 1px solid black;border-left: none"),
                                    column(br(), bsTooltip("chIndexPlot", "Assesses cluster separation and compactness. Higher values are better."), plotOutput("chIndexPlot"),br(),width=4,style="border:1px solid black;border-left:none")
                                    
                                  ),
                              
                                fluidRow(
                                  # Silhouette Plot and Description
                                    column(
                                      width = 4,
                                      h4("Silhouette Score"),
                                      bsTooltip("silhouettePlot", "Measures how similar an object is to its own cluster compared to other clusters."),
                                      p("Interpretation: Values close to 1 indicate that the sample is appropriately clustered. Values around 0 suggest that the sample is on the border between clusters. Negative values indicate misclassification.")
                                    ),
                                  # Davies-Bouldin Index and Description

                                    column(
                                      width = 4,
                                      h4("Davies-Bouldin Index"),
                                      bsTooltip("dbIndexPlot", "Evaluates intra-cluster similarity and inter-cluster differences. Lower values are better."),
                                      p("Interpretation: Lower values indicate better clustering. A value close to 0 suggests well-separated clusters.")
                                  ),
                                    column(
                                      width = 4,
                                      h4("Calinski-Harabasz Index"),
                                      bsTooltip("chIndexPlot", "Assesses cluster separation and compactness. Higher values are better."),
                                      p("Interpretation: Higher values indicate better-defined clusters.")
                                    )
                                  )
                                , width = 9
                              )
                            ),
                            
                            hr(),
                            tags$style(".glyphicon-file {color:#E87722}"),
                            h3(p(em("Further Analysis"),icon("file",lib = "glyphicon"),style="color:black;text-align:center")),
                            br(),
                            sidebarLayout(
                              
                              
                              # Sidebar panel for politician details and parliament selection
                              sidebarPanel(
                                h4("Choose Metrics"),
                                checkboxInput("enable_elbow", "Show Elbow Method", value = TRUE),
                                checkboxInput("enable_bootstrap", "Show Cluster Stability (Bootstrapping)", value = TRUE),
                                numericInput("num_bootstrap", "Number of Bootstrap Samples:", value = 100, min = 10, max = 200, step = 10),
                                width = 3  # Adjust the width to make the sidebar smaller
                              ),
                              
                              # Main panel for displaying the parliament plot
                              mainPanel(
                                fluidRow(
                                  

                                  conditionalPanel(
                                    condition = "input.enable_elbow",
                                    column(
                                      width = 6,
                                      bsTooltip("elbowPlot", "Helps to determine the optimal number of clusters by finding the 'elbow point' where adding more clusters doesn't significantly improve the model."),
                                      plotOutput("elbowPlot"),
                                      p("Interpretation: Look for a point where the rate of decrease sharply changes ('elbow'). The number of clusters at this point is considered optimal.")
                                    )
                                  ),
                                  conditionalPanel(
                                    condition = "input.enable_bootstrap",
                                    column(
                                      width = 6,
                                      bsTooltip("stabilityPlot", "Assesses the stability of clusters by resampling the data multiple times."),
                                      plotOutput("stabilityPlot"),
                                      p("Interpretation: Higher values indicate more stable clusters. The plot shows the stability of clusters across different numbers of clusters.")
                                    )
                                  ),
                                  
                                ), width = 9
                              )
                              
                              
                              
                            )
                          )
                          
                          
                        )
                        
               ),
               
               
               
               # UI code for the "Results" tab
               tabPanel(
                 "Step 5: Results",
                 tags$div(
                   class = "results-tab",
                   tags$style(HTML("
      .results-tab {
        font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif; color: #333;
      }
      .results-tab h1, .results-tab h2, .results-tab h3, .results-tab h4, .results-tab h5, .results-tab h6 {
        font-weight: bold; color: #222;
      }
      .results-tab .subtitle {
        font-size: 18px; color: #666; margin-bottom: 20px;
      }
      .results-tab .byline {
        font-size: 14px; color: #999; margin-top: -10px; margin-bottom: 30px;
      }
      .results-tab .divider {
        border-bottom: 1px solid #e2e2e2; margin: 40px 0;
      }
      .results-tab .epg-button {
        margin: 5px;
      }
      .results-tab .cluster-plot {
        margin-bottom: 30px;
      }
    ")),
                   
                   # Conditional display of the placeholder or results
                   uiOutput("resultsContent")
                 )
               ),
               
               

               tabPanel(icon("book"),
                        
                        # Header with introduction
                        fluidRow(
                          column(width = 2),
                          column(
                            h4(p("Key Terms and References", style = "color:black;text-align:center")),
                            p("Here you’ll find a handy glossary of key terms and concepts, along with a list of references that helped shape this thesis. 
     Think of this section as your go-to guide for understanding the ideas, methods, and insights shared throughout the work.",
                              style = "color:black;text-align:justify"),
                            width = 8, 
                            style = "background-color:#BFF7BB;border-radius: 10px"
                          )
                          
                        ),
                        
                        br(),
                        hr(),
                        navlistPanel(widths=c(2,10),
                                     tabPanel(span(icon("book"), " References"),
                                              
                                              
                                              column(
                                                width = 12,
                                                h4("References", style = "color:black;"),
                                                p(
                                                  "Below is the list of references used throughout the thesis. These references are cited in appropriate sections to provide academic credibility and 
     to acknowledge the authors whose work has contributed to this project.",
                                                  style = "color:black; text-align:justify; margin-bottom:15px;"
                                                ),
                                                tags$ul(
                                                  tags$li(
                                                    "Hix, Simon, Noury, Abdul, and Roland, Gerard. (2005). Dimensions of Politics in the European Parliament. See ",
                                                    tags$a(href = "https://escholarship.org/uc/item/4gb278j5", target = "_blank", "details here.")
                                                  ),
                                                  tags$li(
                                                    "Parltrack.eu: Biographical Data on Members of the European Parliament. Visit ",
                                                    tags$a(href = "https://parltrack.eu/about", target = "_blank", "Parltrack.eu")
                                                  ),
                                                  tags$li(
                                                    "DW-NOMINATE method documentation and applications in legislative studies. Learn more ",
                                                    tags$a(href = "https://voteview.com/about", target = "_blank", "about DW-NOMINATE.")
                                                  ),
                                                  tags$li(
                                                    "UMAP (Uniform Manifold Approximation and Projection) for dimension reduction in clustering analysis. Details available ",
                                                    tags$a(href = "https://arxiv.org/abs/1802.03426", target = "_blank", "here.")
                                                  ),
                                                  tags$li(
                                                    "HDBSCAN clustering methodology and its applications in density-based clustering. Refer to ",
                                                    tags$a(href = "https://hdbscan.readthedocs.io/en/latest/index.html", target = "_blank", "the official documentation.")
                                                  ),
                                                  tags$li(
                                                    "FactoMineR: Statistical Methods for MCA and PCA analysis. Visit ",
                                                    tags$a(href = "http://factominer.free.fr/", target = "_blank", "FactoMineR.")
                                                  ),
                                                  tags$li(
                                                    "Flagpedia for country flag icons. Check ",
                                                    tags$a(href = "https://flagpedia.net/", target = "_blank", "Flagpedia.")
                                                  ),
                                                  tags$li(
                                                    "ggiraph and Shiny for interactive visualizations in R. Documentation available ",
                                                    tags$a(href = "https://davidgohel.github.io/ggiraph/", target = "_blank", "here.")
                                                  ),
                                                  tags$li(
                                                    "Shiny inspired by Didactic modeling process: Linear regression Oscar Daniel Rivera Baena. ",
                                                    tags$a(href = "https://shiny.posit.co/r/gallery/education/didacting-modeling/", target = "_blank", "View here.")
                                                  )
                                                ),
                                                br(),
                                                style = "border: 10px solid transparent;border-image: url(border4.png) 30 round"
                                              )
                                              
                                              
                                     ),
                        

                                     tabPanel(span(icon("file-alt"), "Glossary"),
                                              
                                              column(
                                                width = 12,
                                                h3("Glossary"),
                                                br(),
                                                
                                                # Clustering Terms
                                                p(strong("Cluster:"), "A group of observations that are similar to each other based on a defined set of features. Clustering aims to partition the data into subsets with high internal similarity and high external dissimilarity."),
                                                br(),
                                                p(strong("HDBSCAN (Hierarchical Density-Based Spatial Clustering of Applications with Noise):"), 
                                                  "An advanced clustering algorithm that identifies clusters of varying density in the data. It also designates outliers, often as Cluster 0, which are considered noise."),
                                                br(),
                                                p(strong("K-means Clustering:"), 
                                                  "A partitioning algorithm that divides data into a pre-defined number of clusters by minimizing within-cluster variance."),
                                                br(),
                                                p(strong("PAM (Partitioning Around Medoids):"), 
                                                  "A clustering method that selects medoids as representative points and minimizes the sum of dissimilarities between each data point and its nearest medoid."),
                                                br(),
                                                
                                                # Dimensionality Reduction Terms
                                                p(strong("UMAP (Uniform Manifold Approximation and Projection):"), 
                                                  "A dimensionality reduction technique that preserves the local and global structure of the data while reducing it to 2D or 3D for visualization."),
                                                br(),
                                                p(strong("MCA (Multiple Correspondence Analysis):"), 
                                                  "A method for reducing the dimensionality of categorical data by representing it in a lower-dimensional space."),
                                                br(),
                                                p(strong("DW-NOMINATE:"), 
                                                  "A scaling method commonly used in political science to map voting behavior onto ideological dimensions, such as economic and social dimensions."),
                                                br(),
                                                
                                                # Evaluation Metrics
                                                p(strong("Silhouette Score:"), 
                                                  "A measure of how well each data point fits within its cluster. A higher score indicates better-defined clusters."),
                                                br(),
                                                p(strong("Davies-Bouldin Index:"), 
                                                  "A metric for evaluating clustering quality by measuring the ratio of within-cluster spread to between-cluster separation. Lower values indicate better clustering."),
                                                br(),
                                                p(strong("Calinski-Harabasz Index:"), 
                                                  "An index that evaluates clustering performance by comparing the dispersion of points within clusters to the dispersion between clusters. Higher scores are better."),
                                                br(),
                                                
                                                # Data Preparation Terms
                                                p(strong("Normalization:"), 
                                                  "A process to scale the data so that each feature contributes equally to the analysis, typically by adjusting the values to a standard range or scale."),
                                                br(),
                                                p(strong("Feature Engineering:"), 
                                                  "The process of selecting, transforming, and creating features to improve the performance of machine learning models."),
                                                br(),
                                                p(strong("Complete Case Analysis:"), 
                                                  "A method for handling missing data by excluding observations with any missing values."),
                                                br(),
                                                
                                                # Visualization Terms
                                                p(strong("Radar Chart:"), 
                                                  "A graphical method of displaying multivariate data in the form of a two-dimensional chart of three or more quantitative variables represented on axes starting from the same point."),
                                                br(),
                                                p(strong("Box Plot:"), 
                                                  "A visualization technique for summarizing the distribution of a dataset and identifying potential outliers."),
                                                br(),
                                                p(strong("Interactive Parliament Plot:"), 
                                                  "A custom visualization that maps Members of the European Parliament (MEPs) and their respective parties onto a semicircular plot with clickable interactivity."),
                                                br(),
                                                
                                                # Statistical Concepts
                                                p(strong("Correlation Coefficient:"), 
                                                  "A measure of the strength and direction of the linear relationship between two variables, ranging from -1 (perfect negative correlation) to +1 (perfect positive correlation)."),
                                                br(),
                                                p(strong("R-squared (Coefficient of Determination):"), 
                                                  "Represents the proportion of variance in the dependent variable that is predictable from the independent variables."),
                                                br(),
                                                p(strong("Adjusted R-squared:"), 
                                                  "A modified version of R-squared that adjusts for the number of predictors in the model, penalizing the inclusion of irrelevant variables."),
                                                br(),
                                                
                                                # Political Terms
                                                p(strong("EPG (European Political Group):"), 
                                                  "The official grouping of Members of the European Parliament based on shared political ideologies."),
                                                br(),
                                                p(strong("MEP (Member of the European Parliament):"), 
                                                  "An elected representative in the European Parliament."),
                                                br(),
                                                p(strong("Voting Alignment:"), 
                                                  "A measure of how frequently MEPs or parties vote in line with one another."),
                                                br(),
                                                
                                                # Software and Tools
                                                p(strong("Shiny:"), 
                                                  "An R framework for building interactive web applications and dashboards for data analysis and visualization."),
                                                br(),
                                                p(strong("reactable:"), 
                                                  "A package in R for creating interactive and customizable tables with advanced features."),
                                                br(),
                                                p(strong("ggplot2:"), 
                                                  "A widely used R package for creating elegant and versatile visualizations."),
                                                br(),
                                                
                                                # Other Key Terms
                                                p(strong("Cluster Stability:"), 
                                                  "The extent to which clusters remain consistent across different sampling or parameter settings, often tested through bootstrapping."),
                                                br(),
                                                p(strong("Outliers:"), 
                                                  "Data points that deviate significantly from the majority of observations, often identified and excluded in clustering."),
                                                br(),
                                                p(strong("Bootstrap Analysis:"), 
                                                  "A resampling method used to estimate the stability of clusters by creating multiple datasets through random sampling with replacement."),
                                                br(),
                                                
                                                style = "border: 10px solid transparent; border-image: url(border.png) 30 round"
                                              )
                                              
                                              
                                              ))
                        
                        )
               
               )
  

))
