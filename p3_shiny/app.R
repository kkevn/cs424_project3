######################################################
# CS 424 | Project 3
# Kevin Kowalski - kkowal28
# Samuel Kajah - skajah2
# Vijay Vemu - vvemu3
######################################################
#
# < insert project info and notes here >
#
######################################################

# import libraries
library(comprehenr)
library(dplyr)
library(DT)
library(ggplot2)
library(hashmap)
library(lubridate)
library(readr)
library(shiny)
library(shinydashboard)
library(stringr)
library(tidyverse)

# retrieve helper functions
source("helper.R")

# read in the processed RDS file and subsetted keywords CSV
data <- readRDS(file = "combined_data.rds")
keywords_subset = read.csv('keywords_subset.csv')

########################## DATA NEEDED FOR PLOTTING #####################################
########################## DATA NEEDED FOR PLOTTING #####################################

# output number of entries in data
total_data <- count(data)
print(paste0("---- data, n=", total_data))
unique_films <- length(unique(data$movie))
print(paste0("---- unique movies, n=", unique_films))

# get list of movies where none is duplicated (used for times where you only want to account for a movie once, such as runtime average)
unique_movies <- subset(data, !duplicated(subset(data, select = movie)))

# get min and max years
min_year_all <- min(data$year)
max_year_all <- max(data$year)
print(paste0("---- years range from: ", min_year_all, "-", max_year_all))

# get min and max decades
min_decade_all <- floor(min(data$year) / 10) * 10
max_decade_all <- floor(max(data$year) / 10) * 10
print(paste0("---- decades range from: ", min_decade_all, "-", max_decade_all))

# get min and max runtimes
min_runtime_all <- min(data$runtime)
max_runtime_all <- max(data$runtime)
print(paste0("---- runtimes range from: ", min_runtime_all, "-", max_runtime_all, " minutes"))

# load upper and lower limit variables into helper file
load_data(min_year_all, max_year_all, min_decade_all, max_decade_all, min_runtime_all, max_runtime_all)

# get a count of movies & distribution of things for the entire data (of just unique movies)
by_year <- number_films_per_year(unique_movies)
#by_decade <- number_films_per_decade(unique_movies) # takes a minute to calculate decades
by_month <- number_films_per_month(unique_movies)
by_runtime <- distribution_of_runtimes(unique_movies)
by_certificates <- distribution_of_certificates(unique_movies)
by_genre <- distribution_of_genres(unique_movies)
n = 10
by_keywords <- distribution_of_keywords(keywords_subset, n)

# get the averages for films per year, month, and runtime of entire (unique) data
unique_years = as.numeric(count(by_year))
avg_per_year = trunc(unique_films / unique_years)
print(paste0("---- avg films per year= ", avg_per_year))

unique_months = as.numeric(count(by_month))
avg_by_month <- by_month
avg_by_month$count <- by_month$count / unique_years
names(avg_by_month)[2] <- "average"
avg_per_month = trunc(as.numeric(sum(avg_by_month$average) / unique_months))
print(paste0("---- avg films per month= ", avg_per_month))

avg_runtime = trunc(mean(unique_movies$runtime))
print(paste0("---- avg runtime= ", avg_runtime))

########################## DASHBOARD #####################################
########################## DASHBOARD #####################################

ui = dashboardPage(skin = "yellow",
  
  dashboardHeader(title = "Project 3 - IMDB", titleWidth = 200),
  
  dashboardSidebar(disable = FALSE, collapsed = FALSE,
                   
                   sidebarMenu(
                     # add space to sidebar
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL)),
                   
                   # about button
                   actionButton("about_info", "About", width = 200),
                   
                   sidebarMenu(
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("Make Selections:", tabName = "cheapBlankSpace", icon = NULL)),

                   # year input
                   sliderInput(inputId = "input_year",
                               label = "Select a Year:",
                               min = min_year_all, max = max_year_all, value = max_year_all, sep = ""),

                   # decade input
                   sliderInput(inputId = "input_decade",
                               label = "Select a Decade:",
                               min = min_decade_all, max = max_decade_all, value = max_decade_all, step = 10, sep = "", post = "s"),

                   sidebarMenu(
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL)),

                   # top N input
                   numericInput(inputId = "input_top_n",
                                label = "Select Top N:",
                                min = 10, max = 56048, value = 10),

                   # keyword input
                   textInput(inputId = "input_keyword",
                             label = "Select a Keyword:",
                             value = "", placeholder = "comma separate for more"),

                   sidebarMenu(
                     menuItem("* leave blank for all keywords", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL)),

                   # genre input
                   selectInput(inputId = "input_genre",
                               label = "Select a Genre:",
                               choices = c("All genres", by_genre[1])),

                   # certificate input
                   selectInput(inputId = "input_certificate",
                               label = "Select a Certificate:",
                               choices = c("All certificates", by_certificates[1])),

                   sidebarMenu(
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL)),

                   # runtime input
                   sliderInput(inputId = "input_runtime",
                               label = "Select a Runtime:",
                               min = min_runtime_all, max = max_runtime_all, value = c(60, 120), step = 30, post = " minutes")
                   
  ), # end sidebarMenu
  
  dashboardBody(
    
    # makes gray background
    tags$head(tags$style(HTML('
                                /* logo */
                                .skin-blue .main-header .logo {
                                  background-color: #202020;
                                }
                                
                                .skin-blue .main-header .navbar {
                                  background-color: #202020;
                                }
                                             
                                .skin-blue .main-sidebar {
                                  background-color: #202020;
                                }
                                
                                .content-wrapper, .right-side {
                                  background-color: #202020;
                                }
    '))),
    
    column(4,
           box(title = "Overall Distribution of Films", width = 12, height = 800, solidHeader = TRUE, status = "warning", color = "yellow",

               tabsetPanel(
                 tabPanel("by Year",
                          tabsetPanel(
                            tabPanel("Bar Plot",
                                     tabPanel("Bar Plot", h1("bar plot here"))
                            ),
                            tabPanel("Tabular Format",
                                     tabPanel("Tabular Format", dataTableOutput("tbl_year"))
                            )
                          ) #Close inner tabsetPanel
                 ),
                 tabPanel("by Month",
                          tabsetPanel(
                            tabPanel("Bar Plot",
                                     tabPanel("Bar Plot", h1("bar plot here"))
                            ),
                            tabPanel("Tabular Format",
                                     tabPanel("Tabular Format", dataTableOutput("tbl_month"))
                            )
                          ) #Close inner tabsetPanel
                 ),
                 tabPanel("by Runtime",
                          tabsetPanel(
                            tabPanel("Bar Plot",
                                     tabPanel("Bar Plot", h1("bar plot here"))
                            ),
                            tabPanel("Tabular Format",
                                     tabPanel("Tabular Format", dataTableOutput("tbl_runtime"))
                            )
                          ) #Close inner tabsetPanel
                 ),
                 tabPanel("by Certification",
                          tabsetPanel(
                            tabPanel("Bar Plot",
                                     tabPanel("Bar Plot", h1("bar plot here"))
                            ),
                            tabPanel("Tabular Format",
                                     tabPanel("Tabular Format", dataTableOutput("tbl_certificates"))
                            )
                          ) #Close inner tabsetPanel
                 ),
                 tabPanel("by Genre",
                          tabsetPanel(
                            tabPanel("Bar Plot",
                                     tabPanel("Bar Plot", h1("bar plot here"))
                            ),
                            tabPanel("Tabular Format",
                                     tabPanel("Tabular Format", dataTableOutput("tbl_genres"))
                            )
                          ) #Close inner tabsetPanel
                 ),
                 tabPanel("by Top N Keywords",
                          tabsetPanel(
                            tabPanel("Bar Plot",
                                     tabPanel("Bar Plot", h1("bar plot here"))
                            ),
                            tabPanel("Tabular Format",
                                     tabPanel("Tabular Format", dataTableOutput("tbl_keywords"))
                            )
                          ) #Close inner tabsetPanel
                 )


               )
            )

        ),
    
    # sizing is weird on these boxes
    column(4, 
           fluidRow(
             infoBoxOutput("info_year", width = 5)
           ),
           fluidRow(
             infoBoxOutput("info_month", width = 5)
           ),
           fluidRow(
             infoBoxOutput("info_runtime", width = 5)
           ),
           fluidRow(
             infoBoxOutput("info_total", width = 5)
           )
    ),
    column(4,
           box(title = "Distribution of Films by Selected Genre(s)", width = 12, height = 800, solidHeader = TRUE, status = "warning", color = "yellow",

               tabsetPanel(
                 tabPanel("by Year",
                          tabsetPanel(
                            tabPanel("Bar Plot",
                                     tabPanel("Bar Plot", h1("bar plot here"))
                            ),
                            tabPanel("Tabular Format",
                                     tabPanel("Tabular Format", h1("table here"))
                            )
                          ) #Close inner tabsetPanel
                 ),
                 tabPanel("by Decade",
                          tabsetPanel(
                            tabPanel("Bar Plot",
                                     tabPanel("Bar Plot", h1("bar plot here"))
                            ),
                            tabPanel("Tabular Format",
                                     tabPanel("Tabular Format", h1("table here"))
                            )
                          ) #Close inner tabsetPanel
                 ),
                 tabPanel("by Month",
                          tabsetPanel(
                            tabPanel("Bar Plot",
                                     tabPanel("Bar Plot", h1("bar plot here"))
                            ),
                            tabPanel("Tabular Format",
                                     tabPanel("Tabular Format", h1("table here"))
                            )
                          ) #Close inner tabsetPanel
                 ),
                 tabPanel("by Year %",
                          tabsetPanel(
                            tabPanel("Bar Plot",
                                     tabPanel("Bar Plot", h1("bar plot here"))
                            ),
                            tabPanel("Tabular Format",
                                     tabPanel("Tabular Format", h1("table here"))
                            )
                          ) #Close inner tabsetPanel
                 ),
                 tabPanel("by Decade %",
                          tabsetPanel(
                            tabPanel("Bar Plot",
                                     tabPanel("Bar Plot", h1("bar plot here"))
                            ),
                            tabPanel("Tabular Format",
                                     tabPanel("Tabular Format", h1("table here"))
                            )
                          ) #Close inner tabsetPanel
                 ),
                 tabPanel("by Month %",
                          tabsetPanel(
                            tabPanel("Bar Plot",
                                     tabPanel("Bar Plot", h1("bar plot here"))
                            ),
                            tabPanel("Tabular Format",
                                     tabPanel("Tabular Format", h1("table here"))
                            )
                          ) #Close inner tabsetPanel
                 ),
                 tabPanel("by Runtime",
                          tabsetPanel(
                            tabPanel("Bar Plot",
                                     tabPanel("Bar Plot", h1("bar plot here"))
                            ),
                            tabPanel("Tabular Format",
                                     tabPanel("Tabular Format", h1("table here"))
                            )
                          ) #Close inner tabsetPanel
                 ),
                 tabPanel("by Certification",
                          tabsetPanel(
                            tabPanel("Bar Plot",
                                     tabPanel("Bar Plot", h1("bar plot here"))
                            ),
                            tabPanel("Tabular Format",
                                     tabPanel("Tabular Format", h1("table here"))
                            )
                          ) #Close inner tabsetPanel
                 ),
                 tabPanel("by Top N Keywords",
                          tabsetPanel(
                            tabPanel("Bar Plot",
                                     tabPanel("Bar Plot", h1("bar plot here"))
                            ),
                            tabPanel("Tabular Format",
                                     tabPanel("Tabular Format", h1("table here"))
                            )
                          ) #Close inner tabsetPanel
                 )


               )
           )

    ),
    
  ) # end dashboardBody
)# end dashboardPage


server = function(input, output, session) {
  
  observeEvent(input$about_info, {
    showModal(
      modalDialog(
        HTML(read_file("about.html")),
        easyClose = TRUE
      )
    ) # end showModal
  }) # end about info 
  
  ### COUNT OF FILMS BY YEAR BELOW
  output$tbl_year <- DT::renderDataTable({
    DT::datatable(by_year, options = list(dom = 'f, t, i, p, r', pageLength = 13), rownames = FALSE) %>%
      DT::formatStyle("year", fontSize = "125%") %>%
      DT::formatStyle("count", fontSize = "125%")
  })

  ### COUNT OF FILMS BY MONTH BELOW
  output$tbl_month <- DT::renderDataTable({
    DT::datatable(by_month, options = list(dom = 't, i', pageLength = 13), rownames = FALSE) %>%
      DT::formatStyle("month", fontSize = "125%") %>%
      DT::formatStyle("count", fontSize = "125%")
  })

  ### DISTRIBUTION OF RUNTIMES BELOW
  unique_runtimes = as.numeric(count(by_runtime))

  output$tbl_runtime <- DT::renderDataTable({
    DT::datatable(by_runtime, options = list(dom = 'f, t, i, p, r', pageLength = 13), rownames = FALSE) %>%
      DT::formatStyle("runtime", fontSize = "125%") %>%
      DT::formatStyle("count", fontSize = "125%")
  })

  ### DISTRIBUTION OF CERTIFICATES BELOW
  unique_certificates = as.numeric(count(by_certificates))

  output$tbl_certificates <- DT::renderDataTable({
    DT::datatable(by_certificates, options = list(dom = 't, i', pageLength = 13, order = list(list(1, 'desc'))), rownames = FALSE) %>%
      DT::formatStyle("rating", fontSize = "125%") %>%
      DT::formatStyle("count", fontSize = "125%")
  })

  ### DISTRIBUTION OF KEYWORDS BELOW
  #by_keywords <- distribution_of_keywords(keywords_subset, 10) # REPLACE 10 WITH REACTIVE OF input$input_top_n
  unique_keywords = as.numeric(count(by_keywords))

  output$tbl_keywords <- DT::renderDataTable({
    DT::datatable(by_keywords, options = list(dom = 'f, t, i, p, r', pageLength = 13), rownames = FALSE) %>%
      DT::formatStyle("keyword", fontSize = "125%") %>%
      DT::formatStyle("count", fontSize = "125%")
  })

  ### DISTRIBUTION OF GENRES BELOW
  unique_genres = as.numeric(count(by_genre))

  output$tbl_genres <- DT::renderDataTable({
    DT::datatable(by_genre, options = list(dom = 'f, t, i, p, r', pageLength = 13), rownames = FALSE) %>%
      DT::formatStyle("genre", fontSize = "125%") %>%
      DT::formatStyle("count", fontSize = "125%")
  })
  
  ### AVERAGES OF FILMS PER YEAR, MONTH, AND RUNTIME
  
  output$info_year <- renderInfoBox({
    infoBox(
      "Average/Year:", paste0(avg_per_year, " films"), icon = icon("fas fa-calendar"),
      color = "blue", fill = FALSE
    )
  })
  
  output$info_month <- renderInfoBox({
    infoBox(
      "Average/Month:", paste0(avg_per_month, " films"), icon = icon("far fa-calendar"),
      color = "green", fill = FALSE
    )
  })

  output$info_runtime <- renderInfoBox({
    infoBox(
      "Average Runtime:", paste0(avg_runtime, " minutes"), icon = icon("fas fa-hourglass-half"),
      color = "red", fill = FALSE
    )
  })

  ### TOTAL FILMS FOR CURRENT FILTERS (shows just overall data count now)
  output$info_total <- renderInfoBox({
    infoBox(
      "Total:", paste0(unique_films, " films"), icon = icon("fas fa-film"),
      color = "yellow", fill = TRUE
    )
  })
  
  ##
  ##
  ## B REQUIREMENTS BELOW
  ##
  ##
}

shinyApp(ui=ui, server=server)