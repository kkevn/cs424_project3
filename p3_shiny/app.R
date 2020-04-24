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

# associate movie names with year, month, genre for the keywords
movie_year_map = hashmap(unique_movies$movie, unique_movies$year)
movie_month_map = hashmap(unique_movies$movie, as.character(unique_movies$month))
movie_genre_map = hashmap(unique_movies$movie, as.character(unique_movies$genre))


keywords_subset$year = movie_year_map[[keywords_subset$movie]]
keywords_subset$month = movie_month_map[[keywords_subset$movie]]
keywords_subset$genre = movie_genre_map[[keywords_subset$movie]]


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
print('Getting by Year')
by_year <- number_films_per_year(unique_movies)
print('Getting by Decade')
by_decade <- number_films_per_decade(unique_movies) # takes a minute to calculate decades
print('Getting by Month')
by_month <- number_films_per_month(unique_movies)
print('Getting by Runtime')
by_runtime <- distribution_of_runtimes(unique_movies)
print('Getting by Certificate')
by_certificates <- distribution_of_certificates(unique_movies)
print('Getting by Genre')
by_genre <- distribution_of_genres(unique_movies)
print('Getting by Keyword')
by_keywords <- distribution_of_keywords(keywords_subset)

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
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL)),
                   
                   # about button
                   actionButton("about_info", "About", width = 200),
                   
                   sidebarMenu(
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
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL)),

                   # top N input
                   numericInput(inputId = "input_top_n",
                                label = "Select Top N:",
                                min = 10, max = 56048, value = 10),

                   # keyword input
                   selectInput(inputId = "input_keyword",
                             label = "Select a Keyword(s):",
                             choices = c('All', as.character(by_keywords$keyword)),
                            multiple = TRUE,
                            selected = c('All')),

                   # genre input
                   selectInput(inputId = "input_genre",
                               label = "Select a Genre(s):",
                               choices = c('All', as.character(by_genre$genre)), 
                               multiple = TRUE,
                               selected = c('All')),

                   # certificate input
                   selectInput(inputId = "input_certificate",
                               label = "Select a Certificate(s):",
                               choices =  c('All', by_certificates[1]),
                               multiple = TRUE,
                               selected = c('All')),

                   sidebarMenu(
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
           box(title = "Overall Distribution of Films", width = 12, height = 850, solidHeader = TRUE, status = "warning", color = "yellow",

               tabsetPanel(
                 tabPanel("by Year",
                          tabsetPanel(
                            tabPanel("Bar Plot",
                                     tabPanel("Bar Plot", plotOutput("overview_year_plot"))
                            ),
                            tabPanel("Tabular Format",
                                     tabPanel("Tabular Format", dataTableOutput("tbl_year"))
                            )
                          ) #Close inner tabsetPanel
                 ),
                 tabPanel("by Month",
                          tabsetPanel(
                            tabPanel("Bar Plot",
                                     tabPanel("Bar Plot", plotOutput("overview_month_plot"))
                            ),
                            tabPanel("Tabular Format",
                                     tabPanel("Tabular Format", dataTableOutput("tbl_month"))
                            )
                          ) #Close inner tabsetPanel
                 ),
                 tabPanel("by Runtime",
                          tabsetPanel(
                            tabPanel("Bar Plot",
                                     tabPanel("Bar Plot", plotOutput("overview_runtime_plot"))
                            ),
                            tabPanel("Tabular Format",
                                     tabPanel("Tabular Format", dataTableOutput("tbl_runtime"))
                            )
                          ) #Close inner tabsetPanel
                 ),
                 tabPanel("by Certification",
                          tabsetPanel(
                            tabPanel("Bar Plot",
                                     tabPanel("Bar Plot", plotOutput("overview_certificate_plot"))
                            ),
                            tabPanel("Tabular Format",
                                     tabPanel("Tabular Format", dataTableOutput("tbl_certificates"))
                            )
                          ) #Close inner tabsetPanel
                 ),
                 tabPanel("by Genre",
                          tabsetPanel(
                            tabPanel("Bar Plot",
                                     tabPanel("Bar Plot", plotOutput("overview_genre_plot"))
                            ),
                            tabPanel("Tabular Format",
                                     tabPanel("Tabular Format", dataTableOutput("tbl_genres"))
                            )
                          ) #Close inner tabsetPanel
                 ),
                 tabPanel("by Top N Keywords",
                          tabsetPanel(
                            tabPanel("Bar Plot",
                                     tabPanel("Bar Plot", plotOutput("overview_top_keywords_plot"))
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
           box(title = "Distribution of Films by Selected Genre(s)", width = 12, height = 850, solidHeader = TRUE, status = "warning", color = "yellow",

               tabsetPanel(
                 tabPanel("by Year",
                          tabsetPanel(
                            tabPanel("Bar Plot",
                                     tabPanel("Bar Plot", plotOutput("genre_by_year"))
                            ),
                            tabPanel("Tabular Format",
                                     tabPanel("Tabular Format", h1("table here"))
                            )
                          ) #Close inner tabsetPanel
                 ),
                 tabPanel("by Decade",
                          tabsetPanel(
                            tabPanel("Bar Plot",
                                     tabPanel("Bar Plot", plotOutput("genre_by_decade"))
                            ),
                            tabPanel("Tabular Format",
                                     tabPanel("Tabular Format", h1("table here"))
                            )
                          ) #Close inner tabsetPanel
                 ),
                 tabPanel("by Month",
                          tabsetPanel(
                            tabPanel("Bar Plot",
                                     tabPanel("Bar Plot", plotOutput("genre_by_month"))
                            ),
                            tabPanel("Tabular Format",
                                     tabPanel("Tabular Format", h1("table here"))
                            )
                          ) #Close inner tabsetPanel
                 ),
                 tabPanel("by Year %",
                          tabsetPanel(
                            tabPanel("Bar Plot",
                                     tabPanel("Bar Plot", plotOutput("genre_by_year_percent"))
                            ),
                            tabPanel("Tabular Format",
                                     tabPanel("Tabular Format", h1("table here"))
                            )
                          ) #Close inner tabsetPanel
                 ),
                 tabPanel("by Decade %",
                          tabsetPanel(
                            tabPanel("Bar Plot",
                                     tabPanel("Bar Plot", plotOutput("genre_by_decade_percent"))
                            ),
                            tabPanel("Tabular Format",
                                     tabPanel("Tabular Format", h1("table here"))
                            )
                          ) #Close inner tabsetPanel
                 ),
                 tabPanel("by Month %",
                          tabsetPanel(
                            tabPanel("Bar Plot",
                                     tabPanel("Bar Plot", plotOutput("genre_by_month_percent"))
                            ),
                            tabPanel("Tabular Format",
                                     tabPanel("Tabular Format", h1("table here"))
                            )
                          ) #Close inner tabsetPanel
                 ),
                 tabPanel("by Runtime",
                          tabsetPanel(
                            tabPanel("Bar Plot",
                                     tabPanel("Bar Plot", plotOutput("genre_by_runtime"))
                            ),
                            tabPanel("Tabular Format",
                                     tabPanel("Tabular Format", h1("table here"))
                            )
                          ) #Close inner tabsetPanel
                 ),
                 tabPanel("by Certification",
                          tabsetPanel(
                            tabPanel("Bar Plot",
                                     tabPanel("Bar Plot", plotOutput("genre_by_certificate"))
                            ),
                            tabPanel("Tabular Format",
                                     tabPanel("Tabular Format", h1("table here"))
                            )
                          ) #Close inner tabsetPanel
                 ),
                 tabPanel("by Top N Keywords",
                          tabsetPanel(
                            tabPanel("Bar Plot",
                                     tabPanel("Bar Plot", plotOutput("genre_by_top_keywords"))
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
  
    # Logic to determine if the app started and which was the last input done
    first_view = reactiveVal(TRUE)
    last_decade = reactiveVal(0)
    last_year = reactiveVal(0)
    decade_on = reactiveVal(FALSE)
    year_on = reactiveVal(FALSE)
    last_genre = reactiveVal(c())
    
    which_on = eventReactive(c(input$input_year, input$input_decade), {
        year = input$input_year
        year_on(if (year != last_year()) TRUE else FALSE)
        
        decade = input$input_decade
        decade_on(if (decade != last_decade()) TRUE else FALSE)
        
        last_year(year)
        last_decade(decade)
    })
    
    # Update keywords when N changes 
    observeEvent(input$input_top_n, {
        N = input$input_top_n
        new_choices = c('All', as.character(by_keywords$keyword[1:N]))
        updateSelectInput(session, "input_keyword", choices = new_choices, selected = c('All'))
    })
    
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
    DT::datatable(by_year, options = list(dom = 'f, t, i, p, r', pageLength = 12), rownames = FALSE) %>%
      DT::formatStyle("year", fontSize = "125%") %>%
      DT::formatStyle("count", fontSize = "125%")
  })

  ### COUNT OF FILMS BY MONTH BELOW
  output$tbl_month <- DT::renderDataTable({
    DT::datatable(by_month, options = list(dom = 't, i', pageLength = 12), rownames = FALSE) %>%
      DT::formatStyle("month", fontSize = "125%") %>%
      DT::formatStyle("count", fontSize = "125%")
  })

  ### DISTRIBUTION OF RUNTIMES BELOW
  unique_runtimes = as.numeric(count(by_runtime))

  output$tbl_runtime <- DT::renderDataTable({
    DT::datatable(by_runtime, options = list(dom = 'f, t, i, p, r', pageLength = 12), rownames = FALSE) %>%
      DT::formatStyle("runtime", fontSize = "125%") %>%
      DT::formatStyle("count", fontSize = "125%")
  })

  ### DISTRIBUTION OF CERTIFICATES BELOW
  unique_certificates = as.numeric(count(by_certificates))

  output$tbl_certificates <- DT::renderDataTable({
    DT::datatable(by_certificates, options = list(dom = 't, i', pageLength = 12, order = list(list(1, 'desc'))), rownames = FALSE) %>%
      DT::formatStyle("rating", fontSize = "125%") %>%
      DT::formatStyle("count", fontSize = "125%")
  })

  ### DISTRIBUTION OF KEYWORDS BELOW
  #by_keywords <- distribution_of_keywords(keywords_subset, 10) # REPLACE 10 WITH REACTIVE OF input$input_top_n
  unique_keywords = as.numeric(count(by_keywords))

  output$tbl_keywords <- DT::renderDataTable({
    DT::datatable(by_keywords, options = list(dom = 'f, t, i, p, r', pageLength = 12), rownames = FALSE) %>%
      DT::formatStyle("keyword", fontSize = "125%") %>%
      DT::formatStyle("count", fontSize = "125%")
  })

  ### DISTRIBUTION OF GENRES BELOW
  unique_genres = as.numeric(count(by_genre))

  output$tbl_genres <- DT::renderDataTable({
    DT::datatable(by_genre, options = list(dom = 'f, t, i, p, r', pageLength = 12), rownames = FALSE) %>%
      DT::formatStyle("genre", fontSize = "125%") %>%
      DT::formatStyle("count", fontSize = "125%")
  })
  
  ### AVERAGES OF FILMS PER YEAR, MONTH, AND RUNTIME
  
  output$info_year <- renderInfoBox({
      which_on() # call once to get the year/decade switch working
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
  
  ########## WHEN YEAR/DECADE INPUT CHANGES, UPDATE OVERVIEW GRAPHS#######
  
  observeEvent(c(input$input_year, input$input_decade, input$input_genre, input$input_keyword, input$input_certificate, input$input_runtime), {
      if (first_view()){
          output$overview_year_plot = renderPlot({
              plotYearlyFilms(unique_movies)
          })
          
          output$overview_month_plot= renderPlot({
              plotMonthPerGivenYear(unique_movies, NULL)
          })
          
          output$overview_runtime_plot = renderPlot({
              plotRuntimePerGivenYear(unique_movies, NULL)
          })
          
          output$overview_genre_plot = renderPlot({
              plotGenrePerGivenYear(unique_movies, NULL)
          })
          
          output$overview_certificate_plot = renderPlot({
              plotCertificatesPerGivenYear(unique_movies, NULL)
          })
          
          output$overview_top_keywords_plot = renderPlot({
              plotTopKeywordsPerGivenYear(keywords_subset, NULL, input$input_top_n)
          })
          
          first_view(FALSE)
          
      } else {
          
          filtered_data = getMoviesFromFilter(unique_movies, keywords_subset, input$input_keyword, input$input_genre, input$input_runtime, input$input_certificate)
          
          output$overview_year_plot = renderPlot({
              if (year_on())
                  plotYearlyFilms(filtered_data %>% filter(year == input$input_year))
              else
                  plotFilmsByDecade(filtered_data, input$input_decade)
          })
          
          output$overview_month_plot= renderPlot({
              if (year_on())
                plotMonthPerGivenYear(filtered_data, input$input_year)
              else 
                plotMonthPerGivenDecade(filtered_data, input$input_decade)
          })
          
          output$overview_runtime_plot = renderPlot({
              if (year_on())
                plotRuntimePerGivenYear(filtered_data, input$input_year)
              else
                plotRuntimePerGivenDecade(filtered_data, input$input_decade)
          })
          
          output$overview_genre_plot = renderPlot({
              if (year_on())
                  plotGenrePerGivenYear(filtered_data, input$input_year)
              else
                  plotGenrePerGivenDecade(filtered_data, input$input_decade)
          })
          
          output$overview_certificate_plot = renderPlot({
              if (year_on())
                plotCertificatesPerGivenYear(filtered_data, input$input_year)
              else
                plotCertificatesPerGivenDecade(filtered_data, input$input_decade)
          })
          
          output$overview_top_keywords_plot = renderPlot({
              if (year_on())
                plotTopKeywordsPerGivenYear(keywords_subset %>% filter(movie %in% filtered_data$movie), input$input_year, input$input_top_n)
              else
                plotTopKeywordsPerGivenDecade(keywords_subset %>% filter(movie %in% filtered_data$movie), input$input_decade, input$input_top_n)
          })
      }
  })
  
  ##
  ##
  ## B REQUIREMENTS BELOW
  ##
  ##
  ########## WHEN YEAR/DECADE INPUT CHANGES, UPDATE GENRE GRAPHS#######
  
  observeEvent(input$input_genre, {
      
      genre = input$input_genre
      
      output$genre_by_year = renderPlot({
          plotYearByGenre(unique_movies, genre)
      })
      
      output$genre_by_decade = renderPlot({
          plotDecadeByGenre(unique_movies, genre)
      })
      
      output$genre_by_month = renderPlot({
          plotMonthByGenre(unique_movies, genre)
      })
      
      output$genre_by_year_percent = renderPlot({
          plotYearPercentageByGenre(unique_movies, genre, by_year)
      })
      
      output$genre_by_decade_percent  = renderPlot({
          plotDecadePercentageByGenre(unique_movies, genre, by_decade)
      })
      
      output$genre_by_month_percent = renderPlot({
          plotMonthPercentageByGenre(unique_movies, genre, by_month)
      })
      
      output$genre_by_runtime = renderPlot({
          plotRuntimeByGenre(unique_movies, genre)
      })
      
      output$genre_by_certificate = renderPlot({
          plotCertificatesByGenre(unique_movies, genre)
      })
      
      output$genre_by_top_keywords = renderPlot({
          plotTopKeywordsByGenre(keywords_subset, genre, input$input_top_n)
      })
  })
  
}

shinyApp(ui=ui, server=server)