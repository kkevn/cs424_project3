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

# retrieve helper functions
#source("helper.R")

# read in the processed data
df_certificates = read.csv('data/certificates.csv')
df_genres = read.csv('data/genres.csv')
df_keywords = read.csv('data/keywords.csv')
df_movies = read.csv('data/movies.csv')
df_releases = read.csv('data/releases.csv')
df_runtimes = read.csv('data/runtimes.csv')

########################## DATA NEEDED FOR PLOTTING #####################################
########################## DATA NEEDED FOR PLOTTING #####################################



########################## DASHBOARD #####################################
########################## DASHBOARD #####################################

ui = dashboardPage(
  
  dashboardHeader(title = "Project 3 - IMDB", titleWidth = 200),
  
  dashboardSidebar(disable = FALSE, collapsed = FALSE,
                   sidebarMenu(
                     # add space to sidebar
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL)),
                   
                   # about button
                   actionButton("about_info", "About", width = 200)
                   
  ), # end sidebarMenu
  
  dashboardBody(
    column(4, 
           box(title = "Distribution of Films", width = 12, height = 800, status = "info",
               
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
    column(4,
           fluidRow(
             infoBoxOutput("info_year"),
           ),
           fluidRow(
             infoBoxOutput("info_month")
           ),
           fluidRow(
             infoBoxOutput("info_runtime")
           )
    )
    
    
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
  
  # create empty dataframe of all years in range 1874-2115
  all_years <- data.frame(formatC(1874:2115, width = 2), 0)
  names(all_years)[1] <- "year"
  names(all_years)[2] <- "count"
  all_years$year <- c(1874:2115)
  
  # make dataframe of available years and their frequency
  by_year <- df_movies %>% group_by(year) %>% summarize(count = n())
  
  # join the counts into the full range of years dataframe
  by_year <- full_join(all_years, by_year, by = "year")
  by_year[is.na(by_year)] <- 0
  by_year$count.x <- NULL
  names(by_year)[2] <- "count"
  
  # store some variables
  total_films = as.numeric(sum(by_year$count))
  unique_years = as.numeric(count(by_year))
  
  output$tbl_year <- DT::renderDataTable({
    DT::datatable(by_year, options = list(dom = 'f, t, i, p, r', pageLength = 13), rownames = FALSE) %>%
      DT::formatStyle("year", fontSize = "125%") %>%
      DT::formatStyle("count", fontSize = "125%")
  })
  
  ### COUNT OF FILMS BY MONTH BELOW
  
  # get a count of movies from each month
  by_month <- df_releases %>% group_by(month) %>% summarize(count = n())
  
  # reorder months to be in order
  by_month$month <- factor(by_month$month, levels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))
  by_month <- by_month[order(by_month$month), ]
  
  # store a variable
  unique_months = as.numeric(count(by_month))
  
  output$tbl_month <- DT::renderDataTable({
    DT::datatable(by_month, options = list(dom = 't, i', pageLength = 13), rownames = FALSE) %>%
      DT::formatStyle("month", fontSize = "125%") %>%
      DT::formatStyle("count", fontSize = "125%")
  })
  
  ### DISTRIBUTION OF RUNTIMES BELOW
  
  # create empty dataframe of all runtimes in range 60-125156
  all_runtimes <- data.frame(formatC(60:125156, width = 2), 0)
  names(all_runtimes)[1] <- "runtime"
  names(all_runtimes)[2] <- "count"
  all_runtimes$runtime <- c(60:125156)
  
  # make dataframe of available runtimes and their frequency
  by_runtime <- df_runtimes %>% group_by(runtime) %>% summarize(count = n())
  
  # join the counts into the full range of years dataframe
  by_runtime <- full_join(all_runtimes, by_runtime, by = "runtime")
  by_runtime[is.na(by_runtime)] <- 0
  by_runtime$count.x <- NULL
  names(by_runtime)[2] <- "count"
  
  # store a variable
  unique_runtimes = as.numeric(count(by_runtime))
  
  output$tbl_runtime <- DT::renderDataTable({
    DT::datatable(by_runtime, options = list(dom = 'f, t, i, p, r', pageLength = 13), rownames = FALSE) %>%
      DT::formatStyle("runtime", fontSize = "125%") %>%
      DT::formatStyle("count", fontSize = "125%")
  })

  ### DISTRIBUTION OF CERTIFICATES BELOW
  
  # get a distribution of certificate
  by_certificates <- df_certificates %>% group_by(rating) %>% summarize(count = n())
  
  # store a variable
  unique_certificates = as.numeric(count(by_certificates))
  
  output$tbl_certificates <- DT::renderDataTable({
    DT::datatable(by_certificates, options = list(dom = 't, i', pageLength = 13), rownames = FALSE) %>%
      DT::formatStyle("rating", fontSize = "125%") %>%
      DT::formatStyle("count", fontSize = "125%")
  })
  
  ### DISTRIBUTION OF KEYWORDS BELOW
  
  # get a distribution of top n keywords
  by_keywords <- df_keywords %>% group_by(keyword) %>% summarize(count = n())
  
  # store a variable
  unique_keywords = as.numeric(count(by_keywords))
  
  output$tbl_keywords <- DT::renderDataTable({
    DT::datatable(top_n(by_keywords, 10), options = list(dom = 'f, t, i, p, r', pageLength = 13), rownames = FALSE) %>%
      DT::formatStyle("keyword", fontSize = "125%") %>%
      DT::formatStyle("count", fontSize = "125%")
  })
  
  ### DISTRIBUTION OF GENRES BELOW
  
  # get a distribution of genres
  by_genre <- df_genres %>% group_by(genre) %>% summarize(count = n())
  
  # store a variable
  unique_genres = as.numeric(count(by_genre))
  
  output$tbl_genres <- DT::renderDataTable({
    DT::datatable(by_genre, options = list(dom = 'f, t, i, p, r', pageLength = 13), rownames = FALSE) %>%
      DT::formatStyle("genre", fontSize = "125%") %>%
      DT::formatStyle("count", fontSize = "125%")
  })
  
  ### AVERAGES OF FILMS PER YEAR, MONTH, AND RUNTIME
  
  # average fims per year: sum of films divided by total years observed
  avg_per_year = total_films / unique_years
  
  output$info_year <- renderInfoBox({
    infoBox(
      "Average/Year:", paste0(trunc(avg_per_year), " films"), icon = icon("fas fa-film"),
      color = "blue", fill = TRUE
    )
  })
  
  # average films per month: sum of films each month, each sum divided by total years observed; then average these results
  avg_by_month <- by_month
  avg_by_month$count <- by_month$count / unique_years
  names(avg_by_month)[2] <- "average"
  avg_per_month = as.numeric(sum(avg_by_month$average) / unique_months)
  
  output$info_month <- renderInfoBox({
    infoBox(
      "Average/Month:", paste0(trunc(avg_per_month), " films"), icon = icon("far fa-calendar"),
      color = "green", fill = TRUE
    )
  })
  
  
  # average runtime: sum of each runtime divided by total runtime observations
  avg_runtime = mean(df_runtimes$runtime)
  
  output$info_runtime <- renderInfoBox({
    infoBox(
      "Average Runtime:", paste0(trunc(avg_runtime), " minutes"), icon = icon("fas fa-hourglass-half"),
      color = "yellow", fill = TRUE
    )
  })
  
}

shinyApp(ui=ui, server=server)