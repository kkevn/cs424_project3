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

########################## DATA NEEDED FOR PLOTTING #####################################
########################## DATA NEEDED FOR PLOTTING #####################################



########################## DASHBOARD #####################################
########################## DASHBOARD #####################################

ui = dashboardPage(
  
  dashboardHeader(title = "Project 3", titleWidth = 200),
  
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

}

shinyApp(ui=ui, server=server)