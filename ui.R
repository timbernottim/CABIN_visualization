#Benthic visualization tool- CABIN public database 
library(shiny)
library(RColorBrewer)
library(tidyverse)
library(rlang)
library(DT)
library(rsconnect)
library(vegan)

ui <- fluidPage(
  
  
  titlePanel("Benthic Macroinvertebrate Visualization Tool", windowTitle = "CABIN Visualization"),
  br(),
  sidebarLayout(
    sidebarPanel(
      
      
      
      fileInput(inputId = "benthic_file",
                label = "Upload benthic_matched file:",
                accept = "csv"),
      
      selectInput(inputId = "x", 
                  label = "Select sites of interest:", multiple = T, choices = ""),
      
      # Select years from which the data will be subset 
      selectInput(inputId = "year", 
                  label = "Years of interest:", multiple = T,
                  choices = ""),
      
      # Select taxa from which the data will be subset, doesn't select column names  
      selectInput(inputId = "z", 
                  label = "Taxon of interest:",
                  choices = c("Phylum","Class","Order","Family","Genus","Species"),
                  multiple = F, selected = "Family"),
      
      actionButton(
        inputId = "visualize",
        label = "Visualize your Data!")
      
    ),
    
    mainPanel(
      
      tabsetPanel(
        
        tabPanel("Plot", br(), br(), br(), plotOutput("bar_plot", height = "800px"), hr(), br(),
                 downloadButton(outputId = "summarised_data.csv", "Download Summarized Data"), 
                 br(), br(), br(),
                 DT::dataTableOutput(outputId = "data_table")),
        tabPanel("Biodiversity",  h1("Calculated Biodiversity Statistics"), br(), br(),
                 h4("For more information regarding how biodiversity indexes are calculated, visit the R vegan package
                    documentation", tags$a(href = "https://www.rdocumentation.org/packages/vegan/versions/2.4-2/topics/diversity",
                                           "here")), hr(),
                 br(), 
                 downloadButton(outputId = "biodiversity_statistics.csv", "Download Biodiversity Data"),
                 br(), br(), br(),
                 DT::dataTableOutput(outputId = "biodiversity_table")),
        tabPanel("Help", h1("Where do I get the data?"), br(),
                 h4("Access UNIX data management pipeline and corresponding wiki at:",
                    tags$a(href="https://github.com/timbernottim/cabin-pipe", "Github")), hr(),
                 h1("Raw CABIN Data"),
                 br(),
                 h4("If you are not interested in using a UNIX pipeline for the CABIN public database, 
                    feel free to download the data directly from the CABIN public database",
                    tags$a(href = "http://data.ec.gc.ca/data/substances/monitor/cabin-canadian-aquatic-biomonitoring-network/", "here")),
                 br(),
                 h4(" Be sure to transfer the datafile into Excel using the Data tab 'Get Data' function and save the file as a .csv 
                    file on your computer. In its raw form, the data file is not encoded correctly for R to handle properly."),
                 br(),
                tags$i(h4("Note, if you choose to use the CABIN public data in its raw form know that not all sites are
                           represented across both the benthic file and the habitat file and some biological inferences
                           may be limited."))
                 
        )
        
      )
      
    )
    
  )
)


