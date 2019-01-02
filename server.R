server <- function(input, output, session){
  
  options(shiny.maxRequestSize = 30*1024^2)
  
  #Creates a palette of 60 distinct colours from RColourBrewer 
  n <- 80
  qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
  col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
  
  #My own theme, changed for 90 degree x axis 
  JZ_theme<-theme_classic()+
    theme(plot.title=element_text(hjust=.5,size=20),
          text=element_text(size=16,color = "black"),
          axis.text = element_text(size=12,color="black"),
          axis.title=element_text(size=20,color="black"),
          axis.line = element_line(colour = 'black', size = 1.3),
          axis.ticks = element_line(size=.8,colour="black"),
          axis.text.x = element_text(size=18,angle=90,hjust=1),
          axis.text.y=element_text(size=18),
          legend.title = element_text(size=16),
          legend.text = element_text(size=16,color="Black"))
  
  names<-c("Site","Year","JulianDay","CWSSD_ID","CWSSD_Basin","SamplingDevice","KickTime","MeshSize","SampleNumber","SubSample","TotalSample","Status","QA/QC","Taxonomist","Organization","Address",
           "City","Province","Phylum","Class","Order","Family","Genus","Species","Replicate","Count","ITIS_TSN","Valid")
  
  
  #Proportion function for summary table 
  proportion <- function(x){
    
    x/sum(x)
  }

    
  data <- reactive({
    inFile <- input$benthic_file
    
    if (is.null(inFile))
      return(NULL)
    
    read.csv(inFile$datapath, header = T, col.names = names, na.strings = "")
    
  }
  )
  
  
  #Updates select input with unique site values based on uploaded csv file 
  observe({
    updateSelectInput(
      session,
      "x",
      choices = as.character(unique(levels(data()[["Site"]]))))
  })
  
  
  #Working, sites are filtered by x input, y input is automatically updated based on input$x 
  site_filtered <- reactive({
    if (is.null(input$x)) return(NULL)
    data() %>%  
      filter(Site %in% input$x)
  })
  
  
  
  observe({
    updateSelectInput( 
      session, 
      "year",
      choices = c(unique(unlist(site_filtered()[["Year"]]))))
    
    
  })
  
  
  
  
  #Still need to remove empty values and replace them with description 
  summarised_data<- reactive({
    
    req(input$visualize)
    
    data() %>%
      filter(Site %in% input$x) %>%
      filter(Year %in% input$year) %>%
      group_by(!! rlang::sym(input$z), Site, Year) %>%
      summarise(Sum = sum(Count)) %>%
      ungroup() %>%
      group_by(Site, Year) %>%
      mutate(Prop = proportion(Sum)) %>%
      ungroup()
  })
  
  
  biodiversity_data <- reactive({
    
    req(summarised_data())
    #Data transformation 
    
    #Calculate total # of individuals
    individuals_tally <-
      summarised_data() %>%
      group_by(Site, Year) %>%
      mutate(Total_Individuals = sum(Sum)) %>%
      select(-!! rlang::sym(input$z), -Sum, -Prop) %>%
      distinct()  
    
    #Calculate total # of unique taxa
    taxa_tally <-
      summarised_data() %>%
      group_by(Site, Year) %>%
      distinct(!! rlang::sym(input$z)) %>%
      summarise(Total_Taxa = n())
    
    #Turns rows into columns for given taxa
    summary_diversity <- 
      summarised_data() %>%
      select(-Prop) %>%
      spread(!! rlang::sym(input$z), Sum) 
    
    #Replaces all na values with 0
    summary_diversity[is.na(summary_diversity)] <- 0
    
    #Subsets site and year for final df
    env_df <-
      summary_diversity %>%
      select(Site, Year)
    
    #Removes site and year so vegan::diversity can do its magic
    shannon_df <-
      summary_diversity %>%
      select(-Site, -Year)
    
    #Calculates all possible diversity indexes 
    shannon_summary <- diversity(shannon_df, index = "shannon")
    simpson_summary <- diversity(shannon_df, index = "simpson")
    invsimpson_summary <- diversity(shannon_df, index = "invsimpson")
    
    #Final df for display 
    Final_df <-
      env_df %>%
      bind_cols(Shannon_Index = shannon_summary, 
                Simpson_Index = simpson_summary,Inverse_Simpson_Index = invsimpson_summary) %>%
      bind_cols(Total_Individuals = individuals_tally$Total_Individuals, Total_Taxa = taxa_tally$Total_Taxa)
    
    
    
    
    
  })
  
  
  
  
  
  
  #Groups sites by year, proportion is within year 
  output$bar_plot <- renderPlot(
    
    ggplot(data = summarised_data(), aes(x = Site, y = Prop, fill = !! rlang:: sym(input$z))) + 
      geom_col(position = "stack", width = 0.8, color = "black") +
      geom_text(aes(label = ""), vjust = -0.25) +
      facet_grid(~Year, switch = "x", scales = "free_x", space = "free_x") +
      theme(panel.spacing = unit(0, "lines"), 
            strip.background = element_blank(),
            strip.placement = "outside") + 
      xlab("") +  scale_fill_manual(values = col_vector,name= input$z)+
      labs(y="Macroinvertebrate Taxa Present (prop)") + JZ_theme
    
    
  )
  
  
  #Totally cheating here, filename argument is not working at all...
  output$summarised_data.csv<- downloadHandler(
    filename = "summarised_data.csv",
    content = function(file) {
      write_csv(summarised_data(), path = file)
      
    }
    
    
    
  )
  
  output$biodiversity_statistics.csv <- downloadHandler(
    
    filename = "biodiversity_statistics.csv",
    content = function(file) {
      
      write_csv(biodiversity_data(), path = file)
      
    }
    
    
  )
  
  
  output$data_table <- DT::renderDataTable({
    DT::datatable(data = summarised_data()[, 1:4], 
                  options = list(pageLength = 10), 
                  rownames = T)})
  
  output$biodiversity_table <- DT::renderDataTable({
    DT::datatable(data = biodiversity_data()[, 1:7],
                  rownames =T)
    
  })
  
  
  }