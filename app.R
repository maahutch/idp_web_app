library(shiny)
library(leaflet)
library(magrittr)
library(tibble)
library(plyr)
library(DT)
library(sp)
library(visNetwork)
library(sf)
library(igraph)
library(reticulate)

#use_python('/usr/bin/python3', required = T)


county_zip <- read.csv('data/county_zip.csv')

source_python('db_queries.py', envir = globalenv())
source('savi_fun_V2.R')


counties <- data.frame(county = county_zip[,1])




# Define UI for application that draws a histogram
ui <- navbarPage(theme = "idp_style.css", "",
                 
  
  tabPanel("IDP",
   #tags$style(type='text/css', ".selectize-dropdown-content {max-height: 10%;}"),
     sidebarLayout(
       sidebarPanel(
          tags$style(type = 'text/css',
                     ".selectize-dropdown-content{
                     max-height:1000px;
                     line-height: 26px;
                     }"),
         selectInput("county", 
                     label   = "Select County:",
                     choices = counties[,1]
                     ),
         br(), 
         selectInput("issue", 
                     label   = "Select Issue Area:",
                     choices = c("Education/Workforce Development", 
                                 "Food Security", 
                                 "Healthcare", 
                                 "Housing", 
                                 "Immigration", 
                                 "Opioid Addiction/Substance Abuse", 
                                 "Public Safety", 
                                 "Transportation")
                     )
        ),
       
        mainPanel(
          leafletOutput("home", height = "700px")
        )
      )
    ),
  
  tabPanel("Involved Organizations Map",
    
               
                fluidRow(column(3,
                                  br(),
                                  imageOutput("legend"), 
                                  radioButtons("no_ref_org",
                                               label = uiOutput("ref_dyn_lab"),
                                               choices = list("Yes" = 1, "No" = 2), 
                                               selected = 2),
                                  br(),
                                  br(), 
                                  radioButtons("ref_node_size",
                                             label   = "Size Nodes by:",
                                             choices = list("In-Degree"             = 1, 
                                                            "Out-Degree"            = 2,
                                                            "Degree"                = 3,
                                                            "Betweeness Centrality" = 4,
                                                            "Page Rank"             = 5,
                                                             "None"                  = 6),
                                             selected = 6),
                                br(), 
                                br()
                               ),
                         column(9,         
                                  visNetworkOutput("network", height = "700px")
                               )
                         )
       
                      ),
   
  tabPanel("Potential Collaborators Map",
           tags$style(type = "text/css",
                            ".irs-grid-pol {display: none;}"
                             ),
  fluidRow(column(4,
               uiOutput("pcm_sidebar_lab"),
               br(), 
               br(),
               uiOutput("oi.choices"),
               sliderInput("no_dim", "Select Number of Shared Characteristics:", min = 1, max = 10, value = 1, width = "100%"),
               br(),
               br(),
               actionButton("dim_button", label = "Map it!"),
             br(),
             br(), 
             br(),
             br(),
             DT::dataTableOutput('orgs', width = "75%")
             ),
          column(8,
               tabsetPanel(
                 tabPanel(
                   h4(uiOutput("pcm_map_tab")),
                   leafletOutput("oi.display1", height = "700px"),
                   br(),
          column(12, align = "right",
              textOutput("oi.count")
                )
                 ), 
                 tabPanel(
                   h4("Potential Organizational Collaborators:"),
                   leafletOutput("oi.display", height = "700px"),
                   br(),
          column(12, align = 'right',
                   textOutput("collab.count")
                  )
                 )
               )
             )
           )
  ),
  
  
   # Application title
   tabPanel("Pathways Map", 
  
   sidebarLayout(
      sidebarPanel(
         selectInput("level",
                     label   = "Select Path Step:",
                     choices = c("Entry Point", 
                                 "Treatment Point", 
                                 "Recovery Point",
                                 "Stabilization Point",
                                 "Prevention Point")),
         uiOutput("catChoices"),
         sliderInput("steps", "Select Number of Steps to Map", min = 0, max = 3, value = 0),
         sliderInput("dist", "Select Maximum Distance between Steps (miles)", min = 0, max = 10, value = c(1), step = 1),
         actionButton("button", label = "Map it!"), 
         br(), 
         br(), 
         br(), 
         img(src='pin_key_2_transparent.png', align = "center", width = '100' , height = '200')
      ),
      
      
      mainPanel(
         leafletOutput("in.map", height = "700px"),
         br(), 
         textOutput("edge.count")
      )
    )
  )
)




server <- function(input, output) {
  
  
    output$home <- renderLeaflet({
      
      home.map()
      
    })  
  
    outCat <- reactive({
      
        getCat(input$level)
      
    })
    
    output$catChoices <- renderUI({
        selectizeInput("cat", "Select Organization/Service Type(s):", c("", outCat()), options = list(maxOptions = 5000), multiple = F)
    })
    
    output$oi.choices <- renderUI({
      selectizeInput("oi_org", "Select Organization:", c("", get.oi.org(input$issue)), options = list(maxOptions = 5000), multiple = F)
    })
    
    output$ref_dyn_lab <- renderUI({
      paste0("Show ", input$issue, " Organizations for which Network Data are Lacking:")
    })
    
    output$pcm_sidebar_lab <- renderUI({
      paste0("Select among the organizations addressing ", input$issue, " to identify other organizations with shared characteristics that are not known to be addressing opioids:")
    })
    
    output$pcm_map_tab <- renderUI({
      paste0("Organizations addressing ", input$issue)
    })
    
    output$sp_org_start <- renderUI({
      selectizeInput("start_sp", "Select Start Organization:", c(get.nodes.sp.start(input$issue)), options = list(maxOptions = 5000), multiple = F)
    })
   
    output$sp_org_end <- renderUI({
      selectizeInput("end_sp", "Select End Organization:", c(get.nodes.sp.end(input$issue)), options = list(maxOptions = 5000), multiple = F)
    })
    
    
    output$legend <- renderImage({
      if(input$issue == 'Opioid Addiction/Substance Abuse') Leg <- "www/idp_legend_2.png"
      if(input$issue == 'Education/Workforce Development')  Leg <- "www/idp_legend_1.png"
      
      list(src=Leg)
    }, deleteFile = F)
    
    
    
    org.l <- eventReactive(input$button, {lapply(input$cat, getStart)})
    
    
    display.map <- eventReactive(input$button, {createOutput(org.l = org.l(), dist = input$dist, steps = input$steps)})
       
    output$in.map <- renderLeaflet({

           
          display.map()
       
        })
    
    output$edge.count <- eventReactive(input$button, {edge.org.count()})
    
  
   
#################################
    
    
    shr.dim.combo <- eventReactive(input$dim_button, {get.cat.combo(org.nm = input$oi_org, no = input$no_dim)}, ignoreNULL = T)
    
    shr.dim.source <- eventReactive(input$dim_button, {get.source.org(org.nm = input$oi_org, no = input$no_dim)}, ignoreNULL = T)
    
    shr.dim.map   <- eventReactive(input$dim_button, {org.cat.match(shr.dim.combo(), 
                                                                    no         = input$no_dim, 
                                                                    source.org = shr.dim.source(), 
                                                                    topic     = input$issue
                                                                    )})
    
   
    
    shr.dim.tab   <- eventReactive(input$dim_button, {
      
     
      
        shr.dim.tab   <-  get.match.org.oi(one.size = input$no_dim, 
                                           cats     = shr.dim.combo(), 
                                           issue    = input$issue
                                          )
        
        
        
        
        
        shr.dim.tab  <- subset(shr.dim.tab, shr.dim.tab$org != toupper(input$oi_org))
        
        
        #if(nrow(shr.dim.tab) < 2){
          
         # shr.dim.tab <- data.frame(org = "NO MATCHING ORGANIZATIONS")
          
        #}      
        
      })
  
    
    
    
    output$collab.count <- renderText({
      
      orgs <- unique(shr.dim.tab()$org)
      
      message <- paste0("Number of Organizations being Mapped: ", length(orgs))
      
      return(message)
    })
    
    output$oi.display   <- renderLeaflet({shr.dim.map()})
    
    output$oi.display1  <- renderLeaflet({all.oi(input$issue)})
    output$oi.count     <- renderText({oi.org.count()})
  
    
    output$orgs <- DT::renderDataTable({
      
     orgs <- unique(shr.dim.tab()$org)

     orgC <- c()
     
     for(i in 1:length(orgs)){
       
       orgC <- c(orgC, simpleCap(tolower(orgs[i])))
       
     }
  
     
     orgs <- data.frame(sort(orgC))
     
     colnames(orgs) <- "Organizations With Shared Characteristics"
     
     DT::datatable(orgs, options = list(pageLength = 10))
      
    })
    
##################################
    
    output$network <- renderVisNetwork({make_net(unconn = input$no_ref_org, issue = input$issue, node_size = input$ref_node_size)})
    
    
    path <- eventReactive(input$sp_butt, {short.path.II(start = input$start_sp,
                                                        end   = input$end_sp)})
  
  
    options(shiny.sanitize.errors = FALSE)
}

# Run the application 
shinyApp(ui = ui, server = server)

