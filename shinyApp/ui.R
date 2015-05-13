
shinyUI(fluidPage( tags$head(
  tags$style(HTML("
      @import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700');
      
      h1 {
        font-family: 'Lobster', cursive;
        font-weight: 500;
        line-height: 1.1;
        color: #2983C7;
      }

    "))
),#theme = "bootstrap.css", 

  h1("Marshall's Music Recommender"),
  hr(),
           
     selectizeInput('artist', 'Select an artist:',
                    choices = all.artists),
  
          selectizeInput(
              'songSelect', 'Select a song:', 
              choices = c("first select artist!")
          ),
    fluidRow(actionButton('goAddSong', 'Add Song!'), actionButton("clear", "Clear All Songs")), 

           hr(),
           h4("Songs you like:"), 
           dataTableOutput("allSongsTable"), 
           hr(),

  h4("Recommenders:"), 
  tabsetPanel(type = "pills", 
    tabPanel("Ask a Robot", 
            br(),
           actionButton("goRobot", "get recommendation"),
           br(),
           dataTableOutput("robotRec"), 
           br(), 
           br()
           
    ),
    tabPanel("Ask a Hipster", 
             br(),
             actionButton("goHipster", "get recommendation"),
             br(),
             dataTableOutput("hipsterRec"), 
             br(), 
             br()

    ),
    tabPanel("Ask a Radio DJ",
             br(),
             actionButton("goDJ", "get recommendation"),
             br(),
             dataTableOutput("djRec"),
             br(), 
             br()
        
    ), 
    tabPanel("Ask a Teenage Girl", 
             br(),
             actionButton("goGirl", "get recommendation"),
             br(),
            dataTableOutput("girlRec"), 
            br(), 
            br()
             
   )
  )
))