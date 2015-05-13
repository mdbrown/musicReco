library(shiny)
library(ggplot2)
library(data.table)

source("../R/getSongInfo.R")
source("../R/getRecoFUN.R")

#load("data/songInfo.Rdata") #build using curateData
#load("data/SongsRated.Rdata") #build in InitialThoughts
#load("data/trainTrips.Rdata") #build in InitialThoughts



shinyServer(function(input, output, clientData, session) {
  
  #store the songs the user likes
  allSongID <- reactiveValues(id = NULL)
  
  #clear songs the user likes if button is pressed
  observe({
    if (input$clear > 0){
      allSongID$id <- NULL
    }
  })
  
  
    
    #output$artistUI <- renderUI({
    #  selectizeInput('artist', 'Select an artist:',
    #                  choices = all.artists)
    #})
 
  #this updates the song list when the artist changes
  observe({
    
    updateSelectInput(session, "songSelect",
                      label = "Select a song",
                      choices = getSongSubset(input$artist, songdata = songInfo.dt)$song
    )
  })
  
  # Add a song to the user likes list when the button is clicked
  observe({
  if(input$goAddSong > 0 ){
  
    cat("addingSong")
    allSongID$id <- isolate(append(allSongID$id, getSongID(input$songSelect, 
                                                           input$artist, songdata = songInfo.dt)))
  }
    })
  
 
  # calls main algorithm for weighting songs 'get.reco.is.shiny'
  # all recommenders use these weights as the base of their recommendations
  getRecoWeights <- reactive({
    cat("getting weights!")

    songs.rated <- get.reco.is.shiny(s = allSongID$id, a = .15, q = 3)

    songs.rated
    
  })
  
  
  #the robot uses the weights used from the item based collaborative learning
  getRobotRec <- eventReactive(input$goRobot, {
    robot.rated <- getRecoWeights()
    
    robot.rated <- robot.rated[order(-robot.rated$score)]
    out <- getSongInfo(robot.rated$song, songdata = songInfo.dt)
    
    out 
  })
  
  #hipster recommender
  #upweight obscure songs (downweight popular songs)...except for 1 in 100 when 
  #they recommend only super popular songs that they enjoy ironically.
  getHipsterRec <- eventReactive(input$goHipster, { 
    
    robot.rated <- getRecoWeights()
    if(runif(1)>.01){
    robot.rated[,score:= score*(1/n.i.song)]
    }else{
      robot.rated[,score:= score*(n.i.song)^2]
    }
    robot.rated <- robot.rated[order(-robot.rated$score)]
    out <- getSongInfo(robot.rated$song, songdata = songInfo.dt)
    
    out 
    
    })
  
  #upweight popular songs...everyone likes whats popular!
  getDJRec <- eventReactive(input$goDJ, { 
    
    robot.rated <- getRecoWeights()
    
    robot.rated[,score:= score*n.i.song]
    robot.rated <- robot.rated[order(-robot.rated$score)]
    out <- getSongInfo(robot.rated$song, songdata = songInfo.dt)
    
    out 
    
  })
  #have you ever heard of katy perry or taylor swift...I'm obsessed!
  getGirlRec <- eventReactive(input$goGirl, { 
    
    robot.rated <- getRecoWeights()
    
    robot.rated[,score:= score]
    robot.rated <- robot.rated[order(-robot.rated$score)]
    out <- getSongInfo(robot.rated$song, songdata = songInfo.dt)
    out <- out[is.element(artist, c("Katy Perry", "Taylor Swift", "Justin Bieber")),]
    out 
    
  })
  
  #table showing the songs the user likes
  output$allSongsTable <- renderDataTable({

      getSongInfo(allSongID$id, songdata = songInfo.dt)
 
    
  }, options = list(paging = FALSE, searching = FALSE, searchable = FALSE))
  
  
  #build data tables of recommendations, only take the first 100 for now
  output$robotRec <- renderDataTable({
    input$goRobot
    robot.songInfo <- getRobotRec()
    as.data.frame(robot.songInfo[1:100,])
  }, options = list( searching = FALSE, searchable = FALSE))
    
  output$hipsterRec <- renderDataTable({
    input$goHipster
    hipster.songInfo <- getHipsterRec()
    as.data.frame(hipster.songInfo[1:100,])
  }, options = list( searching = FALSE, searchable = FALSE))
  
  output$djRec <- renderDataTable({
    input$goDJ
    dj.songInfo <- getDJRec()
    as.data.frame(dj.songInfo[1:100,])
  }, options = list( searching = FALSE, searchable = FALSE))
  
  output$girlRec <- renderDataTable({
    input$goGirl
    girl.songInfo <- getGirlRec()
    as.data.frame(girl.songInfo[1:100,])
  }, options = list( searching = FALSE, searchable = FALSE))
  

})