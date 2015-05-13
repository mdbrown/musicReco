get.reco.is.shiny <- function(s, a, q){
  
  songs.rated <- songs.rated.base
  
  #calc I(u) & I(v) for all other item user pairs 
  #songs the user likes


 for(tmpsong in s){

  users.for.tmpsong <- trainTrips.dt[.(tmpsong),]$user
   
  n.u.tmpsong <- length(users.for.tmpsong)

    #figure out what goes here
  v.data <- trainTrips.dt[user %in% users.for.tmpsong,
                          list(num.users.in.common = .N) , by=song]
  songs.rated <- v.data[songs.rated, ]
  songs.rated[is.na(num.users.in.common), num.users.in.common:= 0]

  songs.rated[,score:= score + (num.users.in.common/(((n.u.tmpsong)^(1-a))*((n.i.song)^(a))))^q]
    
  songs.rated$num.users.in.common <- NULL
 }
   #remove the songs already liked, so I don't recommend those
  setkey(songs.rated, song)
  songs.rated.sub <- songs.rated[!J(s)]

  songs.rated.sub[score >0, ]
  
}






get.reco.is <- function(u, a, q){
  
  songs.rated <- songs.rated.base
  
  #calc I(u) & I(v) for all other item user pairs 
  #songs the user likes
  
  user.songs <- validTrips.known.dt %>% filter(user ==as.character(u)) %>% select(song, count)
  
  len.u.songs <- nrow(user.songs)
  
  
  cat(paste("this user liked", nrow(user.songs), "songs"))
  cat("\n scoring ")
  
  s = 0 
  for(tmpsong in user.songs$song){
    s = s+1
    count.tmpsong <- user.songs$count[s]
    #the set of users that like the tmpsong
    
    users.for.tmpsong <- trainTrips.dt[.(tmpsong),]$user
    
    n.u.tmpsong <- length(users.for.tmpsong)
    
    #figure out what goes here
    v.data <- trainTrips.dt[user %in% users.for.tmpsong,
                            list(num.users.in.common = .N) , by=song]
    songs.rated <- v.data[songs.rated, ]
    songs.rated[is.na(num.users.in.common), num.users.in.common:= 0]
    
    songs.rated[,score.cnt:= score.cnt + count.tmpsong*(num.users.in.common/(((n.u.tmpsong)^(1-a))*((n.i.song)^(a))))^q]
    songs.rated[,score:= score + (num.users.in.common/(((n.u.tmpsong)^(1-a))*((n.i.song)^(a))))^q]
    
    songs.rated$num.users.in.common <- NULL
    
    
    cat(".")
    
    
  }
  
  cat("getting prediction!\n")
  #remove the songs already liked, so I don't recommend those
  setkey(songs.rated, song)
  songs.rated.sub <- songs.rated[!J(user.songs$song)]
  prediction <- list(
    songs = songs.rated.sub[order(-score),][1:500, song],
    songs.cnt = songs.rated.sub[order(-score.cnt),][1:500, song],
    user.liked = len.u.songs)
  
  prediction
}

