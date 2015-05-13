
#input vector of songs to get song information
getSongInfo <- function(s, songdata){
  if(is.null(s)) return()
  
 
  return(songdata[J(s), .(artist, song)])
  
}

getSongID <- function(songname, artistname, songdata){
  return(songdata[song == songname & artist == artistname, songid][1])
  
}
#getSongInfo( "SOUNOKS12A8C13BD0E")

#input artist to get which tracks are available
getSongSubset <- function(a, songdata){
  
  return(songdata[artist ==a, .(songid, artist, song)])
  
}

#getSongSubset("Aura")
#getSongSubset("Giorgia")
