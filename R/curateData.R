#get artist and track info for drop down bars
songInfo.dt <- fread("../Data//unique_tracks.txt", sep="\\")
setnames(songInfo.dt, c("trackid", "songid", "artist", "song"))
setkey(songInfo.dt, songid)

mismatch <- readLines("../Data/sid_mismatches.txt", n = -1)
dont.trust <- lapply(mismatch, FUN = function(x) strsplit(gsub(".*<|>.*", "",x), split= " ")[[1]][1])
dont.trust <- unlist(dont.trust)

songInfo.dt <- songInfo.dt[!J(dont.trust)]

#now we need to 

songInfo.dt.sub <- songInfo.dt[is.element(songInfo.dt$songid, songs.rated.base$song),]

all.artists <-   unique(songInfo.dt.sub$artist)
songInfo.dt <- songInfo.dt.sub



save(all.artists, songInfo.dt, file = "../Data/songInfo.Rdata")
  
