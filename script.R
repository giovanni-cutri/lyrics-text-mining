library(geniusr)
library(tidyverse)
library(tidytext)

genius_token()

artist_id <- search_artist("Caparezza")$artist_id
songs <- get_artist_songs(artist_id = artist_id)

songs_id <- c()
for (song in songs$content){
  song_id <- song$id
  songs_id <- append(songs_id, song_id)
}




ids <- songs$content$id
for (id in ids){
  
}