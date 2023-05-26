library(geniusr)
library(tidyverse)
library(tidytext)

genius_token()

artist_id <- search_artist("Caparezza")$artist_id
artist_songs <- get_artist_songs(artist_id = artist_id)

songs_id <- c()
for (song in artist_songs$content) {
  song_id <- song$id
  songs_id <- songs_id %>% append(song_id)
}

song_titles <- c()
song_albums <- c()
for (song_id in songs_id) {
  song <- get_song(song_id)$content
  song_title <- song$title
  song_album <- song$album$name
  # some songs have no album associated with them, so we won't consider them
  if (!is.null(song_album)){
    song_titles <- song_titles %>% append(song_title)
    song_albums <- song_albums %>% append(song_album)
  }
}

songs <- data.frame(title = song_titles, album = song_albums)
songs %>% head()

# we are going to ignore alternative versions of the same song, such as
# live versions, radio edits and remixes
# we keep "demo" versions because they are sufficiently different from the final
# version, knowing Caparezza's discography
test <- songs %>% filter(!grepl("Live|Radio Edit|Radio Version|Remix|RMX", title))
