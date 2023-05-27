library(geniusr)
library(tidyverse)
library(tidytext)
library(quanteda)
library(udpipe)

genius_token()

artist_id <- search_artist("Caparezza")$artist_id
artist_songs <- get_artist_songs(artist_id)

songs_ids <- c()
for (song in artist_songs$content) {
  song_id <- song$id
  songs_ids <- songs_ids %>% append(song_id)
}

songs_titles <- c()
songs_albums <- c()
songs_lyrics <- c()
for (i in 1:length(songs_ids)) {
  cat("Getting", i, "of", length(songs_ids), "id:", songs_ids[i], "\n")
  song <- get_song(songs_ids[i])$content
  song_title <- song$title
  song_album <- song$album$name
  song_lyrics <- list(get_lyrics_id(songs_ids[i]))
  # some songs have no album associated with them, so we won't consider them
  # additionally, some songs have no lyrics, we are excluding them too
  if (!is.null(song_album) & songs_lyrics != ""){
    songs_titles <- songs_titles %>% append(song_title)
    songs_albums <- songs_albums %>% append(song_album)
    songs_lyrics <- songs_lyrics %>% append(song_lyrics) 
  }
}

# collapse all lyrics lines into a single text
for (i in 1:length(songs_lyrics)){
  songs_lyrics[[i]] <- songs_lyrics[[i]]$line %>% paste(collapse = "\n")
}

songs_lyrics <- unlist(songs_lyrics)

songs <- data.frame(title = songs_titles, album = songs_albums, lyrics = songs_lyrics)
songs %>% head(1)

# we are going to ignore alternative versions of the same song, such as
# live versions, radio edits and remixes
# we keep "demo" versions because they are sufficiently different from the final
# version, knowing Caparezza's discography
songs <- songs %>% filter(!grepl("Live|Radio Edit|Radio Version|Remix|RMX", title))

# add id to each song
songs <- songs %>% mutate(id = 1:nrow(songs), .before = 1)

table(songs$album) %>% as.data.frame() %>% arrange(-Freq)

caparezza_corpus <- corpus(songs$lyrics, docnames = songs$id)
summary(caparezza_corpus)

cat(as.character(caparezza_corpus[1]))

corpus_tokens <- caparezza_corpus %>%
  quanteda::tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>%
  tokens_tolower()

txt <- sapply(corpus_tokens, FUN=function(x) paste(x, collapse = "\n"))
udpipe_download_model(language = "italian-isdt")
lang_model <- udpipe_load_model(file = "italian-isdt-ud-2.5-191206.udpipe")
outL <- udpipe_annotate(lang_model, x = txt, tokenizer = "vertical", trace = TRUE) %>%
  as.data.frame()
it_stopwords <- readLines("https://raw.githubusercontent.com/stopwords-iso/stopwords-it/master/stopwords-it.txt")
outL <- outL %>% filter(!(token %in% it_stopwords) & !(lemma %in% it_stopwords))

outL %>% select(doc_id, token, lemma, upos) %>% sample_n(5)

lemmatized_lyrics <- outL %>% group_by(doc_id) %>% summarise(txtL = paste(lemma, collapse = " "))
test$lemmatized <- lemmatized_lyrics$txtL



test <- ? %>% dfm() 

test


freqs <- colSums(test)
words <- colnames(test)
wordlist <- data.frame(words, freqs)
wordlist %>% arrange(-freqs) %>% head(50)

copy[[1]] <- lemmatize_words(copy[[1]])

copy[[1]]
lemmatize_strings(copy[[1]])
copy2 <- tm_map(copy, lemmatize_strings)







corpus_tokens <- caparezza_corpus %>%
  tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>%
  tokens_tolower() %>%
  tokens_replace()

DTM <- dfm(tokens(caparezza_corpus))
DTM

DTM %>% dim()

freqs <- colSums(DTM)
words <- colnames(DTM)
wordlist <- data.frame(words, freqs)
wordlist %>% arrange(-freqs) %>% head()
