library(geniusr)
library(tidyverse)
library(tidytext)
library(quanteda)
library(udpipe)
library(wordcloud)

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
  if (!is.null(song_album) & nrow(songs_lyrics) != 0){
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

# fct_inorder preserves original order of the column
lemmatized_lyrics <- outL %>% group_by(doc_id = fct_inorder(doc_id)) %>%
  summarise(txtL = paste(lemma, collapse = " "))
songs$lemmatized <- lemmatized_lyrics$txtL

caparezza_corpus <- songs$lemmatized %>% corpus(docnames = songs$id)

DTM <- dfm(tokens(caparezza_corpus))
DTM

DTM %>% dim()

freqs <- colSums(DTM)
words <- colnames(DTM)
wordlist <- data.frame(words, freqs)
wordlist %>% arrange(-freqs) %>% head()

corpus_size <- sum(wordlist$freqs)
corpus_size

vocabulary_size <- nrow(wordlist)
vocabulary_size

words_occurrencies <- wordlist %>% group_by(freqs) %>% summarise(vK = n()) %>% arrange(-vK)
words_occurrencies

lexicon_width <- vocabulary_size/corpus_size
lexicon_width
language_refinement <- words_occurrencies$vK[1] / colSums(words_occurrencies)[2]
language_refinement


wordlist
wordcloud(words = wordlist$words, freq = wordlist$freqs, scale = c(3.5, 0.35), max.words = 75, random.order = F,
          colors = RColorBrewer::brewer.pal(name = "Dark2", n = 4))
text(0.5, 1, "wordcloud with TF ponderation", font = 2)



# mamma mia mammà peso


count <- 1
for(i in songs$lyrics){
  if(grepl("don't", i)){
    print(songs$title[[count]])
    readline()
  }
  count <- count + 1
}




outLtest <- outL %>% filter(upos %in% c("NOUN", "PROPN", "ADJ", "VERB", "ADV"))
lemmatized_lyricstest <- outLtest %>% group_by(doc_id = fct_inorder(doc_id)) %>%
  summarise(txtL = paste(lemma, collapse = " "))
songstest <- songs
songstest$lemmatized <- lemmatized_lyricstest$txtL

caparezza_corpus <- songs$lemmatized %>% corpus(docnames = songs$id)

DTM <- dfm(tokens(caparezza_corpus))
DTM

DTM %>% dim()

freqs <- colSums(DTM)
words <- colnames(DTM)
wordlist <- data.frame(words, freqs)
wordlist %>% arrange(-freqs) %>% head()