#' SENET package in R
#' This package reads text files and generates measures of text sentiments based on shortest path in the semantic network
#' @param filename path to the text file
#' @param window_size size of the n-gram
#' @param useDroplist whether to drop a list of words, default is yes
#' @param droplist path to the stopword list, if not specified, the built-in stopword list is used
#' @export
senti <- function(filename,window_size, useDroplist=TRUE, droplist=system.file("extdata", "droplist.txt", package = "senet")) {
  
  text <- unique(read.delim(filename, quote="", stringsAsFactor = FALSE,header=F, encoding="UTF-8")$V1)
  doc <- quanteda::char_tolower(text)
  doc <- gsub("\\w*[0-9]+\\w*\\s*", "",doc)
  doc <- gsub("http.*", "", doc)
  doc <- gsub("@|#|'|-|:", "", doc)
  doc <- gsub("[^[:alnum:][:blank:]?&/\\-]", "", doc)
  
  #tokenize text
  #remove punctuations, tokenize sentences
  toks <- quanteda::tokens(doc, remove_punct = TRUE, remove_url = TRUE, remove_numbers = TRUE, remove_symbols = TRUE, remove_separators = TRUE)
  
  if (useDroplist) {
    #remove stopwords 
    drop <- read.delim(droplist, stringsAsFactor = FALSE,header=F)$V1
    toks <- quanteda::tokens_select(toks, drop, selection = "remove")
  }
  
  fcmat <- quanteda::fcm(toks, context = "window", window = window_size, ordered = TRUE)

  g <- quanteda::as.igraph(fcmat,min_freq=3,omit_isolated=T)
  
  edgelist <- igraph::get.data.frame(g) 
  edgelist$count <- 1
  out <- aggregate(count~from+to,sum,data=edgelist)
  return(out)
}
