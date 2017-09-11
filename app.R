library(shinydashboard)
library(shiny)
library(stringi)
library(tm)
library(RWeka)
library(ANLP)
library(tokenizers)
library(tidyr)
library(data.table)
library(tm)
library(tidytext)
library(DT)

NofWords <- function(x){sum(stri_count_words(x))}

Utest = load('unigram_test.RData')
unigram <- get(Utest)
Btest = load('bigramtest_wordbyword.RData')
bigram <- get(Btest)
Ttest = load('trigramtest_wordbyword.RData')
trigram <- get(Ttest)

profanity <- readLines("en", encoding = "UTF-8")
tokenize <- function(x)
{
  x <- VCorpus(VectorSource(list(x)))
  x <- tm_map(x,content_transformer(tolower))
  x <- tm_map(x,removeNumbers)
  x <- tm_map(x,stripWhitespace)
  x <- tm_map(x,removePunctuation,preserve_intra_word_dashes = TRUE)
  x <- tm_map(x,removeWords,stopwords("en"))
  X <- tm_map(x,removeWords,profanity)
}

predictMatches <- function(sentence) {
  tokenwords <- unlist(tokenize_words(sentence, lowercase = TRUE,strip_punctuation = TRUE, stopwords = stop_words$word, simplify = FALSE))
  lastTok <- tokenwords[[length(tokenwords)]]
  DT <- bigram[bigram$word1==lastTok,]
  DT <- sort(DT$freq, decreasing = TRUE)
  return(DT)
}


predictMatches_Tri <- function(sentence,prevlevel = 0) {
  tokenwords <- unlist(tokenize_words(sentence, stopwords = stop_words$word, lowercase = TRUE,  simplify = FALSE))
  if (length(tokenwords) > 1 && length(tokenwords) > prevlevel)
  {
    if (prevlevel != 0) {
      lastTok <- tokenwords[[length(tokenwords)-prevlevel]]
      prevTok <- tokenwords[[length(tokenwords)-(prevlevel+1)]]
    }
    else {
      lastTok <- tokenwords[[length(tokenwords)]]
      prevTok <- tokenwords[[length(tokenwords)-1]]
    }
  }
  else if (length(tokenwords) == 1)
  {
    lastTok <- tokenwords[[length(tokenwords)]]
    prevTok = ""
  }
  if (length(prevTok) != 0) {
    DT <- trigram[(trigram$word1 == prevTok
                   & trigram$word2 == lastTok ),]
  }
  return(DT)
}

NextWordPredictions <-  function(sentence) {
  sentence <- tokenize_sentences(sentence, lowercase = FALSE, strip_punctuation = TRUE, simplify = FALSE)
  tokenwords <- unlist(tokenize_words(sentence, stopwords = stop_words$word, lowercase = TRUE,  simplify = FALSE))
  if (length(tokenwords) >= 2 ){
    Nextword <- predictMatches_Tri(sentence)
    Nextword <- Nextword$word3
  }
  else if (length(tokenwords) == 1){
    Nextword <- predictMatches(sentence)
    Nextword <- Nextword$word2
  }
  return(as.data.frame(Nextword))
}


ui <- fluidPage(
  titlePanel("An App to predict the next word"),
  sidebarLayout(
    sidebarPanel(
      textInput("text",label = h3("Input"), value = "type in here sentence to predict what will be the next word"),
      submitButton("Predict")
    ),
    mainPanel(
      h2(textOutput("sentence")),
      DT::dataTableOutput("predicted"),
      h2(textOutput("message"), style = "font:bold")
    )
  )
)

server <- function(input,output)
{
  getReaction <- reactive({
    return(sentence = input$text)
  })
  output$sentence <- renderText({input$text})
  output$predicted = DT::renderDataTable({print(NextWordPredictions(getReaction()))}) # output table
  #output$predicted <- renderDataTable({DT::datatable(NextWord)})
  output$message <- renderText({
    if (length(renderDataTable(NextWord)) == 0){
      result == "Sorry We are still advanced modelling the NLP technique and at this moment, I don't know what would come next." 
    }
    else {result = "Hope this is what you expected!!!"}
    return((result))
  })
}

shinyApp(ui,server)