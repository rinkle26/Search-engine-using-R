#R PROJECT 2
#NAME - NETIDs
#Rinkle Seth - rcs170004
#Harshel Jain -hxj170009
#Pancham -pxm172730
#Suddeshna-sxb170035

#------------------------------------------------------------------------------------------
#Loading the libraries
require(tidyverse)
require(tm)
require(dplyr)
require(splus2R)

data <- read.table("http://www.utdallas.edu/~pxm172730/Data/plot_summaries.txt", header = FALSE, sep = "\t", quote = "")
movie_names<-read.csv("https://raw.githubusercontent.com/rinkle26/RData/master/new_name.csv",header=FALSE)

#-----------------------------------------------------------
#considering 2000 documents
df<- head(data, 2000)
df_title <- data.frame(doc_id = df[,1], text = df[,2])
#creating a corpus of documents
corpus <- VCorpus(DataframeSource(df_title))
docList <- list()
for (i in 1:length(corpus)) {
  docList[[i]] <- content(corpus[[i]])
}
Ndocs <- length(docList)
#naming the documents
names(docList) <- df[,1]

#preprocessing data- removal of srop words, stemming, etc----------------------
preprocess_corpus <- function(corpus) {
  #remove punctuation and stop words
  corpus <- tm_map(corpus, removeWords, stopwords('english'))
  corpus <- tm_map(corpus, removePunctuation)
  
  #remove numbers, uppercase, additional spaces
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, stripWhitespace)

}

#constructing the Vector Space Model------------------------------------
get.tf.idf.weights <- function(tf.vec) {
  # Compute tfidf weights from term frequency vector
  n.docs <- length(tf.vec)
  doc.frequency <- length(tf.vec[tf.vec > 0])
  weights <- rep(0, length(tf.vec))
  weights[tf.vec > 0] <- (1 + log2(tf.vec[tf.vec > 0])) * log2(n.docs/doc.frequency)
  return(weights)
}

#function for single word queries- tfidf matrix---------------------------
single_query<-function(query){
  #create the search query
  #query <- "criminal"
  #query <- "movies starring Brad Pitt"
  myDocs <- VectorSource(c(docList))
  myDocs$Names <- c(names(docList))
  
  corpus <- VCorpus(myDocs)
  df_preproc <- preprocess_corpus(corpus)
  #create document matrix in a format that is efficient
  termDocMatrixStm <- TermDocumentMatrix(df_preproc)
  colnames(termDocMatrixStm) <- c(names(docList))
  tfidf.matrix <- t(apply(termDocMatrixStm, 1,
                          FUN = function(row) {get.tf.idf.weights(row)}))
  colnames(tfidf.matrix) <- colnames(termDocMatrixStm)
  tfidf.matrix[0:3, ]
  data <- tfidf.matrix[query, ]
  result <- data.frame(doc = names(data), score = data, row.names = NULL)
  
  l<-c()
  for (i in names(docList))
  {
    y = as.String(movie_names[which(movie_names$V1==i),3])
    l<-c(l,y)
    
  }
  View(l)
  
  sum<-sum(result$score)
  
  #if input given is gibberish
  if (sum==0)
    print("invalid entry")
  else
   { result.df<-cbind(result,l)
   #sorting the result data frame
  result_sorted <- head(result.df[order(result.df$score, decreasing=TRUE),], n=10)
  print(result_sorted)
   }
  
}

#function for multi-word queries-------------------------------------------------
processQuery<-function(query){
  #create the search query
  #query <- "criminal"
  #query <- "movies starring Brad Pitt"
  myDocs <- VectorSource(c(docList, query))
  myDocs$Names <- c(names(docList), "query")
  
  corpus <- VCorpus(myDocs)
  df_preproc <- preprocess_corpus(corpus)
  #create document matrix in a format that is efficient
  termDocMatrixStm <- TermDocumentMatrix(df_preproc)
  colnames(termDocMatrixStm) <- c(names(docList), "query")
  #inspect(termDocMatrixStm[0:length(myDocs), ])
tfidf.matrix <- t(apply(termDocMatrixStm, 1,
                        FUN = function(row) {get.tf.idf.weights(row)}))
colnames(tfidf.matrix) <- colnames(termDocMatrixStm)
tfidf.matrix[0:3, ]


query.vector <- tfidf.matrix[, (Ndocs + 1)]
tfidf.matrix <- tfidf.matrix[, 1:Ndocs]

doc.scores <- t(query.vector) %*% tfidf.matrix

results.df <- data.frame(doc = names(docList), score = t(doc.scores),
                         text = unlist(docList))
j=0
#new_df
l<-c()
for (i in names(docList))
{
  y = as.String(movie_names[which(movie_names$X1==i),3])
  l<-c(l,y)

}
#View(l)
sum<-sum(results.df$score)
if (sum==0)
  print("invalid entry")
else
  {
    results.df<-cbind(results.df,l)
    
  results.df <- results.df[order(results.df$score, decreasing = TRUE), ]

  results<- results.df[,c(1,2,3,4)]
  print(head(results, n=10), row.names = FALSE, right = FALSE, digits = 2)
  }
 #View(results)
}

# Function that prompts user to input query and returns search results (matching documents)
search_engine_query_processor <- function(tfidf.matrix) {
  while(TRUE) {
    start_time = Sys.time()
    query <- readline(prompt = "Input query (or press q to terminate the program): ")
    query <- lowerCase(query)
    # Terminate the program if user presses q
    if(query == 'q') {
      #stopifnot(query != "q")
      opt <- options(show.error.messages = FALSE)
      on.exit(options(opt))
      stop('Program Terminated')
    } else {
      print("Looking for matching documents...")
      temp<-sapply(strsplit(query, " "), length)
      if(temp==1){
        #compute tf-idf matrix and returning top 10 documents
        single_query(query)
      }
      else
      {
        #computing cosine similarity between query and documents
        processQuery(query) 
      }
      
    }
    end_time = Sys.time()
    total_time = round(end_time-start_time,4)
    #printing the time required for execution
    print(paste("Used",total_time," minutes"))
  }
}
search_engine_query_processor(term.doc.matrix)

