### Function

f<-function(x){
  c(3*(x-1)+1, 3*(x-1)+2,3*x)}

stemCompletion2 <- function(x, dictionary) {
  x <- unlist(strsplit(as.character(x), " "))
  # Unexpectedly, stemCompletion completes an empty string to
  # a word in dictionary. Remove empty string to avoid above issue.
  x <- x[x != ""]
  x <- stemCompletion(x, dictionary=dictionary)
  x <- paste(x, sep="", collapse=" ")
  PlainTextDocument(stripWhitespace(x))
}

process<-function(df){
  collapse<-apply(df[,-1],1,paste,collapse=" ")
  collapse<-data.frame(Assoc=collapse) #cue=df$cue,
  
  # Transform text column to character
  collapse$Assoc<-as.character(collapse$Assoc)
  # Delete NA participants; Nothing changed
  #collapse<-na.omit(collapse) #dim(collapse) [1] 73  2
  
  # Clean
  require("tm")
  text_corpus<-Corpus(VectorSource(collapse$Assoc))
  clean<-tm_map(text_corpus,tolower)
  clean<-tm_map(clean,removeNumbers)
  clean<-tm_map(clean,removeWords,stopwords())
  clean<-tm_map(clean,stripWhitespace)
  clean<-tm_map(clean,PlainTextDocument)
  dic<-clean
  clean<-tm_map(clean, stemDocument)
  
  clean <- lapply(clean, stemCompletion2, dictionary=dic)
  clean <- Corpus(VectorSource(clean))
  
  
  dtm <- DocumentTermMatrix(clean)
  #uniwords<-dtm$dimnames$Terms
  #length(unique(uniwords))
  
  freq_dict<-findFreqTerms(dtm,1)
  freq <- DocumentTermMatrix(clean,list(dictionary=freq_dict))
  colnames<-freq$dimnames$Terms
  
  freq<-matrix(freq,nrow= length(cues),ncol=length(colnames),dimnames=list(cues,colnames) )
  freq<-t(freq)
}

histoplot<-function(freq){
  freq[freq>=1] <- 1
  freqt<-t(freq)
  
  freqDF<-data.frame(freqt)
  freqDF<-data.frame(cue=1:73,freqDF)
  freqDF<-mutate(freqDF, response_number=apply(freqDF[,-1],1,sum))
  freqDF$response_number
  freqDF<-freqDF[,c("cue","response_number")]
  dev.off()
  par(mar=c(4,4,2,2))
  par(mfrow=c(1,2))
  hist(freqDF$response_number,main="Histogram of unique response number", xlab="No. of Associations", ylab="frequency",col = 'lightblue', border="black")
  qqnorm(freqDF$response_number, col="blue") ;qqline(freqDF$response_number, col=2)
}

corPlot<-function(freq){
  par(mar=c(2,2,2,2))
  corrplot(cor(freq, use="pairwise.complete.obs"), method="shade",shade.col=NA, tl.col="black", tl.cex=0.4, tl.srt=90)  
}

MST<-function(freq, title){
  dis <- vegdist(t(freq))
  tr <- spantree(dis)
  ## Add tree to a metric scaling 
  #plot(tr, cmdscale(dis), type = "t")
  ## Find a configuration to display the tree neatly
  plot(tr, type = "t",col="red",bg="yellow", pch=1,main=title,cex=0.5)
}

AssocNetPlot<-function(freq,title){
  corTable<-cor(freq)
  diag(corTable) <- 0
  g <- graph.adjacency(corTable, weighted=T, mode = "undirected")
  # Remove edge that have correlation less than 0.2
  g <- delete.edges(g, E(g)[ weight < 0.2 ])
  #freq# remove loops
  g <- simplify(g)
  # set labels and degrees of vertices
  V(g)$label <- V(g)$name
  V(g)$degree <- degree(g)
  # color
  COLS <- ifelse(E(g)$weight >= 0.5, "firebrick2", 
                 ifelse(E(g)$weight>=0.4, "blue",
                        ifelse(E(g)>=0.3, "grey58","grey58")))
  E(g)$color <- COLS
  
  
  par(mar=c(4,4,4,4))
  
  plot(g,vertex.size=3,vertex.label=NA,main=title) #vertex.lable=NA
}

graph<-function(freq){
  corTable<-cor(freq)
  diag(corTable) <- 0
  g <- graph.adjacency(corTable, weighted=T, mode = "undirected")
  # Remove edge that have correlation less than 0.2
  g <- delete.edges(g, E(g)[ weight < 0.2 ])
  #freq# remove loops
  g <- simplify(g)
  # set labels and degrees of vertices
  V(g)$label <- V(g)$name
  V(g)$degree <- degree(g)
  #return g
  g
}

cloud<-function(Hfreq){
  Hword.freq <- sort(rowSums(Hfreq), decreasing = T)
  wordcloud(words = names(Hword.freq), freq = Hword.freq, scale=c(2,.2), min.freq=2,
            max.words=55, random.order=FALSE,
            random.color=F,colors=brewer.pal(8, "Dark2"))
}
###################### correlation cal for each word #############

transform<-function(word){
  wordt<-word[,-1]
  wordt<-t(wordt)
  people_word<-data.frame(id=1:33,text=as.character(rep(NA,33)))
  for (i in 1:33){
    x<-paste(wordt[f(i)[1], ], wordt[f(i)[2], ], wordt[f(i)[3], ])
    x<-as.list(x)
    df<-do.call("rbind",x)
    df<-as.character(df)
    people_word[i,2]<-paste(df,collapse=" ")
    people_word$text<-as.character(people_word$text)
    people_word[,1]<-as.character(people_word[,1])
  }
  # First participants wiredly returned NA value, manually fix it:
  x<-paste(wordt[f(1)[1], ], wordt[f(1)[2], ], wordt[f(1)[3], ]); x<-as.list(x); df<-do.call("rbind",x); df<-as.character(df); people_word[1,2]<-paste(df,collapse=" "); people_word$text<-as.character(people_word$text)
  
  people_word
  
}

processX<-function(df,minnumber){

  require("tm")
  text_corpus<-Corpus(VectorSource(df$text))
  clean<-tm_map(text_corpus,tolower)
  clean<-tm_map(clean,removeNumbers)
  clean<-tm_map(clean,removeWords,stopwords())
  clean<-tm_map(clean,stripWhitespace)
  clean<-tm_map(clean,PlainTextDocument)
  dic<-clean
  clean<-tm_map(clean, stemDocument)
  
  clean <- lapply(clean, stemCompletion2, dictionary=dic)
  clean <- Corpus(VectorSource(clean))
  
  
  dtm <- DocumentTermMatrix(clean)
  #uniwords<-dtm$dimnames$Terms
  #length(unique(uniwords))
  
  freq_dict<-findFreqTerms(dtm,minnumber)
  freq <- DocumentTermMatrix(clean,list(dictionary=freq_dict))
  colnames<-freq$dimnames$Terms
  rownames<-freq$dimnames$Docs
  freq<-matrix(freq,nrow= length(rownames),ncol=length(colnames),dimnames=list(rownames,colnames) )
  freq
}


