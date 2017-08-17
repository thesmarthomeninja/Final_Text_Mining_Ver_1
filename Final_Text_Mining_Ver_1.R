#This is very simple...  Take your transcribed phone calls rtf, txt or whatever format 
#they are in and first convert them all to plain text or text that you want to analyze.  
#Pay attention here...  Take that plain text (.txt) file and PUT IT IN A NEW FOLDER CALLED TEXTS DIRECTLY
# ON YOUR C: DRIVE.  SO put your plain text file in C:\Texts (so c:\texts\yourtextfile.txt)  Store it there...
#Then install the packages below tm (for text mining), wordcloud (for worldcloud. pew pew!) ggplot (for your cool graph/bar diagram)
#And Cluster (for your cluster diagram).  THen grab this whole script below starting at "cname <-" and run
#it all in R and boom.  you're done :)

cname <- file.path("C:", "texts")   
cname   
dir(cname)   

# **Load the R package for text mining and then load your texts into R.**
library(tm)
docs <- VCorpus(DirSource(cname))   
## Preprocessing      
docs <- tm_map(docs,removePunctuation)   
docs <- tm_map(docs, removeNumbers)   
docs <- tm_map(docs, tolower)   
docs <- tm_map(docs, removeWords, stopwords("english")) 
docs <- tm_map(docs, removeWords, c("yeah","one","thirty","hundred","card","see","ill","actually","whats","name","two","nine","ten","eleven","three","going","number","five","like","four","seven","eight","zero","right","need","youre","help","perfect","just","said","called","maybe","first","lets","everything","cant","want","theyre","much","kind","line","take","send","want","cant","probably","sorry","great","make","another","send","sorry","think","something","line","thanks","trying","seventy","give","week","doesnt","correct","thing","thanks","things","stuff","guys","office","trying","can","looks","back","way","thing","know","good","thats","sure","days","pretty","find","work","long","bye","even","six","twenty","seventy","guys","try","able","might","ive","lot","way","put","new","post","day","forty","already","let","get","got","mean","yes","look","also","guess","twelve","fourteen","ones","say","didnt","sixty","iits","fifteen","theres","dot","use","hes","still","next","ninety","dont","thank","let","will","within","well","case"))

docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, PlainTextDocument)
# *This is the end of the preprocessing stage.*   


### Stage the Data      
dtm <- DocumentTermMatrix(docs)   
tdm <- TermDocumentMatrix(docs)   

### Explore your data      
freq <- colSums(as.matrix(dtm))   
length(freq)   
ord <- order(freq)   
m <- as.matrix(dtm)   
dim(m)   
write.csv(m, file="DocumentTermMatrix.csv")   
### FOCUS - on just the interesting stuff...   
#  Start by removing sparse terms:   
dtms <- removeSparseTerms(dtm, 0.1) # This makes a matrix that is 10% empty space, maximum.   
### Word Frequency   
head(table(freq), 20)   
# The above output is two rows of numbers. The top number is the frequency with which 
# words appear and the bottom number reflects how many words appear that frequently. 
#
tail(table(freq), 20)   
# Considering only the 20 greatest frequencies
#
# **View a table of the terms after removing sparse terms, as above.
freq <- colSums(as.matrix(dtms))   
freq   
# The above matrix was created using a data transformation we made earlier. 
# **An alternate view of term frequency:**   
# This will identify all terms that appear frequently (in this case, 50 or more times).   
findFreqTerms(dtm, lowfreq=10)   # Change "50" to whatever is most appropriate for your data.
#
#
#   
### Plot Word Frequencies
# **Plot words that appear at least 50 times.**   
library(ggplot2)   
wf <- data.frame(word=names(freq), freq=freq)   
p <- ggplot(subset(wf, freq>8), aes(word, freq))    
p <- p + geom_bar(stat="identity")   
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))   
p   
#  
## Relationships Between Terms
### Term Correlations
# See the description above for more guidance with correlations.
# If words always appear together, then correlation=1.0.    
findAssocs(dtm, c("order" , "shipping"), corlimit=0.85) # specifying a correlation limit of 0.85
findAssocs(dtms, "ship", corlimit=0.95) # specifying a correlation limit of 0.95   
# 
# Change "country" & "american", or "think" to terms that actually appear in your texts.
# Also adjust the `corlimit= ` to any value you feel is necessary.
#
# 
### Word Clouds!   
# First load the package that makes word clouds in R.    
library(wordcloud)   
dtms <- removeSparseTerms(dtm, 0.15) # Prepare the data (max 15% empty space)   
freq <- colSums(as.matrix(dtm)) # Find word frequencies   
dark2 <- brewer.pal(6, "Dark2")   
wordcloud(names(freq), freq, max.words=500, rot.per=0.2, colors=dark2)    

### Clustering by Term Similarity

### Hierarchal Clustering   
dtms <- removeSparseTerms(dtm, 0.15) # This makes a matrix that is only 15% empty space.
library(cluster)   
d <- dist(t(dtms), method="euclidian")   # First calculate distance between words
fit <- hclust(d=d, method="complete")    # Also try: method="ward.D"   
plot.new()
plot(fit, hang=-1)
groups <- cutree(fit, k=10)   # "k=" defines the number of clusters you are using   
rect.hclust(fit, k=10, border="red") # draw dendogram with red borders around the 5 clusters   