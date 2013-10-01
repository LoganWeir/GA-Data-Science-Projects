train <- read.csv ("train.csv")
names(train) <- c('id', 'title', 'full', 'lraw', 'lnorm', 'ctype', 'ctime', 'com', 'cat', 'salraw', 'salnorm', 'source')

library(tm)
process <- function(charvec, minOccurs) {
  corpus <- Corpus(VectorSource(charvec))
  corpus <- tm_map(corpus, tolower)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removeWords, stopwords('english'))
  counts <- rowSums(as.matrix(TermDocumentMatrix(corpus)))
  counts <- subset(counts, counts > minOccurs)
  return(data.frame(words=names(counts), counts))
}

hamtest <- process (test1$full, 50)
hamorder <- hamtest[order(hamtest$counts, decreasing = T),]

hamitup <- function (data, sparse){
  wut <- DataframeSource(data.frame(as.character(data)))
  lolwut <- Corpus(wut)
  lolwut <- tm_map(lolwut, tolower)
  lolwut <- tm_map(lolwut, removePunctuation)
  lolwut <- tm_map(lolwut, removeNumbers)
  lolwut <- tm_map(lolwut, removeWords, stopwords('english'))
  dafuq <- DocumentTermMatrix(lolwut)
  dafuq <- removeSparseTerms(dafuq, sparse)
  return (as.matrix(dafuq))
}

wut <- DataframeSource(data.frame(as.character(train$FullDescription)))
lolwut <- Corpus(wut)
lolwut <- tm_map(lolwut, tolower)
lolwut <- tm_map(lolwut, removePunctuation)
lolwut <- tm_map(lolwut, removeNumbers)
lolwut <- tm_map(lolwut, removeWords, stopwords('english'))
dafuq <- DocumentTermMatrix(lolwut)
dafuq <- removeSparseTerms(dafuq, .55)
supson <- as.matrix(dafuq)
#.55 for fulldesc
#.98 for title

#also:
inspect(removeSparseTerms(dafuq, .55))

train$logsal <- log(train$salnorm)
train$fulltime <- grepl("full_time", train$ctype)
train$parttime <- grepl("part_time", train$ctype)
train$contract <- grepl("contract", train$ctime)
train$permanent <- grepl("permanent", train$ctime)
train$will <- grepl("will", train$full)
train$experience <- grepl("experience", train$full)
train$work <- grepl("work", train$full)
train$care <- grepl("care", train$full)
train$role <- grepl("role", train$full)
train$engineer <- grepl("Engineer", train$title, ignore.case=T)
train$nurse <- grepl("Nurse", train$title, ignore.case=T)
train$home <- grepl("Home", train$title, ignore.case=T)
train$manager <- grepl("Manager", train$title, ignore.case=T)
train$titlecare <- grepl("Care", train$title, ignore.case=T)
train$hn <- grepl("Healthcare & Nursing Jobs", train$cat)
train$engine <- grepl("Engineering Jobs", train$cat)
train$hrr <- grepl("HR & Recruitment Jobs", train$cat)
train$hcj <- grepl("Hospitality & Catering Jobs", train$cat)
train$itjobs <- grepl("IT Jobs", train$cat)


#cat.counts <- summary(train$cat)
#top.cat <- names(cat.counts[order(cat.counts, decreasing = T)][1:20])
#train$catten <- factor(train$cat, levels=top.cat)
#train$catten[is.na(train$catten)] <- "Consultancy Jobs"

#lnorm.counts <- summary(train$lnorm)
#top.lnorm <- names(lnorm.counts[order(lnorm.counts, decreasing = T)][1:20])
#train$lnormten <- factor(train$lnorm, levels=top.lnorm)
#can add ("ther stuff", "pther other stuff") for non factors
#train$lnormten[is.na(train$lnormten)] <- "(Other)"


#wut <- model.matrix(~train1$cat)
#wazzap <- model.matrix(~train1$lnorm)

wut <- model.matrix(~train$source)