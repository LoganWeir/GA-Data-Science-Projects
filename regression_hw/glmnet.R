
library(tm)

hamitup <- function (data, sparse){
porkchops <- DataframeSource(data.frame(as.character(data)))
bacon <- Corpus(porkchops)
bacon <- tm_map(bacon, tolower)
bacon <- tm_map(bacon, removePunctuation)
bacon <- tm_map(bacon, removeNumbers)
bacon <- tm_map(bacon, removeWords, stopwords('english'))
#bacon <- tm_map(bacon, bounds = list(local = c(1,10)))??? 10 = upper bound
#stripwhitespace!
ribs <- DocumentTermMatrix(bacon)
ribs <- removeSparseTerms(ribs, sparse)
return (as.matrix(ribs))
}

train <- read.csv ("train.csv")

names(train) <- c('id', 'title', 'full', 'lraw', 'lnorm', 'ctype', 'ctime','com', 'cat', 'salraw', 'salnorm', 'source')

titledtm <- hamitup (train$title, .97)

fulldtm <- hamitup (train$full, .66)

lnormdtm <- hamitup (train$lnorm, .99)

catdtm <- hamitup (train$cat, .98)

fulltime <- grepl("full_time", train$ctype)

parttime <- grepl("part_time", train$ctype)

contract <- grepl("contract", train$ctime)

permanent <- grepl("permanent", train$ctime)

logsalnorm <- log(train$salnorm)

trainnew <- cbind (titledtm, fulldtm, lnormdtm, catdtm, fulltime, parttime, contract, permanent, logsalnorm)

trainnew <- data.frame(trainnew)

trainset <- sample (1:nrow(trainnew), .7*nrow(trainnew))
train1 <- trainnew[trainset,]
test1 <- trainnew[-trainset,]

trainnew.matrix <- model.matrix(~trainnew$engineer + trainnew$will + trainnew$experience
                                + trainnew$work + trainnew$care + trainnew$role + trainnew$team
                                + trainnew$manager + trainnew$nurse + trainnew$senior
                                + trainnew$sales + trainnew$within + trainnew$skills + trainnew$support
                                + trainnew$developer + trainnew$consultant + trainnew$assistant
                                + trainnew$london + trainnew$west + trainnew$manchester + trainnew$leeds
                                + trainnew$east + trainnew$south + trainnew$north
                                + trainnew$city + trainnew$healthcare + trainnew$nursing
                                + trainnew$engineering + trainnew$accounting + trainnew$finance
                                + trainnew$recruitment + trainnew$catering + trainnew$hospitality
                                + trainnew$sales.1 + trainnew$fulltime + trainnew$parttime + trainnew$contract
                                + trainnew$permanent)

library(glmnet)

model.cvglmnet <- cv.glmnet (trainnew.matrix,matrix(trainnew$logsalnorm))

test <- read.csv("test.csv")

names(test) <- c('id', 'title', 'full', 'lraw', 'lnorm', 'ctype', 'ctime', 'com', 'cat', 'source')

id <- test$id

test.titledtm <- hamitup (test$title, .992)

test.fulldtm <- hamitup (test$full, .8)

test.lnormdtm <- hamitup (test$lnorm, .99)

test.catdtm <- hamitup (test$cat, .99)

fulltime <- grepl("full_time", test$ctype)

parttime <- grepl("part_time", test$ctype)

contract <- grepl("contract", test$ctime)

permanent <- grepl("permanent", test$ctime)

testnew <- cbind (id, test.titledtm, test.fulldtm, test.lnormdtm, test.catdtm, fulltime, parttime, contract, permanent)

testnew <- data.frame(testnew)

testnew.matrix <- model.matrix(~testnew$engineer + testnew$will
                            + testnew$experience + testnew$work + testnew$care
                            + testnew$role + testnew$team + testnew$manager
                            + testnew$nurse + testnew$senior
                            + testnew$sales + testnew$within + testnew$skills
                            + testnew$support + testnew$developer + testnew$consultant
                            + testnew$assistant + testnew$london + testnew$west
                            + testnew$manchester
                            + testnew$leeds + testnew$east + testnew$south
                            + testnew$north + testnew$city
                            + testnew$healthcare + testnew$nursing + testnew$engineering
                            + testnew$accounting + testnew$finance + testnew$recruitment
                            + testnew$catering + testnew$hospitality + testnew$sales.1
                            + testnew$fulltime + testnew$parttime + testnew$contract
                            + testnew$permanent)

prediction <- as.vector(exp(predict (model.cvglmnet, testnew.matrix, s="lambda.min")))

finalprediction <- cbind(id, prediction)

write.csv (finalprediction, file="finalprediction_cv.csv", row.names= F)

#!!!!! Interaction effect!!! permanent:accounting (or whatever) -  how to choose?!?!?!?