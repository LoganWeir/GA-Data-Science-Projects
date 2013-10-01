
library(tm)

hamitup <- function (data, sparse){
  porkchops <- DataframeSource(data.frame(as.character(data)))
  bacon <- Corpus(porkchops)
  bacon <- tm_map(bacon, tolower)
  bacon <- tm_map(bacon, removePunctuation)
  bacon <- tm_map(bacon, removeNumbers)
  bacon <- tm_map(bacon, removeWords, stopwords('english'))
  ribs <- DocumentTermMatrix(bacon)
  ribs <- removeSparseTerms(ribs, sparse)
  return (as.matrix(ribs))
}


train <- read.csv ("train.csv")
names(train) <- c('id', 'title', 'full', 'lraw', 'lnorm', 'ctype', 'ctime', 
                  'com', 'cat', 'salraw', 'salnorm', 'source')

titledtm <- hamitup (train$title, .96)

fulldtm <- hamitup (train$full, .61)

lnormdtm <- hamitup (train$lnorm, .989)

catdtm <- hamitup (train$cat, .975)

fulltime <- as.numeric(grepl("full_time", train$ctype))

parttime <- as.numeric(grepl("part_time", train$ctype))

contract <- as.numeric(grepl("contract", train$ctime))

permanent <- as.numeric(grepl("permanent", train$ctime))

salnorm <- train$salnorm

trainnew <- cbind (titledtm, fulldtm, lnormdtm, catdtm, fulltime, parttime, contract, permanent, salnorm)

trainnew <- data.frame(trainnew)

trainset <- sample (1:nrow(trainnew), .7*nrow(trainnew))
train1 <- trainnew[trainset,]
test1 <- trainnew[-trainset,]

model1 <- lm(log(salnorm) ~ assistant + consultant + developer + engineer + job + manager
             + sales + senior + support + apply + client + excellent + experience
             + looking + please + role + skills + team + will + within + work + working 
             + birmingham + city + east + leeds + london + manchester + north + south + surrey
             + west + accounting + catering + customer + engineering + healthcare
             + recruitment + sales.1 + teaching + fulltime + parttime + contract + permanent, data = train1)

test.predict1 <- predict (model1, test1)

mae <- function(x,y)
{
  mean ( abs(x-y) )
}

mae (exp(test.predict1), test1$salnorm)

summary(model1)

test <- read.csv("test.csv")
names(test) <- c('id', 'title', 'full', 'lraw', 'lnorm', 'ctype', 'ctime', 'com', 'cat', 'source')

id <- test$id

test.titledtm <- hamitup (test$title, .99)

test.fulldtm <- hamitup (test$full, .7)

test.lnormdtm <- hamitup (test$lnorm, .99)

test.catdtm <- hamitup (test$cat, .99)

fulltime <- as.numeric(grepl("full_time", test$ctype))

parttime <- as.numeric(grepl("part_time", test$ctype))

contract <- as.numeric(grepl("contract", test$ctime))

permanent <- as.numeric(grepl("permanent", test$ctime))

testnew <- cbind (id, test.titledtm, test.fulldtm, test.lnormdtm, test.catdtm, 
                  test.fulltime, test.parttime, test.contract, test.permanent)

testnew <- data.frame(testnew)

prediction <- exp(predict (model1, testnew))

melt.prediction <- melt(prediction)

finalprediction <- cbind(id, melt.prediction)

write.csv (finalprediction, file="finalprediction.csv")


model.cvglmnet <- cv.glmnet (model.matrix(~train1$care + train1$engineer + train1$will + train1$experience
                                          + train1$work + train1$care + train1$role + train1$team
                                          + train1$manager + train1$nurse + train1$home + train1$senior
                                          + train1$sales + train1$within + train1$skills + train1$support
                                          + train1$developer + train1$consultant + train1$assistant
                                          + train1$london + train1$west + train1$manchester + train1$leeds
                                          + train1$belfast + train1$east + train1$south + train1$north
                                          + train1$city + train1$yorkshire + train1$healthcare + train1$nursing
                                          + train1$engineering + train1$accounting + train1$finance
                                          + train1$recruitment + train1$catering + train1$hospitality
                                          + train1$sales.1 + train1$fulltime + train1$parttime + train1$contract
                                          + train1$permanent),matrix(train1$logsalnorm))

test.predict.cvglmnet <- predict(model.cvglmnet,
                                 model.matrix(~test1$care + test1$engineer + test1$will
                                              + test1$experience + test1$work + test1$care
                                              + test1$role + test1$team + test1$manager
                                              + test1$nurse + test1$home + test1$senior
                                              + test1$sales + test1$within + test1$skills
                                              + test1$support + test1$developer + test1$consultant
                                              + test1$assistant + test1$london + test1$west + test1$manchester
                                              + test1$leeds + test1$belfast + test1$east + test1$south
                                              + test1$north + test1$city + test1$yorkshire
                                              + test1$healthcare + test1$nursing + test1$engineering
                                              + test1$accounting + test1$finance + test1$recruitment
                                              + test1$catering + test1$hospitality + test1$sales.1
                                              + test1$fulltime + test1$parttime + test1$contract
                                              + test1$permanent), s="lambda.min")

mae <- function(x,y)
{
  mean ( abs(x-y) )
}

mae (exp(test.predict.cvglmnet), exp(test1$logsalnorm))


test.matrix <- model.matrix(~testnew$engineer + testnew$will
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

train.matrix <- model.matrix(~test1$care + test1$engineer + test1$will
                             + test1$experience + test1$work + test1$care
                             + test1$role + test1$team + test1$manager
                             + test1$nurse + test1$home + test1$senior
                             + test1$sales + test1$within + test1$skills
                             + test1$support + test1$developer + test1$consultant
                             + test1$assistant + test1$london + test1$west + test1$manchester
                             + test1$leeds + test1$belfast + test1$east + test1$south
                             + test1$north + test1$city + test1$yorkshire
                             + test1$healthcare + test1$nursing + test1$engineering
                             + test1$accounting + test1$finance + test1$recruitment
                             + test1$catering + test1$hospitality + test1$sales.1
                             + test1$fulltime + test1$parttime + test1$contract
                             + test1$permanent)

here <- model.matrix(~.,data=trainnew[,c(1:40)])
#~.?
#?formula to figure out ~.