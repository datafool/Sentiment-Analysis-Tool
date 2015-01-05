library(data.table)
library(Matrix)
library(tm)
rm(list = ls())
gc()
setwd("D:/self/Kaggle")

training = fread('./train.tsv')
training[, Phrase:= gsub("[[:punct:]]","",Phrase)]
training <- training[Phrase != "",]
training <- unique(training[, Phrase:=tolower(Phrase)])
training[, PhraseIdNew := 1]
training[, PhraseIdNew := cumsum(PhraseIdNew)]
training[,PhraseId := PhraseIdNew]
# calculating this separately just in case there is something wrong with trainingmatrix
UniqueWords = data.table(
  Word = unique(unlist(strsplit(training[,paste(Phrase, collapse = ' ')], ' ')))
)
UniqueWords <- UniqueWords[Word != ""]
stopWords <- stopwords("en")
#UniqueWords[,stopWords]
setkey(UniqueWords, Word)
UniqueWords[, WordId := .I]

# Word wordcount by each phrase
trainingdatatable <- training[,
                              list(Word = unlist(strsplit(Phrase,' '))), 
                              by = list(PhraseId,SentenceId,Sentiment)
                              ][,
                                .N, 
                                by = list(PhraseId,SentenceId,Sentiment,Word)
                                ]

trainingdatatable <- trainingdatatable[Word !=""]
#trainingdatatable[, Key :=1]
trainingdatatable = merge(trainingdatatable, UniqueWords, by = c('Word'), all = T)
setkey(trainingdatatable)
trainingdatatable <- unique(trainingdatatable)

#### Testing Data
testing = fread('./test.tsv')
#testing[, Phrase:= gsub("[[:punct:]]","",Phrase)]
#testing <- testing[Phrase != "",]
#testing <- unique(testing[, Phrase:=tolower(Phrase)])
testing[, PhraseIdNew := 1]
testing[, PhraseIdNew := cumsum(PhraseIdNew)]
testing[,PhraseId := PhraseIdNew]


# Word wordcount by each phrase
testingdatatable <- testing[,
                              list(Word = unlist(strsplit(Phrase,' '))), 
                              by = list(PhraseId,SentenceId)
                              ][,
                                .N, 
                                by = list(PhraseId,SentenceId,Word)
                                ]

testingdatatable <- testingdatatable[Word !=""]
#trainingdatatable[, Key :=1]
testingdatatable = merge(testingdatatable, UniqueWords, by = c('Word'), all = T)
setkey(testingdatatable)
testingdatatable <- unique(testingdatatable)

testingdatatable[!is.na(WordId) & is.na(PhraseId),':=' (PhraseId = 1, SentenceId = 1, N = 0)]
                                      
testingdatatable=testingdatatable[!is.na(WordId)]
# sparse matrix
testingmatrix_X <- sparseMatrix(
  testingdatatable[,PhraseId], 
  testingdatatable[,WordId], 
  x = testingdatatable[,N]
)


setkeyv(trainingdatatable,c("PhraseId", "Sentiment"))
trainingmatrix_y <- matrix(training[, Sentiment])


save(testingmatrix_X, file ="./TestingMatrix_X.Rda")
save(trainingmatrix_y, file ="./trainingMatrix_y.Rda")
# load("./trainingMatrix_X.R")
# m <- dim(trainingmatrix_X)[1]
# n <- dim(trainingmatrix_X)[2]
# set.seed(m*.6)
# index <- seq(1,m,1)
# 
# trainingDataIndex <- sample(seq(from = 1, to = m, by = 1), size = m*.6, replace = F)
# trainingData_x <-trainingmatrix_X[trainingDataIndex,]
# trainingData_y <- trainingmatrix_y[trainingDataIndex,]
# save(trainingData_x, file = "./trainingData_X.Rda")
# save(trainingData_y,file ="./trainingData_y.Rda")
# 
# trainingmatrix_X <-trainingmatrix_X[!trainingDataIndex,]
# 
