library(snowfall)
library(jsonlite)
articles <- fromJSON("articles.json", flatten=TRUE)
# Get all the text bodies as vec from the articles dataframe
texts.vec = articles$text #run on 1000 entries first
i <- seq(1,964, 1)
names(texts.vec) = i[1:964]
texts.vec[1]
texts.vec[964]
# Get source functions
source("MakeDtmFunctions.R")
source("TopicModelingFunctions.R")

dtm = Vec2Dtm(vec = texts.vec)
dtm = DepluralizeDtm(dtm = dtm)
dim(dtm)

tf.mat = data.frame(term = colnames(dtm), tf = colSums(dtm), df = colSums(dtm>0), stringsAsFactors = F)
#^ could add idf column
keep.terms = tf.mat$term[tf.mat$df>=5 & tf.mat$df < nrow(dtm)/2]
# Func to get rid of words less than length 4
library(stringr)
getRidOfLessThan4 = function(terms){
  ret = c()
  for(i in terms){
    if(str_length(i) > 4){
      ret = c(ret, i)
    }else{
      
      ret = c(ret,i)
      
    }
  }
  return(ret)
}
keep.terms = getRidOfLessThan4(keep.terms)
dtm = dtm[,keep.terms]
doc.length = rowSums(dtm)
which(doc.length==0)
dropped.docs=rownames(dtm)[doc.length <20]
dtm = dtm[!rownames(dtm) %in% dropped.docs,]
dim(dtm)
str(dtm)

new.docs = Dtm2Docs(dtm.sparse = dtm, parallel = F, cpus = 8)
lex = lexicalize(doclines = new.docs, sep = " ",vocab = keep.terms )
rm(new.docs);gc()
head(lex)
#
k.list = seq(50, 250, by = 50)
k.list = c(10,25, k.list)

sfInit(parallel = T, cpus = 8)
sfExport(list=c("lex","keep.terms"))
sfLibrary(lda)
models = sfLapply(k.list,function(k){
  result = lda.collapsed.gibbs.sampler(documents = lex, K = k, vocab = keep.terms, num.iterations = 5,
                                       alpha = 5/k,eta = .05, compute.log.likelihood = T)
})
#test = lda.collapsed.gibbs.sampler(documents = lex, K = 10, vocab = keep.terms, num.iterations = 5,  alpha = .01,eta = .05, compute.log.likelihood = T)
##

sfStop()
names(models) = paste("k.", k.list, sep="")
plot(models$k.250$log.likelihoods[2,],type="l")
##models obtained
View(models$k.250$topics)
model.output= lapply(models, function(x){ ExtractLdaResults_lda(lda.result = x, docnames = rownames(dtm))})
str(model.output)
#choose k
r2 = lapply(model.output, function(x){TopicModelR2(dtm.sparse = dtm, topic.terms = x$topic.terms, doc.topics = x$doc.topics, normalize = F, parallel = T,cpus = 8)})

str(r2[[2]])
sfInit(parallel=T,cpus = 8)
sfExport("dtm")
sfSource("TopicModelingFunctions.R")
pcoh = sfLapply(model.output, function(x){
  result = apply(x$topic.terms, 1, function(y){
    ProbCoherence(topic = y,M = 5,dtm.sparse = dtm)
  })
  result
})
sfStop()
metrics.mat = data.frame(k = k.list, ll=sapply(model.output, function(x){
  x$likelihood[2,ncol(x$likelihood)]}),
  r2 = sapply(r2, function(x) x$r2),
  pcoh = sapply(pcoh,median),
  stringsAsFactors = F)

plot(metrics.mat$k, metrics.mat$ll, type="o")
plot(metrics.mat$k, metrics.mat$r2, type="o")
plot(metrics.mat$k, metrics.mat$pcoh, type="o")

top.terms3 = GetTopTerms(topic.terms = model.output$k.10$topic.terms, M = 3)
View(top.terms3)
top.terms10 = GetTopTerms(topic.terms = model.output$k.10$topic.terms, M = 10)
View(top.terms10)

#summary and visualization
topic.summary = data.frame(topic = colnames(top.terms10),
                           top.terms = apply(top.terms10, 2, function(x){paste(x, collapse = " | ")}),
                           pcoh = pcoh$k.10,
                           prevalence = colSums(model.output$k.10$doc.topics)/sum(colSums(model.output$k.10$doc.topics))*100,
                           stringsAsFactors = F)
View(topic.summary)

for(j in 1:nrow(model.output$k.100$topic.terms)){

  TopicWordCloud(term.freq.vec = model.output$k.100$topic.terms[j,],
                 title = rownames(model.output$k.100$topic.terms)[j],
                 outfilepath = "/"   )
}

