###################################################################
# these are functions used for topic modeling. Many of them are
# dependent on the "lda" package written by Jonathan Chang
###################################################################


###################################################################
# load required libraries
###################################################################
library(lda)
library(snowfall)
library(wordcloud)
library(Matrix)
library(RColorBrewer)
library(wordcloud)
library(compiler) # 8/15/2014 I'd like to get rid of this dependency - TWJ

###################################################################
###################################################################
# Begin Functions
###################################################################
###################################################################



MakeBatches <- function(vec, num.batches){
  # divides vec into num.batches
  
  delta <- floor(length(vec) / num.batches)
  
  batches <- vector(mode = "list", length = num.batches)
  
  for(j in 1:num.batches){
    batches[[ j ]] <- (delta * (j - 1) + 1):(j * delta)
  }
  
  if(max(batches[[ num.batches ]]) != length(vec) ){
  
    if(max(batches[[ num.batches ]]) > length(vec)){
      batches[[ num.batches ]][ batches[[ num.batches ]] > max(1:length(vec)) ] <- NULL
    }else{
      batches[[ num.batches ]] <- c(batches[[ num.batches ]], (max(batches[[ num.batches ]]) + 1):length(vec))
    }
  }
  
  result <- lapply(batches, function(BATCH){
    vec[ BATCH ]
  })
  
  return(result)
}


JSD<-cmpfun(function(p,q){
  
  #This calculates the Jensen Shannon Divergence for two probability vectors, p and q.
  p[p==0] <- 10^-4
  q[q==0] <- 10^-4
  
  p <- p/sum(p)
  q <- q/sum(q)
  
  m=(p+q)/2
    
  jsd <- (0.5 * sum(log(p / m) * p)) + (0.5 * sum(log(q / m) * q))
  
  return(jsd)
})

HellDist = function(p,q){
  #Calculates the hellinger distances between two discrete probability distributions p and q
  
  # don't divide by zero, we'll all die
  p[p==0] <- 10^-4
  q[q==0] <- 10^-4
  
  #make unit length
  p = p/sum(p)
  q = q/sum(q)
  
  # set up for vectorization
  m <- sqrt(p) - sqrt(q)
  
  m <- m * m
  
  result <- 1/sqrt(2) * sqrt(sum(m))
  
  return(result)
}



ProbCoherence <- cmpfun(function( topic, M, dtm.sparse, pct=FALSE, num.docs=nrow(dtm.sparse) ){
  require(Matrix)
  # ordered vector of most probable M terms given a topic
  terms <- names(topic)[ order(topic, decreasing=TRUE ) ][ 1:M ]
  
  # sparse subset of dtm for terms, columns ordered by decreasing probability
  dtm.t <- dtm.sparse[ , terms ]
  dtm.t[ dtm.t > 0 ] <- 1
  count.mat <- t(dtm.t) %*% dtm.t
  
  p.mat <- count.mat / num.docs
  
  
  result <- sapply( 1:(ncol(count.mat) - 1), function(x){
    if(! pct){
        mean(p.mat[ x, (x + 1):ncol(p.mat) ]/p.mat[ x , x ] - diag(p.mat)[ (x + 1):ncol(p.mat) ], na.rm=TRUE)
    }else{
        mean( (p.mat[ x, (x + 1):ncol(p.mat) ]/p.mat[ x , x ] - diag(p.mat)[ (x + 1):ncol(p.mat) ])/diag(p.mat)[ (x + 1):ncol(p.mat) ], na.rm=TRUE ) * 100
    }
    
  })
  return( mean(result, na.rm=TRUE) )
})

GetTopTerms <- function(topic.terms, M){
  # Takes topics X terms matrix and returns top M terms for each topic
  
  result <- apply(topic.terms, 1, function(x){
    names(x)[ order(x, decreasing=TRUE) ][ 1:M ]
  })
  
  return(result)
}

ExtractLdaResults_lda <- function(lda.result, docnames, likelihood=TRUE, smooth=FALSE){
  # extracts outputs from LDA model estimated with "lda" package by Jonathan Chang
    
    doc.topics <- t(lda.result$document_sums)
    
  
  # Normalize topic vectors and doc vectors, smooth if necessary
    if(smooth){ 
      doc.topics <- doc.topics + 0.0001 
    }
	doc.topics <- doc.topics/rowSums(doc.topics)
	rownames(doc.topics) <- docnames
	colnames(doc.topics) <- paste("t.", 1:ncol(doc.topics), sep="" )
  
  

	topic.terms <- lda.result$topics
  
	if(smooth){ 
        topic.terms <- topic.terms + 1/ncol(topic.terms) 
	}
  
	topic.terms <- topic.terms/rowSums(topic.terms)
	rownames(topic.terms) <- colnames(doc.topics)

  # pull doc.topics and topic.terms into the result
	result <- list(doc.topics=doc.topics, topic.terms=topic.terms)
  
  # capture document_expects, if it exists (document_expects is over multiple runs, document_sums is over a single run)
  if(! is.null(dim(lda.result$document_expects))){
    doc.topics.expects <- t(lda.result$document_expects)
    
    doc.topics.expects <- doc.topics.expects/rowSums(doc.topics.expects)
    rownames(doc.topics.expects) <- docnames
    colnames(doc.topics.expects) <- paste("t.", 1:ncol(doc.topics.expects), sep="" )
    
    if(smooth){ 
      doc.topics.expects <- doc.topics.expects + 0.0001 
    }
    
    
    result$doc.topics.expects <- doc.topics.expects
    
  }
  

  # add in likelihoods if necessary
	if(likelihood){ result$likelihood <- lda.result$log.likelihoods }

  # return result
	return(result)
}

Dtm2Docs <- cmpfun(function(dtm.sparse, parallel=FALSE, cpus=NULL){
	# function creates a corpus of text documents based on term frequencies of a document term matrix
	terms <- colnames(dtm.sparse)
    if( ! parallel ){
        result <- apply(dtm.sparse, 1, function(x){
            paste( unlist( mapply( function(x,y) rep(y, x), x, terms)), collapse=" " )
        })
    }else{
        library( snowfall )
        sfInit( parallel=TRUE, cpus=cpus )
        sfLibrary(Matrix)
        sfExport( "terms" )
        
        result <- sfApply(dtm.sparse, 1, function(x){
            paste( unlist( mapply( function(x,y) rep(y, x), x, terms)), collapse=" " )
        })
        
        sfStop()
    }
    
    gc()
    
    return(result)
})



TopicWordCloud <- function(term.freq.vec, title="", outfilepath=""){
    # Takes a numeric vector whose names are the terms we wish to display
    # Does not return a value; plots the word cloud
    df <- data.frame(term=names(term.freq.vec)[ term.freq.vec > 0],  freq=term.freq.vec[ term.freq.vec > 0])
    
    df$freq <- round(df$freq/max(df$freq) * 100)
    
    col <- c("#313695", rev(brewer.pal(4, "RdYlBu")))
    
    png( paste(outfilepath, "wc.png", sep=""), width=8, height=8, units='in', res=300)
        par(bg="#F8F8FF")
        wordcloud(words=df$term, freq=df$freq, scale=c(4, 0.5), min.freq=1, max.words=100, colors=col, random.order=FALSE)
        title(main=title, line=-2)
    dev.off()
}

TopicModelR2 <- function(dtm.sparse, topic.terms, doc.topics, normalize=TRUE, parallel=TRUE, cpus=4){
    # Function to calculate R-squared for a topic model. 
    # This uses the interpretation of R-squared as the proportion of variance
    # explained by the model.
    #
    # Inputs: 
    # dtm.sparse = a documents X terms dimensional document term matrix in 
    #   sparse format from the Matrix package or a regular R matrix. 
    #   Will *not* work on DTMs from the tm package or simple triplet matrices from the slam package.
    # topic.terms = a topics X terms dimensional matrix where each entry is p(term|topic)
    # doc.topics = a documents X topics dimensional matrix where each entry is p(topic|document)
    # normalize = a logical. Do you want to normalize all vectors so they add to 1? 
    #   (removes effect of document length on SSE and SST)
    # parallel = a logical. Do you have snowfall installed? Would you like to parallelize?
    # cpus = number of threads over which to parallelize.
    #
    # Note: all input matrices must have rownames and colnames
    #
    # Output:
    # a list with 3 elements - 
    #   r2 = R-squared of the model
    #   sse = the sum of squared errors for each document. This is a vector, the square root of which
    #       gives the l2-norm or euclidean distance from each document to its fitted value
    #   sst = the total sum of squares for each document. This is a vector, the square root of which 
    #       gives the l2-norm or euclidean distance from each document to the "mean" document.
    
    # ensure that all inputs are sorted correctly
    topic.terms <- topic.terms[ colnames(doc.topics) , colnames(dtm.sparse) ]
    
    doc.topics <- doc.topics[ rownames(dtm.sparse) , ]
    
    # get ybar, the "average" document
    ybar.row <- colMeans(dtm.sparse)
    
    # declare functions to calculate SSE and SST for single documents
    SSE <- function(dtm.row, doc.topic.row, topic.terms, normalize){
        y <- as.numeric(dtm.row) 
        yhat <- as.numeric( doc.topic.row ) %*% topic.terms
        
        if( normalize ){
            y <- y / sum(y)
        }else{
            yhat <- yhat * sum(y) # makes yhat a "document" as long as y
        }
        
        ydiff <- yhat - y
        
        result <- sum( ydiff * ydiff )
        
        return(result)
    }
    
    SST <- function(dtm.row, ybar.row, normalize){
        y <- as.numeric(dtm.row)
        ybar <- as.numeric(ybar.row)
        
        if( normalize ){
            y <- y / sum(y)
            ybar <- ybar / sum(ybar)
        }
        
        ydiff <- ybar - y
        
        result <- sum( ydiff * ydiff )
        
        return(result)
    }
    
    if( ! parallel ){
        sse.result <- vector(mode="list", length=nrow(dtm.sparse))
        sst.result <- sse.result
        
        for( j in 1:nrow(dtm.sparse)){
            sse.result[[ j ]] <- SSE(dtm.row=dtm.sparse[ j , ],
                                     doc.topic.row=doc.topics[ j , ],
                                     topic.terms=topic.terms,
                                     normalize=normalize)
            
            sst.result[[ j ]] <- SST(dtm.row=dtm.sparse[ j , ],
                                     ybar.row=ybar.row,
                                     normalize=normalize)
        }
        
        sse.result <- unlist(sse.result)
        sst.result <- unlist(sst.result)
        names(sse.result) <- rownames(dtm.sparse)
        names(sst.result) <- rownames(dtm.sparse)
        
    }else{
        require(snowfall)
        
        # get batches of documents to parallelize over
        parallel.list <- vector(mode="list", length=cpus)
        
        div <- floor(nrow(dtm.sparse) / cpus)
        remainder <- nrow(dtm.sparse) - cpus * div
        
        parallel.list[[ 1 ]] <- 1:div
        
        for( j in 2:(cpus - 1) ){
            parallel.list[[ j ]] <- (max(parallel.list[[ j - 1 ]]) + 1 ):(j * div)
        }
        
        parallel.list[[ cpus ]] <- (max(parallel.list[[ cpus - 1 ]]) + 1):nrow(dtm.sparse)
        
        # put the distinct rows of dtm.sparse into a list to parallelize over, saves on memory
        parallel.list <- lapply(parallel.list, function(ROWS){
            my.dtm <- dtm.sparse[ ROWS , ]
            my.doc.topics <- doc.topics[ ROWS , ]
            return(list(my.dtm=my.dtm, my.doc.topics=my.doc.topics))
        })
        
        sfInit( parallel=TRUE, cpus=cpus)
        sfExport(list=c("doc.topics", "topic.terms", "ybar.row", "SSE", "SST", "normalize"))
        sfLibrary(Matrix)
        
        pll.result <- sfLapply(parallel.list, function(PARTIAL){
            parallel.result.sse <- vector(mode="list", length=nrow(PARTIAL$my.dtm))
			parallel.result.sst <- parallel.result.sse
            
            for(j in 1:length(parallel.result.sse)){
                parallel.result.sse[[ j ]] <- SSE(dtm.row=PARTIAL$my.dtm[ j , ],
                                                  doc.topic.row=PARTIAL$my.doc.topics[ j , ],
                                                  topic.terms=topic.terms,
                                                  normalize=normalize)
                parallel.result.sst[[ j ]] <- SST(dtm.row=PARTIAL$my.dtm[ j , ],
                                                  ybar.row=ybar.row,
                                                  normalize=normalize)
            }
            
            return(list(sse.result=parallel.result.sse, sst.result=parallel.result.sst))
        })
        
		sfStop()
		
        sse.result <- unlist(lapply(pll.result, function(x) x$sse.result))
        sst.result <- unlist(lapply(pll.result, function(x) x$sst.result))
        
        names(sse.result) <- rownames(dtm.sparse)
        names(sst.result) <- rownames(dtm.sparse)
    }
    
    r2 <- 1 - sum(sse.result) / sum(sst.result)
    
    final.result <- list(r2=r2, sse=sse.result, sst=sst.result)
    
    return(final.result)
}

EnsembleTopicDist <- function(posteriors, method="cosine"){
	# calculate the pairwise distance between topics in a list of topic models
	# Assumes that rownames of topic.terms matrices are unique, meaning that
	# rownames also include information on which model the topic came from
	
  library(Matrix)
  
	# combine topic.terms matrices into one gigantic matrix
  combined.topics <- lapply(posteriors, function(x) Matrix(x$topic.tems, sparse=TRUE))
  
  combined.topics <- do.call(rBind, combined.topics)
	
	# calculate distance based on method
	if(method=="cosine"){
		Dfun <- function(input.matrix){
			# get unit length vectors (in parallel)
		  vec.length <- sqrt( rowSums( input.matrix * input.matrix) )
			input.matrix <- input.matrix / vec.length
      
			# take dot product for cosine similarity
			input.matrix <- input.matrix %*% t(input.matrix)
      
			# take 1 - cosine similarity for distance
			input.matrix <- 1 - input.matrix
      
			# return matrix
      return(input.matrix)
		}
	}else{ 
      Dfun <- function(input.matrix){
        return("As of 8/15/2014  only method = 'cosine' is supported")
      } 
	}
    
  result <- Dfun(input.matrix = combined.topics)
  
  return(result)
}

AggregateTopicModels <- function(posteriors, assignments, weights=NULL, drops=NULL){
  ########################################################################
  # Function aggregates output of several topic models into
  # a single ensemble topic model.
  # 
  # Inputs:
  #   posteriors = list of models. Each element contains two
  #       matrices, "doc.topics" and "topic.terms". "doc.topics"
  #       is the posterior prediction of topics distributed over
  #       documents. "topic.terms" is the posterior prediction of 
  #       terms distributed over topics.
  #   assignments = a vector where each element corresponds to a 
  #       single topic from a single model. So, for example, if you 
  #       have 100 models of 50 topics, this vector will be of length
  #       100 * 50 = 5000. The entries of the vector are numeric, 
  #       characters, or factors and provide a mapping of topics between 
  #       models. The names of this vector, correspond to *unique* names
  #       of topics in each model.
  #   weights = vector of weights of each model in aggregating. If this
  #       is NULL, the default, each model is given equal weighting in 
  #       aggregation.
  #   drops = vector of values of assignments indicating which assignments 
  #       of topics we want to drop. If NULL, the default, no topics are
  #       dropped.
  #
  #   Output:
  #       a list with two elements, "doc.topics" and "topic.terms" of
  #       the ensemble model.
  ##########################################################################
  
  # pull out topic.terms and doc.topics from each model
  # this is done to avoid a memory leak later on
  doc.topics <- lapply(posteriors, function(x) x$doc.topics)
  
  topic.terms <- lapply(posteriors, function(x) x$topic.terms)
  
  # Rename topics in each model according to assignments
  # remove any topics/assignments in "drops"
  # this will silently drop these as we move forward
  if( ! is.null(drops) ) assignments <- assignments[ ! assignments %in% drops ]
  
  nameFun <- function(x){ # assumes doc.topics, must transpose for topic.terms
    # get column/row names in order
    # this first step will FAIL if your names aren't in order before starting
    names.assigned <- assignments[ names(assignments) %in% colnames(x) ]
    
    x <- x[ , names(names.assigned) ] # make sure columns are in correct order
    colnames(x) <- names.assigned
    
    ########
    # if you're going to collapse topics in the same cluster w/i a model, do it here.
    ########
    
    return(x)
  }
  
  doc.topics <- lapply(doc.topics, nameFun)
  
  topic.terms <- lapply(topic.terms, function(x){
    result <- nameFun( x=t(x) )
    
    return(t(result))
  })
  
  # Apply weights
  if( is.null(weights) ) weights <- rep(1/length(posteriors), length(posteriors) ) # if you don't have your own weights...
  
  weightFun <- function(weight, posterior){
    result <- weight * posterior
    return(result)
  }
  
  doc.topics <- mapply(weightFun, weight=weights, posterior=doc.topics, SIMPLIFY = FALSE)
  
  topic.terms <- mapply(weightFun, weight=weights, posterior=topic.terms, SIMPLIFY = FALSE)
  
  # Combine matrices together in one swoop
  doc.topics <- do.call(cbind, doc.topics)
  
  topic.terms <- do.call(rbind, topic.terms)
  
  # Combine (sum) columns/rows with the same assignments
  
  doc.topics <- doc.topics[ , sort(colnames(doc.topics) ) ]
  
  topic.terms <- topic.terms[ sort(rownames(topic.terms)) , ]    
  
  agg.topics <- sort(unique(colnames(doc.topics)))
  
  indices <- lapply(agg.topics, function(x)  which(colnames(doc.topics) == x )) # grep(x, colnames(doc.topics))
  
  names(indices) <- paste("t.", agg.topics, sep="")
  
  doc.topics <- sapply(indices, function(x){
    result <- rowSums(doc.topics[ , x ])
    return(result)
  })
  
  topic.terms <- sapply(indices, function(x){
    result <- colSums(topic.terms[ x , ])
    return(result)
  })
  
  # normalize rows/columns as appropriate
  doc.topics <- doc.topics / rowSums(doc.topics)    
  
  topic.terms <- t(topic.terms)
  
  topic.terms <- topic.terms / rowSums(topic.terms)
  
  
  # finally, return the result
  return(list(doc.topics=doc.topics, topic.terms=topic.terms))
  
}

ParallelJSD <- function(x, by.rows=TRUE, cpus=8){
    library(snowfall)
    
    if(! by.rows ) x <- t(x)
    
    rows <- nrow(x)
    
    sfInit(parallel=T, cpus=cpus)
    sfExport(list=c("JSD", "x", "rows"))
    
    result <- sfLapply(1:(rows - 1), function(j){
        drow <- rep(0, rows)
        
        for(k in (j + 1):rows ){
            drow[ k ] <- JSD(p=x[ j , ], q=x[ k , ])
        }
        
        return(drow)
    })
    
    sfStop()
    
    result <- rbind(do.call(rbind, result), rep(0, rows))
    
    rownames(result) <- rownames(x)
    colnames(result) <- rownames(x)
    
    result <- result + t(result)
    
    return(result)
}


Lex2Dtm = function(lex, d = NULL, v = NULL){
  #Takes the lexicalized object taken as an argument to Jonathan 
  #Chang's "lda" package and converts it into the observed document
  #term matrix. 
  #
  #Args:
  #  lex: List of length D of Integer Matrices, 2 x N.d, where N.d is the 
  #       length of the d'th document. This is the form expected by Jonathan
  #       Chang's "lda" package. 
  #  d: Integer. Number of documents, the length of lex. Default is NULL, in
  #     which case it is computed.
  #  V: Integer. Number of vocabulary terms in the corpus. Default is NULL, 
  #     in which case it is computed. 
  # 
  #Returns: Integer matrix, d x v (D x V). Document term matrix for the 
  #         corpus specified with a sparse representation in lex.
  #############################################################################
  
  #compute the number of documents if it is null
  if(is.null(d)){
  
    d <- length(d)
    
  }
  
  #compute the number of vocabulary terms
  if(is.null(v)){ 
    
    max.vocab <- 0;
    
    for(i in 1:d){
      max.vocab <- max(max.vocab,lex[[i]][1,])
    }

    #since lex is stored with 0-based indexing
    v <- max.vocab + 1 
    
  
  }
  
  dtm <- matrix(0, nrow = d, ncol = v) # D x V
  
  n.word.vec <- lapply(1:d,function(i,lex){    
                            return(sum(lex[[i]][2,]))
                          },
                       lex = lex)
  for(i in 1:d){
  
    words <- n.word.vec[[i]]
    
    for(j in 1:words){
      
      dtm[i, lex[[i]][1,j]+1] <- dtm[i, lex[[i]][1,j]+1] + 1

    }
  
  }
  
  return(dtm);
}

CalcTopMse = function(true.top, est.top, dist.fun){
  #Given an estimated and true topic matrix, measure the total distance
  #The True topic matrix provided is V x K, and the estimated topic matrix is
  #V x K', where K and K' are not necessarily the same, and where V 
  #is number of tokens in vocab. dist.fun is a user-provided distance function
  #to calculate distance between two probability vectors of length V, capped at 1
  #
  #Each of the metrics use the true number of topics in the denominator
  #when finding the 
  #
  #find pairwise distances between each of the topics
  #this distance matrix will be used to calculate the MSE without repeating
  #distance calculations

  K = dim(true.top)[2]
 
  V = dim(true.top)[1]
  
  Kp = dim(est.top)[2]
  
  dist.mat = matrix(0, nrow = K, ncol = Kp)
  
  for(k in 1:K){
    for(kk in 1:Kp){
      dist.mat[k, kk] = dist.fun(true.top[,k],est.top[,kk])
    }
  }
  
  #With every distance calculated, we want to calculate average error of the closest topic
  #while taking out each pairing as we go.
  mse = 0
  
  if(K>=Kp){
    #The true number of topics is greater than the estimated number of topics
    remaining.true <- 1:K
    
    for(i in 1:Kp){    
      indx <- which(dist.mat==min(dist.mat),arr.ind=T)[1,]
      
      mse <- mse + dist.mat[indx[1],indx[2]]^2
      
      dist.mat[indx[1],] <- Inf
      
      remaining.true <- setdiff(remaining.true, indx[1])
      
      dist.mat[,indx[2]] <- Inf
    }
    #Now the first Kp, topics are left in remaining.true
    #add 1 for each topic missed
    
    mse <- mse + length(remaining.true)
    
    mse <- sqrt(mse)/K
    
  } else {
    
    #The estimated number of topics is greater than the true number of topics
    remaining.ests <- 1:Kp
    
    for(i in 1:K){
      indx <- which(dist.mat==min(dist.mat),arr.ind=T)[1,]
      
      mse <- mse + dist.mat[indx[1],indx[2]]^2
      
      dist.mat[indx[1],] <- Inf
      
      remaining.ests <- setdiff(remaining.ests, indx[2])
      
      dist.mat[,indx[2]] <- Inf
    }
    
    #Now the first Kp, topics are left in remaining.true
    mse <- mse + length(remaining.ests)
   
    mse <- sqrt(mse)/K
    
  }
    
    return(list("mse" = mse, "sse" = mse*K))
  
}

FindKeepTerms = function(dtm, ntop = 7000, ptop = NULL){
  #Find the top ntop (or ptop if desire a proportion of words) to retain as informative
  #for the model fitting.
  if(!is.null(ptop)){
    ntop = ptop*dim(dtm)[2]
  }
  
  idf = FindIDF(dtm)
  
  #for()
  
  
}

FindIDF = function(dtm){
  #find the inverse document frequency
  #arg: dtm, d x v sparse
  #returns: vector, length v
  doc.appearances = rep(0, dim(dtm)[2])
  for(i in 1:dim(dtm)[2]){
    if((i %% 10000) == 0){
      print(paste("term ", i , " at ", Sys.time()))
    }
    doc.appearances[i] = sum(dtm[i,]>0)
  }
  idf = log(dim(dtm)[1]/doc.appearances)
  
  
  
}

library(stringr)
ManualTrimVocab = function(terms){
  ret = c()
  for(i in terms){
    if(str_length(i) > 4){
      ret = c(ret, i)
    }else{
      response = readline(paste(i, " (y/n?):"))
      if(response == "y" | response == 1 ){
        ret = c(ret,i)
      }
    }
  }
  return(ret)
}

