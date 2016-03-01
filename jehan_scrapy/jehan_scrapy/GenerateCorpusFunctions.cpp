#define ARMA_64BIT_WORD
#include <RcppArmadillo.h>


using namespace Rcpp;
using namespace arma;

// Below is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar)

// For more on using Rcpp click the Help button on the editor toolbar

// [[Rcpp::export]]
int timesTwo(int x) {
   return x * 2;
}

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
double ppareto(double x, double scale, double shape){
  //calculate the cdf of the pareto distribution.
  //
  //Args:
  // x:  double. Value cdf should be evauluated at
  // scale: double. Minimum value a draw is allowed to be
  // shape: double. powerlaw parameter
  //
  //Returns:
  // ret: double. value of cdf.
  /////////////////////////////////////////////////////////////////////////////
  
  if(x > scale){
    
    return(1 - scale / x);
  
  } else {
    
    return(0);
    
  }
  
}

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
double qpareto(double y, double scale, double shape){
  //Calculates the inverse cdf of a pareto distribution
  //
  //Args:
  // y: double. CDF Value between 0 and 1. Will return NaN if not.
  // scale: double. Minimum value pareto distribution allows
  // shape: double. Power law parameter
  //
  //Returns:
  // ret: double. Value of the inverse cdf.
  /////////////////////////////////////////////////////////////////////////////
  if((y >= 0) && (y <= 1)){
  
    return(scale * (pow(1-y, -1/shape))); 
  
  }else{
  
    return(NA_REAL);
  
  }
  
}

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
arma::vec rpareto(int n, double scale, double shape){
  //Generates n numbers from pareto distribution with given scale and shape
  //parameters
  //
  //Args:
  // n: integer. number of draws
  // scale: double. smallest value that pareto can draw from (scale > 0)
  // shape: double. Power law parameter
  //
  //Returns:
  // ret: arma::vec, length n. Draws from the pareto distribution.
  /////////////////////////////////////////////////////////////////////////////
  
  arma::vec ret = randu<arma::vec>(n);
  for(int i = 0; i < n; i++){
    ret(i) = qpareto(ret(i), scale, shape);
  }
  
  return(ret);
  
}


// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
arma::vec MyRdirichlet(arma::vec a){
  //Returns a draw from the dirichlet distribution
  //asymmetric dirichlet distribution
  //
  //Args:
  // a: arma::vec, length k. concentration parameter
  //
  //Returns:
  // Draw from Dirichlet distribution
  /////////////////////////////////////////////////////////////////////////////
  
  int k = a.n_elem;
  
  arma::vec y(k);
  
  for(int i = 0; i<k; i++){
    
    y(i) = R::rgamma(a(i), 1);
    
  }
  
  y = y/accu(y);
  
  return(y);
  
}

// [[Rcpp::export]]
int SampleMultinom2(arma::uvec x, arma::vec p){
  //Samples an element from x given the probability vector p
  //
  //Args:
  // x: arma::uvec, length p. The vector of word locations to sample from
  // p: arma::vec, length p. The multinomial probability
  //
  //Returns:
  // int. The sampled element from x.
  /////////////////////////////////////////////////////////////////////////////
  
  //Draw u
  
  double u = R::runif(0,1);
  
  int i = 0;
  
  double test = p(i);
  
  while(test <= u){
    
    i++;
    
    test += p(i);
    
  }
  
  return(x(i));
  
}

// [[Rcpp::export]]
int SampleMultinom1(arma::vec p){
  //Gives a single multinomial draw with probabilities p 
  //
  //Args:
  // p: arma::vec, length p. The multinomial probability
  //
  //Returns:
  // int. The index of the sample value (starting at 0)
  /////////////////////////////////////////////////////////////////////////////
  
  double u = R::runif(0,1);
  
  int i = 0;
  
  double test = p(i);
  
  while(test <= u){
    
    i++;
    
    test += p(i);
    
  }
  
  return(i);
  
}

 // [[Rcpp::depends(RcppArmadillo)]]
 // [[Rcpp::export]]
Rcpp::List GenCorpusLDA(int d, int v, int k, int ldoc, int ltopic,
                        double a, double b, bool usedir = false, 
                        double a_con = 1){
  //Generates a Corpus according to the latent dirichlet allocation model.
  //Uses a power law - shuffle to generate the topics. That result can be used
  //directly as the multinomial parameter, or it can be used as the center for
  //a dirichlet draw.
  //
  //Args:
  // d: int. Number of documents.
  // v: int. Number of vocab terms.
  // k: int. Number of topics.
  // ldoc: int. Average length of document
  // ltopic: int. Number of non-zero entries in each topic (can be v)
  // a: double. Power law parameter for topics.
  // b: double. Dirichlet concentration parameter for topic proporitions
  // usedir: boolean. Optionally draw from dirichlet for topic expressions,
  //         using the pareto draw for the localization parameter
  // a_con: double. Concentration parameter for dirichlet. Default 1.
  //
  //Returns:
  // dtm: sp_umat. Sparse matrix of unsigned integers. The document term
  //      matrix for generated corpus.
  //////////////////////////////////////////////////////////////////////
  
  //Generate the topic expressions
  
  arma::mat topic_vals(ltopic, k);
  
  arma::umat loc_vals(ltopic, k);
  
  for(int i = 0; i < k; i++){
    R_CheckUserInterrupt();
    
    topic_vals.col(i) = rpareto(ltopic, 1, a);
    
    topic_vals.col(i) = topic_vals.col(i)/accu(topic_vals.col(i));
    
    topic_vals.col(i) = sort(topic_vals.col(i), "descend");
    
    loc_vals.col(i) = randi<arma::uvec>(ltopic, distr_param(0, v - 1));
    
    //for(j = 0; j < ltopic; j++){
      
      //topics(i,loc_val(j)) = topic_val(j);
      
    //}
    
  }
  
  //Generate the topic proportions
  
  arma::mat topics_prop(k, d);
  
  for(int s = 0; s < d; s++){
    
    R_CheckUserInterrupt();
    
    topics_prop.col(s) = MyRdirichlet(ones(k)*b);
    
  }
  
  //Generate document lengths
  
  arma::ivec doc_lengths(d);
  
  for(int ii = 0; ii < d; ii++){
    
    doc_lengths(ii) = R::rpois(ldoc);
    
  }
  
  //Rcpp::Rcout << "intializing dtm" << std::endl;
  sp_umat dtm(d,v);
  
  //for each document 
  for(int iii = 0; iii < d; iii++){
    
    if(iii % 500 == 0){
      Rcpp::Rcout << "Generating document " << iii <<  std::endl;
    }
    R_CheckUserInterrupt();
    
    //for each word
    for(int j = 0; j < doc_lengths(iii); j++){
      
      //first sample topic
      int t = SampleMultinom1(topics_prop.col(iii));
      
      //Then sample the word, add it to dtm
      
      int w = SampleMultinom2(loc_vals.col(t), topic_vals.col(t));
      
      dtm(iii,w)++;
       
    }
    
  }
  
    return(Rcpp::List::create(Rcpp::Named("dtm") = dtm,
                            Rcpp::Named("topic.vals") = topic_vals,
                            Rcpp::Named("topic.locs") = loc_vals,
                            Rcpp::Named("topic.props") = topics_prop,
                            Rcpp::Named("k") = k));
  
  
}

// [[Rcpp::depends(RcppArmadillo)]]
 // [[Rcpp::export]]
Rcpp::List GenCorpusLDAVan(int d, int v, int k, int ldoc, int ltopic,
                        double a, double b){
  //Generates a Corpus according to the latent dirichlet allocation model.
  //Returns basic LDA
  //
  //Args:
  // d: int. Number of documents.
  // v: int. Number of vocab terms.
  // k: int. Number of topics.
  // ldoc: int. Average length of document
  // ltopic: int. Number of non-zero entries in each topic (can be v)
  // a: double. Dirichlet concentration parameter for topic expression
  // b: double. Dirichlet concentration parameter for topic proporitions
  //
  //Returns:
  // dtm: sp_umat. Sparse matrix of unsigned integers. The document term
  //      matrix for generated corpus.
  //////////////////////////////////////////////////////////////////////
  
  //Generate the topic expressions
  
  arma::mat topic_vals(ltopic, k);
  
  arma::umat loc_vals(ltopic, k);
  
  for(int i = 0; i < k; i++){
    R_CheckUserInterrupt();
    
    topic_vals.col(i) = MyRdirichlet(ones(ltopic)*a);
    
    topic_vals.col(i) = sort(topic_vals.col(i), "descend");
    
    loc_vals.col(i) = randi<arma::uvec>(ltopic, distr_param(0, v - 1));
    
    //for(j = 0; j < ltopic; j++){
      
      //topics(i,loc_val(j)) = topic_val(j);
      
    //}
    
  }
  
  //Generate the topic proportions
  
  arma::mat topics_prop(k, d);
  
  for(int s = 0; s < d; s++){
    
    R_CheckUserInterrupt();
    
    topics_prop.col(s) = MyRdirichlet(ones(k)*b);
    
  }
  
  //Generate document lengths
  
  arma::ivec doc_lengths(d);
  
  for(int ii = 0; ii < d; ii++){
    
    doc_lengths(ii) = R::rpois(ldoc);
    
  }
  
  //Rcpp::Rcout << "intializing dtm" << std::endl;
  sp_umat dtm(d,v);
  
  //for each document 
  for(int iii = 0; iii < d; iii++){
    
    if(iii % 100 == 0){
      Rcpp::Rcout << "Generating document " << iii <<  std::endl;
    }
    R_CheckUserInterrupt();
    
    //for each word
    for(int j = 0; j < doc_lengths(iii); j++){
      
      //first sample topic
      int t = SampleMultinom1(topics_prop.col(iii));
      
      //Then sample the word, add it to dtm
      
      int w = SampleMultinom2(loc_vals.col(t), topic_vals.col(t));
      
      dtm(iii,w)++;
       
    }
    
  }
  
    return(Rcpp::List::create(Rcpp::Named("dtm") = dtm,
                            Rcpp::Named("topic.vals") = topic_vals,
                            Rcpp::Named("topic.locs") = loc_vals,
                            Rcpp::Named("topic.props") = topics_prop,
                            Rcpp::Named("k") = k));
  
  
}


// [[Rcpp::depends(RcppArmadillo)]]
 // [[Rcpp::export]]
Rcpp::List GenCorpusLDAGamma(int d, int v, int k, int ldoc, int ltopic,
                        double a_gamma, double b_gamma, double b){
  //Generates a Corpus according to the latent dirichlet allocation model.
  //Returns basic LDA
  //
  //Args:
  // d: int. Number of documents.
  // v: int. Number of vocab terms.
  // k: int. Number of topics.
  // ldoc: int. Average length of document
  // ltopic: int. Number of non-zero entries in each topic (can be v)
  // a_gamma: double. gamma concentration parameter for topic expression.
  // b_gamma: double. Gamma shape parameter for topic expression.
  // b: double. Dirichlet concentration parameter for topic proporitions
  //
  //Returns:
  // dtm: sp_umat. Sparse matrix of unsigned integers. The document term
  //      matrix for generated corpus.
  //////////////////////////////////////////////////////////////////////
  
  //Generate the topic expressions
  
  arma::mat topic_vals(ltopic, k);
  
  arma::umat loc_vals(ltopic, k);
  
  for(int i = 0; i < k; i++){
    R_CheckUserInterrupt();
    
    arma::vec topic_val_prior(ltopic);
    
    for(int j = 0; j < ltopic; j++){
      topic_val_prior(j) =  R::rgamma(a_gamma, b_gamma);
    }
     
    
    topic_vals.col(i) = MyRdirichlet(topic_val_prior);
    
    topic_vals.col(i) = sort(topic_vals.col(i), "descend");
    
    loc_vals.col(i) = randi<arma::uvec>(ltopic, distr_param(0, v - 1));
    
    //for(j = 0; j < ltopic; j++){
      
      //topics(i,loc_val(j)) = topic_val(j);
      
    //}
    
  }
  
  //Generate the topic proportions
  
  arma::mat topics_prop(k, d);
  
  for(int s = 0; s < d; s++){
    
    R_CheckUserInterrupt();
    
    topics_prop.col(s) = MyRdirichlet(ones(k)*b);
    
  }
  
  //Generate document lengths
  
  arma::ivec doc_lengths(d);
  
  for(int ii = 0; ii < d; ii++){
    
    doc_lengths(ii) = R::rpois(ldoc);
    
  }
  
  //Rcpp::Rcout << "intializing dtm" << std::endl;
  sp_umat dtm(d,v);
  
  //for each document 
  for(int iii = 0; iii < d; iii++){
    
    if(iii % 100 == 0){
      Rcpp::Rcout << "Generating document " << iii <<  std::endl;
    }
    R_CheckUserInterrupt();
    
    //for each word
    for(int j = 0; j < doc_lengths(iii); j++){
      
      //first sample topic
      int t = SampleMultinom1(topics_prop.col(iii));
      
      //Then sample the word, add it to dtm
      
      int w = SampleMultinom2(loc_vals.col(t), topic_vals.col(t));
      
      dtm(iii,w)++;
       
    }
    
  }
  
    return(Rcpp::List::create(Rcpp::Named("dtm") = dtm,
                            Rcpp::Named("topic.vals") = topic_vals,
                            Rcpp::Named("topic.locs") = loc_vals,
                            Rcpp::Named("topic.props") = topics_prop,
                            Rcpp::Named("k") = k));
  
  
}



