#include <RcppArmadillo.h>
using namespace Rcpp;


// This is a modified version of one of Rcpp functions in CARBayes R package (Duncan Lee, 2013):

// [[Rcpp::depends(RcppArmadillo)]]
  


  // [[Rcpp::export]]
  double quadform(NumericMatrix Wtriplet, NumericVector Wtripletsum, const int n_triplet, const int nsites, 
                  NumericVector phi, NumericVector theta, double rho)
  {
    // Compute a quadratic form for the random effects
    // Create new objects 
    double tau2_posteriorscale;
    double tau2_quadform = 0, tau2_phisq = 0;
    int row, col;
    
    
    // Compute the off diagonal elements of the quadratic form
    for(int l = 0; l < n_triplet; l++)
    {
      row = Wtriplet(l,0) - 1;
      col = Wtriplet(l,1) - 1;
      if(row == col) {
        tau2_quadform = tau2_quadform + 0;
      } else {
        tau2_quadform = tau2_quadform + phi[(Wtriplet(l,0) - 1)] * theta[(Wtriplet(l,1) - 1)] * Wtriplet(l,2); 
      }
    }
    
    
    // Compute the diagonal elements of the quadratic form          
    for(int l = 0; l < nsites; l++)
    {
      tau2_phisq = tau2_phisq + phi[l] * theta[l] * (rho * Wtripletsum[l] + 1 - rho);    
    }
    
    
    // Compute the quadratic form
    tau2_posteriorscale = 0.5 * (tau2_phisq - rho * tau2_quadform);
    
    
    // Return the simulated value
    return tau2_posteriorscale;
  }
  

// [[Rcpp::export]]
double quadformadj(NumericMatrix Wtriplet, NumericVector Wtripletsum, const int n_triplet, const int nsites,
                NumericVector phi, NumericVector theta, double rho)
{
  // Compute a quadratic form for the random effects
  // Create new objects
  double tau2_posteriorscale;
  double tau2_quadform = 0, tau2_phisq = 0;
  int row, col;
  
  
  // Compute the off diagonal elements of the quadratic form
  for(int l = 0; l < n_triplet; l++)
  {
    row = Wtriplet(l,0) - 1;
    col = Wtriplet(l,1) - 1;
    if(row == col) {
      tau2_quadform = tau2_quadform + 0;
    } else {
      tau2_quadform = tau2_quadform + phi[(Wtriplet(l,0) - 1)] * theta[(Wtriplet(l,1) - 1)] * Wtriplet(l,2);
    }
  }
  
  
  // Compute the diagonal elements of the quadratic form
  for(int l = 0; l < nsites; l++)
  {
    tau2_phisq = tau2_phisq + phi[l] * theta[l] * (rho * (Wtripletsum[l] -1) + 1 - rho);
  }
  
  
  // Compute the quadratic form
  tau2_posteriorscale = 0.5 * (tau2_phisq - rho * tau2_quadform);
  
  
  // Return the simulated value
  return tau2_posteriorscale;
}

 
  
  // [[Rcpp::export]]
NumericVector gaussiancarupdate(NumericMatrix Wtriplet, NumericMatrix Wbegfin,
                          NumericVector Wtripletsum, const int nsites, NumericVector phi, double tau2,
                          double rho, double nu2, NumericVector offset)
  {
    // Update the spatially correlated random effects 
    //Create new objects
    int rowstart=0, rowend=0;
    double sumphi;
    double fcprecision, fcsd, fcmean;
    double priorvardenom, priormean, priorvar;
    NumericVector phinew(nsites);
    

    
    //  Update each random effect in turn
    phinew = phi;
    
    for(int j = 0; j < nsites; j++)
    {
      // Calculate prior variance
      priorvardenom = rho * (Wtripletsum[j]) + 1 - rho;
      priorvar = tau2 / priorvardenom;
      
      // Calculate the prior mean
      rowstart = Wbegfin(j,0) - 1;
      rowend = Wbegfin(j,1);
      sumphi = 0;
      for(int l = rowstart; l < rowend; l++) sumphi += Wtriplet(l, 2) * phinew[(Wtriplet(l,1) - 1)];
      priormean = rho * sumphi / priorvardenom;
      

        // propose a value
        fcprecision = (1/nu2) + (1/priorvar);
        fcsd = pow((1/fcprecision),0.5);
        fcmean = (priormean / priorvar + offset[j]/nu2) / fcprecision;
        phinew[j] = rnorm(1, fcmean, fcsd)[0];
    }
      
      return phinew;
  }
  
  
// [[Rcpp::export]]
NumericVector gaussiancarupdate0(NumericMatrix Wtriplet, NumericMatrix Wbegfin,
                        NumericVector Wtripletsum, const int nsites, NumericVector phi, double tau2,
                        double rho, double nu2, NumericVector offset)
{
  // Update the spatially correlated random effects
  //Create new objects
  int rowstart=0, rowend=0;
  double sumphi;
  double fcprecision, fcsd, fcmean;
  double priorvardenom, priormean, priorvar;
  NumericVector phinew(nsites);
  

  
  //  Update each random effect in turn
  phinew = phi;
  
  for(int j = 0; j < nsites; j++)
  {
    // Calculate prior variance
    priorvardenom = rho * (Wtripletsum[j]) + 1 - rho;
    priorvar = tau2 / priorvardenom;
    
    // Calculate the prior mean
    rowstart = Wbegfin(j,0) - 1;
    rowend = Wbegfin(j,1);
    sumphi = 0;
    for(int l = rowstart; l < rowend; l++) sumphi += Wtriplet(l, 2) * phinew[(Wtriplet(l,1) - 1)];
    priormean = rho * sumphi / priorvardenom;
    

      // propose a value
      fcprecision = (1/nu2) + (1/priorvar);
      fcsd = pow((1/fcprecision),0.5);
      fcmean = (priormean / priorvar + offset[j]/nu2) / fcprecision;
      phinew[j] = rnorm(1, fcmean, fcsd)[0];
  }
    
    return phinew;
}



  // [[Rcpp::export]]
NumericVector gaussiancarupdateadj(NumericMatrix Wtriplet, NumericMatrix Wbegfin,
                          NumericVector Wtripletsum, const int nsites, NumericVector phi, double tau2,
                          double rho, double nu2, NumericVector offset)
  {
    // Update the spatially correlated random effects
    //Create new objects
    int rowstart=0, rowend=0;
    double sumphi;
    double fcprecision, fcsd, fcmean;
    double priorvardenom, priormean, priorvar;
    NumericVector phinew(nsites);
    

    
    //  Update each random effect in turn
    phinew = phi;
    
    for(int j = 0; j < nsites; j++)
    {
      // Calculate prior variance
      priorvardenom = rho * (Wtripletsum[j]-1) + 1 - rho;
      priorvar = tau2 / priorvardenom;
      
      // Calculate the prior mean
      rowstart = Wbegfin(j,0) - 1;
      rowend = Wbegfin(j,1);
      sumphi = 0;
      for(int l = (rowstart+1); l < rowend; l++) sumphi += Wtriplet(l, 2) * phinew[(Wtriplet(l,1) - 1)];
      priormean = rho * sumphi / priorvardenom;
      

        // propose a value
        fcprecision = (1/nu2) + (1/priorvar);
        fcsd = pow((1/fcprecision),0.5);
        fcmean = (priormean / priorvar + offset[j]/nu2) / fcprecision;
        phinew[j] = rnorm(1, fcmean, fcsd)[0];
    }
      
      return phinew;
  }
  
  
