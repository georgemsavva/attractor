#include <Rcpp.h>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//
// [[Rcpp::export]]
NumericMatrix imageAttractorCPP(NumericVector startPos,NumericVector p, 
                                int n, int res, NumericVector xlim, NumericVector ylim,
                                int mutation=0){
  
  NumericMatrix out(res, res);
  double xd;
  double yd;
  int x;
  int y;
  
  NumericVector X(2);
  X = startPos;
  NumericVector prev(2);
  xd = res/(xlim(1)-xlim(0)); 
  yd = res/(ylim(1)-ylim(0)); 
  
      for(int i=0;i<n;i++){
        prev[0]=X[0];
        prev[1]=X[1];
        if(mutation==0){
          X[0] = sin(p[1]*prev[1])+p[2]*sin(p[1]*prev[0]);
          X[1] = sin(p[0]*prev[0])+p[3]*sin(p[0]*prev[1]);
        }
        if(mutation==1){
          X[0] = sin(p[1]*prev[1])+pow(sin(p[1]*prev[0]),2)+pow(sin(p[1]*prev[0]),3);
          X[1] = sin(p[0]*prev[0])+pow(sin(p[0]*prev[1]),2)+pow(sin(p[0]*prev[1]),3);
        }
        if(mutation==2){
          X[0] = sin(p[1]*prev[1])+pow(sin(p[1]*prev[0]),2);
          X[1] = sin(p[0]*prev[0])+pow(sin(p[0]*prev[1]),2);
        }
        if(mutation==3){
          X[0] = abs(sin(p[1]*prev[1]))+pow(sin(p[1]*prev[0]),2);
          X[1] = abs(sin(p[0]*prev[0]))+pow(sin(p[0]*prev[1]),2);
        }
        x = floor(xd * (X[0] - xlim[0]) );
        y = floor(yd * (X[1] - ylim[0]) );
        out(x,y) = out(x,y)+1  ;
        if(i<10) printf("x=%0.3f, y=%0.3f\n", X[0], X[1] );
      }
  return out;
}