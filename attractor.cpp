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
                                int n, int res, NumericVector xlim, NumericVector ylim){
  
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
  
  printf("%0.3f",xd);
    
      for(int i=0;i<n;i++){
        prev[0]=X[0];
        prev[1]=X[1];
        X[0] = sin(p[0]*prev[1])+p[2]*sin(p[0]*prev[0]);
        X[1] = sin(p[1]*prev[0])+p[3]*sin(p[1]*prev[1]);
        x = floor(xd * (X[0] - xlim[0]) );
        y = floor(yd * (X[1] - ylim[0]) );
        out(x,y) = out(x,y)+1  ;
        if(i<10) printf("x=%0.3f, y=%0.3f\n", X[0], X[1] );
      }
  return out;
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

/*** R
library(imager)
kingCPP <- imageAttractorCPP(startPos = c(0.1,0.1), n = 1e9, res=4000, p=lordsDreams[[3]],
                    xlim=3*c(-1,1), ylim=3*c(-1,1))
img1 <- as.cimg(.999^kingCPP)
save.image(img1,"kingcpp1.png")
 

*/
