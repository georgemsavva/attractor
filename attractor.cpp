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
double triangleCPP(double x){
  x = fmod(12*M_PI+x,2*M_PI);
  if(x<M_PI/2) return x/(M_PI/2);
  if(x<3*M_PI/2) return 2-(x/(M_PI/2));
  return (x/(M_PI/2))-4;
}

// [[Rcpp::export]]
double sawtoothCPP(double x){
  x = fmod(12*M_PI+x,M_PI);
  return (x/(M_PI));
}



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
        if(i<20) Rprintf("x=%0.4f, y=%0.4f\n", prev[0], prev[1]);
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
        if(mutation==4){
          X[0] = sin(p[1]*prev[1])+p[2]*cos(p[1]*prev[0]);
          X[1] = sin(p[0]*prev[0])+p[3]*cos(p[0]*prev[1]);
        }
        if(mutation==5){
          X[0] = triangleCPP(p[1]*prev[1])+p[2]*triangleCPP(p[1]*prev[0]);
          X[1] = triangleCPP(p[0]*prev[0])+p[3]*triangleCPP(p[0]*prev[1]);
        }
        if(mutation==6){
          X[0] = triangleCPP(p[1]*prev[1])+p[2]*triangleCPP(p[1]*prev[0]+M_PI/2);
          X[1] = triangleCPP(p[0]*prev[0])+p[3]*triangleCPP(p[0]*prev[1]+M_PI/2);
        }
        if(mutation==7){
          X[0] = triangleCPP(p[1]*prev[1])+p[2]*triangleCPP(p[1]*prev[0]+M_PI/2);
          X[1] = triangleCPP(p[0]*prev[0])+p[3]*triangleCPP(p[0]*prev[1]);
        }
        if(mutation==8){
          X[0] = sin(p[1]*prev[1])+p[2]*sin(p[1]*prev[0]+M_PI/2);
          X[1] = triangleCPP(p[0]*prev[0])+p[3]*triangleCPP(p[0]*prev[1]+M_PI/2);
        }
        if(mutation==9){
          X[0] = sin(p[1]*prev[1])+p[2]*triangleCPP(p[1]*prev[0]+M_PI/2);
          X[1] = sin(p[0]*prev[0])+p[3]*triangleCPP(p[0]*prev[1]+M_PI/2);
        }
        if(mutation==10){
          X[0] = sawtoothCPP(p[1]*prev[1])+p[2]*sawtoothCPP(p[1]*prev[0]+M_PI/2);
          X[1] = sawtoothCPP(p[0]*prev[0])+p[3]*sawtoothCPP(p[0]*prev[1]+M_PI/2);
        }
        if(mutation==11){
          X[0] = sawtoothCPP(p[1]*prev[1])+p[2]*sawtoothCPP(p[1]*prev[0]+M_PI/2);
          X[1] = triangleCPP(p[0]*prev[0])+p[3]*triangleCPP(p[0]*prev[1]+M_PI/2);
        }
        if(mutation==12){
          X[0] = sin(p[1]*prev[1])+p[2]*sawtoothCPP(p[1]*prev[0]+M_PI/2);
          X[1] = sawtoothCPP(p[0]*prev[0])+p[3]*sawtoothCPP(p[0]*prev[1]+M_PI/2);
        }
        if(mutation==14){
          X[0] = sin(p[1]*prev[1])+p[2]*cos(p[1]*prev[0]) + p[4]*sin(p[6]*prev[0]);
          X[1] = sin(p[0]*prev[0])+p[3]*cos(p[0]*prev[1]) + p[5]*sin(p[7]*prev[1]);
        }
        if(mutation==15){
          X[0] = sin(p[1]*prev[1])+p[2]*cos(p[1]*prev[0]) - (prev[1]>0)*1;
          X[1] = sin(p[0]*prev[0])+p[3]*cos(p[0]*prev[1]) + (prev[0]<0)*1;
        }
        if(mutation==16){
    
          X[0] = p[6]*(sin(p[0]*prev[1])+cos(p[0]*prev[0])) + p[4]*(sin(p[1]*prev[1])+cos(p[1]*prev[0]))+ p[3]*(sin(p[2]*prev[1]+p[5])+cos(p[2]*prev[0]+p[5]));
          X[1] = p[6]*(sin(p[0]*prev[0])-cos(p[0]*prev[1])) + p[4]*(sin(p[1]*prev[0])-cos(p[1]*prev[1]))+ p[3]*(sin(p[2]*prev[0]+p[5])-cos(p[2]*prev[1]+p[5]));
        }
        
        x = floor(xd * (X[0] - xlim[0]) );
        y = floor(yd * (X[1] - ylim[0]) );
        if((x>0) & (x<res) & (y>0) & (y<res)) out(x,y) = out(x,y)+1  ;
        if(i<10) printf("x=%0.3f, y=%0.3f\n", X[0], X[1] );
      }
  return out;
}


