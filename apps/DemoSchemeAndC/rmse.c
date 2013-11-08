#include <stdio.h>

// RMSE takes three arguments: the data structure, its length, and the reference value.
double rmse(double *vals, int len, double ref){
  double sum=0;
  printf("From C: len=%d\n",len);
  // Calculate the rmse value
  int i;
  for (i=0;i<len;i++){
    sum+=pow(vals[i]-ref,2);
  }
  return sqrt(sum/len);
}

