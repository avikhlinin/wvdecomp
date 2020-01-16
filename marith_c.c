void add_real_arrays_ (float *x, float *y, int *N)
{ /* x+=y */
 register int i, n,m;
 float x1,x2,x3,y1,y2,y3;
 n = *N;
 for (i=0;i<n;i++){
   x[i]+=y[i];
 }
}
  
