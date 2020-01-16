void copy_real_arrays_ (float *to, float *from, int *N)
{
  int i;
  float *f, *t;
  f = from; t = to;

  for (i=*N; i; i--, f++, t++) *t=*f;
  /*  for (i=0;i<n;i++){ */
  /*    to[i]=from[i];   */
  /*  }                  */
  
}


void copy_real_arrays_step1_ (float *to, float *from, int *N, int *Step)
{
  int i, s;
  float *f, *t;
  f = from; t = to;
  
  s = *Step;
  for (i=*N; i; i--, f++, t+=s) *t=*f;

  /*  n=*N; s=*Step;
      for (k=0,i=0;k<n*s;k+=s){
      to[k]=from[i];i++;
  }
  */
}


void copy_real_arrays_step2_ (float *to, float *from, int *N, int *Step)
{
  int i, s;
  float *f, *t;
  f = from; t = to;
  
  s = *Step;
  for (i=*N; i; i--, f+=s, t++) *t=*f;

  /*  n=*N; s=*Step;
      for (i=0, k=0;i<n;i++){
      to[i]=from[k];k+=s;
      }
  */
}
