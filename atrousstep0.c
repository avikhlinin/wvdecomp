#include <stdlib.h>
#include <math.h>

void atrousstep_02_ (float *cc, float *c, int *N, int *jscale)
{

  float cc1, cc2, cc3, cc4, cc5;

  int step1, step2, i, n, istart;

  step1=pow(2,(*jscale-1));
  step2=2*step1;
  n = *N;
  
  for (i=n+8192;i<n+step2+8192;i++)
    {
      cc[i]=cc[n+8192-1];
    }
  for (i=8192-step2;i<8192;i++)
    {
      cc[i]=cc[8192];
    }
  
  for (istart = 0; istart < step1; istart ++){
    i = istart;
    cc1 = cc[i+8192-step2];
    cc2 = cc[i+8192-step1];
    cc3 = cc[i+8192];
    cc4 = cc[i+8192+step1];

    for (i=istart;i<n; i+=step1) {
      cc5 = cc[i+8192+step2];
      c[i+8192]=(cc1+cc5+(cc2+cc4)*4+cc3*6)/16;
      
      i+=step1;      if (i>=n) break;
      cc1 = cc[i+8192+step2];
      c[i+8192]=(cc1+cc2+(cc3+cc5)*4+cc4*6)/16;
      
      i+=step1;      if (i>=n) break;
      cc2 = cc[i+8192+step2];
      c[i+8192] = (cc2+cc3+(cc1+cc4)*4+cc5*6)/16;

      i+=step1;      if (i>=n) break;
      cc3 = cc[i+8192+step2];
      c[i+8192] = (cc3+cc4+(cc2+cc5)*4+cc1*6)/16;

      i+=step1;      if (i>=n) break;
      cc4 = cc[i+8192+step2];
      c[i+8192] = (cc4+cc5+(cc3+cc1)*4+cc2*6)/16;
    }
  }
}
  
