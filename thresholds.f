!       subroutine calc_poisson_nfalse_threshold 
!      ~                (threshimg,bg,n,nfalse,scale,kernel)
!       implicit none
!       integer n
!       real threshimg(n), bg(n)
!       real nfalse
!       integer scale, kernel
!       integer i
!       real poisson_threshold
!       external poisson_threshold
! 
!       do i=1,n
!         threshimg(i) = poisson_threshold (bg(i),nfalse,scale,kernel)
!       enddo
! 
!       return
!       end

      subroutine calc_poisson_nfalse_threshold 
     ~    (threshimg,bg,n,nfalse,scale,kernel)
      implicit none
      integer n
      real threshimg(n), bg(n)
      real nfalse
      integer scale, kernel
      integer i
      real poisson_threshold
      external poisson_threshold
*      real threshold_atrous
*      external threshold_atrous
      
      if (kernel.eq.1) then
*        do i=1,n
*         threshimg(i)= threshold_atrous (bg(i),nfalse,scale)
**         print*,i,bg(i),threshimg(i)
*       enddo
        write (0,*) 'atrous unsupported in the nfalse threshold mode'
      else 
        do i=1,n
          threshimg(i) = poisson_threshold (bg(i),nfalse,scale,kernel)
        enddo
      endif
      
      return
      end
      

*      REAL function threshold_atrous (MEAN,NFALSE,JSCALE)
*      REAL MEAN,NFALSE
*      INTEGER JSCALE
*
*      PARAMETER (JMAX= 6)
*      PARAMETER (NFALSE_MAX=10)
*      PARAMETER (SIZE=NFALSE_MAX*JMAX*4)
*      INTEGER IDX
*      
**      double precision fit_tab(SIZE)
*      double precision  fit_tab(4,JMAX,NFALSE_MAX)
*c      WRITE(*,*) 'MEAN: ',MEAN,' NFALSE: ',NFALSE,' SCALE: ',JSCALE
*      data fit_tab/
*c     /* NFALSE=1 */
*     ~4.126243, 2.061689, 1.065060, 0.268453,
*     ~0.898974, 0.196051, 1.060000, 0.055000,
*     ~0.365444, 0.042050, 1.030000, 0.010000,
*     ~0.164838, 0.015000, 1.020000, 0.003800,
*     ~0.075004, 0.007000, 1.050000, 0.004000,
*     ~0.034175, 0.005000, 1.200000, 0.007000,
*c    /* NFALSE=2 */
*     ~3.936149, 1.968946, 1.072930, 0.287063,
*     ~0.863780, 0.187662, 1.055310, 0.047776,
*     ~0.348488, 0.035759, 1.100550, 0.035040,
*     ~0.154586, 0.006561, 1.516190, 0.079796,
*     ~0.069640, 0.000091, 2.021450, 0.071134,
*     ~0.030938, 0.000629, 1.887800, 0.027467,
*c    /* NFALSE=3 */
*     ~3.838373, 1.899426, 1.048290, 0.185355,
*     ~0.842273, 0.184290, 1.045920, 0.038677,
*     ~0.339498, 0.033909, 1.120610, 0.040947,
*     ~0.149811, 0.005623, 1.627130, 0.093951,
*     ~0.066015, 0.001161, 2.017880, 0.067195,
*     ~0.029272, 0.000495, 1.923560, 0.027034,
*c    /* NFALSE=4 */
*     ~3.768147, 1.848867, 1.055790, 0.210225,
*     ~0.828396, 0.178990, 1.041600, 0.034461,
*     ~0.333258, 0.032479, 1.125850, 0.041940,
*     ~0.145085, 0.007166, 1.205260, 0.029780,
*     ~0.063994, 0.000796, 1.732560, 0.046879,
*     ~0.027964, 0.000447, 1.868700, 0.024292,
*c     /* NFALSE=5 */
*     ~3.716977, 1.800665, 1.039480, 0.146746,
*     ~0.818764, 0.171987, 1.028100, 0.023007,
*     ~0.327363, 0.032622, 1.057030, 0.018670,
*     ~0.142062, 0.007087, 1.233170, 0.033125,
*     ~0.062276, 0.000742, 1.785310, 0.048906,
*     ~0.026906, 0.000369, 1.823990, 0.022170,
*c     /* NFALSE=6 */
*     ~3.686084, 1.745483, 1.048850, 0.180065,
*     ~0.809302, 0.169379, 1.035160, 0.028455,
*     ~0.322940, 0.031901, 1.081490, 0.026316,
*     ~0.139833, 0.006535, 1.196570, 0.027487,
*     `0.060499, 0.001102, 1.803410, 0.048606,
*     ~0.026147, 0.000145, 1.789780, 0.020650,
*c     /* NFALSE=7 */
*     ~3.648399, 1.723629, 1.048340, 0.176364,
*     ~0.801254, 0.166494, 1.022370, 0.017924,
*     ~0.319023, 0.031652, 1.071570, 0.022832,
*     ~0.137449, 0.006697, 1.211450, 0.029064,
*     ~0.059397, 0.000898, 1.856670, 0.050884,
*     ~0.025517, -0.000144, 1.860620, 0.021961,
*c     /* NFALSE=8 */
*     ~3.611073, 1.710319, 1.018120, 0.065433,
*     ~0.793720, 0.166363, 1.022410, 0.017787,
*     ~0.315536, 0.031335, 1.090530, 0.028565,
*     ~0.135553, 0.007011, 1.216780, 0.029385,
*     ~0.058238, 0.000929, 1.890550, 0.051864,
*     ~0.024815, -0.000057, 1.905070, 0.022459,
*c    /* NFALSE=9 */
*     ~3.578373, 1.699839, 1.020370, 0.072891,
*     ~0.786542, 0.165893, 1.023620, 0.018578,
*     ~0.313004, 0.029936, 1.065590, 0.020530,
*     ~0.134123, 0.006531, 1.247670, 0.033218,
*     ~0.057347, 0.000768, 1.936180, 0.053687,
*     ~0.024044, 0.000164, 1.944470, 0.022709,
*c     /* NFALSE=10 */
*     ~3.550305, 1.689432, 1.015540, 0.055172,
*     ~0.780892, 0.164180, 1.023390, 0.018265,
*     ~0.309935, 0.030113, 1.035850, 0.011111,
*     ~0.132752, 0.006200, 1.273000, 0.036241,
*     ~0.056746, 0.000314, 2.002940, 0.056912,
*     ~0.023582, -0.000031, 2.002100, 0.023632/
*      
**      IDX=(NFALSE-1)*JMAX+(JSCALE-1)*4
*      IF (MEAN .lt. 1.0) THEN
*         threshold_atrous=
**     ~fit_tab(IDX+1)*SQRT(MEAN)*fit_tab(IDX+3)+fit_tab(IDX+2)    
*     ~      fit_tab(1,jscale,nfalse)*SQRT(MEAN)*fit_tab(3,jscale,nfalse)
*     ~      + fit_tab(2,jscale,nfalse)    
*      ELSE
*         threshold_atrous=
*     ~      fit_tab(1,jscale,nfalse)*SQRT(MEAN)+fit_tab(2,jscale,nfalse)
*     ~      +fit_tab(4,jscale,nfalse)    
*      ENDIF
*      END




c      real function threshold_atrous (bg,nfalse,scale)
c      implicit none
c      real bg, nfalse
c      integer scale
c      print *,bg,nfalse,scale 
c      threshold_atrous=0.0
c      return 
c      end
      
*xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

*      x = poisson_threshold (0.003,1.0,5,2)
*      print *,x
*      end
*
      real function poisson_threshold (bg,nfalse,scale,kernel)
      implicit none
      real bg, nfalse
      integer scale, kernel

      integer nthreshold
      parameter (nthreshold=13)
      
      real a_gw_1(nthreshold), a_gw_2(nthreshold), a_gw_3(nthreshold), 
     ~    a_gw_4(nthreshold), a_gw_5(nthreshold), a_gw_6(nthreshold)
      real b_gw_1(nthreshold), b_gw_2(nthreshold), b_gw_3(nthreshold), 
     ~    b_gw_4(nthreshold), b_gw_5(nthreshold), b_gw_6(nthreshold)

      data a_gw_1 /5.4053855, 5.2422719, 5.2352457, 5.1638274, 5.0609927, 5
     ~    .3703513, 5.2370458, 4.7637968, 5.0025048, 4.6729369, 4.7648511, 4
     ~    .0239735, 4.4533181/
      data b_gw_1 /2.5704300, 2.6347704, 2.5997653, 2.5947676, 2.5371401, 2
     ~    .1397161, 2.1062927, 2.3322740, 1.8181710, 1.9035916, 1.5571944, 1
     ~    .9376086, 1.2596880/
      data a_gw_2 /0.7744129, 0.7024001, 0.6810294, 0.6829411, 0.6750912, 0
     ~    .6567348, 0.6337451, 0.6183532, 0.6002623, 0.5828905, 0.5629064, 0
     ~    .5425054, 0.5207035/
      data b_gw_2 /0.1939876, 0.2009067, 0.1911447, 0.1691117, 0.1571689, 0
     ~    .1511799, 0.1467297, 0.1387145, 0.1303925, 0.1219212, 0.1143354, 0
     ~    .1062726, 0.0987081/
      data a_gw_3 /0.5927348, 0.5809445, 0.5657698, 0.5556908, 0.5491481, 0
     ~    .5332669, 0.5189599, 0.5053969, 0.4895512, 0.4726347, 0.4560201, 0
     ~    .4386883, 0.4195993/
      data b_gw_3 /0.0940348, 0.0888385, 0.0858717, 0.0772181, 0.0704943, 0
     ~    .0667556, 0.0630542, 0.0584287, 0.0542948, 0.0507058, 0.0465527, 0
     ~    .0424441, 0.0385486/
      data a_gw_4 /0.2746670, 0.2718366, 0.2662930, 0.2592975, 0.2499306, 0
     ~    .2428910, 0.2345180, 0.2267343, 0.2179423, 0.2096130, 0.2004359, 0
     ~    .1908897, 0.1807240/
      data b_gw_4 /0.0215291, 0.0195207, 0.0175845, 0.0161496, 0.0154617, 0
     ~    .0143611, 0.0134531, 0.0124948, 0.0117210, 0.0106567, 0.0097161, 0
     ~    .0087401, 0.0077695/
      data a_gw_5 /0.1328869, 0.1278839, 0.1242991, 0.1207107, 0.1167055, 0
     ~    .1125724, 0.1078794, 0.1035735, 0.0991736, 0.0942964, 0.0892804, 0
     ~    .0839036, 0.0782242/
      data b_gw_5 /0.0044624, 0.0041118, 0.0038744, 0.0035269, 0.0032022, 0
     ~    .0029794, 0.0027686, 0.0025877, 0.0023492, 0.0021335, 0.0018810, 0
     ~    .0016553, 0.0014156/
      data a_gw_6 /0.0577636, 0.0556609, 0.0541130, 0.0516855, 0.0494620, 0
     ~    .0476199, 0.0456545, 0.0434927, 0.0413100, 0.0387674, 0.0361295, 0
     ~    .0331096, 0.0298884/
      data b_gw_6 /0.0009727, 0.0008309, 0.0007589, 0.0007453, 0.0007450, 0
     ~    .0006455, 0.0005938, 0.0005238, 0.0004512, 0.0003969, 0.0003355, 0
     ~    .0002791, 0.0001945/

      save a_gw_1, a_gw_2, a_gw_3, a_gw_4, a_gw_5, a_gw_6, b_gw_1, b_gw_2,
     ~    b_gw_3, b_gw_4, b_gw_5, b_gw_6
     
     
c Data are taken from the calibration file:
*
*scale nfalse      a           b
*  1   0.001   5.4053855   2.5704300
*  1   0.002   5.2422719   2.6347704
*  1   0.004   5.2352457   2.5997653
*  1   0.008   5.1638274   2.5947676
*  1   0.016   5.0609927   2.5371401
*  1   0.032   5.3703513   2.1397161
*  1   0.064   5.2370458   2.1062927
*  1   0.128   4.7637968   2.3322740
*  1   0.256   5.0025048   1.8181710
*  1   0.512   4.6729369   1.9035916
*  1   1.024   4.7648511   1.5571944
*  1   2.048   4.0239735   1.9376086
*  1   4.096   4.4533181   1.2596880
*  2   0.001   0.7744129   0.1939876
*  2   0.002   0.7024001   0.2009067
*  2   0.004   0.6810294   0.1911447
*  2   0.008   0.6829411   0.1691117
*  2   0.016   0.6750912   0.1571689
*  2   0.032   0.6567348   0.1511799
*  2   0.064   0.6337451   0.1467297
*  2   0.128   0.6183532   0.1387145
*  2   0.256   0.6002623   0.1303925
*  2   0.512   0.5828905   0.1219212
*  2   1.024   0.5629064   0.1143354
*  2   2.048   0.5425054   0.1062726
*  2   4.096   0.5207035   0.0987081
*  3   0.001   0.5927348   0.0940348
*  3   0.002   0.5809445   0.0888385
*  3   0.004   0.5657698   0.0858717
*  3   0.008   0.5556908   0.0772181
*  3   0.016   0.5491481   0.0704943
*  3   0.032   0.5332669   0.0667556
*  3   0.064   0.5189599   0.0630542
*  3   0.128   0.5053969   0.0584287
*  3   0.256   0.4895512   0.0542948
*  3   0.512   0.4726347   0.0507058
*  3   1.024   0.4560201   0.0465527
*  3   2.048   0.4386883   0.0424441
*  3   4.096   0.4195993   0.0385486
*  4   0.001   0.2746670   0.0215291
*  4   0.002   0.2718366   0.0195207
*  4   0.004   0.2662930   0.0175845
*  4   0.008   0.2592975   0.0161496
*  4   0.016   0.2499306   0.0154617
*  4   0.032   0.2428910   0.0143611
*  4   0.064   0.2345180   0.0134531
*  4   0.128   0.2267343   0.0124948
*  4   0.256   0.2179423   0.0117210
*  4   0.512   0.2096130   0.0106567
*  4   1.024   0.2004359   0.0097161
*  4   2.048   0.1908897   0.0087401
*  4   4.096   0.1807240   0.0077695
*  5   0.001   0.1328869   0.0044624
*  5   0.002   0.1278839   0.0041118
*  5   0.004   0.1242991   0.0038744
*  5   0.008   0.1207107   0.0035269
*  5   0.016   0.1167055   0.0032022
*  5   0.032   0.1125724   0.0029794
*  5   0.064   0.1078794   0.0027686
*  5   0.128   0.1035735   0.0025877
*  5   0.256   0.0991736   0.0023492
*  5   0.512   0.0942964   0.0021335
*  5   1.024   0.0892804   0.0018810
*  5   2.048   0.0839036   0.0016553
*  5   4.096   0.0782242   0.0014156
*  6   0.001   0.0577636   0.0009727
*  6   0.002   0.0556609   0.0008309
*  6   0.004   0.0541130   0.0007589
*  6   0.008   0.0516855   0.0007453
*  6   0.016   0.0494620   0.0007450
*  6   0.032   0.0476199   0.0006455
*  6   0.064   0.0456545   0.0005938
*  6   0.128   0.0434927   0.0005238
*  6   0.256   0.0413100   0.0004512
*  6   0.512   0.0387674   0.0003969
*  6   1.024   0.0361295   0.0003355
*  6   2.048   0.0331096   0.0002791
*  6   4.096   0.0298884   0.0001945


*     integer ith1, ith2, ith
*     real xith
      real a, b
*     real w1, w2
      save a, b
      integer scaleold, nfalseold
      save scaleold, nfalseold
      data scaleold /-1/


      if (kernel.ne.2) then
        !!!!!!!!!!!!! CURRENTLY, ONLY FOR GAUSS KERNEL !!!!!!!!!!!!!!!!
        call exiterror
     ~      ('nfalse thresholds are calibrated only for gauss kernel')
      endif

      if ( (scale.ne.scaleold) .or. (nfalse.ne.nfalseold) ) then
        scaleold = scale
        nfalseold = nfalse

        call do_threshold_par_interpolation (
     ~      nfalse, scale, 0.001, 4.096, 
     ~      a_gw_1, b_gw_1,
     ~      a_gw_2, b_gw_2,
     ~      a_gw_3, b_gw_3,
     ~      a_gw_4, b_gw_4,
     ~      a_gw_5, b_gw_5,
     ~      a_gw_6, b_gw_6,
     ~      nthreshold,
     ~      a, b)
      endif

      poisson_threshold = a*sqrt(max(bg,0.0))+b
      return
      end

      subroutine do_threshold_par_interpolation (
     ~      nfalse, scale, pmin, pmax, 
     ~      a_1, b_1,
     ~      a_2, b_2,
     ~      a_3, b_3,
     ~      a_4, b_4,
     ~      a_5, b_5,
     ~      a_6, b_6,
     ~      nthreshold,
     ~      a, b)
      implicit none
      real nfalse
      integer scale
      real pmin, pmax
      integer nthreshold
      real a_1(nthreshold), a_2(nthreshold), a_3(nthreshold), a_4(nthreshold),
     ~    a_5(nthreshold), a_6(nthreshold)
      real b_1(nthreshold), b_2(nthreshold), b_3(nthreshold), b_4(nthreshold),
     ~    b_5(nthreshold), b_6(nthreshold)
      real a,b
      integer ith1,ith2
      real xith, w1,w2
      real a1,a2,b1,b2
      
      xith = log(nfalse/pmin)/log(2.0)+1.0
      if (nfalse.lt.pmin) then
        ith1 = 1
        ith2 = 2
      else if (nfalse.gt.pmax) then
        ith1 = nthreshold - 1
        ith2 = nthreshold
      else
        ith1 = int(xith)
        ith2 = ith1 + 1
      endif
      w1 = ith2 - xith
      w2 = xith - ith1
        
      if (scale .eq. 1 ) then
        a = w1*a_1(ith1)+w2*a_1(ith2)
        b = w1*b_1(ith2)+w2*b_1(ith2)
      else if (scale .eq. 2 ) then
        a = w1*a_2(ith1)+w2*a_2(ith2)
        b = w1*b_2(ith2)+w2*b_2(ith2)
      else if (scale .eq. 3 ) then
        a = w1*a_3(ith1)+w2*a_3(ith2)
        b = w1*b_3(ith2)+w2*b_3(ith2)
      else if (scale .eq. 4 ) then
        a = w1*a_4(ith1)+w2*a_4(ith2)
        b = w1*b_4(ith2)+w2*b_4(ith2)
      else if (scale .eq. 5 ) then
        a = w1*a_5(ith1)+w2*a_5(ith2)
        b = w1*b_5(ith2)+w2*b_5(ith2)
      else if (scale .eq. 6 ) then
        a = w1*a_6(ith1)+w2*a_6(ith2)
        b = w1*b_6(ith2)+w2*b_6(ith2)
      else
c
c It can be shown that log of the coefficient changes linearly with scale.
c so we can extrapolate to large scales
c
        a1 = w1*a_5(ith1)+w2*a_5(ith2)
        b1 = w1*b_5(ith2)+w2*b_5(ith2)
        a2 = w1*a_6(ith1)+w2*a_6(ith2)
        b2 = w1*b_6(ith2)+w2*b_6(ith2)
        a = exp ( log(a2) + (log(a2)-log(a1))*(scale-6) )
        b = exp ( log(b2) + (log(b2)-log(b1))*(scale-6) )
      endif
      return
      end
      
