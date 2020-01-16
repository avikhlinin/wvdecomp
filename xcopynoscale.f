
C******************************************************************************
C SUBROUTINE:
C      xcopynoscale
C
C DESCRIPTION:
C      This subroutine moves the extra keywords,i.e., the
C      keywords which don't contain: 
C        SIMPLE
C        BITPIX
C        NAXIS
C        NAXISn
C        EXTEND
C        XTENSION
C        EXTNAME
C        PCOUNT
C        GCOUNT
C        TFIELDS
C        TTYPEn
C        TBCOLn 
C        TFORMn 
C        TSCALn
C        TZEROn
C        TNULLn
C        TUNITn
C        THEAP
C        TDIMn
C        TDISPn
C        GROUPS
C        BSCALE
C        BZERO
C        BUNIT
C        BLANK
C        CTYPEn
C        CRPIXn
C        CROTAn
C        CRVALn
C        CDELTn
C        TLMINn
C        TLMAXn
C        OPTICn
C        TCRPXn
C        TCRVLn
C        TCDLTn
C        TCTYPn
C        TCDnnnn
C        TCROTn
C        PLTSCLn
C        END
C      from the input file to the output file
C
C AUTHOR/DATE:
C
C      James Kent Blackburn 11/05/91
C
C MODIFICATION HERSTORY:
C       Copied from FPROJECT routine FIMEXK 
C              and modified for FAINT task   EAG  8/4/92
C               modified for FBURST task             EAG 10/14/92
C               and now for FMASKFILT  EAG 4/20/93
C       and to xcopynoscale so this doesn't have to keep being done!
C               EAG 4/21/93
C       7/22/93 Should not copy EXTNAME
C       10/7/93 EAG should not copy TLMINn or TLMAXn
C       12/16/93 EAG should not copy OPTICn, TCRPXn, TCRVLn, TCDLTn, TCTYPn
C       1/25/94 EAG should not copy TCDnnnn, TCROT, PLTSCLn
C
C NOTES:
C       
C USAGE:
C      call xcopynoscale (iunit,ounit,status)
C
C ARGUMENTS:
C      iunit - input unit number
C      ounit - output unit number
C      status  - error number
C
C PRIMARY LOCAL VARIABLES:
C      i* -index to substring 
C      l* - substring presence flag
C      nkeys - number of keywords
C      copyflg - copy keyword flag
C
C CALLED ROUTINES:
C      subroutine ftghsp - get number of keywords in extension
C      subroutine ftgrec - get keyword record
C      subroutine ftprec - put keyword record
C
C******************************************************************************
      subroutine xcopynoscale (iunit,ounit,status)

      integer     iunit,ounit,status
      integer     i,nkeys,nmore, fcstln
      logical     l1,l2,l3,l4,l5,l6,l7,l8,l9,l10,l11, l12,l13,l14,l15
      logical     l16, l17, l18, l19, l20, l21, l22, l23, l24, l25
      logical     l26, l27, l28, l29, l30, l31, l32, l33, l34, l35
      logical     l36, l37, l38, l39, l40
      logical     copyflg
      character  record*80

C find how many keys there are:
      call ftghsp(iunit,nkeys,nmore,status)

C loop over all keys
      do 10 i = 1, nkeys
         call ftgrec(iunit,i,record,status)
         l1 = index(record(1:6),'SIMPLE') .le. 0
         l2 = index(record(1:6),'BITPIX') .le. 0
         l3 = index(record(1:5),'NAXIS') .le. 0
         l4 = index(record(1:6),'EXTEND') .le. 0
         l5 = index(record(1:8),'XTENSION') .le. 0
         l6 = index(record(1:6),'PCOUNT') .le. 0
         l7 = index(record(1:6),'GCOUNT') .le. 0
         l8 = index(record(1:7),'TFIELDS') .le. 0
         l9 = index(record(1:5),'TTYPE') .le. 0
         l10 = index(record(1:5),'TBCOL') .le. 0
         l11 = index(record(1:5),'TFORM') .le. 0
         l12 = index(record(1:5),'TSCAL') .le. 0
         l13 = index(record(1:5),'TZERO') .le. 0
         l14 = index(record(1:5),'TNULL') .le. 0
         l15 = index(record(1:5),'TUNIT') .le. 0
         l16 = index(record(1:5),'THEAP') .le. 0
         l17 = index(record(1:4),'TDIM') .le. 0
         l18 = index(record(1:5),'TDISP') .le. 0
         l19 = index(record(1:6),'GROUPS') .le. 0
         l20 = index(record(1:6),'BSCALE') .le. 0
         l21 = index(record(1:5),'BZERO') .le. 0
         l22 = index(record(1:5),'BUNIT') .le. 0
         l23 = index(record(1:5),'BLANK') .le. 0
         l24 = index(record(1:5),'CTYPE') .le. 0
         l25 = index(record(1:5),'CRPIX') .le. 0
         l26 = index(record(1:5),'CROTA') .le. 0
         l27 = index(record(1:5),'CRVAL') .le. 0
         l28 = index(record(1:5),'CDELT') .le. 0
         l29 = index(record(1:3),'END') .le. 0
         l30 = index(record(1:7),'EXTNAME') .le. 0
         l31 = index(record(1:5),'TLMIN') .le. 0
         l32 = index(record(1:5),'TLMAX') .le. 0
         l33 = index(record(1:5),'OPTIC') .le. 0
         l34 = index(record(1:5),'TCRPX') .le. 0
         l35 = index(record(1:5),'TCRVL') .le. 0
         l36 = index(record(1:5),'TCDLT') .le. 0
         l37 = index(record(1:5),'TCTYP') .le. 0
         l38 = index(record(1:5),'TCROT') .le. 0
         l39 = index(record(1:6), 'PLTSCL') .le. 0
         l40 = index(record(1:3), 'TCD') .le. 0 .and. 
     &        fcstln(record) .ge. 7

C check if this is a good record
         copyflg = l1 .and. l2 .and. l3 .and. l4 .and. l5
     &        .and. l6 .and. l7 .and. l8 .and. l9 .and. l10 
     &        .and. l11 .and. l12 .and. l13 .and. l14 .and. l15
     &        .and. l16 .and. l17 .and. l18 .and. l19 .and. l20
     &        .and. l21 .and. l22 .and. l23 .and. l24 .and. l25
     &        .and. l26 .and. l27 .and. l28 .and. l29 .and. l30
     &        .and. l31 .and. l32 .and. l33 .and. l34 .and. l35
     &        .and. l36 .and. l37 .and. l38 .and. l39 .and. l40
         if ( copyflg ) call ftprec(ounit,record,status)

 10   continue

C save any additional space that is in the input header
C       if (nmore .gt. 0) call fthdef (ounit, nmore, status)

      return
      end
