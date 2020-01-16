
C******************************************************************************
C SUBROUTINE:
C      xcopyscale
C
C DESCRIPTION:
C      This subroutine moves the extra keywords,i.e., the
C      keywords which don't contain: 
C        SIMPLE
C        BITPIX
C        NAXIS
C        NAXISn
C        EXTEND
C        EXTNAME
C        XTENSION
C        PCOUNT
C        GCOUNT
C        TFIELDS
C        TTYPEn
C        TBCOLn
C        TFORMn
C        TUNITn
C        THEAP
C        TDIMn
C        GROUPS
C        END
C      from the input file to the output file
C
C       this routine DOES copy the following values, so it should be
C       used with caution:
C        TDISPn
C        TSCALn
C        TZEROn
C        TNULLn
C        BLANK
C        CRPIXn
C        CROTAn
C        CRVALn
C        CDELTn
C        CTYPEn
C        BSCALE
C        BZERO
C        BUNIT
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
C       and to xcopyscale so this doesn't have to keep being done!
C               EAG 4/21/93
C       7/22/93 EAG should not copy EXTNAME keyword
C       12/2/93 EAG save any additional space in the input header in the output
C
C NOTES:
C       
C USAGE:
C      call xcopyscale (iunit,ounit,status)
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
      subroutine xcopyscale (iunit,ounit,status)

      integer     iunit,ounit,status
      integer     i,nkeys,nmore
      logical     l1,l2,l3,l4,l5,l6,l7,l8,l9,l10,l11, l12,l13,l14,l15
      logical     l16, l17
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
         l12 = index(record(1:5),'TUNIT') .le. 0
         l13 = index(record(1:5),'THEAP') .le. 0
         l14 = index(record(1:4),'TDIM') .le. 0
         l15 = index(record(1:6),'GROUPS') .le. 0
         l16 = index(record(1:3),'END') .le. 0
         l17 = index(record(1:7),'EXTNAME') .le. 0

C check if this is a good record
         copyflg = l1 .and. l2 .and. l3 .and. l4 .and. l5
     &        .and. l6 .and. l7 .and. l8 .and. l9 .and. l10 
     &        .and. l11 .and. l12 .and. l13 .and. l14 .and. l15
     &        .and. l16 .and. l17
         if ( copyflg ) call ftprec(ounit,record,status)

 10   continue

C save any additional space that is in the input header
C       if (nmore .gt. 0) call fthdef (ounit, nmore, status)

      return
      end
