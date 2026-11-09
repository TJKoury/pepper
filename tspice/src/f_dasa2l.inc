!     Include File:  F_DASA2L Parameters
!
!        f_dasa2l.inc  Version 1    12-APR-2014 (NJB)
!
!

!
!     The parameters below are names of logic paths in DASA2L.
!     These are used by F_DASA2L and P_DASA2L (an instrumented
!     test version of DASA2L).
!
      CHARACTER*(*)         SAMEFL
      PARAMETER           ( SAMEFL = 'SAME FILE' )

      CHARACTER*(*)         NOTSAM
      PARAMETER           ( NOTSAM = 'NOT SAME FILE' )

      CHARACTER*(*)         TYPCHK
      PARAMETER           ( TYPCHK = 'DATA TYPE CHECK' )

      CHARACTER*(*)         UNKNWN
      PARAMETER           ( UNKNWN = 'UNKNOWN FILE' )

      CHARACTER*(*)         BUFSHF
      PARAMETER           ( BUFSHF = 'BUFFER SHIFT' )

      CHARACTER*(*)         BUFINS
      PARAMETER           ( BUFINS = 'BUFFER INSERTION' )

      CHARACTER*(*)         GFSUM0
      PARAMETER           ( GFSUM0 = 'GET FILE SUMMARY (0)' )

      CHARACTER*(*)         GFSUM1
      PARAMETER           ( GFSUM1 = 'GET FILE SUMMARY (1)' )

      CHARACTER*(*)         GFSUM2
      PARAMETER           ( GFSUM2 = 'GET FILE SUMMARY (2)' )

      CHARACTER*(*)         ADDCHK
      PARAMETER           ( ADDCHK = 'ADDRESS RANGE CHECK' )

      CHARACTER*(*)         SLOWSR
      PARAMETER           ( SLOWSR = 'SLOW FILE CLUSTER SEARCH' )

      CHARACTER*(*)         SLOWST
      PARAMETER           ( SLOWST = 'SLOW FILE SET CLUSTER PARAMS' )

      CHARACTER*(*)         FASTST
      PARAMETER           ( FASTST = 'FAST FILE SET CLUSTER PARAMS' )

      CHARACTER*(*)         CALC
      PARAMETER           ( CALC   = 'CALCULATE RECORD AND WORD' )

      CHARACTER*(*)         READON
      PARAMETER           ( READON = 'READ ONLY' )

      CHARACTER*(*)         NOTRDO
      PARAMETER           ( NOTRDO = 'NOT READ ONLY' )

      CHARACTER*(*)         SEGCHK
      PARAMETER           ( SEGCHK = 'SEGREGATION CHECK' )

      CHARACTER*(*)         SETTBF
      PARAMETER           ( SETTBF = 'SET TBFAST' )

      CHARACTER*(*)         BADDIR
      PARAMETER           ( BADDIR = 'BAD DIRECTORY' )

      CHARACTER*(*)         GETACC
      PARAMETER           ( GETACC = 'LOOK UP ACCESS METHOD' )

!
!     End Include File:  F_DASA2L Parameters
!
