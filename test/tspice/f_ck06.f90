!$Procedure F_CK06 ( CK data type 06 tests )

      SUBROUTINE F_CK06 ( OK )

! $ Abstract
!
!     Exercise routines associated with CK data type 06.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     TEST FAMILY
!
! $ Declarations

      IMPLICIT NONE

      INCLUDE 'ck06.inc'

      LOGICAL               OK

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     OK         O   logical indicating test status.
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     OK         is a logical that indicates the test status to the
!                caller.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     This routine does not generate any errors. Routines in its
!     call tree may generate errors that are either intentional and
!     trapped or unintentional and need reporting.  The test family
!     utilities manage this.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine tests routines that write and read type 06 CK
!     data.
!
! $ Examples
!
!     None.
!
! $ Restrictions
!
!     None.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman     (JPL)
!
! $ Version
!
!-    TSPICE Version 2.0.0, 11-AUG-2015 (NJB)
!
!        Added test case for CKW06 invalid input SCLK rate.
!
!-    TSPICE Version 1.0.0, 24-MAR-2014 (NJB)
!
!
!-&

!
!     SPICELIB functions
!
      LOGICAL               EVEN
      LOGICAL               EXISTS

!
!     Local Parameters
!
      CHARACTER*(*)         CK06E
      PARAMETER           ( CK06E  = 'test06err.bc' )

      CHARACTER*(*)         CK06T0
      PARAMETER           ( CK06T0 = 'ck06t0.bc' )

      CHARACTER*(*)         CK06T1
      PARAMETER           ( CK06T1 = 'ck06t1.bc' )

      CHARACTER*(*)         CK06MU
      PARAMETER           ( CK06MU = 'ck06mult.bc' )

      CHARACTER*(*)         CK06B0
      PARAMETER           ( CK06B0 = 'ck06big0.bc' )

      CHARACTER*(*)         CK06B1
      PARAMETER           ( CK06B1 = 'ck06big1.bc' )

      CHARACTER*(*)         CK06B2
      PARAMETER           ( CK06B2 = 'ck06big2.bc' )



      DOUBLE PRECISION      VTIGHT
      PARAMETER           ( VTIGHT = 1.D-14 )

      DOUBLE PRECISION      TIGHT
      PARAMETER           ( TIGHT  = 1.D-10 )

      DOUBLE PRECISION      MTIGHT
      PARAMETER           ( MTIGHT = 1.D-8 )

      DOUBLE PRECISION      MED
      PARAMETER           ( MED    = 1.D-6 )

      INTEGER               FILSIZ
      PARAMETER           ( FILSIZ = 255 )

      INTEGER               MAXIVL
      PARAMETER           ( MAXIVL = 21000 )

      INTEGER               BIGN
      PARAMETER           ( BIGN   = MAXIVL * 40 )


      INTEGER               DSCSIZ
      PARAMETER           ( DSCSIZ = 5 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

      INTEGER               MSGLEN
      PARAMETER           ( MSGLEN = 800 )

      INTEGER               ND
      PARAMETER           ( ND     = 2 )

      INTEGER               NI
      PARAMETER           ( NI     = 6 )

      INTEGER               SIDLEN
      PARAMETER           ( SIDLEN = 60 )

      INTEGER               FRNMLN
      PARAMETER           ( FRNMLN = 32 )

      INTEGER               RECSIZ
      PARAMETER           ( RECSIZ = MAXRSZ + 4 )

      INTEGER               MAXSEG
      PARAMETER           ( MAXSEG = 3 )

!
!     Local Variables
!
      CHARACTER*(FILSIZ)    CK
      CHARACTER*(FRNMLN)    CKFRAM
      CHARACTER*(LNSIZE)    NAME
      CHARACTER*(SIDLEN)    SEGID
      CHARACTER*(MSGLEN)    TITLE
      CHARACTER*(FRNMLN)    XREF
      CHARACTER*(FRNMLN)    XREFS  ( MAXSEG )

      DOUBLE PRECISION      AV     ( 3 )
      DOUBLE PRECISION      AVBUFF ( 3, BIGN )
      DOUBLE PRECISION      AVQ    ( 0 : 3 )
      DOUBLE PRECISION      AVVS   ( 3, BIGN )
      DOUBLE PRECISION      CKTOL
      DOUBLE PRECISION      CLKOUT
      DOUBLE PRECISION      CMAT   ( 3, 3 )
      DOUBLE PRECISION      DAVV   ( 3 )
      DOUBLE PRECISION      DC     ( ND )
      DOUBLE PRECISION      DELTA
      DOUBLE PRECISION      DESCR  ( DSCSIZ )
      DOUBLE PRECISION      EPOCHS ( BIGN )
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      FIRST
      DOUBLE PRECISION      IVLBDS ( MAXIVL + 1 )
      DOUBLE PRECISION      LAST
      DOUBLE PRECISION      LSTEPC
      DOUBLE PRECISION      PACKTS ( BIGN * C06PS0 )
      DOUBLE PRECISION      QNEG   ( 0:3 )
      DOUBLE PRECISION      QUATS  ( 0:3, BIGN )
      DOUBLE PRECISION      RATE
      DOUBLE PRECISION      RATES  ( MAXIVL )
      DOUBLE PRECISION      RECORD ( RECSIZ )
      DOUBLE PRECISION      SCLKDP ( BIGN )
      DOUBLE PRECISION      T
      DOUBLE PRECISION      T0
      DOUBLE PRECISION      T1
      DOUBLE PRECISION      TOL
      DOUBLE PRECISION      XAV    ( 3 )
      DOUBLE PRECISION      XAVVS  ( 3, BIGN )
      DOUBLE PRECISION      XCMAT  ( 3, 3 )
      DOUBLE PRECISION      XCMATS ( 9, BIGN )
      DOUBLE PRECISION      XDQ    ( 0 : 3 )
      DOUBLE PRECISION      XEPOCH ( BIGN )
      DOUBLE PRECISION      XFORM  ( 6, 6 )
      DOUBLE PRECISION      XIVBDS ( MAXIVL + 1 )
      DOUBLE PRECISION      XQ     ( 0 : 3 )
      DOUBLE PRECISION      XQUATS ( 0 : 3, BIGN )
      DOUBLE PRECISION      XREC   ( RECSIZ )
      DOUBLE PRECISION      XT

      INTEGER               BEGREC
      INTEGER               DEGRES ( MAXIVL )
      INTEGER               DATIDX ( MAXIVL )
      INTEGER               HANDLE
      INTEGER               I
      INTEGER               IC     ( NI )
      INTEGER               J
      INTEGER               JMAX
      INTEGER               K
      INTEGER               M
      INTEGER               MNSGNO
      INTEGER               N
      INTEGER               NINTVL
      INTEGER               NIVALS ( MAXSEG )
      INTEGER               NMINI
      INTEGER               NPKTS  ( MAXIVL )
      INTEGER               NREC
      INTEGER               NSEG
      INTEGER               PAD
      INTEGER               PSIZES ( MAXIVL )
      INTEGER               PKTBEG ( MAXIVL )
      INTEGER               PKTPTR
      INTEGER               PKTSIZ
      INTEGER               PKTSZS ( 0 : 3 )
      INTEGER               PKTTOT
      INTEGER               RECTOT
      INTEGER               SEGNO
      INTEGER               SEGPTR
      INTEGER               SIZE
      INTEGER               SKIP
      INTEGER               SUBTPS ( MAXIVL )
      INTEGER               SUBTYP
      INTEGER               XINST
      INTEGER               XINSTS ( MAXSEG )
      INTEGER               WINSIZ
      INTEGER               XWNSIZ

      LOGICAL               FOUND
      LOGICAL               SELLST
      LOGICAL               XAVFLG

!
!     Saved values
!
      SAVE

!
!     Initial values
!
      DATA                  PKTSZS / C06PS0, C06PS1, C06PS2, C06PS3 /
!
!     Open the test family.
!
      CALL TOPEN ( 'F_CK06' )


!*****************************************************************
!*
!*    CKW06 error cases:
!*
!*****************************************************************


!
!     Test CKW06:  start out with error handling.
!

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'CKW06 setup' )

!
!     Initialize the inputs to CKW06.
!
      XINST  = 3
      XAVFLG = .TRUE.
      XREF   = 'J2000'
!
!     Create a segment identifier.
!
      SEGID = 'CK type 06 test segment'

!
!     Open a new CK file.
!
      IF ( EXISTS(CK06E) ) THEN
         CALL DELFIL(CK06E)
         CALL CHCKXC( .FALSE., ' ', OK )
      END IF

      CALL CKOPN( CK06E, 'Type 06 CK error file', 4, HANDLE )
      CALL CHCKXC( .FALSE., ' ', OK )


      NINTVL    = 1
      NPKTS(1)  = 2
      RATES(1)  = 1.D0
      SUBTPS(1) = C06TP1
      DEGRES(1) = 1


      RECTOT = 0
      PKTTOT = 0

      DO I = 1, NINTVL
!
!        Update total counts of records and packet elements.
!        The latter depends on the packet size of the current
!        mini-segment.
!
         IF ( SUBTPS(I) .EQ. C06TP0 ) THEN

            PKTSIZ = C06PS0

         ELSE IF ( SUBTPS(I) .EQ. C06TP1 ) THEN

            PKTSIZ = C06PS1

         ELSE

            CALL SETMSG ( 'Unexpected CK type 06 subtype # ' &
            //            'found in mini-segment #.'          )
            CALL ERRINT ( '#',  SUBTPS(I)                     )
            CALL ERRINT ( '#',  I                             )
            CALL SIGERR ( 'SPICE(INVALIDSUBTYPE)'             )


            CALL CHCKXC( .FALSE., ' ', OK )

         END IF

         PKTTOT = PKTTOT + ( NPKTS(I) * PKTSIZ )

         RECTOT = RECTOT + NPKTS(I)

      END DO

      CALL FILLD ( 0.5D0, PKTTOT, PACKTS )



      DO I = 1, RECTOT
         SCLKDP(I) = DBLE(I)
      END DO

      FIRST     = SCLKDP(1)
      LAST      = SCLKDP(2)

      IVLBDS(1) = FIRST
      IVLBDS(2) = LAST

      SELLST    = .TRUE.


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'CKW06 error case: descriptor times swapped.' )


      CALL CKW06 ( HANDLE, &
                   XINST, &
                   XREF, &
                   XAVFLG, &
                   LAST, &
                   FIRST, &
                   SEGID, &
                   NINTVL, &
                   NPKTS, &
                   SUBTPS, &
                   DEGRES, &
                   PACKTS, &
                   RATES, &
                   SCLKDP, &
                   IVLBDS, &
                   SELLST           )


      CALL CHCKXC ( .TRUE., 'SPICE(BADDESCRTIMES)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'CKW06 error case: bad frame name.'   )

      CALL CKW06 ( HANDLE, &
                   XINST, &
                   'SPUD', &
                   XAVFLG, &
                   FIRST, &
                   LAST, &
                   SEGID, &
                   NINTVL, &
                   NPKTS, &
                   SUBTPS, &
                   DEGRES, &
                   PACKTS, &
                   RATES, &
                   SCLKDP, &
                   IVLBDS, &
                   SELLST           )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDREFFRAME)', OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'CKW06 error case: SEGID too long.'   )

      CALL CKW06 ( HANDLE, &
                   XINST, &
                   XREF, &
                   XAVFLG, &
                   FIRST, &
                   LAST, &
                   'X                                               X', &
                   NINTVL, &
                   NPKTS, &
                   SUBTPS, &
                   DEGRES, &
                   PACKTS, &
                   RATES, &
                   SCLKDP, &
                   IVLBDS, &
                   SELLST           )

      CALL CHCKXC ( .TRUE., 'SPICE(SEGIDTOOLONG)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'CKW06 error case: unprintable SEGID characters.' )

      CALL CKW06 ( HANDLE, &
                   XINST, &
                   XREF, &
                   XAVFLG, &
                   FIRST, &
                   LAST, &
                   CHAR(7), &
                   NINTVL, &
                   NPKTS, &
                   SUBTPS, &
                   DEGRES, &
                   PACKTS, &
                   RATES, &
                   SCLKDP, &
                   IVLBDS, &
                   SELLST           )

      CALL CHCKXC ( .TRUE., 'SPICE(NONPRINTABLECHARS)', OK )



!
! --- Case: ------------------------------------------------------
!


      CALL TCASE ( 'CKW06 error case: non-positive mini-segment count' )

      CALL CKW06 ( HANDLE, &
                   XINST, &
                   XREF, &
                   XAVFLG, &
                   FIRST, &
                   LAST, &
                   SEGID, &
                   0, &
                   NPKTS, &
                   SUBTPS, &
                   DEGRES, &
                   PACKTS, &
                   RATES, &
                   SCLKDP, &
                   IVLBDS, &
                   SELLST           )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDCOUNT)', OK )


      NINTVL = 1



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'CKW06 error case: non-increasing ' &
      //           'mini-segment bounds' )


      IVLBDS(2) = IVLBDS(1)
      FIRST     = 1.5D0
      LAST      = 1.6D0

      CALL CKW06 ( HANDLE, &
                   XINST, &
                   XREF, &
                   XAVFLG, &
                   FIRST, &
                   LAST, &
                   SEGID, &
                   NINTVL, &
                   NPKTS, &
                   SUBTPS, &
                   DEGRES, &
                   PACKTS, &
                   RATES, &
                   SCLKDP, &
                   IVLBDS, &
                   SELLST           )

      CALL CHCKXC ( .TRUE., 'SPICE(BOUNDSOUTOFORDER)', OK )

      IVLBDS(1) = 1.D0
      IVLBDS(2) = 2.D0

      FIRST     = IVLBDS(1)
      LAST      = IVLBDS(2)


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'CKW06 error case: descriptor  ' // &
                   'start time is too early.'       )


      CALL CKW06 ( HANDLE, &
                   XINST, &
                   XREF, &
                   XAVFLG, &
                   FIRST - 1.D0, &
                   LAST, &
                   SEGID, &
                   NINTVL, &
                   NPKTS, &
                   SUBTPS, &
                   DEGRES, &
                   PACKTS, &
                   RATES, &
                   SCLKDP, &
                   IVLBDS, &
                   SELLST           )

      CALL CHCKXC ( .TRUE., 'SPICE(COVERAGEGAP)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'CKW06 error case: descriptor end time is ' // &
                  'too late.'                                  )

      CALL CKW06 ( HANDLE, &
                   XINST, &
                   XREF, &
                   XAVFLG, &
                   FIRST, &
                   LAST + 1.D0, &
                   SEGID, &
                   NINTVL, &
                   NPKTS, &
                   SUBTPS, &
                   DEGRES, &
                   PACKTS, &
                   RATES, &
                   SCLKDP, &
                   IVLBDS, &
                   SELLST           )

      CALL CHCKXC ( .TRUE., 'SPICE(COVERAGEGAP)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'CKW06 error case: too few packets' )

      NPKTS(1) = 1

      CALL CKW06 ( HANDLE, &
                   XINST, &
                   XREF, &
                   XAVFLG, &
                   FIRST, &
                   LAST, &
                   SEGID, &
                   NINTVL, &
                   NPKTS, &
                   SUBTPS, &
                   DEGRES, &
                   PACKTS, &
                   RATES, &
                   SCLKDP, &
                   IVLBDS, &
                   SELLST           )


      CALL CHCKXC ( .TRUE., 'SPICE(TOOFEWPACKETS)', OK )

      NPKTS(1) = 2



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'CKW06 error case: invalid SCLK rate.' )

      RATES(1) = 0.D0

      CALL CKW06 ( HANDLE, &
                   XINST, &
                   XREF, &
                   XAVFLG, &
                   FIRST, &
                   LAST, &
                   SEGID, &
                   NINTVL, &
                   NPKTS, &
                   SUBTPS, &
                   DEGRES, &
                   PACKTS, &
                   RATES, &
                   SCLKDP, &
                   IVLBDS, &
                   SELLST           )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDSCLKRATE)', OK )

      RATES(1) = -1.D0

      CALL CKW06 ( HANDLE, &
                   XINST, &
                   XREF, &
                   XAVFLG, &
                   FIRST, &
                   LAST, &
                   SEGID, &
                   NINTVL, &
                   NPKTS, &
                   SUBTPS, &
                   DEGRES, &
                   PACKTS, &
                   RATES, &
                   SCLKDP, &
                   IVLBDS, &
                   SELLST           )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDSCLKRATE)', OK )

      RATES(1) = 1.D0


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'CKW06 error case: invalid subtype.' )

      SUBTPS(1) = -1

      CALL CKW06 ( HANDLE, &
                   XINST, &
                   XREF, &
                   XAVFLG, &
                   FIRST, &
                   LAST, &
                   SEGID, &
                   NINTVL, &
                   NPKTS, &
                   SUBTPS, &
                   DEGRES, &
                   PACKTS, &
                   RATES, &
                   SCLKDP, &
                   IVLBDS, &
                   SELLST           )


      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDSUBTYPE)', OK )

      SUBTPS(1) = C06TP1



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'CKW06 error case: polynomial degree too high.' )

      DEGRES(1) = 40

      CALL CKW06 ( HANDLE, &
                   XINST, &
                   XREF, &
                   XAVFLG, &
                   FIRST, &
                   LAST, &
                   SEGID, &
                   NINTVL, &
                   NPKTS, &
                   SUBTPS, &
                   DEGRES, &
                   PACKTS, &
                   RATES, &
                   SCLKDP, &
                   IVLBDS, &
                   SELLST           )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDDEGREE)', OK )

      DEGRES(1) = 1


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'CKW06 error case: polynomial degree too low.' )

      DEGRES(1) = 0

      CALL CKW06 ( HANDLE, &
                   XINST, &
                   XREF, &
                   XAVFLG, &
                   FIRST, &
                   LAST, &
                   SEGID, &
                   NINTVL, &
                   NPKTS, &
                   SUBTPS, &
                   DEGRES, &
                   PACKTS, &
                   RATES, &
                   SCLKDP, &
                   IVLBDS, &
                   SELLST           )


      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDDEGREE)', OK )

      DEGRES(1) = 1


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'CKW06 error case:  odd window size.' )

      DEGRES(1) = 2

      CALL CKW06 ( HANDLE, &
                   XINST, &
                   XREF, &
                   XAVFLG, &
                   FIRST, &
                   LAST, &
                   SEGID, &
                   NINTVL, &
                   NPKTS, &
                   SUBTPS, &
                   DEGRES, &
                   PACKTS, &
                   RATES, &
                   SCLKDP, &
                   IVLBDS, &
                   SELLST           )


      CALL CHCKXC ( .TRUE., 'SPICE(BADWINDOWSIZE)', OK )

      DEGRES(1) = 1



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'CKW06 error case: epochs for first ' // &
                   'mini-segment are out of order.'      )

      NINTVL    = 1
      NPKTS(1)  = 4

      DO I = 1, NPKTS(1)
         SCLKDP(I) = DBLE(I)
      END DO

      FIRST     = SCLKDP( 1 )
      LAST      = SCLKDP( NPKTS(1) )

      IVLBDS(1) = FIRST
      IVLBDS(2) = LAST

      CALL SWAPD ( SCLKDP(2), SCLKDP(3) )

      CALL CKW06 ( HANDLE, &
                   XINST, &
                   XREF, &
                   XAVFLG, &
                   FIRST, &
                   LAST, &
                   SEGID, &
                   NINTVL, &
                   NPKTS, &
                   SUBTPS, &
                   DEGRES, &
                   PACKTS, &
                   RATES, &
                   SCLKDP, &
                   IVLBDS, &
                   SELLST           )

      CALL CHCKXC ( .TRUE., 'SPICE(TIMESOUTOFORDER)', OK )

      CALL SWAPD ( SCLKDP(2), SCLKDP(3) )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'CKW06 error case: first mini-segment ' &
      //           'epoch follows interval start.'        )


      SCLKDP(1) = IVLBDS(1) + 0.1D0

      CALL CKW06 ( HANDLE, &
                   XINST, &
                   XREF, &
                   XAVFLG, &
                   FIRST, &
                   LAST, &
                   SEGID, &
                   NINTVL, &
                   NPKTS, &
                   SUBTPS, &
                   DEGRES, &
                   PACKTS, &
                   RATES, &
                   SCLKDP, &
                   IVLBDS, &
                   SELLST           )

      CALL CHCKXC ( .TRUE., 'SPICE(BOUNDSDISAGREE)', OK )


      SCLKDP(1) = 1.D0

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'CKW06 error case: last mini-segment ' &
      //           'epoch precedes interval start.'        )


      DO I = 1, NPKTS(1)
         SCLKDP(I) = DBLE(I) - NPKTS(1) - 1.D0
      END DO

      CALL CKW06 ( HANDLE, &
                   XINST, &
                   XREF, &
                   XAVFLG, &
                   FIRST, &
                   LAST, &
                   SEGID, &
                   NINTVL, &
                   NPKTS, &
                   SUBTPS, &
                   DEGRES, &
                   PACKTS, &
                   RATES, &
                   SCLKDP, &
                   IVLBDS, &
                   SELLST           )

      CALL CHCKXC ( .TRUE., 'SPICE(BOUNDSDISAGREE)', OK )

      DO I = 1, NPKTS(1)
         SCLKDP(I) = DBLE(I)
      END DO

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'CKW06 *non* error case: epochs for first ' &
      //           'mini-segment don''t cover first mini-segment ' &
      //           'interval. A gap exists at the end of the ' &
      //           'first mini-segment.'                           )

      NINTVL    = 1
      NPKTS(1)  = 4

      DO I = 1, NPKTS(1)

         SCLKDP(I) = DBLE(I)
      END DO

      FIRST     = SCLKDP( 1 )
      LAST      = SCLKDP( NPKTS(1) )

      IVLBDS(1) = FIRST
      IVLBDS(2) = LAST

      SCLKDP(4) = SCLKDP(4) - 0.5D0

      PKTTOT = C06PS1 * NPKTS(1)

      CALL FILLD ( 0.5D0, PKTTOT, PACKTS )

      CALL CKW06 ( HANDLE, &
                   XINST, &
                   XREF, &
                   XAVFLG, &
                   FIRST, &
                   LAST, &
                   SEGID, &
                   NINTVL, &
                   NPKTS, &
                   SUBTPS, &
                   DEGRES, &
                   PACKTS, &
                   RATES, &
                   SCLKDP, &
                   IVLBDS, &
                   SELLST           )

      CALL CHCKXC ( .FALSE., ' ', OK )

      DO I = 1, NPKTS(1)
         SCLKDP(I) = DBLE(I)
      END DO



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'CKW06 error case: zero quaternion.'  )

      CALL FILLD ( 0.D0, 4, PACKTS )

      CALL CKW06 ( HANDLE, &
                   XINST, &
                   XREF, &
                   XAVFLG, &
                   FIRST, &
                   LAST, &
                   SEGID, &
                   NINTVL, &
                   NPKTS, &
                   SUBTPS, &
                   DEGRES, &
                   PACKTS, &
                   RATES, &
                   SCLKDP, &
                   IVLBDS, &
                   SELLST           )

      CALL CHCKXC ( .TRUE., 'SPICE(ZEROQUATERNION)', OK )

      DO I = 1, NPKTS(1)
         SCLKDP(I) = DBLE(I)
      END DO


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'CKW06 error case: quaternion sign error for ' &
      //           'subtype 0.'                                  )


      SEGNO  = 1
      MNSGNO = 1
      BEGREC = 1
      N      = 201
      XREF   = 'MARSIAU'

      XINST     = -1000
      XAVFLG    = .TRUE.

      NINTVL    = 1
      NPKTS(1)  = N

      RATES(1)  = 1.D-3
      RATE      = RATES(1)

      SUBTPS(1) = C06TP0
      SUBTYP    = SUBTPS(1)

      PKTSIZ    = C06PS0

      DEGRES(1) = 3


      CALL T_GENCSM ( SEGNO, MNSGNO, BEGREC, NPKTS(MNSGNO), XREF, &
                      RATE,  SUBTYP, SCLKDP, QUATS, AVBUFF, OK   )
      CALL CHCKXC ( .FALSE., ' ', OK )


      XIVBDS(1) = SCLKDP(1)
      XIVBDS(2) = SCLKDP(N)

      FIRST     = XIVBDS(1)
      LAST      = XIVBDS(2)

      PKTPTR = 1

      CALL CLEARD( 3, DAVV )

      DO I = 1, N

         PKTPTR = 1 + (I-1)*PKTSIZ

         CALL T_XSUBTP ( QUATS(0,I), AVBUFF(1,I), DAVV, &
                         SUBTYP,     PKTSIZ,      PACKTS(PKTPTR) )

!
!        Create error in quaternion sequence at packet index 5:
!
         IF ( I .EQ. 5 ) THEN
            CALL VMINUG ( PACKTS(PKTPTR), 4, QNEG           )
            CALL MOVED  ( QNEG,           4, PACKTS(PKTPTR) )
         END IF


      END DO


      SEGID     = 'Subtype 0 test segment'
      SELLST    = .TRUE.

      CALL CKW06 ( HANDLE, &
                   XINST, &
                   XREF, &
                   XAVFLG, &
                   FIRST, &
                   LAST, &
                   SEGID, &
                   NINTVL, &
                   NPKTS, &
                   SUBTPS, &
                   DEGRES, &
                   PACKTS, &
                   RATES, &
                   SCLKDP, &
                   XIVBDS, &
                   SELLST           )
      CALL CHCKXC ( .TRUE., 'SPICE(BADQUATSIGN)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'CKW06 error case: quaternion sign error for ' &
      //           'subtype 2.'                                  )

!
!     Use the inputs from the previous case, where possible.
!
      SUBTPS(1) = C06TP2
      PKTSIZ    = C06PS2

      PKTPTR = 1

      DO I = 1, N

         PKTPTR = 1 + (I-1)*PKTSIZ

         CALL T_XSUBTP ( QUATS(0,I), AVBUFF(1,I), DAVV, &
                         SUBTYP,     PKTSIZ,      PACKTS(PKTPTR) )

!
!        Create error in quaternion sequence at packet index 6:
!
         IF ( I .EQ. 6 ) THEN
            CALL VMINUG ( PACKTS(PKTPTR), 4, QNEG           )
            CALL MOVED  ( QNEG,           4, PACKTS(PKTPTR) )
         END IF

      END DO

      CALL CKW06 ( HANDLE, &
                   XINST, &
                   XREF, &
                   XAVFLG, &
                   FIRST, &
                   LAST, &
                   SEGID, &
                   NINTVL, &
                   NPKTS, &
                   SUBTPS, &
                   DEGRES, &
                   PACKTS, &
                   RATES, &
                   SCLKDP, &
                   XIVBDS, &
                   SELLST           )
      CALL CHCKXC ( .TRUE., 'SPICE(BADQUATSIGN)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'CKW06 non-error case: quaternion negation for ' &
      //           'subtype 1. Make sure no error is signaled.'   )

!
!     Use the inputs from the previous case, where possible.
!
      SUBTPS(1) = C06TP1
      SUBTYP    = SUBTPS(1)
      PKTSIZ    = C06PS1

      PKTPTR = 1

      DO I = 1, N

         PKTPTR = 1 + (I-1)*PKTSIZ

         CALL T_XSUBTP ( QUATS(0,I), AVBUFF(1,I), DAVV, &
                         SUBTYP,     PKTSIZ,      PACKTS(PKTPTR) )

!
!        Create error in quaternion sequence at packet index 7:
!
         IF ( I .EQ. 7 ) THEN
            CALL VMINUG ( PACKTS(PKTPTR), 4, QNEG           )
            CALL MOVED  ( QNEG,           4, PACKTS(PKTPTR) )
         END IF

      END DO

      CALL CKW06 ( HANDLE, &
                   XINST, &
                   XREF, &
                   XAVFLG, &
                   FIRST, &
                   LAST, &
                   SEGID, &
                   NINTVL, &
                   NPKTS, &
                   SUBTPS, &
                   DEGRES, &
                   PACKTS, &
                   RATES, &
                   SCLKDP, &
                   XIVBDS, &
                   SELLST           )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'CKW06 non-error case: quaternion negation for ' &
      //           'subtype 3. Make sure no error is signaled.'   )

!
!     Use the inputs from the previous case, where possible.
!
      SUBTPS(1) = C06TP3
      SUBTYP    = SUBTPS(1)
      PKTSIZ    = C06PS3

      PKTPTR = 1

      DO I = 1, N

         PKTPTR = 1 + (I-1)*PKTSIZ

         CALL T_XSUBTP ( QUATS(0,I), AVBUFF(1,I), DAVV, &
                         SUBTYP,     PKTSIZ,      PACKTS(PKTPTR) )

!
!        Create error in quaternion sequence at packet index 8:
!
         IF ( I .EQ. 8 ) THEN
            CALL VMINUG ( PACKTS(PKTPTR), 4, QNEG           )
            CALL MOVED  ( QNEG,           4, PACKTS(PKTPTR) )
         END IF

      END DO

      CALL CKW06 ( HANDLE, &
                   XINST, &
                   XREF, &
                   XAVFLG, &
                   FIRST, &
                   LAST, &
                   SEGID, &
                   NINTVL, &
                   NPKTS, &
                   SUBTPS, &
                   DEGRES, &
                   PACKTS, &
                   RATES, &
                   SCLKDP, &
                   XIVBDS, &
                   SELLST           )
      CALL CHCKXC ( .FALSE., ' ', OK )



!*****************************************************************
!*
!*    End of CKW06 error cases:
!*
!*****************************************************************

!
!     Close the CK file at the DAF level; CKCLS won't close
!     a file without segments.
!
      CALL DAFCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )




!*****************************************************************
!*
!*     CKW06 Non-error cases:
!*
!*****************************************************************


!*****************************************************************
!*
!*    Trivial case: write an CK containing a small subtype 1
!*    segment.
!*
!*****************************************************************


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Test CKW06: create trivial subtype 1 segment.' )

      SEGNO  = 1
      MNSGNO = 1
      BEGREC = 1
      N      = 201
      XREF   = 'MARSIAU'

      CALL T_GENC06 ( SEGNO, MNSGNO, BEGREC, N,    XREF, &
                      1.D0,  SCLKDP, PACKTS, AVVS, OK )

      CALL CHCKXC ( .FALSE., ' ', OK )

      XINST     = -1000
      XAVFLG    = .TRUE.

      NINTVL    = 1
      NPKTS(1)  = N
      RATES(1)  = 1.D-3
      SUBTPS(1) = C06TP1
      DEGRES(1) = 3

      XIVBDS(1) = SCLKDP(1)
      XIVBDS(2) = SCLKDP(N)

      FIRST     = XIVBDS(1)
      LAST      = XIVBDS(2)

      SEGID     = 'Subtype 1 test segment'

      SELLST    = .TRUE.


!
!     Open new C-kernel.
!
      IF ( EXISTS(CK06T1) ) THEN
         CALL DELFIL( CK06T1 )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF

      CALL CKOPN ( CK06T1, CK06T1, 0, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Write segment.
!
      CALL CKW06 ( HANDLE, &
                   XINST, &
                   XREF, &
                   XAVFLG, &
                   FIRST, &
                   LAST, &
                   SEGID, &
                   NINTVL, &
                   NPKTS, &
                   SUBTPS, &
                   DEGRES, &
                   PACKTS, &
                   RATES, &
                   SCLKDP, &
                   XIVBDS, &
                   SELLST           )


!
!     Close kernel.
!
      CALL CKCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'Read data records from trivial subtype 1 segment' )

      CALL DAFOPR ( CK06T1, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Get mini-segment count.
!
      CALL DAFBFS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFFNA ( FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFGS  ( DESCR )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CKNM06 ( HANDLE, DESCR, NMINI )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CKMP06 ( HANDLE, DESCR, 1,      RATE, SUBTYP, &
                    WINSIZ, NREC,  IVLBDS, LSTEPC )
      CALL CHCKXC ( .FALSE., ' ', OK )


      PKTSIZ = C06PS1

      DO I = 1, NREC

         CALL CKGR06 ( HANDLE, DESCR, 1, I, RECORD )
         CALL CHCKXC ( .FALSE., ' ', OK )
!
!        Check the record. The time tag is the first element.
!        We expect exact matches for all elements.
!
         NAME = 'Time tag *'
         CALL REPMI ( NAME, '*', I, NAME )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSD ( NAME, RECORD(1), '=', SCLKDP(I), 0.D0, OK )

         NAME = 'Subtype code *'
         CALL REPMI ( NAME, '*', I, NAME )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSD ( NAME, RECORD(2),         '=', &
                             DBLE( SUBTPS(1) ), 0.D0, OK )

         NAME = 'Clock rate *'
         CALL REPMI ( NAME, '*', I, NAME )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSD ( NAME, RECORD(3), '=', RATES(1),  0.D0, OK )

!
!        Check the record data.
!
         NAME = 'Quaternion *'
         CALL REPMI ( NAME, '*', I, NAME )
         CALL CHCKXC ( .FALSE., ' ', OK )

         K = PKTSIZ*(I-1) + 1

         CALL CHCKAD ( NAME, RECORD(4), '=', &
                             PACKTS(K), PKTSIZ, 0.D0, OK )

      END DO

!
!     Close kernel.
!
      CALL CKCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'Read data records from trivial subtype 1 segment. ' &
      //            'Open CK for write access but don''t modify the ' &
      //            'file.'                                            )

      CALL DAFOPW ( CK06T1, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Get mini-segment count.
!
      CALL DAFBFS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFFNA ( FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFGS  ( DESCR )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CKNM06 ( HANDLE, DESCR, NMINI )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CKMP06 ( HANDLE, DESCR, 1,      RATE, SUBTYP, &
                    WINSIZ, NREC,  IVLBDS, LSTEPC )
      CALL CHCKXC ( .FALSE., ' ', OK )


      PKTSIZ = C06PS1

      DO I = 1, NREC

         CALL CKGR06 ( HANDLE, DESCR, 1, I, RECORD )
         CALL CHCKXC ( .FALSE., ' ', OK )
!
!        Check the record. The time tag is the first element.
!        We expect exact matches for all elements.
!
         NAME = 'Time tag *'
         CALL REPMI ( NAME, '*', I, NAME )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSD ( NAME, RECORD(1), '=', SCLKDP(I), 0.D0, OK )

         NAME = 'Subtype code *'
         CALL REPMI ( NAME, '*', I, NAME )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSD ( NAME, RECORD(2),         '=', &
                             DBLE( SUBTPS(1) ), 0.D0, OK )

         NAME = 'Clock rate *'
         CALL REPMI ( NAME, '*', I, NAME )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSD ( NAME, RECORD(3), '=', RATES(1),  0.D0, OK )

!
!        Check the record data.
!
         NAME = 'Quaternion *'
         CALL REPMI ( NAME, '*', I, NAME )
         CALL CHCKXC ( .FALSE., ' ', OK )

         K = PKTSIZ*(I-1) + 1

         CALL CHCKAD ( NAME, RECORD(4), '=', &
                             PACKTS(K), PKTSIZ, 0.D0, OK )

      END DO

!
!     Close kernel.
!
      CALL CKCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )





!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'Read from trivial subtype 1 segment using CKGP' )

      CALL FURNSH ( CK06T1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CKTOL = 0.D0
      TOL   = VTIGHT

      DO I = 1, NREC

         CALL CKGP ( XINST, SCLKDP(I), CKTOL, &
                     XREF,  CMAT,      CLKOUT, FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )
!
!        Make sure data were found.
!
         NAME = 'FOUND *'
         CALL REPMI ( NAME, '*', I, NAME )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( NAME, FOUND, .TRUE., OK )

!
!        Check the output time.
!
         NAME = 'CLKOUT *'
         CALL REPMI ( NAME, '*', I, NAME )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSD ( NAME, CLKOUT, '=', SCLKDP(I), 0.D0, OK )

!
!        Check the C-matrix. Some round-off will occur here.
!
         NAME = 'CMAT *'
         CALL REPMI ( NAME, '*', I, NAME )
         CALL CHCKXC ( .FALSE., ' ', OK )

         K = PKTSIZ*(I-1) + 1

         CALL Q2M ( PACKTS(K), XCMAT )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKAD ( NAME, CMAT, '~~/', XCMAT, 9, VTIGHT, OK )

      END DO

      CALL UNLOAD ( CK06T1 )
      CALL CHCKXC ( .FALSE., ' ', OK )



!*****************************************************************
!*
!*     Tests involving segment containing mini-segments having
!*     varied attributes
!*
!*****************************************************************

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Test CKW06: create segment containing ' &
      //           'mini-segments of all subtypes.'        )

!
!     Each mini-segment will have a gap at the end.
!
      XINST     = -1000
      XAVFLG    = .TRUE.
      XREF      = 'GALACTIC'
      CKFRAM    = 'IAU_EARTH'
      NINTVL    = 4
      SEGID     = 'Subtype 1 test segment'
      SELLST    = .TRUE.
      SEGNO     = 1
      N         = 0

!
!     Below, we'll use different clock rates for each mini-segment. But
!     where we can,we'll adjust the angular velocity and quaternion
!     derivatives for the Hermite types so that the angular velocities
!     match when converted to radians/s.
!
!     Where applicable, angular velocity will be scaled by the
!     corresponding rate. This will allow recovery of the original
!     angular velocity after conversion to radians/s.
!

!
!     Pointer into packet array.
!
      PKTPTR    = 1
!
!     Mini-segment 1:
!
      MNSGNO    = 1
      BEGREC    = 1
      NPKTS(1)  = 11
      SUBTPS(1) = C06TP0
      PKTSIZ    = C06PS0
      PSIZES(1) = PKTSIZ
      DEGRES(1) = 3
      RATES(1)  = 1.D-3

      CALL T_GENC06 ( SEGNO,    MNSGNO, BEGREC, NPKTS(1), XREF, &
                      RATES(1), SCLKDP, QUATS,  AVVS,     OK   )
      CALL CHCKXC ( .FALSE., ' ', OK )


      XIVBDS(1) = SCLKDP(1)

      FIRST     = XIVBDS(1)

!
!     Create quaternion derivatives from quaternions and avvs.
!     Pack quaternions and their derivatives into the PACKTS array.
!
      CALL CLEARD ( 3, DAVV )

      DO I = 1, NPKTS(1)
!
!        We're not using the derivative of angular velocity for
!        this subtype.
!
         CALL T_XSUBTP ( QUATS(0,I), AVVS(1,I),  DAVV, &
                         SUBTPS(1),  PKTSIZ,     PACKTS(PKTPTR) )

         PKTPTR = PKTPTR + PKTSIZ

      END DO

!
!     Mini-segment 2:
!
      MNSGNO    = 2
      BEGREC    = BEGREC + NPKTS(1)
      NPKTS(2)  = 12
      SUBTPS(2) = C06TP1
      PKTSIZ    = C06PS1
      PSIZES(2) = PKTSIZ
      DEGRES(2) = 7
      RATES(2)  = 2.D-3

      CALL T_GENC06 ( SEGNO,    MNSGNO, BEGREC, NPKTS(2), XREF, &
                      RATES(2), EPOCHS, QUATS,  AVVS,     OK )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     No AVV scaling is needed because the vector is
!     already considered to have units of radians/s.
!
      CALL MOVED ( EPOCHS, NPKTS(2), SCLKDP(PKTPTR) )

      XIVBDS(2) = SCLKDP( BEGREC )

!
!     This mini-segment has subtype 1; we can just
!     transfer the quaternions into the packet array.
!
      CALL MOVED ( QUATS, 4*NPKTS(2), PACKTS(PKTPTR) )

      PKTPTR = PKTPTR + NPKTS(2)*PKTSIZ


!
!     Mini-segment 3:
!
      MNSGNO    = 3
      BEGREC    = BEGREC + NPKTS(2)
      NPKTS(3)  = 40
      SUBTPS(3) = C06TP2
      PKTSIZ    = C06PS2
      PSIZES(3) = PKTSIZ
      DEGRES(3) = 23
      RATES(3)  = 3.D-3

      CALL T_GENC06 ( SEGNO,    MNSGNO, BEGREC, NPKTS(3), XREF, &
                      RATES(3), EPOCHS, QUATS,  AVVS,     OK )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL MOVED ( EPOCHS, NPKTS(3), SCLKDP(BEGREC) )

      XIVBDS(3) = SCLKDP( BEGREC )

!
!     Create quaternion derivatives from quaternions and avvs.
!     Pack quaternions and their derivatives, as well as the
!     avvs and their derivatives, into the PACKTS array.
!
      DO I = 1, NPKTS(3)
!
!        We need to generate a derivative of the AVV. We'll
!        compute a 2-sided derivative if one's available;
!        otherwise we'll use a 1-sided derivative.
!
         IF ( I .EQ. 1 ) THEN

            DELTA = ( SCLKDP(2) - SCLKDP(1) ) * RATES(3)

            CALL VLCOM (  1.D0/DELTA, AVVS(1,I+1), &
                         -1.D0/DELTA, AVVS(1,I), DAVV )


         ELSE IF ( I .LT. NPKTS(3) ) THEN

            DELTA = ( SCLKDP(I+1) - SCLKDP(I) ) * RATES(3) / 2

            CALL VLCOM (  1.D0/DELTA, AVVS(1,I+1), &
                         -1.D0/DELTA, AVVS(1,I-1), DAVV )

         ELSE

            DELTA = ( SCLKDP(I) - SCLKDP(I-1) ) * RATES(3)

            CALL VLCOM (  1.D0/DELTA, AVVS(1,I  ), &
                         -1.D0/DELTA, AVVS(1,I-1), DAVV )

         END IF

         CALL T_XSUBTP ( QUATS(0,I),  AVVS(1,I), DAVV, &
                         SUBTPS(3),   PKTSIZ,    PACKTS(PKTPTR) )

         PKTPTR = PKTPTR + PKTSIZ

      END DO


!
!     Mini-segment 4:
!
      MNSGNO    = 4
      BEGREC    = BEGREC + NPKTS(3)
      NPKTS(4)  = 50
      SUBTPS(4) = C06TP3
      PKTSIZ    = C06PS3
      PSIZES(4) = PKTSIZ
      DEGRES(4) = 23
      RATES(4)  = 4.D-3

      CALL T_GENC06 ( SEGNO,    MNSGNO, BEGREC, NPKTS(4), XREF, &
                      RATES(4), EPOCHS, QUATS,  AVVS,     OK )
      CALL CHCKXC ( .FALSE., ' ', OK )


      XIVBDS(4) = EPOCHS( 1        )
      XIVBDS(5) = EPOCHS( NPKTS(4) ) + 1.D0
      LAST      = XIVBDS(5)

      CALL MOVED ( EPOCHS, NPKTS(4), SCLKDP(BEGREC) )

!
!     Copy both quaternions and avvs into the packet array.
!
      DO I = 1, NPKTS(4)

         CALL MOVED( QUATS(0,I), 4, PACKTS(PKTPTR) )
         PKTPTR = PKTPTR + 4

         CALL MOVED( AVVS(1,I),  3, PACKTS(PKTPTR) )
         PKTPTR = PKTPTR + 3

      END DO

!
!     Open new C-kernel.
!
      IF ( EXISTS(CK06MU) ) THEN
         CALL DELFIL( CK06MU )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF

      CALL CKOPN ( CK06MU, CK06MU, 0, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Write segment.
!
      CALL CKW06 ( HANDLE, &
                   XINST, &
                   XREF, &
                   XAVFLG, &
                   FIRST, &
                   LAST, &
                   SEGID, &
                   NINTVL, &
                   NPKTS, &
                   SUBTPS, &
                   DEGRES, &
                   PACKTS, &
                   RATES, &
                   SCLKDP, &
                   XIVBDS, &
                   SELLST           )


!
!     Close kernel.
!
      CALL CKCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )



!*****************************************************************
!*
!*     CKNM06, CKMP06, CKGR06 Non-error cases:
!*
!*****************************************************************

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'Examine the segment in the file CK06MU.' )

      CALL DAFOPR ( CK06MU, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFBFS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFFNA ( FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

      CALL DAFGS ( DESCR )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check the mini-segment count:
!
      CALL CKNM06 ( HANDLE, DESCR, NMINI )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NMINI', NMINI, '=', 4, 0, OK )

!
!     Check the mini-segment parameters:
!
      BEGREC = 1
      PKTPTR = 1

      DO I = 1, NMINI

         CALL CKMP06 ( HANDLE, DESCR,  I,    RATE, &
                       SUBTYP, WINSIZ, NREC, IVLBDS, LSTEPC )

         NAME = 'Clock rate *'
         CALL REPMI ( NAME, '*', I, NAME )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSD ( NAME, RATE, '=', RATES(I),  0.D0, OK )

         NAME = 'Subtype code *'
         CALL REPMI ( NAME, '*', I, NAME )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSI ( NAME, SUBTYP, '=', SUBTPS(I), 0, OK )

         NAME = 'Packet count *'
         CALL REPMI ( NAME, '*', I, NAME )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSI ( NAME, NREC, '=', NPKTS(I), 0, OK )

         NAME = 'Window size *'
         CALL REPMI ( NAME, '*', I, NAME )
         CALL CHCKXC ( .FALSE., ' ', OK )

         IF ( ( SUBTYP .EQ. 0 ) .OR. ( SUBTYP .EQ. 2 ) ) THEN

            XWNSIZ = ( DEGRES(I) + 1 ) / 2
         ELSE
            XWNSIZ =   DEGRES(I) + 1
         END IF

         CALL CHCKSI ( NAME, WINSIZ, '=', XWNSIZ, 0, OK )

         NAME = 'Interval start *'
         CALL REPMI ( NAME, '*', I, NAME )
         CALL CHCKXC ( .FALSE., ' ', OK )

         XT   = XIVBDS(I)

         CALL CHCKSD ( NAME, IVLBDS(1), '=', XT,  0.D0, OK )

         NAME = 'Interval stop *'
         CALL REPMI ( NAME, '*', I, NAME )
         CALL CHCKXC ( .FALSE., ' ', OK )

         XT   = XIVBDS(I+1)

         CALL CHCKSD ( NAME, IVLBDS(2), '=', XT,  0.D0, OK )

         NAME = 'Last epoch *'
         CALL REPMI ( NAME, '*', I, NAME )
         CALL CHCKXC ( .FALSE., ' ', OK )

         XT   = SCLKDP(BEGREC + NPKTS(I) - 1)

         CALL CHCKSD ( NAME, LSTEPC, '=', XT,  0.D0, OK )

!
!        Check the packets.
!
         PKTSIZ = PSIZES(I)

         DO J = 1, NPKTS(I)

            CALL CKGR06 ( HANDLE, DESCR, I, J, RECORD )
            CALL CHCKXC ( .FALSE., ' ', OK )

            K = PKTPTR + (J-1)*PKTSIZ


!
!           Check the record's time tag.
!
            NAME = 'Mini-segment *; record time tag *'
            CALL REPMI ( NAME, '*', I, NAME )
            CALL CHCKXC ( .FALSE., ' ', OK )
            CALL REPMI ( NAME, '*', J, NAME )
            CALL CHCKXC ( .FALSE., ' ', OK )

            XT   = SCLKDP(BEGREC + J - 1)

            CALL CHCKSD ( NAME, RECORD(1), '=', XT, 0.D0, OK )
!
!           Check the subtype and rate.
!
            NAME = 'Mini-segment *; record subtype *'
            CALL REPMI ( NAME, '*', I, NAME )
            CALL CHCKXC ( .FALSE., ' ', OK )
            CALL REPMI ( NAME, '*', J, NAME )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKSI ( NAME, NINT(RECORD(2)), '=', SUBTYP, 0, OK )

            NAME = 'Mini-segment *; record rate *'
            CALL REPMI ( NAME, '*', I, NAME )
            CALL CHCKXC ( .FALSE., ' ', OK )
            CALL REPMI ( NAME, '*', J, NAME )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKSD ( NAME, RECORD(3), '=', RATE, 0.D0, OK )


!
!           Check the data portion of the record.
!
            NAME = 'Mini-segment *; packet *'
            CALL REPMI ( NAME, '*', I, NAME )
            CALL CHCKXC ( .FALSE., ' ', OK )
            CALL REPMI ( NAME, '*', J, NAME )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKAD ( NAME, RECORD(4), '=', &
                                PACKTS(K), PKTSIZ, 0.D0, OK )

         END DO

!
!        Update pointers.
!
         BEGREC = BEGREC + NPKTS(I)
         PKTPTR = PKTPTR + NPKTS(I)*PKTSIZ

      END DO


      CALL DAFCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )






!*****************************************************************
!*
!*     CKE06 Non-error cases:
!*
!*****************************************************************



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'Look up and check data from CK06MU. Look ups ' &
      //            'occur at time tags. Check first mini-segment.' )

      CALL FURNSH ( CK06MU )
      CALL CHCKXC ( .FALSE., ' ', OK )

      SEGNO = 1

      MNSGNO    = 1
      BEGREC    = 1
      PKTSIZ    = C06PS0

!
!     Generate test data again.
!
      CALL T_GENC06 ( SEGNO,    MNSGNO, BEGREC, NPKTS(1), XREF, &
                      RATES(1), SCLKDP, QUATS,  AVVS,     OK )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check data from the first mini-segment.
!
      PKTPTR = 1

      DO I = 1, NPKTS(1)

         J = BEGREC - 1 + I

         CALL CKGPAV ( XINST, SCLKDP(J), CKTOL, &
                       XREF,  CMAT,      AV,    CLKOUT, FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

!
!        Generate expected values.
!
         K = PKTPTR + (I-1)*PKTSIZ

         CALL MOVED ( PACKTS(K), 4, XQ )

         CALL Q2M ( XQ, XCMAT )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL VEQU ( AVVS(1,J), XAV )


         NAME = 'Mini-segment *;  * CMAT'
         CALL REPMI ( NAME, '*', 1, NAME )
         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL REPMI ( NAME, '*', I, NAME )
         CALL CHCKXC ( .FALSE., ' ', OK )


         CALL CHCKAD ( NAME, CMAT, '~~/', XCMAT, 9, VTIGHT, OK )

         NAME = 'Mini-segment *;  * AVV'
         CALL REPMI ( NAME, '*', 1, NAME )
         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL REPMI ( NAME, '*', I, NAME )
         CALL CHCKXC ( .FALSE., ' ', OK )
!
!        For this case, we cannot expect high accuracy.
!        We're comparing angular velocity computed from
!        interpolated quaternion derivatives to the original
!        value.
!
        CALL CHCKAD ( NAME, AV, '~~/', XAV, 3, MED, OK )

      END DO

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'Look up and check data from CK06MU. Look ups ' &
      //            'occur at time tags. Check second mini-segment.' )



!
!     Check data from the second mini-segment.
!
      MNSGNO    = 2
      BEGREC    = BEGREC + NPKTS(1)
      PKTSIZ    = C06PS1

!
!     Generate test data again.
!
      CALL T_GENC06 ( SEGNO,    MNSGNO, BEGREC, NPKTS(2), XREF, &
                      RATES(2), EPOCHS, QUATS,  AVVS,     OK )
      CALL CHCKXC ( .FALSE., ' ', OK )

      PKTPTR = NPKTS(1)*PSIZES(1) + 1

      DO I = 1, NPKTS(2)

         CALL CKGPAV ( XINST, EPOCHS(I), CKTOL, &
                       XREF,  CMAT,      AV,    CLKOUT, FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

!
!        Generate expected values.
!
         K = PKTPTR + (I-1)*PKTSIZ

         CALL MOVED ( PACKTS(K), 4, XQ )

         CALL Q2M ( XQ, XCMAT )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL VEQU ( AVVS(1,I), XAV )
!
!        Scale the expected AV to be compatible with
!        AV derived from the quaternions.
!
!         CALL VSCLIP ( 1/RATES(2), XAV )


         NAME = 'Mini-segment *;  * CMAT'
         CALL REPMI ( NAME, '*', 2, NAME )
         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL REPMI ( NAME, '*', I, NAME )
         CALL CHCKXC ( .FALSE., ' ', OK )


         CALL CHCKAD ( NAME, CMAT, '~~/', XCMAT, 9, VTIGHT, OK )

         NAME = 'Mini-segment *;  * AVV'
         CALL REPMI ( NAME, '*', 2, NAME )
         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL REPMI ( NAME, '*', I, NAME )
         CALL CHCKXC ( .FALSE., ' ', OK )
!
!        For this case, we cannot expect high accuracy.
!        We're comparing angular velocity computed from
!        interpolated quaternion derivatives to the original
!        value.
!
         CALL CHCKAD ( NAME, AV, '~~/', XAV, 3, MED, OK )

      END DO


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'Look up and check data from CK06MU. Look ups ' &
      //            'occur at time tags. Check third mini-segment.' )

!
!     Check data from the third mini-segment.
!

      MNSGNO    = 3
      BEGREC    = BEGREC + NPKTS(2)
      PKTSIZ    = C06PS2

!
!     Generate test data again.
!
      CALL T_GENC06 ( SEGNO,    MNSGNO, BEGREC, NPKTS(3), XREF, &
                      RATES(3), EPOCHS, QUATS,  AVVS,     OK )
      CALL CHCKXC ( .FALSE., ' ', OK )


      PKTPTR = PKTPTR + NPKTS(2)*PSIZES(2)

      DO I = 1, NPKTS(3)

         CALL CKGPAV ( XINST, EPOCHS(I), CKTOL, &
                       XREF,  CMAT,      AV,    CLKOUT, FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

!
!        Generate expected values.
!
         K = PKTPTR + (I-1)*PKTSIZ

         CALL MOVED ( PACKTS(K), 4, XQ )

         CALL Q2M ( XQ, XCMAT )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL VEQU ( AVVS(1,I), XAV )
!
!        Scale the expected AV to be compatible with
!        AV derived from the quaternions.
!
!         CALL VSCLIP ( 1/RATES(3), XAV )


         NAME = 'Mini-segment *;  * CMAT'
         CALL REPMI ( NAME, '*', 3, NAME )
         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL REPMI ( NAME, '*', I, NAME )
         CALL CHCKXC ( .FALSE., ' ', OK )


         CALL CHCKAD ( NAME, CMAT, '~~/', XCMAT, 9, VTIGHT, OK )

         NAME = 'Mini-segment *;  * AVV'
         CALL REPMI ( NAME, '*', 3, NAME )
         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL REPMI ( NAME, '*', I, NAME )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKAD ( NAME, AV, '~~/', XAV, 3, VTIGHT, OK )

      END DO


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'Look up and check data from CK06MU. Look ups ' &
      //            'occur at time tags. Check fourth mini-segment.' )

!
!     Check data from the fourth mini-segment.
!

      MNSGNO    = 4
      BEGREC    = BEGREC + NPKTS(3)
      PKTSIZ    = C06PS3

      CALL T_GENC06 ( SEGNO,    MNSGNO, BEGREC, NPKTS(4), XREF, &
                      RATES(4), EPOCHS, QUATS,  AVVS,     OK )
      CALL CHCKXC ( .FALSE., ' ', OK )

      PKTPTR = 1

      DO I = 1, 3
         PKTPTR = PKTPTR + NPKTS(I)*PSIZES(I)
      END DO


      DO I = 1, NPKTS(4)

         CALL CKGPAV ( XINST, EPOCHS(I), CKTOL,  XREF, &
                       CMAT,  AV,        CLKOUT, FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

!
!        Generate expected values.
!
         K = PKTPTR + (I-1)*PKTSIZ

         CALL MOVED ( PACKTS(K), 4, XQ )

         CALL Q2M ( XQ, XCMAT )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL VEQU ( AVVS(1,I), XAV )


         NAME = 'Mini-segment *;  * CMAT'
         CALL REPMI ( NAME, '*', 4, NAME )
         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL REPMI ( NAME, '*', I, NAME )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Check C-matrix.
!
         CALL CHCKAD ( NAME, CMAT, '~~/', XCMAT, 9, VTIGHT, OK )

         NAME = 'Mini-segment *;  * AVV'
         CALL REPMI ( NAME, '*', 4, NAME )
         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL REPMI ( NAME, '*', I, NAME )
         CALL CHCKXC ( .FALSE., ' ', OK )
!
!        Check AV.
!
         CALL CHCKAD ( NAME, AV, '~~/', XAV, 3, VTIGHT, OK )

      END DO


      CALL UNLOAD ( CK06MU )
      CALL CHCKXC ( .FALSE., ' ', OK )


!*****************************************************************
!*
!*     CKR06 Error cases:
!*
!*****************************************************************

!
!     Bad mini-segment subtype test:
!
!
! --- Case: ------------------------------------------------------
!
      TITLE = 'Create a trivial segment with an invalid subtype. Try ' &
      //      'to read from this segment.'

      CALL TCASE ( TITLE )

      SEGNO  = 1
      MNSGNO = 1
      BEGREC = 1
      N      = 201
      XREF   = 'MARSIAU'

      CALL T_GENC06 ( SEGNO, MNSGNO, BEGREC, N,    XREF, &
                      1.D0,  SCLKDP, PACKTS, AVVS, OK )

      CALL CHCKXC ( .FALSE., ' ', OK )

      XINST     = -1000

!
!     Set the AV flag to FALSE to set up next test.
!
      XAVFLG    = .FALSE.

      NINTVL    = 1
      NPKTS(1)  = N
      RATES(1)  = 1.D-3
      SUBTPS(1) = -1
      DEGRES(1) = 3

      XIVBDS(1) = SCLKDP(1)
      XIVBDS(2) = SCLKDP(N)

      FIRST     = XIVBDS(1)
      LAST      = XIVBDS(2)

      SEGID     = 'Subtype 1 test segment'

      SELLST    = .TRUE.


!
!     Open new C-kernel.
!
      IF ( EXISTS(CK06E) ) THEN
         CALL DELFIL(CK06E)
         CALL CHCKXC( .FALSE., ' ', OK )
      END IF

      CALL CKOPN( CK06E, 'Type 06 CK error file', 4, HANDLE )
      CALL CHCKXC( .FALSE., ' ', OK )

!
!     Write segment.
!
      CALL T_CKW06 ( HANDLE, &
                     XINST, &
                     XREF, &
                     XAVFLG, &
                     FIRST, &
                     LAST, &
                     SEGID, &
                     NINTVL, &
                     NPKTS, &
                     SUBTPS, &
                     DEGRES, &
                     PACKTS, &
                     RATES, &
                     SCLKDP, &
                     XIVBDS, &
                     SELLST )

!
!     Close kernel.
!
      CALL CKCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Load kernel for read access.
!
      CALL FURNSH ( CK06E )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Try a lookup.
!
      CKTOL = 0.D0
      CALL CKGP ( XINST, SCLKDP(1), CKTOL, &
                  XREF,  CMAT,      CLKOUT, FOUND )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDSUBTYPE)', OK )


      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )

!
!     Unload kernel.
!
      CALL UNLOAD ( CK06E )
      CALL CHCKXC ( .FALSE., ' ', OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Try to read record with AV.' )

!
!     Use the CK from the previous test case.
!
      CALL DAFOPR ( CK06E, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFBFS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFFNA ( FOUND  )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFGS  ( DESCR  )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CKR06 ( HANDLE, DESCR, XIVBDS(1), 0.D0, .TRUE., &
                   RECORD, FOUND                           )

      CALL CHCKXC ( .TRUE., 'SPICE(NOAVDATA)', OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Bad segment data type' )


      CALL DAFUS ( DESCR, ND, NI, DC, IC )
      IC(3) = -3

      CALL DAFPS ( ND, NI, DC, IC, DESCR )


      CALL CKR06 ( HANDLE, DESCR, XIVBDS(1), 0.D0, .FALSE., &
                   RECORD, FOUND                           )

      CALL CHCKXC ( .TRUE., 'SPICE(WRONGCKTYPE)', OK )


!
!     Close kernel.
!
      CALL DAFCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
! --- Case: ------------------------------------------------------
!
      TITLE = 'Create a trivial segment with subtype 0 and a ' &
      //      'quaternion sign error. Try to read from this segment.'

      CALL TCASE ( TITLE )

      SEGNO  = 1
      MNSGNO = 1
      BEGREC = 1
      N      = 201
      XREF   = 'MARSIAU'
      SUBTYP = C06TP0
      PKTSIZ = C06PS0

      CALL T_GENC06 ( SEGNO, MNSGNO, BEGREC, N,    XREF, &
                      1.D0,  SCLKDP, QUATS,  AVVS, OK )

      CALL CHCKXC ( .FALSE., ' ', OK )

      DO I = 1, N

         PKTPTR = 1 + (I-1)*PKTSIZ

         CALL T_XSUBTP ( QUATS(0,I), AVVS(1,I), DAVV, &
                         SUBTYP,     PKTSIZ,    PACKTS(PKTPTR) )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Create error in quaternion sequence at packet index 6:
!
         IF ( I .EQ. 6 ) THEN
            CALL VMINUG ( PACKTS(PKTPTR), 4, QNEG           )
            CALL MOVED  ( QNEG,           4, PACKTS(PKTPTR) )
         END IF

      END DO


      XINST     = -1000
      XAVFLG    = .TRUE.

      NINTVL    = 1
      NPKTS(1)  = N
      RATES(1)  = 1.D-3
      SUBTPS(1) = SUBTYP
      DEGRES(1) = 15

      XIVBDS(1) = SCLKDP(1)
      XIVBDS(2) = SCLKDP(N)

      FIRST     = XIVBDS(1)
      LAST      = XIVBDS(2)

      SEGID     = 'Subtype 0 test segment'

      SELLST    = .TRUE.


!
!     Open new C-kernel.
!
      IF ( EXISTS(CK06E) ) THEN
         CALL DELFIL(CK06E)
         CALL CHCKXC( .FALSE., ' ', OK )
      END IF

      CALL CKOPN( CK06E, 'Type 06 CK error file', 4, HANDLE )
      CALL CHCKXC( .FALSE., ' ', OK )

!
!     Write segment.
!
      CALL T_CKW06 ( HANDLE, &
                     XINST, &
                     XREF, &
                     XAVFLG, &
                     FIRST, &
                     LAST, &
                     SEGID, &
                     NINTVL, &
                     NPKTS, &
                     SUBTPS, &
                     DEGRES, &
                     PACKTS, &
                     RATES, &
                     SCLKDP, &
                     XIVBDS, &
                     SELLST )

!
!     Close kernel.
!
      CALL CKCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Load kernel for read access.
!
      CALL FURNSH ( CK06E )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Try a lookup.
!
      CKTOL = 0.D0
      CALL CKGP ( XINST, SCLKDP(6), CKTOL, &
                  XREF,  CMAT,      CLKOUT, FOUND )
      CALL CHCKXC ( .TRUE., 'SPICE(BADQUATSIGN)', OK )


      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )

!
!     Unload kernel.
!
      CALL UNLOAD ( CK06E )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
! --- Case: ------------------------------------------------------
!
      TITLE = 'Create a trivial segment with subtype 2 and a ' &
      //      'quaternion sign error. Try to read from this segment.'

      CALL TCASE ( TITLE )

      SEGNO  = 1
      MNSGNO = 1
      BEGREC = 1
      N      = 201
      XREF   = 'MARSIAU'
      SUBTYP = C06TP2
      PKTSIZ = C06PS2

      CALL T_GENC06 ( SEGNO, MNSGNO, BEGREC, N,    XREF, &
                      1.D0,  SCLKDP, QUATS,  AVVS, OK )

      CALL CHCKXC ( .FALSE., ' ', OK )

      DO I = 1, N

         PKTPTR = 1 + (I-1)*PKTSIZ

         CALL T_XSUBTP ( QUATS(0,I), AVVS(1,I), DAVV, &
                         SUBTYP,     PKTSIZ,    PACKTS(PKTPTR) )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Create error in quaternion sequence at packet index 7:
!
         IF ( I .EQ. 7 ) THEN
            CALL VMINUG ( PACKTS(PKTPTR), 4, QNEG           )
            CALL MOVED  ( QNEG,           4, PACKTS(PKTPTR) )
         END IF

      END DO


      XINST     = -1000
      XAVFLG    = .TRUE.

      NINTVL    = 1
      NPKTS(1)  = N
      RATES(1)  = 1.D-3
      SUBTPS(1) = SUBTYP
      DEGRES(1) = 15

      XIVBDS(1) = SCLKDP(1)
      XIVBDS(2) = SCLKDP(N)

      FIRST     = XIVBDS(1)
      LAST      = XIVBDS(2)

      SEGID     = 'Subtype 2 test segment'

      SELLST    = .TRUE.


!
!     Open new C-kernel.
!
      IF ( EXISTS(CK06E) ) THEN
         CALL DELFIL(CK06E)
         CALL CHCKXC( .FALSE., ' ', OK )
      END IF

      CALL CKOPN( CK06E, 'Type 06 CK error file', 4, HANDLE )
      CALL CHCKXC( .FALSE., ' ', OK )

!
!     Write segment.
!
      CALL T_CKW06 ( HANDLE, &
                     XINST, &
                     XREF, &
                     XAVFLG, &
                     FIRST, &
                     LAST, &
                     SEGID, &
                     NINTVL, &
                     NPKTS, &
                     SUBTPS, &
                     DEGRES, &
                     PACKTS, &
                     RATES, &
                     SCLKDP, &
                     XIVBDS, &
                     SELLST )

!
!     Close kernel.
!
      CALL CKCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Load kernel for read access.
!
      CALL FURNSH ( CK06E )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Try a lookup.
!
      CKTOL = 0.D0
      CALL CKGP ( XINST, SCLKDP(6), CKTOL, &
                  XREF,  CMAT,      CLKOUT, FOUND )
      CALL CHCKXC ( .TRUE., 'SPICE(BADQUATSIGN)', OK )


      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )

!
!     Unload kernel.
!
      CALL UNLOAD ( CK06E )
      CALL CHCKXC ( .FALSE., ' ', OK )



!
! --- Case: ------------------------------------------------------
!
      TITLE = 'Create a trivial segment with subtype 2 and a ' &
      //      'window size equivalent to 2 mod 4. Try to read ' &
      //      'from this segment.'

      CALL TCASE ( TITLE )

      SEGNO  = 1
      MNSGNO = 1
      BEGREC = 1
      N      = 201
      XREF   = 'MARSIAU'
      SUBTYP = C06TP2
      PKTSIZ = C06PS2

      CALL T_GENC06 ( SEGNO, MNSGNO, BEGREC, N,    XREF, &
                      1.D0,  SCLKDP, QUATS,  AVVS, OK )

      CALL CHCKXC ( .FALSE., ' ', OK )

      DO I = 1, N

         PKTPTR = 1 + (I-1)*PKTSIZ

         CALL T_XSUBTP ( QUATS(0,I), AVVS(1,I), DAVV, &
                         SUBTYP,     PKTSIZ,    PACKTS(PKTPTR) )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO


      XINST     = -1000
      XAVFLG    = .TRUE.

      NINTVL    = 1
      NPKTS(1)  = N
      RATES(1)  = 1.D-3
      SUBTPS(1) = SUBTYP
      DEGRES(1) = 14

      XIVBDS(1) = SCLKDP(1)
      XIVBDS(2) = SCLKDP(N)

      FIRST     = XIVBDS(1)
      LAST      = XIVBDS(2)

      SEGID     = 'Subtype 2 test segment'

      SELLST    = .TRUE.


!
!     Open new C-kernel.
!
      IF ( EXISTS(CK06E) ) THEN
         CALL DELFIL(CK06E)
         CALL CHCKXC( .FALSE., ' ', OK )
      END IF

      CALL CKOPN( CK06E, 'Type 06 CK error file', 4, HANDLE )
      CALL CHCKXC( .FALSE., ' ', OK )

!
!     Write segment.
!
      CALL T_CKW06 ( HANDLE, &
                     XINST, &
                     XREF, &
                     XAVFLG, &
                     FIRST, &
                     LAST, &
                     SEGID, &
                     NINTVL, &
                     NPKTS, &
                     SUBTPS, &
                     DEGRES, &
                     PACKTS, &
                     RATES, &
                     SCLKDP, &
                     XIVBDS, &
                     SELLST )

!
!     Close kernel.
!
      CALL CKCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Load kernel for read access.
!
      CALL FURNSH ( CK06E )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Try a lookup.
!
      CKTOL = 0.D0
      CALL CKGP ( XINST, SCLKDP(6), CKTOL, &
                  XREF,  CMAT,      CLKOUT, FOUND )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDVALUE)', OK )


      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )

!
!     Unload kernel.
!
      CALL UNLOAD ( CK06E )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
! --- Case: ------------------------------------------------------
!
      TITLE = 'Create a trivial segment with subtype 1 and an ' &
      //      'even window size. Try to read from this segment.'

      CALL TCASE ( TITLE )

      CALL DAFOPW ( CK06E, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      SUBTYP = C06TP1
      PKTSIZ = C06PS1

      CALL T_GENC06 ( SEGNO, MNSGNO, BEGREC, N,    XREF, &
                      1.D0,  SCLKDP, QUATS,  AVVS, OK )

      CALL CHCKXC ( .FALSE., ' ', OK )

      DO I = 1, N

         PKTPTR = 1 + (I-1)*PKTSIZ

         CALL T_XSUBTP ( QUATS(0,I), AVVS(1,I), DAVV, &
                         SUBTYP,     PKTSIZ,    PACKTS(PKTPTR) )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO


      XINST     = -2000
      XAVFLG    = .TRUE.

      NINTVL    = 1
      NPKTS(1)  = N
      RATES(1)  = 1.D-3
      SUBTPS(1) = SUBTYP
      DEGRES(1) = 14

      XIVBDS(1) = SCLKDP(1)
      XIVBDS(2) = SCLKDP(N)

      FIRST     = XIVBDS(1)
      LAST      = XIVBDS(2)

      SEGID     = 'Subtype 1 test segment'

      SELLST    = .TRUE.

!
!     Write segment.
!
      CALL T_CKW06 ( HANDLE, &
                     XINST, &
                     XREF, &
                     XAVFLG, &
                     FIRST, &
                     LAST, &
                     SEGID, &
                     NINTVL, &
                     NPKTS, &
                     SUBTPS, &
                     DEGRES, &
                     PACKTS, &
                     RATES, &
                     SCLKDP, &
                     XIVBDS, &
                     SELLST )

!
!     Close kernel.
!
      CALL CKCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Load kernel for read access.
!
      CALL FURNSH ( CK06E )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Try a lookup.
!
      CKTOL = 0.D0
      CALL CKGP ( -2000, SCLKDP(6), CKTOL, &
                  XREF,  CMAT,      CLKOUT, FOUND )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDVALUE)', OK )


      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )

!
!     Unload kernel.
!
      CALL UNLOAD ( CK06E )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
! --- Case: ------------------------------------------------------
!
      TITLE = 'Create a trivial segment with subtype 1 and ' &
      //      'window size too small. Try to read from this segment.'

      CALL TCASE ( TITLE )

      CALL DAFOPW ( CK06E, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      SUBTYP = C06TP1
      PKTSIZ = C06PS1

      CALL T_GENC06 ( SEGNO, MNSGNO, BEGREC, N,    XREF, &
                      1.D0,  SCLKDP, QUATS,  AVVS, OK )

      CALL CHCKXC ( .FALSE., ' ', OK )

      DO I = 1, N

         PKTPTR = 1 + (I-1)*PKTSIZ

         CALL T_XSUBTP ( QUATS(0,I), AVVS(1,I), DAVV, &
                         SUBTYP,     PKTSIZ,    PACKTS(PKTPTR) )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO


      XINST     = -2000
      XAVFLG    = .TRUE.

      NINTVL    = 1
      NPKTS(1)  = N
      RATES(1)  = 1.D-3
      SUBTPS(1) = SUBTYP
      DEGRES(1) = 0

      XIVBDS(1) = SCLKDP(1)
      XIVBDS(2) = SCLKDP(N)

      FIRST     = XIVBDS(1)
      LAST      = XIVBDS(2)

      SEGID     = 'Subtype 1 test segment'

      SELLST    = .TRUE.

!
!     Write segment.
!
      CALL T_CKW06 ( HANDLE, &
                     XINST, &
                     XREF, &
                     XAVFLG, &
                     FIRST, &
                     LAST, &
                     SEGID, &
                     NINTVL, &
                     NPKTS, &
                     SUBTPS, &
                     DEGRES, &
                     PACKTS, &
                     RATES, &
                     SCLKDP, &
                     XIVBDS, &
                     SELLST )

!
!     Close kernel.
!
      CALL CKCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Load kernel for read access.
!
      CALL FURNSH ( CK06E )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Try a lookup.
!
      CKTOL = 0.D0
      CALL CKGP ( -2000, SCLKDP(6), CKTOL, &
                  XREF,  CMAT,      CLKOUT, FOUND )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDVALUE)', OK )


      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )

!
!     Unload kernel.
!
      CALL UNLOAD ( CK06E )
      CALL CHCKXC ( .FALSE., ' ', OK )



!
! --- Case: ------------------------------------------------------
!
      TITLE = 'Create a trivial segment with subtype 1 and ' &
      //      'window size too large. Try to read from this segment.'

      CALL TCASE ( TITLE )

      CALL DAFOPW ( CK06E, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      SUBTYP = C06TP1
      PKTSIZ = C06PS1

      CALL T_GENC06 ( SEGNO, MNSGNO, BEGREC, N,    XREF, &
                      1.D0,  SCLKDP, QUATS,  AVVS, OK )

      CALL CHCKXC ( .FALSE., ' ', OK )

      DO I = 1, N

         PKTPTR = 1 + (I-1)*PKTSIZ

         CALL T_XSUBTP ( QUATS(0,I), AVVS(1,I), DAVV, &
                         SUBTYP,     PKTSIZ,    PACKTS(PKTPTR) )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO


      XINST     = -2000
      XAVFLG    = .TRUE.

      NINTVL    = 1
      NPKTS(1)  = N
      RATES(1)  = 1.D-3
      SUBTPS(1) = SUBTYP
      DEGRES(1) = 29

      XIVBDS(1) = SCLKDP(1)
      XIVBDS(2) = SCLKDP(N)

      FIRST     = XIVBDS(1)
      LAST      = XIVBDS(2)

      SEGID     = 'Subtype 1 test segment'

      SELLST    = .TRUE.

!
!     Write segment.
!
      CALL T_CKW06 ( HANDLE, &
                     XINST, &
                     XREF, &
                     XAVFLG, &
                     FIRST, &
                     LAST, &
                     SEGID, &
                     NINTVL, &
                     NPKTS, &
                     SUBTPS, &
                     DEGRES, &
                     PACKTS, &
                     RATES, &
                     SCLKDP, &
                     XIVBDS, &
                     SELLST )

!
!     Close kernel.
!
      CALL CKCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Load kernel for read access.
!
      CALL FURNSH ( CK06E )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Try a lookup.
!
      CKTOL = 0.D0
      CALL CKGP ( -2000, SCLKDP(6), CKTOL, &
                  XREF,  CMAT,      CLKOUT, FOUND )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDVALUE)', OK )


      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )

!
!     Unload kernel.
!
      CALL UNLOAD ( CK06E )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
! --- Case: ------------------------------------------------------
!

!
!     This is a "non-error" case: we want to make sure that
!     quaternion inversion is handled for subtype 1. The
!     result of interpolation should be the same as if all
!     the quaternions had compatible signs.
!

      TITLE = 'Create a trivial segment with subtype 1 and a ' &
      //      'quaternion sign error. Try to read from this segment.'

      CALL TCASE ( TITLE )

      SEGNO  = 1
      MNSGNO = 1
      BEGREC = 1
      N      = 201
      XREF   = 'MARSIAU'
      SUBTYP = C06TP1
      PKTSIZ = C06PS1

      CALL T_GENC06 ( SEGNO, MNSGNO, BEGREC, N,    XREF, &
                      1.D0,  SCLKDP, QUATS,  AVVS, OK )

      CALL CHCKXC ( .FALSE., ' ', OK )

      DO I = 1, N

         PKTPTR = 1 + (I-1)*PKTSIZ

         CALL T_XSUBTP ( QUATS(0,I), AVVS(1,I), DAVV, &
                         SUBTYP,     PKTSIZ,    PACKTS(PKTPTR) )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Create error in quaternion sequence at every even-
!        indexed quaternion.
!
         IF ( EVEN(I) ) THEN
            CALL VMINUG ( PACKTS(PKTPTR), 4, QNEG           )
            CALL MOVED  ( QNEG,           4, PACKTS(PKTPTR) )
         END IF

      END DO


      XINST     = -1000
      XAVFLG    = .TRUE.

      NINTVL    = 1
      NPKTS(1)  = N
      RATES(1)  = 1.D-3
      SUBTPS(1) = SUBTYP
      DEGRES(1) = 15

      XIVBDS(1) = SCLKDP(1)
      XIVBDS(2) = SCLKDP(N)

      FIRST     = XIVBDS(1)
      LAST      = XIVBDS(2)

      SEGID     = 'Partially inverted subtype 1 segment'

      SELLST    = .TRUE.


!
!     Open new C-kernel.
!
      IF ( EXISTS(CK06E) ) THEN
         CALL DELFIL(CK06E)
         CALL CHCKXC( .FALSE., ' ', OK )
      END IF

      CALL CKOPN( CK06E, 'Type 06 CK error file', 4, HANDLE )
      CALL CHCKXC( .FALSE., ' ', OK )

!
!     Write segment. We can use the SPICELIB type 6 writer
!     since it's supposed to allow the quaternion mess we've
!     created when the subtype is either 1 or 3.
!
      CALL CKW06 ( HANDLE, &
                   XINST, &
                   XREF, &
                   XAVFLG, &
                   FIRST, &
                   LAST, &
                   SEGID, &
                   NINTVL, &
                   NPKTS, &
                   SUBTPS, &
                   DEGRES, &
                   PACKTS, &
                   RATES, &
                   SCLKDP, &
                   XIVBDS, &
                   SELLST )
      CALL CHCKXC( .FALSE., ' ', OK )

!
!     Create a second segment for instrument -2000. This one
!     has a normal quaternion sequence.
!
      CALL T_GENC06 ( SEGNO, MNSGNO, BEGREC, N,    XREF, &
                      1.D0,  SCLKDP, QUATS,  AVVS, OK )

      CALL CHCKXC ( .FALSE., ' ', OK )

      DO I = 1, N

         PKTPTR = 1 + (I-1)*PKTSIZ

         CALL T_XSUBTP ( QUATS(0,I), AVVS(1,I), DAVV, &
                         SUBTYP,     PKTSIZ,    PACKTS(PKTPTR) )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO


      XINST     = -2000
      XAVFLG    = .TRUE.

      NINTVL    = 1
      NPKTS(1)  = N
      RATES(1)  = 1.D-3
      SUBTPS(1) = SUBTYP
      DEGRES(1) = 15

      XIVBDS(1) = SCLKDP(1)
      XIVBDS(2) = SCLKDP(N)

      FIRST     = XIVBDS(1)
      LAST      = XIVBDS(2)

      SEGID     = 'Normal subtype 1 test segment'

      SELLST    = .TRUE.

      CALL CKW06 ( HANDLE, &
                   XINST, &
                   XREF, &
                   XAVFLG, &
                   FIRST, &
                   LAST, &
                   SEGID, &
                   NINTVL, &
                   NPKTS, &
                   SUBTPS, &
                   DEGRES, &
                   PACKTS, &
                   RATES, &
                   SCLKDP, &
                   XIVBDS, &
                   SELLST )
      CALL CHCKXC( .FALSE., ' ', OK )

!
!     Close kernel.
!
      CALL CKCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Load kernel for read access.
!
      CALL FURNSH ( CK06E )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Try lookups for each instrument.
!
      DO I = 1, N-1

         T = ( SCLKDP(I) + SCLKDP(I+1) ) / 2

         CKTOL = 0.D0
         CALL CKGPAV ( -2000, T,     CKTOL, &
                       XREF,  XCMAT, XAV,   CLKOUT, FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL CHCKSL ( 'FOUND 1', FOUND, .TRUE., OK )


         CALL CKGPAV ( -1000, T,    CKTOL, &
                       XREF,  CMAT, AV,   CLKOUT, FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL CHCKSL ( 'FOUND 2', FOUND, .TRUE., OK )


         CALL CHCKAD ( 'CMAT', CMAT, '~~/', XCMAT, 9, VTIGHT, OK )
         CALL CHCKAD ( 'CMAT', AV,   '~~/', XAV,   3, VTIGHT, OK )

      END DO
!
!     Unload kernel.
!
      CALL UNLOAD ( CK06E )
      CALL CHCKXC ( .FALSE., ' ', OK )



!
! --- Case: ------------------------------------------------------
!

!
!     This is a "non-error" case: same deal as before, this time
!     for subtype 3.
!

      TITLE = 'Create a trivial segment with subtype 3 and a ' &
      //      'quaternion sign error. Try to read from this segment.'

      CALL TCASE ( TITLE )

      SEGNO  = 1
      MNSGNO = 1
      BEGREC = 1
      N      = 201
      XREF   = 'MARSIAU'
      SUBTYP = C06TP3
      PKTSIZ = C06PS3

      CALL T_GENC06 ( SEGNO, MNSGNO, BEGREC, N,    XREF, &
                      1.D0,  SCLKDP, QUATS,  AVVS, OK )

      CALL CHCKXC ( .FALSE., ' ', OK )

      DO I = 1, N

         PKTPTR = 1 + (I-1)*PKTSIZ

         CALL T_XSUBTP ( QUATS(0,I), AVVS(1,I), DAVV, &
                         SUBTYP,     PKTSIZ,    PACKTS(PKTPTR) )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Create error in quaternion sequence at every even-
!        indexed quaternion.
!
         IF ( EVEN(I) ) THEN
            CALL VMINUG ( PACKTS(PKTPTR), 4, QNEG           )
            CALL MOVED  ( QNEG,           4, PACKTS(PKTPTR) )
         END IF

      END DO


      XINST     = -1000
      XAVFLG    = .TRUE.

      NINTVL    = 1
      NPKTS(1)  = N
      RATES(1)  = 1.D-3
      SUBTPS(1) = SUBTYP
      DEGRES(1) = 15

      XIVBDS(1) = SCLKDP(1)
      XIVBDS(2) = SCLKDP(N)

      FIRST     = XIVBDS(1)
      LAST      = XIVBDS(2)

      SEGID     = 'Partially inverted subtype 3 segment'

      SELLST    = .TRUE.


!
!     Open new C-kernel.
!
      IF ( EXISTS(CK06E) ) THEN
         CALL DELFIL(CK06E)
         CALL CHCKXC( .FALSE., ' ', OK )
      END IF

      CALL CKOPN( CK06E, 'Type 06 CK error file', 4, HANDLE )
      CALL CHCKXC( .FALSE., ' ', OK )

!
!     Write segment. We can use the SPICELIB type 6 writer
!     since it's supposed to allow the quaternion mess we've
!     created when the subtype is either 1 or 3.
!
      CALL CKW06 ( HANDLE, &
                   XINST, &
                   XREF, &
                   XAVFLG, &
                   FIRST, &
                   LAST, &
                   SEGID, &
                   NINTVL, &
                   NPKTS, &
                   SUBTPS, &
                   DEGRES, &
                   PACKTS, &
                   RATES, &
                   SCLKDP, &
                   XIVBDS, &
                   SELLST )
      CALL CHCKXC( .FALSE., ' ', OK )

!
!     Create a second segment for instrument -2000. This one
!     has a normal quaternion sequence.
!
      CALL T_GENC06 ( SEGNO, MNSGNO, BEGREC, N,    XREF, &
                      1.D0,  SCLKDP, QUATS,  AVVS, OK )

      CALL CHCKXC ( .FALSE., ' ', OK )

      DO I = 1, N

         PKTPTR = 1 + (I-1)*PKTSIZ

         CALL T_XSUBTP ( QUATS(0,I), AVVS(1,I), DAVV, &
                         SUBTYP,     PKTSIZ,    PACKTS(PKTPTR) )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO


      XINST     = -2000
      XAVFLG    = .TRUE.

      NINTVL    = 1
      NPKTS(1)  = N
      RATES(1)  = 1.D-3
      SUBTPS(1) = SUBTYP
      DEGRES(1) = 15

      XIVBDS(1) = SCLKDP(1)
      XIVBDS(2) = SCLKDP(N)

      FIRST     = XIVBDS(1)
      LAST      = XIVBDS(2)

      SEGID     = 'Normal subtype 3 test segment'

      SELLST    = .TRUE.

      CALL CKW06 ( HANDLE, &
                   XINST, &
                   XREF, &
                   XAVFLG, &
                   FIRST, &
                   LAST, &
                   SEGID, &
                   NINTVL, &
                   NPKTS, &
                   SUBTPS, &
                   DEGRES, &
                   PACKTS, &
                   RATES, &
                   SCLKDP, &
                   XIVBDS, &
                   SELLST )
      CALL CHCKXC( .FALSE., ' ', OK )

!
!     Close kernel.
!
      CALL CKCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Load kernel for read access.
!
      CALL FURNSH ( CK06E )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Try lookups for each instrument.
!
      DO I = 1, N-1

         T = ( SCLKDP(I) + SCLKDP(I+1) ) / 2

         CKTOL = 0.D0
         CALL CKGPAV ( -2000, T,     CKTOL, &
                       XREF,  XCMAT, XAV,   CLKOUT, FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL CHCKSL ( 'FOUND 1', FOUND, .TRUE., OK )


         CALL CKGPAV ( -1000, T,    CKTOL, &
                       XREF,  CMAT, AV,   CLKOUT, FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL CHCKSL ( 'FOUND 2', FOUND, .TRUE., OK )


         CALL CHCKAD ( 'CMAT', CMAT, '~~/', XCMAT, 9, VTIGHT, OK )
         CALL CHCKAD ( 'CMAT', AV,   '~~/', XAV,   3, VTIGHT, OK )

      END DO


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'CKR06 error case: negative tolerance.' )

!
!     We can use the CK from the previous test case.
!

!
!     Unload kernel; open for DAF read access.
!
      CALL UNLOAD ( CK06E )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFOPR ( CK06E, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFBFS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFFNA ( FOUND  )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFGS  ( DESCR  )
      CALL CHCKXC ( .FALSE., ' ', OK )

      T     = SCLKDP(1)

      CKTOL = -1.D0

      CALL CKR06 ( HANDLE, DESCR, T, CKTOL, .TRUE., RECORD, FOUND )
      CALL CHCKXC ( .TRUE., 'SPICE(NEGATIVETOL)', OK )


!
!     Unload kernel.
!
      CALL DAFCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )



!*****************************************************************
!*
!*     CKR06 Non-error cases:
!*
!*****************************************************************

!
!     Use CK from previous tests to examine gap and non-zero
!     tolerance logic in CKR06.
!
!     These tests use CK06MU. To start out, restore the arrays
!     of parameters used to create this file.
!

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'Tolerance is positive; lookup time precedes ' &
      //            'segment start by less than the tolerance. ' &
      //            'Populate data arrays for further tests.'    )

      CALL DAFOPR ( CK06MU, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFBFS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFFNA ( FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

      CALL DAFGS  ( DESCR )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CKNM06 ( HANDLE, DESCR, NINTVL )
      CALL CHCKXC ( .FALSE., ' ', OK )

      IF ( OK ) THEN

         DO I = 1, NINTVL

            CALL CKMP06 ( HANDLE,    DESCR,  I,        RATES(I), &
                          SUBTPS(I), WINSIZ, NPKTS(I), IVLBDS, &
                          LSTEPC                                )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END DO

      END IF

      CALL DAFCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL FURNSH ( CK06MU )
      CALL CHCKXC ( .FALSE., ' ', OK )




!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'Tolerance is positive; lookup time precedes ' &
      //            'segment start by less than the tolerance.'   )


      XINST     = -1000
      XREF      = 'GALACTIC'
      SEGNO     = 1

!
!     Re-generate data for the first mini-segment.
!
      MNSGNO    = 1
      BEGREC    = 1
      PKTSIZ    = NPKTS(1)

      N         = NPKTS(1)

      CALL T_GENC06 ( SEGNO,    MNSGNO, BEGREC, N,    XREF, &
                      RATES(1), EPOCHS, QUATS,  AVVS, OK )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CKTOL = 0.5D0
      T     = EPOCHS( 1 ) - CKTOL + 1.D-6

      T = EPOCHS(1)

      FIRST = EPOCHS(1)
      LAST  = EPOCHS(N) + 1.D0


      CALL CKGPAV ( XINST, T,    CKTOL, &
                    XREF,  CMAT, AV,   CLKOUT, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

!
!     The data should be those for the first epoch.
!
      CALL CHCKSD ( 'CLKOUT', CLKOUT, '=', EPOCHS(1), 0.D0, OK )


      CALL VEQU ( AVVS(1,1),  XAV )

      CALL CHCKAD ( 'AV', AV, '~~/', XAV, 3, MTIGHT, OK )

      CALL MOVED ( QUATS(0,1), 4, XQ )

      CALL Q2M  ( XQ, XCMAT )

      CALL CHCKAD ( 'CMAT', CMAT, '~~/', XCMAT, 9, VTIGHT, OK )


!
!     Convert expected AV to quaternion derivative and back.
!     The purpose of this test is to quantify the error made
!     by these conversions; this error should dominate the
!     error in the AV test above.
!
      CALL CLEARD ( 4,           AVQ    )
      CALL VSCL   ( -0.5D0, AV,  AVQ(1) )
      CALL QXQ    ( XQ,     AVQ, XDQ    )
      CALL QDQ2AV ( XQ,     XDQ, AV     )

      CALL CHCKAD ( 'AV 2', AV, '~~/', XAV, 3, MTIGHT, OK )

!
!     Make sure a second look-up produces the same results.
!

      CALL CKGPAV ( XINST, T,    CKTOL, &
                    XREF,  CMAT, AV,   CLKOUT, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

!
!     The data should be those for the first epoch.
!
      CALL CHCKSD ( 'CLKOUT', CLKOUT, '=', EPOCHS(1), 0.D0, OK )


      CALL VEQU ( AVVS(1,1),  XAV )

      CALL CHCKAD ( 'AV', AV, '~~/', XAV, 3, MTIGHT, OK )

      CALL MOVED ( QUATS(0,1), 4, XQ )

      CALL Q2M  ( XQ, XCMAT )

      CALL CHCKAD ( 'CMAT', CMAT, '~~/', XCMAT, 9, VTIGHT, OK )





!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'Tolerance is positive; lookup time precedes ' &
      //            'segment start by more than the tolerance.'   )

      CALL UNLOAD ( CK06MU )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CKTOL = 0.5D0
      T     = EPOCHS( 1 ) - CKTOL - 1.D-6


      CALL DAFOPR ( CK06MU, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFBFS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFFNA ( FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFGS  ( DESCR )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CKR06 ( HANDLE, DESCR, T, CKTOL, .FALSE., &
                   RECORD, FOUND                           )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'Tolerance is positive; lookup time exceeds ' &
      //            'segment stop by more than the tolerance.'   )


      CKTOL = 0.5D0

      CALL DAFUS ( DESCR, ND, NI, DC, IC )

      T     = DC(2) + CKTOL + 1.D-6



      CALL CKR06 ( HANDLE, DESCR, T, CKTOL, .FALSE., &
                   RECORD, FOUND                      )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )


      CALL DAFCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'Tolerance is positive; lookup time exceeds ' &
      //            'segment stop by less than the tolerance, but ' &
      //            'is too far from the last epoch of the last ' &
      //            'mini-segment for data to be found.'          )

      CALL FURNSH ( CK06MU )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     Re-generate data for the last mini-segment.
!
      MNSGNO    = 4
      BEGREC    = NPKTS(1) + NPKTS(2) + NPKTS(3) + 1
      PKTSIZ    = C06PS3

      N         = NPKTS(4)

      CALL T_GENC06 ( SEGNO,    MNSGNO, BEGREC, N,    XREF, &
                      RATES(4), EPOCHS, QUATS,  AVVS, OK )
      CALL CHCKXC ( .FALSE., ' ', OK )

      FIRST = EPOCHS(1)
      LAST  = EPOCHS(N) + 1.D0

      CKTOL = 0.5D0
      T     = LAST  + CKTOL/2

      CALL CKGPAV ( XINST, T,    CKTOL, &
                    XREF,  CMAT, AV,   CLKOUT, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'Tolerance is positive; lookup time exceeds ' &
      //            'segment stop by less than the tolerance, and ' &
      //            'is close enough to the last epoch of the last ' &
      //            'mini-segment for data to be found.'          )


!
!     Re-generate data for the last mini-segment.
!
      MNSGNO    = 4
      BEGREC    = NPKTS(1) + NPKTS(2) + NPKTS(3) + 1
      PKTSIZ    = C06PS3

      N         = NPKTS(4)


      CALL T_GENC06 ( SEGNO,    MNSGNO, BEGREC, N,    XREF, &
                      RATES(4), EPOCHS, QUATS,  AVVS, OK )
      CALL CHCKXC ( .FALSE., ' ', OK )

      FIRST = EPOCHS(1)
      LAST  = EPOCHS(N) + 1.D0

      CKTOL = 2.0D0
      T     = LAST  + CKTOL/2 - 1.D-6


      CALL CKGPAV ( XINST, T,    CKTOL, &
                    XREF,  CMAT, AV,   CLKOUT, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )


!
!     The data should be those for the last epoch.
!
      CALL CHCKSD ( 'CLKOUT', CLKOUT, '=', EPOCHS(N), 0.D0, OK )


      CALL VEQU ( AVVS(1,N),  XAV )

      CALL CHCKAD ( 'AV', AV, '~~/', XAV, 3, TIGHT, OK )

      CALL MOVED ( QUATS(0,N), 4, XQ )

      CALL Q2M  ( XQ, XCMAT )

      CALL CHCKAD ( 'CMAT', CMAT, '~~/', XCMAT, 9, TIGHT, OK )

!
!     Make sure a second look-up produces the same results.
!
      CALL CKGPAV ( XINST, T,    CKTOL, &
                    XREF,  CMAT, AV,   CLKOUT, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )


!
!     The data should be those for the last epoch.
!
      CALL CHCKSD ( 'CLKOUT', CLKOUT, '=', EPOCHS(N), 0.D0, OK )


      CALL VEQU ( AVVS(1,N),  XAV )

      CALL CHCKAD ( 'AV', AV, '~~/', XAV, 3, TIGHT, OK )

      CALL MOVED ( QUATS(0,N), 4, XQ )

      CALL Q2M  ( XQ, XCMAT )

      CALL CHCKAD ( 'CMAT', CMAT, '~~/', XCMAT, 9, TIGHT, OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'Lookup time is gap midpoint; tolerance is zero' )



      XINST     = -1000
      XAVFLG    = .TRUE.
      XREF      = 'GALACTIC'
      NINTVL    = 4
      SELLST    = .TRUE.
      SEGNO     = 1

!
!     Re-generate data for the third mini-segment.
!
      MNSGNO    = 3
      BEGREC    = NPKTS(1) + NPKTS(2) + 1
      PKTSIZ    = C06PS2

      N         = NPKTS(3)

      CALL T_GENC06 ( SEGNO,    MNSGNO, BEGREC, N,    XREF, &
                      RATES(3), EPOCHS, QUATS,  AVVS, OK )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Let T be the midpoint of the gap at the end of the
!     third mini-segment.
!
      T     = EPOCHS( N ) + 0.5D0
      CKTOL = 0.D0


      CALL CKGPAV ( XINST, T,    CKTOL, &
                    XREF,  CMAT, AV,   CLKOUT, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'Lookup time is gap midpoint; tolerance is ' &
      //            'positive but too small to find pointing.'  )

      T     = EPOCHS( N ) + 0.5D0
      CKTOL = 1.D-3


      CALL CKGPAV ( XINST, T,    CKTOL, &
                    XREF,  CMAT, AV,   CLKOUT, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'Lookup time is in gap and is before gap ' &
      //            'midpoint; tolerance is ' &
      //            '1/2 gap length. Mini-segment is not the last.' )

      CKTOL = 0.5D0

      T     = EPOCHS( N ) + 0.5D0 - 1.D-6

      CALL CKGPAV ( XINST, T,    CKTOL, &
                    XREF,  CMAT, AV,   CLKOUT, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

!
!     The data should be those for the final epoch.
!
      CALL CHCKSD ( 'CLKOUT', CLKOUT, '=', EPOCHS(N), 0.D0, OK )


      CALL VEQU ( AVVS(1,N),  XAV )

      CALL CHCKAD ( 'AV', AV, '~~/', XAV, 3, TIGHT, OK )

      CALL MOVED ( QUATS(0,N), 4, XQ )

      CALL Q2M  ( XQ, XCMAT )

      CALL CHCKAD ( 'CMAT', CMAT, '~~/', XCMAT, 9, TIGHT, OK )

!
!     Make sure a second look-up produces the same results.
!
      CALL CKGPAV ( XINST, T,    CKTOL, &
                    XREF,  CMAT, AV,   CLKOUT, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

!
!     The data should be those for the final epoch.
!
      CALL CHCKSD ( 'CLKOUT', CLKOUT, '=', EPOCHS(N), 0.D0, OK )


      CALL VEQU ( AVVS(1,N),  XAV )

      CALL CHCKAD ( 'AV', AV, '~~/', XAV, 3, TIGHT, OK )

      CALL MOVED ( QUATS(0,N), 4, XQ )

      CALL Q2M  ( XQ, XCMAT )

      CALL CHCKAD ( 'CMAT', CMAT, '~~/', XCMAT, 9, TIGHT, OK )




!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'Lookup time is in gap and is after gap ' &
      //            'midpoint; tolerance is ' &
      //            '1/2 gap length.'                          )

      CKTOL = 0.5D0

      T     = EPOCHS( N ) + 0.5D0 + 1.D-6

      CALL CKGPAV ( XINST, T,    CKTOL, &
                    XREF,  CMAT, AV,   CLKOUT, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

!
!     The data should be those for the first epoch of the
!     fourth mini-segment. Re-generate data for that mini-segment.
!
      MNSGNO    = 4
      BEGREC    = NPKTS(1) + NPKTS(2) + NPKTS(3) + 1
      PKTSIZ    = C06PS3

      N         = NPKTS(4)

      CALL T_GENC06 ( SEGNO,    MNSGNO, BEGREC, N,    XREF, &
                      RATES(4), EPOCHS, QUATS,  AVVS, OK )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSD ( 'CLKOUT', CLKOUT, '=', EPOCHS(1), 0.D0, OK )


      CALL VEQU ( AVVS(1,1),  XAV )

      CALL CHCKAD ( 'AV', AV, '~~/', XAV, 3, TIGHT, OK )

      CALL MOVED ( QUATS(0,1), 4, XQ )

      CALL Q2M  ( XQ, XCMAT )

      CALL CHCKAD ( 'CMAT', CMAT, '~~/', XCMAT, 9, TIGHT, OK )


!
!     Make sure a second look-up produces the same results.
!
      CALL CKGPAV ( XINST, T,    CKTOL, &
                    XREF,  CMAT, AV,   CLKOUT, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

!
!     The data should be those for the first epoch of the
!     fourth mini-segment. Re-generate data for that mini-segment.
!
      MNSGNO    = 4
      BEGREC    = NPKTS(1) + NPKTS(2) + NPKTS(3) + 1
      PKTSIZ    = C06PS3

      N         = NPKTS(4)


      CALL CHCKSD ( 'CLKOUT', CLKOUT, '=', EPOCHS(1), 0.D0, OK )


      CALL VEQU ( AVVS(1,1),  XAV )

      CALL CHCKAD ( 'AV', AV, '~~/', XAV, 3, TIGHT, OK )

      CALL MOVED ( QUATS(0,1), 4, XQ )

      CALL Q2M  ( XQ, XCMAT )

      CALL CHCKAD ( 'CMAT', CMAT, '~~/', XCMAT, 9, TIGHT, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'Lookup time is in gap and is before gap ' &
      //            'midpoint in last mini-segment; tolerance is ' &
      //            '1/2 gap length.'                             )

!
!     The data should be those for the last epoch of the
!     fourth mini-segment. Re-generate data for that mini-segment.
!
      MNSGNO    = 4
      BEGREC    = NPKTS(1) + NPKTS(2) + NPKTS(3) + 1
      PKTSIZ    = C06PS3

      N         = NPKTS(4)

      CALL T_GENC06 ( SEGNO,    MNSGNO, BEGREC, N,    XREF, &
                      RATES(4), EPOCHS, QUATS,  AVVS, OK )
      CALL CHCKXC ( .FALSE., ' ', OK )


      T = EPOCHS(N) + 0.5D0 - 1.D-6

      CALL CKGPAV ( XINST, T,    CKTOL, &
                    XREF,  CMAT, AV,   CLKOUT, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )


      CALL CHCKSD ( 'CLKOUT', CLKOUT, '=', EPOCHS(N), 0.D0, OK )


      CALL VEQU ( AVVS(1,N),  XAV )

      CALL CHCKAD ( 'AV', AV, '~~/', XAV, 3, TIGHT, OK )

      CALL MOVED ( QUATS(0,N), 4, XQ )

      CALL Q2M  ( XQ, XCMAT )

      CALL CHCKAD ( 'CMAT', CMAT, '~~/', XCMAT, 9, TIGHT, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'Lookup time is in gap and is after gap ' &
      //            'midpoint in last mini-segment; tolerance is ' &
      //            '1/2 gap length.'                             )

!
!     The data should be those for the last epoch of the
!     fourth mini-segment. Re-generate data for that mini-segment.
!
      MNSGNO    = 4
      BEGREC    = NPKTS(1) + NPKTS(2) + NPKTS(3) + 1
      PKTSIZ    = C06PS3

      N         = NPKTS(4)

      CALL T_GENC06 ( SEGNO,    MNSGNO, BEGREC, N,    XREF, &
                      RATES(4), EPOCHS, QUATS,  AVVS, OK )
      CALL CHCKXC ( .FALSE., ' ', OK )


      T = EPOCHS(N) + 0.5D0 + 1.D-6

      CALL CKGPAV ( XINST, T,    CKTOL, &
                    XREF,  CMAT, AV,   CLKOUT, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )


      CALL UNLOAD ( CK06MU )
      CALL CHCKXC ( .FALSE., ' ', OK )



!*****************************************************************
!*****************************************************************
!*
!*     CKR06: create CKs for search tests
!*
!*****************************************************************
!*****************************************************************

!$$$

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'Create CK for packet selection logic tests.' )

!
!     Create CKs for testing.
!
!     The first kernel contains three mini-segments, all for the
!     same instrument. The packet counts in these mini-segments
!     are such that the first one contains no packet directories,
!     the second contains fewer than one directory buffer full,
!     and the third contains too many to be buffered.
!
!     The selection flag indicates "select last."
!
      CK        = CK06B0

      XINST     = -1000
      XREF      = 'GALACTIC'
      CKFRAM    = 'IAU_EARTH'
      SEGNO     = 1
      SELLST    = .TRUE.

      NINTVL    = 3

      NPKTS(1)  = 98
      NPKTS(2)  = 398
      NPKTS(3)  = 10381

      SUBTPS(1) = C06TP0
      SUBTPS(2) = C06TP1
      SUBTPS(3) = C06TP3

      PSIZES(1) = C06PS0
      PSIZES(2) = C06PS1
      PSIZES(3) = C06PS3

      DEGRES(1) = 7
      DEGRES(2) = 3
      DEGRES(3) = 23

      RATES(1)  = 1.D1
      RATES(2)  = 2.D0
      RATES(3)  = 3.D0

      PAD       = 3

!
!     Create time tags and packets.
!
      BEGREC = 1
      N      = 1
      PKTPTR = 1

      DO MNSGNO = 1, NINTVL
!
!        Record the index of the first non-pad time tag of
!        the current mini-segment.
!
         DATIDX(MNSGNO) = N + PAD

!
!        Record the corresponding start index in the packet array.
!
         PKTBEG(MNSGNO) = PKTPTR

!
!        Generate time tags and packet data for the current
!        mini-segment.
!
         RATE   = RATES (MNSGNO)
         SUBTYP = SUBTPS(MNSGNO)

         CALL T_GENCSM ( SEGNO, MNSGNO, BEGREC, NPKTS(MNSGNO), XREF, &
                         RATE,  SUBTYP, EPOCHS, QUATS,  AVVS,  OK   )
         CALL CHCKXC ( .FALSE., ' ', OK )
!
!        The start time of the current interval is the epoch at
!        PAD positions later than the start epoch.
!
         XIVBDS( MNSGNO ) = EPOCHS( 1 + PAD )

!
!        Setting up the data arrays for CKW06 is simple: we simply
!        append the data for each mini-segment.
!
         CALL MOVED ( EPOCHS, NPKTS(MNSGNO),   SCLKDP(N) )

         CALL MOVED ( AVVS,   3*NPKTS(MNSGNO), XAVVS (1,N) )
         CALL MOVED ( QUATS,  4*NPKTS(MNSGNO), XQUATS(0,N) )

         DO J = 1, NPKTS(MNSGNO)
            CALL Q2M ( QUATS(0,J), XCMATS(1,N-1+J) )
         END DO

!
!        We're not using angular acceleration.
!
         CALL CLEARD ( 3, DAVV )

         DO J = 1, NPKTS(MNSGNO)

            CALL T_XSUBTP ( QUATS(0,J),     AVVS(1,J),      DAVV, &
                            SUBTPS(MNSGNO), PSIZES(MNSGNO), RECORD )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL MOVED ( RECORD, PSIZES(MNSGNO), PACKTS(PKTPTR) )

            PKTPTR = PKTPTR + PSIZES(MNSGNO)

         END DO

!
!        Adjust the starting record number of the next mini-segment
!        to account for padding.
!
         BEGREC = (BEGREC-1) + NPKTS(MNSGNO) - 2*PAD

!
!        Update the count of the records in the segment.
!
         N      = N + NPKTS(MNSGNO)

      END DO

!
!     The stop time of the last interval is the epoch at
!     PAD positions before the final epoch.
!
      XIVBDS( NINTVL+1 ) = SCLKDP( N - PAD )

!
!     Let the segment bounds coincide with the start and stop
!     times of the first and last mini-segments.
!
      FIRST = XIVBDS(1)
      LAST  = XIVBDS(NINTVL+1)

!
!     Create the CK.
!
      IF ( EXISTS(CK) ) THEN
         CALL DELFIL(CK)
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF

      CALL CKOPN ( CK, CK, 0, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CKW06 ( HANDLE, &
                   XINST, &
                   XREF, &
                   XAVFLG, &
                   FIRST, &
                   LAST, &
                   SEGID, &
                   NINTVL, &
                   NPKTS, &
                   SUBTPS, &
                   DEGRES, &
                   PACKTS, &
                   RATES, &
                   SCLKDP, &
                   XIVBDS, &
                   SELLST           )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CKCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


!*****************************************************************
!*
!*     CKR06 reduction of order tests
!*
!*****************************************************************

!
!     These tests examine the behavior of the CKR06 interpolation
!     window construction algorithm for request times near mini-segment
!     interpolation interval boundaries.
!
!     We'll work with the third mini-segment of CK06B0. We start by
!     looking up the relevant mini-segment attributes:
!
!        - Interpolation interval bounds
!        - Window size
!        - Packet count
!

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'Set up CKR06 reduction of order tests.' )

      CK   = CK06B0

      CALL DAFOPR ( CK, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFBFS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFFNA ( FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFGS  ( DESCR )
      CALL CHCKXC ( .FALSE., ' ', OK )

      MNSGNO = 3
      CALL CKMP06 ( HANDLE, DESCR, MNSGNO, RATE,  SUBTYP, &
                    WINSIZ, NREC,  IVLBDS, LSTEPC        )
      CALL CHCKXC ( .FALSE., ' ', OK )


      PKTSIZ = PKTSZS ( SUBTYP )

!
!     Determine the left hand pad size.
!
      PAD = 0
      I   = 1

      CALL CKGR06 ( HANDLE, DESCR, MNSGNO, I, RECORD )
      CALL CHCKXC ( .FALSE., ' ', OK )

      T   = RECORD(1)

      DO WHILE ( T .LT. IVLBDS(1) )

         PAD = PAD + 1
         I   = I + 1

         CALL CKGR06 ( HANDLE, DESCR, MNSGNO, I, RECORD )
         CALL CHCKXC ( .FALSE., ' ', OK )
         T   = RECORD(1)

      END DO

!
!     See whether CKR06 returns the expected window for
!     request times near the left boundary of the interval.
!
      DO I = ( 1 + WINSIZ/2 ), PAD+2, -1
!
! --- Case: ------------------------------------------------------
!
         TITLE = 'CK06B0: Reduction of order on left side. ' &
         //      'Request time is between tags at indices # ' &
         //      'and #.'

         CALL REPMI  ( TITLE, '#', I-1, TITLE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL REPMI  ( TITLE, '#', I,   TITLE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL TCASE  ( TITLE )

!
!        Construct the expected record for a request time
!        between time tags at indices I-1 and I.
!
!
!        N is the expected packet count.
!
         N = I + (WINSIZ/2) - 1

         DO J = 1, N

            CALL CKGR06 ( HANDLE, DESCR, MNSGNO, J, RECORD )
            CALL CHCKXC ( .FALSE., ' ', OK )

            K = 4 + (J-1)*PKTSIZ + 1
!
!           Insert packet data into the expected record.
!
            CALL MOVED ( RECORD(4), PKTSIZ, XREC(K) )
            CALL CHCKXC ( .FALSE., ' ', OK )
!
!           Insert the time tag into the expected record.
!
            K = 4 + N*PKTSIZ + J

            XREC(K) = RECORD(1)

         END DO

!
!        Get the time tags that bracket the request time we
!        want to use.
!
         K  = 4 + N*PKTSIZ + (I-1)

         T0 = XREC(K  )
         T1 = XREC(K+1)

!
!        Generate the request time.
!
         T  = ( T0 + T1 ) / 2

!
!        Insert the initial parameters into the expected record.
!
         XREC(1) = T
         XREC(2) = SUBTYP
         XREC(3) = N
         XREC(4) = RATE

!
!        Look up the record for request time SCLKDP.
!
         CALL CKR06 ( HANDLE, DESCR, T, &
                      0.D0,  .TRUE., RECORD, FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )


         NAME = 'Record # packet count'
         CALL REPMI  ( NAME, '#', I, NAME )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSI ( NAME, NINT(RECORD(3)), '=', N, 0, OK )

         IF ( OK ) THEN
            NAME = 'Record #'
            CALL REPMI  ( NAME, '#', I, NAME )
            CALL CHCKXC ( .FALSE., ' ', OK )

            SIZE  = 4  +  N*(PKTSIZ+1)

            CALL CHCKAD ( NAME, RECORD, '=', XREC, SIZE, 0.D0, OK )

         END IF

      END DO




!
!@@@
!

!
!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Determine the right hand pad size.' )

      PAD = 0
      I   = NREC

      CALL CKGR06 ( HANDLE, DESCR, MNSGNO, I, RECORD )
      CALL CHCKXC ( .FALSE., ' ', OK )

      T   = RECORD(1)

      DO WHILE ( T .GT. IVLBDS(2) )

         PAD = PAD + 1
         I   = I   - 1

         CALL CKGR06 ( HANDLE, DESCR, MNSGNO, I, RECORD )
         CALL CHCKXC ( .FALSE., ' ', OK )
         T   = RECORD(1)

      END DO

!
!     See whether CKR06 returns the expected window for
!     request times near the right boundary of the interval.
!
      DO I = ( NREC - WINSIZ/2 + 1 ),   NREC-PAD
!
! --- Case: ------------------------------------------------------
!
!
!        Let M represent the loop iteration count. This is the
!        offset of I from the value preceding the loop start
!        value.
!
         M = I - ( NREC - WINSIZ/2 )


         TITLE = 'CK06B0: Reduction of order on left side. ' &
         //      'Request time is between tags at indices # ' &
         //      'and #. Iteration = #.'

         CALL REPMI  ( TITLE, '#', I-1, TITLE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL REPMI  ( TITLE, '#', I,   TITLE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL REPMI  ( TITLE, '#', M,   TITLE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL TCASE  ( TITLE )


!
!        Construct the expected record for a request time
!        between time tags at indices I-1 and I.
!
!
!        N is the expected packet count.
!
         N = WINSIZ + 1 - M


         DO J = 1, N

            K = NREC - N + J

            CALL CKGR06 ( HANDLE, DESCR, MNSGNO, K, RECORD )
            CALL CHCKXC ( .FALSE., ' ', OK )

            K = 4 + (J-1)*PKTSIZ + 1
!
!           Insert packet data into the expected record.
!
            CALL MOVED ( RECORD(4), PKTSIZ, XREC(K) )
            CALL CHCKXC ( .FALSE., ' ', OK )
!
!           Insert the time tag into the expected record.
!
            K = 4 + N*PKTSIZ + J

            XREC(K) = RECORD(1)

         END DO

!
!        Get the time tags that bracket the request time we
!        want to use. Note that for this case, the offset
!        is independent of J.
!
         K  = 4  + N*PKTSIZ  +  WINSIZ/2

         T0 = XREC(K  )
         T1 = XREC(K+1)

!
!        Generate the request time.
!
         T  = ( T0 + T1 ) / 2

!
!        Insert the initial parameters into the expected record.
!
         XREC(1) = T
         XREC(2) = SUBTYP
         XREC(3) = N
         XREC(4) = RATE

!
!        Look up the record for request time SCLKDP.
!
         CALL CKR06 ( HANDLE, DESCR, T, &
                      0.D0,  .TRUE., RECORD, FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )


         NAME = 'Record # packet count'
         CALL REPMI  ( NAME, '#', I, NAME )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSI ( NAME, NINT(RECORD(3)), '=', N, 0, OK )

         IF ( OK ) THEN
            NAME = 'Record #'
            CALL REPMI  ( NAME, '#', I, NAME )
            CALL CHCKXC ( .FALSE., ' ', OK )

            SIZE  = 4  +  N*(PKTSIZ+1)

            CALL CHCKAD ( NAME, RECORD, '=', XREC, SIZE, 0.D0, OK )

         END IF


      END DO


      CALL DAFCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )



!*****************************************************************
!*
!*     CKR06 packet selection logic tests
!*
!*****************************************************************


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'Packet selection logic test: look up data at ' &
      //            'time tags.'                                  )


      CK = CK06B0
      CALL FURNSH ( CK )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CKTOL = 0.D0

      BEGREC = 1
      N      = 1
      PKTPTR = 1

      DO MNSGNO = 1, NINTVL
!
!        Create the expected CMAT and AV values.
!
         RATE   = RATES (MNSGNO)
         SUBTYP = SUBTPS(MNSGNO)

!         CALL MOVED ( XAVVS (1,N), 3*NPKTS(MNSGNO), AVVS )

         CALL CHCKXC ( .FALSE., ' ', OK )

         J      = DATIDX(MNSGNO)

!         N      = NPKTS (MNSGNO) - 2*PAD

         T      = SCLKDP(J)

         DO WHILE ( T .LT. XIVBDS(MNSGNO+1) )


            CALL VEQU  ( XAVVS  (1,J),    XAV   )
            CALL MOVED ( XCMATS(1,J), 9, XCMAT )

!
!           Look up pointing data.
!
            CALL CKGPAV ( XINST, T,    CKTOL, &
                          XREF,  CMAT, AV,    CLKOUT, FOUND )
            CALL CHCKXC ( .FALSE., ' ', OK )

            IF ( .NOT. OK ) THEN
               CALL SIGERR( 'NO POINTING' )
               CALL CHCKXC ( .FALSE., ' ', OK )
               STOP
            END IF

            CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

!
!           Check output time.
!
            NAME = 'Mini-segment *; CLKOUT no. *'
            CALL REPMI ( NAME, '*', MNSGNO, NAME )
            CALL CHCKXC ( .FALSE., ' ', OK )
            CALL REPMI ( NAME, '*', J, NAME )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKSD ( NAME, CLKOUT, '~', T, 0.D0, OK )

!
!           Check C-matrix.
!

            NAME = 'Mini-segment *; CMAT no. *'
            CALL REPMI ( NAME, '*', MNSGNO, NAME )
            CALL CHCKXC ( .FALSE., ' ', OK )
            CALL REPMI ( NAME, '*', J, NAME )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKAD ( NAME, CMAT, '~~/', XCMAT, 9, VTIGHT, OK )

!
!           Check AV.
!
            NAME = 'Mini-segment *; AV no. *'
            CALL REPMI ( NAME, '*', MNSGNO, NAME )
            CALL CHCKXC ( .FALSE., ' ', OK )
            CALL REPMI ( NAME, '*', J, NAME )
            CALL CHCKXC ( .FALSE., ' ', OK )


!
!           Check angular velocity.
!
!           For derived AV, use a looser tolerance.
!
            IF ( SUBTPS(MNSGNO) .EQ. C06TP1 ) THEN

               TOL = MTIGHT

!               TOL = MED

            ELSE IF ( SUBTPS(MNSGNO) .EQ. C06TP0 ) THEN

               TOL = TIGHT

            ELSE IF ( SUBTPS(MNSGNO) .EQ. C06TP3 ) THEN

               TOL = VTIGHT
            ELSE
               TOL = TIGHT
            END IF

            CALL CHCKAD ( NAME, AV, '~~/', XAV, 3, TOL, OK )

            IF ( .NOT. OK ) STOP


            J      = J      + 1
            PKTPTR = PKTPTR + PSIZES(MNSGNO)

            T      = SCLKDP(J)

         END DO
!
!        Adjust the starting record number of the next mini-segment
!        to account for padding.
!
         BEGREC = (BEGREC-1) + NPKTS(MNSGNO) - 2*PAD

         N      = N + NPKTS (MNSGNO)

      END DO





!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'Packet selection logic test: look up data at ' &
      //            'midpoints between time tags.'               )


      DO MNSGNO = 1, NINTVL
!
!        Create the expected CMAT and AV values.
!
         RATE   = RATES (MNSGNO)
         SUBTYP = SUBTPS(MNSGNO)


         J      = DATIDX(MNSGNO)
         PKTPTR = PKTBEG(MNSGNO) + PAD*PSIZES(MNSGNO)
         N      = NPKTS (MNSGNO) - 2*PAD

         IF ( J .LT. NPKTS(MNSGNO) ) THEN

            T = ( SCLKDP(J) + SCLKDP(J+1) ) / 2
         ELSE
            T = LAST + 1.D0
         END IF

         DO WHILE ( T .LT. XIVBDS(MNSGNO+1) )
!
!           Create expected CMAT and AV. This will only be
!           an approximation.
!
            ET = T * RATE

            CALL SXFORM ( XREF, CKFRAM, ET, XFORM )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL XF2RAV ( XFORM, XCMAT, XAV )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL M2Q ( XCMAT, XQ )

            IF ( XQ(0) .LT. 0.D0 ) THEN
               DO I = 0, 3
                  XQ(I) = - XQ(I)
               END DO
            END IF

!
!           Look up pointing data.
!
            CALL CKGPAV ( XINST, T, CKTOL, &
                          XREF,  CMAT,      AV, CLKOUT, FOUND )
            CALL CHCKXC ( .FALSE., ' ', OK )

            IF ( .NOT. OK ) THEN
               CALL SIGERR( 'NO POINTING' )
               CALL CHCKXC ( .FALSE., ' ', OK )
               STOP
            END IF

            CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

!
!           Check output time.
!
            NAME = 'Mini-segment *; CLKOUT no. *'
            CALL REPMI ( NAME, '*', MNSGNO, NAME )
            CALL CHCKXC ( .FALSE., ' ', OK )
            CALL REPMI ( NAME, '*', J, NAME )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKSD ( NAME, CLKOUT, '~', T, 0.D0, OK )

!
!           Check C-matrix.
!
            CALL Q2M ( XQ, XCMAT )
            CALL CHCKXC ( .FALSE., ' ', OK )

            NAME = 'Mini-segment *; CMAT no. *'
            CALL REPMI ( NAME, '*', MNSGNO, NAME )
            CALL CHCKXC ( .FALSE., ' ', OK )
            CALL REPMI ( NAME, '*', J, NAME )
            CALL CHCKXC ( .FALSE., ' ', OK )

!
!           Use a tight tolerance for CMAT midpoint comparisons.
!
            CALL CHCKAD ( NAME, CMAT, '~~/', XCMAT, 9, VTIGHT, OK )
!
!           Check angular velocity.
!
            NAME = 'Mini-segment *; AV no. *'
            CALL REPMI ( NAME, '*', MNSGNO, NAME )
            CALL CHCKXC ( .FALSE., ' ', OK )
            CALL REPMI ( NAME, '*', J, NAME )
            CALL CHCKXC ( .FALSE., ' ', OK )

!
!           Set the tolerance for the AV check.
!
            IF ( SUBTPS(MNSGNO) .EQ. C06TP1 ) THEN
!
!              For derived AV, use a looser tolerance.
!
               TOL = MED

            ELSE IF ( SUBTPS(MNSGNO) .EQ. C06TP0 ) THEN

               TOL = TIGHT

            ELSE IF ( SUBTPS(MNSGNO) .EQ. C06TP3 ) THEN

               TOL = VTIGHT
            ELSE
               TOL = TIGHT
            END IF

            CALL CHCKAD ( NAME, AV, '~~/', XAV, 3, TOL, OK )

            J      = J      + 1
            PKTPTR = PKTPTR + PSIZES(MNSGNO)

            IF ( J .LT. NPKTS(MNSGNO) ) THEN

               T = ( SCLKDP(J) + SCLKDP(J+1) ) / 2
            ELSE
               T = LAST + 1.D0
            END IF

         END DO


      END DO

      CALL UNLOAD ( CK )
      CALL CHCKXC ( .FALSE., ' ', OK )






!*****************************************************************
!*
!*     CKR06 interval selection logic tests
!*
!*****************************************************************

!
!     $$$
!

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'Create CKs for interval selection logic tests.' )

!
!     Create CKs for testing.
!
!     The first kernel contains three segments for the three different
!     instruments. The interval counts in these segments
!     are such that the first one contains no interval directories,
!     the second contains fewer than one directory buffer full,
!     and the third contains too many to be buffered.
!
!     The selection flag for the first CK is set to indicate
!     "select first."
!
      CK = CK06B1

!
!     Create the CK.
!
      IF ( EXISTS(CK) ) THEN
         CALL DELFIL(CK)
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF

      CALL CKOPN ( CK, CK, 0, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


      NSEG = 3

      DO I = 1, NSEG
         XINSTS(I) = -1000 * I
      END DO

      XREFS(1)  = 'GALACTIC'
      XREFS(2)  = 'MARSIAU'
      XREFS(3)  = 'GALACTIC'
      CKFRAM    = 'IAU_EARTH'

      SELLST    = .FALSE.

!
!     Set the interval counts for the three segments.
!
      NIVALS(1)  = 98
      NIVALS(2)  = 398
      NIVALS(3)  = 10381

!
!     Create data for the segments and write them to the file.
!
      N      = 0
      SEGPTR = 0

      DO SEGNO = 1, NSEG

         SEGPTR = SEGPTR + N
!
!        Set the mini-segment attributes for the current segment.
!
         XINST  = XINSTS(SEGNO)
         NINTVL = NIVALS(SEGNO)
         XREF   = XREFS (SEGNO)

         DO I = 1, NINTVL

            IF ( MOD(I,3) .EQ. 0 ) THEN

               SUBTPS(I) = C06TP3
               PSIZES(I) = C06PS3
               DEGRES(I) = 3
               RATES (I) = 1.1D0
               NPKTS (I) = 5

            ELSE IF ( MOD(I,3) .EQ. 1 ) THEN

               SUBTPS(I) = C06TP1
               PSIZES(I) = C06PS1
               DEGRES(I) = 7
               RATES (I) = 0.8D0
               NPKTS (I) = 9

            ELSE

               SUBTPS(I) = C06TP0
               PSIZES(I) = C06PS0
               DEGRES(I) = 7
               RATES (I) = 4.D0
               NPKTS (I) = 11

            END IF

         END DO

         PAD = 1

!
!        Create time tags and packets for the current segment.
!
         BEGREC = 1
         N      = 1
         PKTPTR = 1

         DO MNSGNO = 1, NINTVL
!
!           Record the index of the first non-pad time tag of
!           the current mini-segment.
!
            DATIDX(MNSGNO) = N + PAD

!
!           Record the corresponding start index in the packet array.
!
            PKTBEG(MNSGNO) = PKTPTR

!
!           Generate time tags and packet data for the current
!           mini-segment.
!
            RATE   = RATES (MNSGNO)
            SUBTYP = SUBTPS(MNSGNO)

            CALL T_GENCSM ( SEGNO, MNSGNO, BEGREC, NPKTS(MNSGNO), XREF, &
                            RATE,  SUBTYP, EPOCHS, QUATS,  AVVS,  OK   )
            CALL CHCKXC ( .FALSE., ' ', OK )

!
!           The start time of the current interval is the epoch at
!           PAD positions later than the start epoch.
!
            XIVBDS( MNSGNO ) = EPOCHS( 1 + PAD )

!
!           Setting up the data arrays for CKW06 is simple: we simply
!           append the data for each mini-segment.
!
            CALL MOVED ( EPOCHS,   NPKTS(MNSGNO), SCLKDP(N) )

            CALL MOVED ( EPOCHS,   NPKTS(MNSGNO), XEPOCH(SEGPTR+N)   )
            CALL MOVED ( AVVS,   3*NPKTS(MNSGNO), XAVVS (1,SEGPTR+N) )
            CALL MOVED ( QUATS,  4*NPKTS(MNSGNO), XQUATS(0,SEGPTR+N) )

            DO J = 1, NPKTS(MNSGNO)
               CALL Q2M ( QUATS(0,J), XCMATS(1,SEGPTR+N+J-1) )
            END DO


!
!           We're not using angular acceleration.
!
            CALL CLEARD ( 3, DAVV )

            DO J = 1, NPKTS(MNSGNO)

               CALL T_XSUBTP ( QUATS(0,J),     AVVS(1,J),      DAVV, &
                               SUBTPS(MNSGNO), PSIZES(MNSGNO), RECORD )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL MOVED ( RECORD, PSIZES(MNSGNO), PACKTS(PKTPTR) )

               PKTPTR = PKTPTR + PSIZES(MNSGNO)

            END DO

!
!           Adjust the starting record number of the next mini-segment
!           to account for padding.
!
            BEGREC = (BEGREC-1) + NPKTS(MNSGNO) - 2*PAD

!
!           Update the count of the records in the segment.
!
            N      = N + NPKTS(MNSGNO)

         END DO

!
!        The stop time of the last interval is the epoch at
!        PAD positions before the final epoch.
!
         XIVBDS( NINTVL+1 ) = SCLKDP( N - PAD )

!
!        Let the segment bounds coincide with the start and stop
!        times of the first and last mini-segments.
!
         FIRST = XIVBDS(1)
         LAST  = XIVBDS(NINTVL+1)

         CALL CKW06 ( HANDLE, &
                      XINST, &
                      XREF, &
                      XAVFLG, &
                      FIRST, &
                      LAST, &
                      SEGID, &
                      NINTVL, &
                      NPKTS, &
                      SUBTPS, &
                      DEGRES, &
                      PACKTS, &
                      RATES, &
                      SCLKDP, &
                      XIVBDS, &
                      SELLST           )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

!
!     Close the kernel.
!
      CALL CKCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )



!     The selection flag for the second CK is set to indicate
!     "select last." Other than that, the CK is identical to
!     the first.
!
      CK = CK06B2

!
!     Create the CK.
!
      IF ( EXISTS(CK) ) THEN
         CALL DELFIL(CK)
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF

      CALL CKOPN ( CK, CK, 0, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!      CALL TOSTDO ( 'Opened CK '//CK )

      NSEG      = 3

      DO I = 1, NSEG
         XINSTS(I) = -1000 * I
      END DO

      XREFS(1)  = 'GALACTIC'
      XREFS(2)  = 'MARSIAU'
      XREFS(3)  = 'GALACTIC'
      CKFRAM    = 'IAU_EARTH'

      SELLST    = .TRUE.

!
!     Set the interval counts for the three segments.
!
      NIVALS(1)  = 98
      NIVALS(2)  = 398
      NIVALS(3)  = 10381

!
!     Create data for the segments and write them to the file.
!
      N      = 0
      SEGPTR = 0


      DO SEGNO = 1, NSEG

         SEGPTR = SEGPTR + N

!
!        Set the mini-segment attributes for the current segment.
!
         XINST  = XINSTS(SEGNO)
         NINTVL = NIVALS(SEGNO)
         XREF   = XREFS (SEGNO)

         DO I = 1, NINTVL

            IF ( MOD(I,3) .EQ. 0 ) THEN

               SUBTPS(I) = C06TP3
               PSIZES(I) = C06PS3
               DEGRES(I) = 3
               RATES (I) = 1.1D0
               NPKTS (I) = 5

            ELSE IF ( MOD(I,3) .EQ. 1 ) THEN

               SUBTPS(I) = C06TP1
               PSIZES(I) = C06PS1
               DEGRES(I) = 7
               RATES (I) = 0.8D0
               NPKTS (I) = 9

            ELSE

               SUBTPS(I) = C06TP0
               PSIZES(I) = C06PS0
               DEGRES(I) = 7
               RATES (I) = 4.D0
               NPKTS (I) = 11

            END IF

         END DO

         PAD = 1

!
!        Create time tags and packets for the current segment.
!
         BEGREC = 1
         N      = 1
         PKTPTR = 1

         DO MNSGNO = 1, NINTVL
!
!           Record the index of the first non-pad time tag of
!           the current mini-segment.
!
            DATIDX(MNSGNO) = N + PAD

!
!           Record the corresponding start index in the packet array.
!
            PKTBEG(MNSGNO) = PKTPTR

!
!           Generate time tags and packet data for the current
!           mini-segment.
!
            RATE   = RATES (MNSGNO)
            SUBTYP = SUBTPS(MNSGNO)

            CALL MOVED( XQUATS(0,SEGPTR+N), 4*NPKTS(MNSGNO), QUATS  )
            CALL MOVED( XAVVS (1,SEGPTR+N), 3*NPKTS(MNSGNO), AVVS   )
            CALL MOVED( XEPOCH(  SEGPTR+N),   NPKTS(MNSGNO), EPOCHS )

!
!           The start time of the current interval is the epoch at
!           PAD positions later than the start epoch.
!
            XIVBDS( MNSGNO ) = EPOCHS( 1 + PAD )

!
!           Setting up the data arrays for CKW06 is simple: we simply
!           append the data for each mini-segment.
!
            CALL MOVED ( EPOCHS, NPKTS(MNSGNO), SCLKDP(N) )

!
!           We're not using angular acceleration.
!
            CALL CLEARD ( 3, DAVV )

            DO J = 1, NPKTS(MNSGNO)

               CALL T_XSUBTP ( QUATS(0,J),     AVVS(1,J),      DAVV, &
                               SUBTPS(MNSGNO), PSIZES(MNSGNO), RECORD )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL MOVED ( RECORD, PSIZES(MNSGNO), PACKTS(PKTPTR) )

               PKTPTR = PKTPTR + PSIZES(MNSGNO)

            END DO

!
!           Adjust the starting record number of the next mini-segment
!           to account for padding.
!
            BEGREC = (BEGREC-1) + NPKTS(MNSGNO) - 2*PAD

!
!           Update the count of the records in the segment.
!
            N      = N + NPKTS(MNSGNO)

         END DO

!
!        The stop time of the last interval is the epoch at
!        PAD positions before the final epoch.
!
         XIVBDS( NINTVL+1 ) = SCLKDP( N - PAD )

!
!        Let the segment bounds coincide with the start and stop
!        times of the first and last mini-segments.
!
         FIRST = XIVBDS(1)
         LAST  = XIVBDS(NINTVL+1)

         CALL CKW06 ( HANDLE, &
                      XINST, &
                      XREF, &
                      XAVFLG, &
                      FIRST, &
                      LAST, &
                      SEGID, &
                      NINTVL, &
                      NPKTS, &
                      SUBTPS, &
                      DEGRES, &
                      PACKTS, &
                      RATES, &
                      SCLKDP, &
                      XIVBDS, &
                      SELLST           )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

!
!     Close the kernel.
!
      CALL CKCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )



!
! --- Case: ------------------------------------------------------
!
!  ###

!
!     Perform tests on CK06B1.
!
      CK     = CK06B1
      SELLST = .FALSE.

      TITLE = 'Interval/Packet selection logic test:  ' &
      //      'look up data at time tags within all ' &
      //      'three segments of CK #.'

      CALL REPMC  ( TITLE, '#', CK, TITLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL TCASE  ( TITLE )

!
!     Load the test CK.
!
      CALL FURNSH ( CK )
      CALL CHCKXC ( .FALSE., ' ', OK )

      N      = 0
      SEGPTR = 0

      DO SEGNO = 1, NSEG

         SEGPTR = SEGPTR + N

!
! --- Case: ------------------------------------------------------
!

         TITLE = 'Interval/Packet selection logic test:  ' &
      //         'look up data at time tags within ' &
      //         'segment # of CK #.'

         CALL REPMI  ( TITLE, '#', SEGNO, TITLE )
         CALL REPMC  ( TITLE, '#', CK,    TITLE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL TCASE  ( TITLE )

         XINST  = XINSTS(SEGNO)
         NINTVL = NIVALS(SEGNO)
         XREF   = XREFS (SEGNO)

         DO I = 1, NINTVL

            IF ( MOD(I,3) .EQ. 0 ) THEN

               SUBTPS(I) = C06TP3
               PSIZES(I) = C06PS3
               DEGRES(I) = 3
               RATES (I) = 1.1D0
               NPKTS (I) = 5

            ELSE IF ( MOD(I,3) .EQ. 1 ) THEN

               SUBTPS(I) = C06TP1
               PSIZES(I) = C06PS1
               DEGRES(I) = 7
               RATES (I) = 0.8D0
               NPKTS (I) = 9

            ELSE

               SUBTPS(I) = C06TP0
               PSIZES(I) = C06PS0
               DEGRES(I) = 7
               RATES (I) = 4.D0
               NPKTS (I) = 11

            END IF

         END DO


!
!        Create time tags and packets for the current segment.
!
         BEGREC = 1
         N      = 1
         PKTPTR = 1

         DO MNSGNO = 1, NINTVL
!
!           Record the index of the first non-pad time tag of
!           the current mini-segment.
!
            DATIDX(MNSGNO) = N + PAD
!
!           Generate time tags and packet data for the current
!           mini-segment.
!
            CALL MOVED ( XEPOCH(SEGPTR+N), NPKTS(MNSGNO), SCLKDP(N) )

!           Update the count of the records in the segment.
!
            N = N + NPKTS(MNSGNO)

         END DO

!
!        Check data for each interval.
!
         IF ( SEGNO .EQ. 3 ) THEN
!
!           SKIP can be reset to 40 to increase execution speed.
!
            SKIP = 1
         ELSE
            SKIP = 1
         END IF

         DO MNSGNO = 1, NINTVL, SKIP

            J = DATIDX(MNSGNO)

            IF ( ( .NOT. SELLST ) .AND. ( MNSGNO .GT. 1 ) ) THEN
!
!              Skip the first time tag of each mini-segment but the
!              first, since the correct data will come from the
!              previous interval.
!
               J = J + 1

            END IF

            JMAX = DATIDX(MNSGNO) - 1 + NPKTS(MNSGNO) - 2*PAD


            DO WHILE ( J .LE. JMAX )

               T = SCLKDP(J)

               CALL MOVED ( XAVVS (1,SEGPTR+J), 3, XAV )

               CALL MOVED ( XCMATS(1,SEGPTR+J), 9, XCMAT )

!
!              Look up pointing data.
!
               CKTOL = 0.D0

               CALL CKGPAV ( XINST, T,    CKTOL, &
                             XREF,  CMAT, AV, CLKOUT, FOUND )
               CALL CHCKXC ( .FALSE., ' ', OK )

               IF ( .NOT. OK ) THEN
                  CALL SIGERR( 'NO POINTING' )
                  CALL CHCKXC ( .FALSE., ' ', OK )
                  STOP
               END IF

               CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

!
!              Check output time.
!
               CALL CHCKSD ( 'CLKOUT', CLKOUT, '~', T, 0.D0, OK )

               IF ( .NOT. OK ) THEN
!
!                 Re-check with a more specific item name.
!
                  NAME = 'Mini-segment *; CLKOUT no. *'
                  CALL REPMI ( NAME, '*', MNSGNO, NAME )
                  CALL CHCKXC ( .FALSE., ' ', OK )
                  CALL REPMI ( NAME, '*', J, NAME )
                  CALL CHCKXC ( .FALSE., ' ', OK )

                  CALL CHCKSD ( NAME, CLKOUT, '~', T, 0.D0, OK )

               END IF

!
!              Check C-matrix.
!
               CALL CHCKAD ( 'CMAT', CMAT, '~~/', XCMAT, 9, VTIGHT, OK )

               IF ( .NOT. OK ) THEN

                  NAME = 'Mini-segment *; CMAT no. *'
                  CALL REPMI ( NAME, '*', MNSGNO, NAME )
                  CALL CHCKXC ( .FALSE., ' ', OK )
                  CALL REPMI ( NAME, '*', J, NAME )
                  CALL CHCKXC ( .FALSE., ' ', OK )

                  CALL CHCKAD ( NAME, CMAT,  '~~/', &
                                      XCMAT, 9,     VTIGHT, OK )
               END IF

!
!              Check angular velocity.
!
!              For derived AV, use a looser tolerance.
!
               IF ( SUBTPS(MNSGNO) .EQ. C06TP1 ) THEN

                  TOL = MED

               ELSE IF ( SUBTPS(MNSGNO) .EQ. C06TP0 ) THEN

                  TOL = TIGHT

               ELSE IF ( SUBTPS(MNSGNO) .EQ. C06TP3 ) THEN

                  TOL = VTIGHT
               ELSE
                  TOL = TIGHT
               END IF


               CALL CHCKAD ( 'AV', AV, '~~/', XAV, 3, TOL, OK )

               IF ( .NOT. OK ) THEN

                  NAME = 'Mini-segment *; AV no. *'
                  CALL REPMI ( NAME, '*', MNSGNO, NAME )
                  CALL CHCKXC ( .FALSE., ' ', OK )
                  CALL REPMI ( NAME, '*', J, NAME )
                  CALL CHCKXC ( .FALSE., ' ', OK )

                  CALL CHCKAD ( 'AV', AV, '~~/', XAV, 3, TOL, OK )

               END IF


               J = J + 1

            END DO
!
!           End of tests for current interval.
!
         END DO
!
!        End of tests for current segment.
!
      END DO





!
! --- Case: ------------------------------------------------------
!
!
!     Perform tests using data lookups at midpoints
!     between consecutive time tags. This set of tests
!     is for segments using the "select first" option.
!
      DO SEGNO = 1, NSEG
!
! --- Case: ------------------------------------------------------
!

         TITLE = 'Interval/Packet selection logic test:  ' &
      //         'look up data at midpoints between time tags within ' &
      //         'segment # of CK #.'

         CALL REPMI  ( TITLE, '#', SEGNO, TITLE )
         CALL REPMC  ( TITLE, '#', CK,    TITLE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL TCASE  ( TITLE )


         SELLST = .FALSE.

         XINST  = XINSTS(SEGNO)
         NINTVL = NIVALS(SEGNO)
         XREF   = XREFS (SEGNO)

         DO I = 1, NINTVL

            IF ( MOD(I,3) .EQ. 0 ) THEN

               SUBTPS(I) = C06TP3
               PSIZES(I) = C06PS3
               DEGRES(I) = 3
               RATES (I) = 1.1D0
               NPKTS (I) = 5

            ELSE IF ( MOD(I,3) .EQ. 1 ) THEN

               SUBTPS(I) = C06TP1
               PSIZES(I) = C06PS1
               DEGRES(I) = 7
               RATES (I) = 0.8D0
               NPKTS (I) = 9

            ELSE

               SUBTPS(I) = C06TP0
               PSIZES(I) = C06PS0
               DEGRES(I) = 7
               RATES (I) = 4.D0
               NPKTS (I) = 11

            END IF

         END DO


!
!        Create time tags for the current segment.
!
         BEGREC = 1
         N      = 1
         PKTPTR = 1

         DO MNSGNO = 1, NINTVL
!
!           Record the index of the first non-pad time tag of
!           the current mini-segment.
!
            DATIDX(MNSGNO) = N + PAD

            RATE   = RATES (MNSGNO)
            SUBTYP = SUBTPS(MNSGNO)

            CALL T_GENTAG ( BEGREC, NPKTS(MNSGNO), EPOCHS )
            CALL CHCKXC ( .FALSE., ' ', OK )

!
!           The start time of the current interval is the epoch at
!           PAD positions later than the start epoch.
!
            XIVBDS( MNSGNO ) = EPOCHS( 1 + PAD )

            CALL MOVED ( EPOCHS, NPKTS(MNSGNO), SCLKDP(N) )

!
!           Adjust the starting record number of the next mini-segment
!           to account for padding.
!
            BEGREC = (BEGREC-1) + NPKTS(MNSGNO) - 2*PAD

!
!           Update the count of the records in the segment.
!
            N      = N + NPKTS(MNSGNO)

         END DO

!
!        Check data for each interval.
!
         IF ( SEGNO .EQ. 3 ) THEN
            SKIP = 1
         ELSE
            SKIP = 1
         END IF

         DO MNSGNO = 1, NINTVL, SKIP

            RATE = RATES(MNSGNO)
            J    = DATIDX(MNSGNO)

            JMAX = DATIDX(MNSGNO) - 1 + NPKTS(MNSGNO) - 2*PAD

            DO WHILE ( J .LT. JMAX )

               T  = ( SCLKDP(J) + SCLKDP(J+1) ) / 2

!              The following formula for ET looks strange, but it's
!              correct.
!
               ET = T * RATE
!
!              Generate expected results for ET.
!
               CALL SXFORM ( XREF, CKFRAM, ET, XFORM )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL XF2RAV ( XFORM, XCMAT, XAV )
!
!              Look up pointing data.
!
               CKTOL = 0.D0
               CALL CKGPAV ( XINST, T,    CKTOL, &
                             XREF,  CMAT, AV, CLKOUT, FOUND )
               CALL CHCKXC ( .FALSE., ' ', OK )

               IF ( .NOT. OK ) THEN
                  CALL SIGERR( 'NO POINTING' )
                  CALL CHCKXC ( .FALSE., ' ', OK )
                  STOP
               END IF

               CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

!
!              Check output time.
!
               CALL CHCKSD ( 'CLKOUT', CLKOUT, '~', T, 0.D0, OK )

               IF ( .NOT. OK ) THEN
!
!                 Re-check with a more specific item name.
!
                  NAME = 'Mini-segment *; CLKOUT no. *'
                  CALL REPMI ( NAME, '*', MNSGNO, NAME )
                  CALL CHCKXC ( .FALSE., ' ', OK )
                  CALL REPMI ( NAME, '*', J, NAME )
                  CALL CHCKXC ( .FALSE., ' ', OK )

                  CALL CHCKSD ( NAME, CLKOUT, '~', T, 0.D0, OK )

               END IF

!
!              Check C-matrix.
!
               CALL CHCKAD ( 'CMAT', CMAT, '~~/', XCMAT, 9, VTIGHT, OK )

               IF ( .NOT. OK ) THEN

                  NAME = 'Mini-segment *; CMAT no. *'
                  CALL REPMI ( NAME, '*', MNSGNO, NAME )
                  CALL CHCKXC ( .FALSE., ' ', OK )
                  CALL REPMI ( NAME, '*', J, NAME )
                  CALL CHCKXC ( .FALSE., ' ', OK )

                  CALL CHCKAD ( NAME, CMAT,  '~~/', &
                                      XCMAT, 9,     VTIGHT, OK )

               END IF


!
!              Check angular velocity.
!
!              For derived AV, use a looser tolerance.
!
               IF ( SUBTPS(MNSGNO) .EQ. C06TP1 ) THEN

                  TOL = MED

               ELSE IF ( SUBTPS(MNSGNO) .EQ. C06TP0 ) THEN

                  TOL = TIGHT

               ELSE IF ( SUBTPS(MNSGNO) .EQ. C06TP3 ) THEN

                  TOL = VTIGHT
               ELSE
                  TOL = TIGHT
               END IF


               CALL CHCKAD ( 'AV', AV, '~~/', XAV, 3, TOL, OK )

               IF ( .NOT. OK ) THEN

                  NAME = 'Mini-segment *; AV no. *'
                  CALL REPMI ( NAME, '*', MNSGNO, NAME )
                  CALL CHCKXC ( .FALSE., ' ', OK )
                  CALL REPMI ( NAME, '*', J, NAME )
                  CALL CHCKXC ( .FALSE., ' ', OK )

                  CALL CHCKAD ( 'AV', AV, '~~/', XAV, 3, TOL, OK )

               END IF

               J      = J      + 1
               PKTPTR = PKTPTR + PSIZES(MNSGNO)


            END DO
!
!           End of tests for current interval.
!
         END DO
!
!        End of tests for current segment.
!
      END DO

      CALL UNLOAD ( CK )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
! --- Case: ------------------------------------------------------
!

!
!     Perform tests on CK06B2.
!
      CK     = CK06B2
      SELLST = .TRUE.

      TITLE = 'Interval/Packet selection logic test:  ' &
      //      'look up data at time tags within all ' &
      //      'three segments of CK #.'

      CALL REPMC  ( TITLE, '#', CK, TITLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL TCASE  ( TITLE )

!
!     Load the test CK.
!
      CALL FURNSH ( CK )
      CALL CHCKXC ( .FALSE., ' ', OK )

      N      = 0
      SEGPTR = 0

      DO SEGNO = 1, NSEG

         SEGPTR = SEGPTR + N
!
! --- Case: ------------------------------------------------------
!

         TITLE = 'Interval/Packet selection logic test:  ' &
      //         'look up data at time tags within ' &
      //         'segment # of CK #.'

         CALL REPMI  ( TITLE, '#', SEGNO, TITLE )
         CALL REPMC  ( TITLE, '#', CK,    TITLE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL TCASE  ( TITLE )

         XINST  = XINSTS(SEGNO)
         NINTVL = NIVALS(SEGNO)
         XREF   = XREFS (SEGNO)

         DO I = 1, NINTVL

            IF ( MOD(I,3) .EQ. 0 ) THEN

               SUBTPS(I) = C06TP3
               PSIZES(I) = C06PS3
               DEGRES(I) = 3
               RATES (I) = 1.1D0
               NPKTS (I) = 5

            ELSE IF ( MOD(I,3) .EQ. 1 ) THEN

               SUBTPS(I) = C06TP1
               PSIZES(I) = C06PS1
               DEGRES(I) = 7
               RATES (I) = 0.8D0
               NPKTS (I) = 9

            ELSE

               SUBTPS(I) = C06TP0
               PSIZES(I) = C06PS0
               DEGRES(I) = 7
               RATES (I) = 4.D0
               NPKTS (I) = 11

            END IF

         END DO

!
!        Create time tags and packets for the current segment.
!
         BEGREC = 1
         N      = 1
         PKTPTR = 1

         DO MNSGNO = 1, NINTVL
!
!           Record the index of the first non-pad time tag of
!           the current mini-segment.
!
            DATIDX(MNSGNO) = N + PAD

            CALL MOVED ( XEPOCH(SEGPTR+N), NPKTS(MNSGNO), SCLKDP(N) )
!
!           Update the count of the records in the segment.
!
            N = N + NPKTS(MNSGNO)

         END DO

!
!        The stop time of the last interval is the epoch at
!        PAD positions before the final epoch.
!
         XIVBDS( NINTVL+1 ) = SCLKDP( N - PAD )

!
!        Let the segment bounds coincide with the start and stop
!        times of the first and last mini-segments.
!
         FIRST = XIVBDS(1)
         LAST  = XIVBDS(NINTVL+1)

!
!        Check data for each interval.
!
         IF ( SEGNO .EQ. 3 ) THEN
            SKIP = 1
         ELSE
            SKIP = 1
         END IF

         N = 1

         DO MNSGNO = 1, NINTVL, SKIP

            J      = DATIDX(MNSGNO)

            JMAX   = DATIDX(MNSGNO) - 1 + NPKTS(MNSGNO) - 2*PAD

            IF (  SELLST  .AND.  ( MNSGNO .LT. NINTVL ) ) THEN
!
!              Skip the last time tag of each mini-segment but the
!              last, since the correct data will come from the
!              next interval.
!
               JMAX = JMAX - 1

            END IF

!
!           In the test below, ".LT." is used for the "select last"
!           option.
!
            DO WHILE ( J .LT. JMAX )

               T = SCLKDP(J)

               CALL MOVED ( XQUATS(0,SEGPTR+J), 4, XQ  )
               CALL MOVED ( XAVVS (1,SEGPTR+J), 3, XAV )

               CALL Q2M ( XQ, XCMAT )
               CALL CHCKXC ( .FALSE., ' ', OK )

!
!              Look up pointing data.
!
               CALL CKGPAV ( XINST, T, CKTOL, &
                             XREF,  CMAT,      AV, CLKOUT, FOUND )
               CALL CHCKXC ( .FALSE., ' ', OK )

               IF ( .NOT. OK ) THEN
                  CALL SIGERR( 'NO POINTING' )
                  CALL CHCKXC ( .FALSE., ' ', OK )
                  STOP
               END IF

               CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )


!
!              Check output time.
!
               CALL CHCKSD ( 'CLKOUT', CLKOUT, '~', T, 0.D0, OK )

               IF ( .NOT. OK ) THEN
!
!                 Re-check with a more specific item name.
!
                  NAME = 'Mini-segment *; CLKOUT no. *'
                  CALL REPMI ( NAME, '*', MNSGNO, NAME )
                  CALL CHCKXC ( .FALSE., ' ', OK )
                  CALL REPMI ( NAME, '*', J, NAME )
                  CALL CHCKXC ( .FALSE., ' ', OK )

                  CALL CHCKSD ( NAME, CLKOUT, '~', T, 0.D0, OK )

               END IF

!
!              Check C-matrix.
!
               CALL CHCKAD ( 'CMAT', CMAT, '~~/', XCMAT, 9, VTIGHT, OK )

               IF ( .NOT. OK ) THEN

                  NAME = 'Mini-segment *; CMAT no. *'
                  CALL REPMI ( NAME, '*', MNSGNO, NAME )
                  CALL CHCKXC ( .FALSE., ' ', OK )
                  CALL REPMI ( NAME, '*', J, NAME )
                  CALL CHCKXC ( .FALSE., ' ', OK )

                  CALL CHCKAD ( NAME, CMAT,  '~~/', &
                                      XCMAT, 9,     VTIGHT, OK )

               END IF


!
!              Check angular velocity.
!
!              For derived AV, use a looser tolerance.
!
               IF ( SUBTPS(MNSGNO) .EQ. C06TP1 ) THEN

                  TOL = MED

               ELSE IF ( SUBTPS(MNSGNO) .EQ. C06TP0 ) THEN

                  TOL = TIGHT

               ELSE IF ( SUBTPS(MNSGNO) .EQ. C06TP3 ) THEN

                  TOL = VTIGHT
               ELSE
                  TOL = TIGHT
               END IF


               CALL CHCKAD ( 'AV', AV, '~~/', XAV, 3, TOL, OK )

               IF ( .NOT. OK ) THEN

                  NAME = 'Mini-segment *; AV no. *'
                  CALL REPMI ( NAME, '*', MNSGNO, NAME )
                  CALL CHCKXC ( .FALSE., ' ', OK )
                  CALL REPMI ( NAME, '*', J, NAME )
                  CALL CHCKXC ( .FALSE., ' ', OK )

                  CALL CHCKAD ( 'AV', AV, '~~/', XAV, 3, TOL, OK )

               END IF

               J = J + 1

            END DO

            N = N + NPKTS(MNSGNO )
!
!           End of tests for current interval.
!
         END DO
!
!        End of tests for current segment.
!
      END DO




!
! --- Case: ------------------------------------------------------
!
!
!     Perform tests using data lookups at midpoints
!     between consecutive time tags. This set of tests
!     is for segments using the "select last" option.
!
!     CK is CK06B2.
!

      DO SEGNO = 1, NSEG
!
! --- Case: ------------------------------------------------------
!

         TITLE = 'Interval/Packet selection logic test:  ' &
      //         'look up data at midpoints between time tags within ' &
      //         'segment # of CK #.'

         CALL REPMI  ( TITLE, '#', SEGNO, TITLE )
         CALL REPMC  ( TITLE, '#', CK,    TITLE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL TCASE  ( TITLE )


         SELLST = .TRUE.

         XINST  = XINSTS(SEGNO)
         NINTVL = NIVALS(SEGNO)
         XREF   = XREFS (SEGNO)
         CKFRAM = 'IAU_EARTH'

         DO I = 1, NINTVL

            IF ( MOD(I,3) .EQ. 0 ) THEN

               SUBTPS(I) = C06TP3
               PSIZES(I) = C06PS3
               DEGRES(I) = 3
               RATES (I) = 1.1D0
               NPKTS (I) = 5

            ELSE IF ( MOD(I,3) .EQ. 1 ) THEN

               SUBTPS(I) = C06TP1
               PSIZES(I) = C06PS1
               DEGRES(I) = 7
               RATES (I) = 0.8D0
               NPKTS (I) = 9

            ELSE

               SUBTPS(I) = C06TP0
               PSIZES(I) = C06PS0
               DEGRES(I) = 7
               RATES (I) = 4.D0
               NPKTS (I) = 11

            END IF

         END DO


!
!        Create time tags for the current segment.
!
         BEGREC = 1
         N      = 1
         PKTPTR = 1

         DO MNSGNO = 1, NINTVL
!
!           Record the index of the first non-pad time tag of
!           the current mini-segment.
!
            DATIDX(MNSGNO) = N + PAD

            RATE   = RATES (MNSGNO)
            SUBTYP = SUBTPS(MNSGNO)

            CALL T_GENTAG ( BEGREC, NPKTS(MNSGNO), EPOCHS )
            CALL CHCKXC ( .FALSE., ' ', OK )

!
!           The start time of the current interval is the epoch at
!           PAD positions later than the start epoch.
!
            XIVBDS( MNSGNO ) = EPOCHS( 1 + PAD )

            CALL MOVED ( EPOCHS, NPKTS(MNSGNO), SCLKDP(N) )

!
!           Adjust the starting record number of the next mini-segment
!           to account for padding.
!
            BEGREC = (BEGREC-1) + NPKTS(MNSGNO) - 2*PAD

!
!           Update the count of the records in the segment.
!
            N      = N + NPKTS(MNSGNO)

         END DO

!
!        The stop time of the last interval is the epoch at
!        PAD positions before the final epoch.
!
         XIVBDS( NINTVL+1 ) = SCLKDP( N - PAD )

!
!        Let the segment bounds coincide with the start and stop
!        times of the first and last mini-segments.
!
         FIRST = XIVBDS(1)
         LAST  = XIVBDS(NINTVL+1)


!
!        Check data for each interval.
!
         IF ( SEGNO .EQ. 3 ) THEN
            SKIP = 1
         ELSE
            SKIP = 1
         END IF


         DO MNSGNO = 1, NINTVL, SKIP

            RATE = RATES(MNSGNO)
            J    = DATIDX(MNSGNO)

            JMAX = DATIDX(MNSGNO) - 1 + NPKTS(MNSGNO) - 2*PAD

            DO WHILE ( J .LT. JMAX )

               T  = ( SCLKDP(J) + SCLKDP(J+1) ) / 2

!              The following formula for ET looks strange, but it's
!              correct.
!
               ET = T * RATE
!
!              Generate expected results for ET.
!
               CALL SXFORM ( XREF, CKFRAM, ET, XFORM )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL XF2RAV ( XFORM, XCMAT, XAV )
!
!              Look up pointing data.
!
               CALL CKGPAV ( XINST, T, CKTOL, &
                             XREF,  CMAT,      AV, CLKOUT, FOUND )
               CALL CHCKXC ( .FALSE., ' ', OK )

               IF ( .NOT. OK ) THEN
                  CALL SIGERR( 'NO POINTING' )
                  CALL CHCKXC ( .FALSE., ' ', OK )
                  STOP
               END IF

               CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

!
!              Check output time.
!
               CALL CHCKSD ( 'CLKOUT', CLKOUT, '~', T, 0.D0, OK )

               IF ( .NOT. OK ) THEN
!
!                 Re-check with a more specific item name.
!
                  NAME = 'Mini-segment *; CLKOUT no. *'
                  CALL REPMI ( NAME, '*', MNSGNO, NAME )
                  CALL CHCKXC ( .FALSE., ' ', OK )
                  CALL REPMI ( NAME, '*', J, NAME )
                  CALL CHCKXC ( .FALSE., ' ', OK )

                  CALL CHCKSD ( NAME, CLKOUT, '~', T, 0.D0, OK )

               END IF

!
!              Check C-matrix.
!
               CALL CHCKAD ( 'CMAT', CMAT, '~~/', XCMAT, 9, VTIGHT, OK )

               IF ( .NOT. OK ) THEN

                  NAME = 'Mini-segment *; CMAT no. *'
                  CALL REPMI ( NAME, '*', MNSGNO, NAME )
                  CALL CHCKXC ( .FALSE., ' ', OK )
                  CALL REPMI ( NAME, '*', J, NAME )
                  CALL CHCKXC ( .FALSE., ' ', OK )

                  CALL CHCKAD ( NAME, CMAT,  '~~/', &
                                      XCMAT, 9,     VTIGHT, OK )

               END IF


!
!              Check angular velocity.
!
!              For derived AV, use a looser tolerance.
!
               IF ( SUBTPS(MNSGNO) .EQ. C06TP1 ) THEN

                  TOL = MED

               ELSE IF ( SUBTPS(MNSGNO) .EQ. C06TP0 ) THEN

                  TOL = TIGHT

               ELSE IF ( SUBTPS(MNSGNO) .EQ. C06TP3 ) THEN

                  TOL = VTIGHT
               ELSE
                  TOL = TIGHT
               END IF


               CALL CHCKAD ( 'AV', AV, '~~/', XAV, 3, TOL, OK )

               IF ( .NOT. OK ) THEN

                  NAME = 'Mini-segment *; AV no. *'
                  CALL REPMI ( NAME, '*', MNSGNO, NAME )
                  CALL CHCKXC ( .FALSE., ' ', OK )
                  CALL REPMI ( NAME, '*', J, NAME )
                  CALL CHCKXC ( .FALSE., ' ', OK )

                  CALL CHCKAD ( 'AV', AV, '~~/', XAV, 3, TOL, OK )

               END IF


               J      = J      + 1
               PKTPTR = PKTPTR + PSIZES(MNSGNO)

            END DO
!
!           End of tests for current interval.
!
         END DO
!
!        End of tests for current segment.
!
      END DO



      CALL UNLOAD ( CK )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'Clean up:  delete CK files.' )


      CALL DAFCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      IF (  EXISTS( CK06E )  ) THEN
         CALL DELFIL ( CK06E )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF

      IF (  EXISTS( CK06MU )  ) THEN
         CALL DELFIL ( CK06MU )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF

      IF (  EXISTS( CK06T0 )  ) THEN
         CALL DELFIL ( CK06T0 )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF

      IF (  EXISTS( CK06T1 )  ) THEN
         CALL DELFIL ( CK06T1 )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF

      IF (  EXISTS( CK06B0 )  ) THEN
         CALL DELFIL ( CK06B0 )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF

      IF (  EXISTS( CK06B1 )  ) THEN
         CALL DELFIL ( CK06B1 )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF

      IF (  EXISTS( CK06B2 )  ) THEN
         CALL DELFIL ( CK06B2 )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF

!
!     Close out the test family.
!
      CALL T_SUCCESS ( OK )
      RETURN
      END







!*****************************************************************
!*****************************************************************
!*
!*
!*
!*
!*    T E S T   U T I L I T I E S
!*
!*
!*
!*
!*****************************************************************
!*****************************************************************



!*****************************************************************
!
!     T_GENC06
!
!*****************************************************************

!
!     Generate C-matrices for CK type 06 software testing.
!
!        Version 1.0.0 03-FEB-2014 (NJB)
!
!
!     Output AVV has units of radians/s.
!
      SUBROUTINE T_GENC06 ( SEGNO, MNSGNO, BEGREC, N,    FRAME, &
                            RATE,  EPOCHS, QUATS,  AVVS, OK    )
      IMPLICIT NONE

      INTEGER               SEGNO
      INTEGER               MNSGNO
      INTEGER               BEGREC
      INTEGER               N
      CHARACTER*(*)         FRAME
      DOUBLE PRECISION      RATE
      DOUBLE PRECISION      EPOCHS ( * )
      DOUBLE PRECISION      QUATS  ( 0:3, * )
      DOUBLE PRECISION      AVVS   ( 3, * )
      LOGICAL               OK

!
!     SPICELIB functions
!
      DOUBLE PRECISION      DPR

      LOGICAL               EQSTR
      LOGICAL               RETURN

!
!     Local parameters
!
      CHARACTER*(*)         IREF
      PARAMETER           ( IREF    = 'J2000' )

!
!     Local variables
!
      DOUBLE PRECISION      ANGLE1
      DOUBLE PRECISION      ANGLE
      DOUBLE PRECISION      AXIS   ( 3 )
      DOUBLE PRECISION      CMAT   ( 3, 3 )
      DOUBLE PRECISION      DANGL1
      DOUBLE PRECISION      M
      DOUBLE PRECISION      RESX   ( 6, 6 )
      DOUBLE PRECISION      S
      DOUBLE PRECISION      XF     ( 6, 6 )
      DOUBLE PRECISION      XFORM  ( 6, 6 )

      INTEGER               I
      INTEGER               J
      INTEGER               K

      LOGICAL               SAMFRM


      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'T_GENC06' )

!
!     We'll construct a sequence of C-matrices and angular velocities
!     representing rotations about a constant axis in the frame
!     designated by IREF. Those rotations and corresponding angular
!     velocities will be transformed to the requested output frame.
!
      SAMFRM = EQSTR( IREF, FRAME )

!
!     Below we define some parameters used for rotation angle
!     construction.
!
!     S is a scale factor for the segment number.
!
      S = 2.5D9
!
!     M is a scale factor for the mini-segment number.
!
      M = 5.D4

!
!     AXIS is the rotation axis in the J2000 frame.
!
      CALL VPACK ( 0.D0, 0.D0, 1.D0, AXIS )

!
!     ANGLE1 is the base angle used to generate all of the rotation
!     angles. These angles are computed using the formula
!
!                                          S*SEGNO + M*MNSGNO + I
!        ANGLE( SEGNO, MNSGNO, I ) = ANGLE1
!


      ANGLE1 = DPR() ** ( 1.D0 / (S*SEGNO + M*MNSGNO + 100000) )

      DO I = 1, N

         J         = BEGREC - 1 + I

         EPOCHS(I) = DBLE(J)

         ANGLE     = ANGLE1 ** (S*SEGNO + M*MNSGNO + J)

         CALL AXISAR ( AXIS, -ANGLE, CMAT )
!
!        Create an angular velocity vector that is roughly consistent
!        with the C-matrix sequence. We'll treat the angle as a
!        function of I and differentiate the angle formula with respect
!        to I. Note that
!
!           ANGLE = EXP( LOG(ANGLE1) * (S*SEGNO + M*MNSGNO + I) )
!
!
         DANGL1 = ANGLE * LOG(ANGLE1)
!
!        Scale the angular rate from radians/tick to radians/s.
!
!        RATE has units of seconds/tick.
!
         DANGL1 = DANGL1 / RATE

         CALL VSCL ( DANGL1, AXIS, AVVS(1,I) )
!
!        Transform the C-matrix and AVV to the output frame
!        if necessary. The output frame is the new base frame.
!
         IF ( .NOT. SAMFRM ) THEN

            CALL SXFORM ( FRAME,  IREF,  EPOCHS(I),  XFORM      )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL RAV2XF ( CMAT,   AVVS(1,I),  XF                )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL MXMG   ( XF,     XFORM,  6, 6, 6,    RESX      )

            CALL XF2RAV ( RESX,   CMAT,               AVVS(1,I) )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END IF

         CALL M2Q ( CMAT, QUATS(0,I) )
         CALL CHCKXC ( .FALSE., ' ', OK )

         IF ( QUATS(0,I) .LT. 0.D0 ) THEN

            DO K = 0, 3
               QUATS(K,I) = -QUATS(K,I)
            END DO

         END IF

      END DO

      CALL CHKOUT ( 'T_GENC06' )
      RETURN
      END



!*****************************************************************
!
!     T_GENCSM ( Generate C-matrices from smooth data )
!
!*****************************************************************

!
!     Generate C-matrices for CK type 06 software testing.
!     Use smooth data: earth rotation from a test utility PCK.
!
!        Version 1.0.0 03-FEB-2014 (NJB)
!
      SUBROUTINE T_GENCSM ( SEGNO, MNSGNO, BEGREC, N,     FRAME, &
                            RATE,  SUBTYP, EPOCHS, QUATS, AVVS, OK )
      IMPLICIT NONE
      INCLUDE 'ck06.inc'

      INTEGER               SEGNO
      INTEGER               MNSGNO
      INTEGER               BEGREC
      INTEGER               N
      CHARACTER*(*)         FRAME
      DOUBLE PRECISION      RATE
      INTEGER               SUBTYP
      DOUBLE PRECISION      EPOCHS ( * )
      DOUBLE PRECISION      QUATS  ( 0:3, * )
      DOUBLE PRECISION      AVVS   ( 3,   * )
      LOGICAL               OK
!
!     SPICELIB functions
!
      DOUBLE PRECISION      VDISTG
      INTEGER               TOUCHI
      LOGICAL               RETURN

!
!     Local parameters
!
      CHARACTER*(*)         CKFRAM
      PARAMETER           ( CKFRAM = 'IAU_EARTH' )

      CHARACTER*(*)         PCK0
      PARAMETER           ( PCK0   = 'test.tpc' )

!
!     Local variables
!
      DOUBLE PRECISION      CMAT   ( 3, 3 )
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      QN     ( 0:3 )
      DOUBLE PRECISION      XFORM  ( 6, 6 )

      INTEGER               I
      INTEGER               J

      LOGICAL               PASS1

!
!     Saved variables
!
      SAVE                  PASS1

!
!     Initial values
!
      DATA                  PASS1 / .TRUE. /


      IF ( RETURN() ) THEN
         RETURN
      END IF

!      CALL CHKIN ( 'T_GENCSM' )

      I = TOUCHI( MNSGNO )
      I = TOUCHI( SEGNO  )

!
!     Load a test PCK on the first pass.
!
      IF ( PASS1 ) THEN

         CALL TSTPCK ( PCK0, .TRUE., .FALSE. )
         CALL CHCKXC ( .FALSE., ' ', OK )

         PASS1 = .FALSE.

      END IF

!
!     We'll construct a sequence of C-matrices and angular velocities
!     for the orientation of the earth relative to the IREF frame.
!     Those rotations and corresponding angular velocities will be
!     transformed to the requested output frame.
!
      DO I = 1, N

         J         = BEGREC - 1 + I

         EPOCHS(I) = DBLE(J)

!
!        Convert SCLK to ET for SXFORM lookup. This is necessary
!        in order to derived angular velocity consistent with
!        orientation. RATE has units of seconds/tick.
!
         ET        = EPOCHS(I) * RATE

!
!         Initial orientation is that of the earth relative
!         to J2000.
!
         CALL SXFORM ( FRAME, CKFRAM, ET, XFORM )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL XF2RAV ( XFORM, CMAT, AVVS(1,I) )
         CALL CHCKXC ( .FALSE., ' ', OK )


         CALL M2Q ( CMAT, QUATS(0,I) )
         CALL CHCKXC ( .FALSE., ' ', OK )


!
!        Adjust data to achieve better consistency, if necessary.
!
         IF ( I .GT. 1 ) THEN

            IF (      ( SUBTYP .EQ. C06TP0 ) &
                 .OR. ( SUBTYP .EQ. C06TP2 ) ) THEN


               CALL VMINUG ( QUATS(0,I), 4, QN )

               IF (      VDISTG( QUATS(0,I), QUATS(0,I-1), 4 ) &
                    .GT. VDISTG( QN,         QUATS(0,I-1), 4 )  ) THEN
!
!                 Replace the original quaternion with its negative.
!
                  CALL MOVED  ( QN, 4, QUATS(0,I) )

               END IF

            END IF

         END IF

      END DO

!      CALL CHKOUT ( 'T_GENCSM' )
      RETURN
      END



!*****************************************************************
!
!     T_GENTAG (Generate time tags for T_GENCSM)
!
!*****************************************************************

!
!     Generate just the time tag sequence generated by T_GENCSM
!     for a given set of inputs.
!
!        Version 1.0.0 07-FEB-2014 (NJB)
!
      SUBROUTINE T_GENTAG ( BEGREC, N, EPOCHS )
      IMPLICIT NONE
      INCLUDE 'ck06.inc'

      INTEGER               BEGREC
      INTEGER               N
      DOUBLE PRECISION      EPOCHS ( * )
!
!     SPICELIB functions
!
      LOGICAL               RETURN

!
!     Local parameters
!

!
!     Local variables
!
      INTEGER               I
      INTEGER               J


      IF ( RETURN() ) THEN
         RETURN
      END IF

!
!     We'll construct a sequence of C-matrices and angular velocities
!     for the orientation of the earth relative to the IREF frame.
!     Those rotations and corresponding angular velocities will be
!     transformed to the requested output frame.
!
      DO I = 1, N

         J         = BEGREC - 1 + I

         EPOCHS(I) = DBLE(J)

      END DO

      RETURN
      END







!*****************************************************************
!
!     T_XSUBTP
!
!*****************************************************************

!
!     Transform record subtype. Input is always a quaternion
!     and avv; output is any other type.
!
!     For subtype 2, the caller must supply the derivative
!     with respect to ticks of the angular velocity.
!
      SUBROUTINE T_XSUBTP ( QUAT, AVV, DAVV, SUBTYP, SIZE, RECORD )
      IMPLICIT NONE

      INCLUDE 'ck06.inc'

      DOUBLE PRECISION      QUAT   ( 0:3 )
      DOUBLE PRECISION      AVV    ( 3 )
      DOUBLE PRECISION      DAVV   ( 3 )
      INTEGER               SUBTYP
      INTEGER               SIZE
      DOUBLE PRECISION      RECORD ( * )

!
!     SPICELIB functions
!
!      DOUBLE PRECISION      VDOTG
      LOGICAL               RETURN

!
!     Local variables
!
      DOUBLE PRECISION      AVQ ( 0 : 3 )
      DOUBLE PRECISION      DQ  ( 0 : 3 )


      IF ( RETURN() ) THEN
         RETURN
      END IF

!      CALL CHKIN ( 'T_XSUBTP' )

      CALL CLEARD ( SIZE, RECORD )

      IF ( SUBTYP .EQ. C06TP0 ) THEN
!
!        Create a quaternion derivative from a quaternion and avv. Pack
!        the quaternion and its derivative into the RECORD array.
!
         CALL MOVED( QUAT, 4, RECORD )
!
!        From the header of QDQ2AV we have the equation:
!
!                             *
!           AV  =  Im ( -2 * Q  * DQ )                        (1)
!
!        Here AV can be viewed as a quaternion with real part
!        equal to 0. Then
!
!           DQ = -0.5 * Q * AV
!

         AVQ(0) = 0.D0

         CALL VSCL  ( -0.5D0, AVV, AVQ(1)    )
         CALL QXQ   ( QUAT,   AVQ, DQ        )
         CALL MOVED ( DQ,     4,   RECORD(5) )


      ELSE IF ( SUBTYP .EQ. C06TP1 ) THEN
!
!        No transformation required.
!
         CALL MOVED ( QUAT, 4, RECORD )

      ELSE IF ( SUBTYP .EQ. C06TP2 ) THEN
!
!        Create a quaternion derivative from a quaternion and avv. Pack
!        the quaternion and its derivative into the RECORD array.
!
         CALL MOVED( QUAT, 4, RECORD )
!
!        From the header of QDQ2AV we have the equation:
!
!                             *
!           AV  =  Im ( -2 * Q  * DQ )                        (1)
!
!        Here AV can be viewed as a quaternion with real part
!        equal to 0. Then
!
!           DQ = -0.5 * Q * AV
!
!
         AVQ(0) = 0.D0

         CALL VSCL  ( -0.5D0, AVV, AVQ(1)    )
         CALL QXQ   ( QUAT,   AVQ, DQ        )
         CALL MOVED ( DQ,     4,   RECORD(5) )
!
!        Transfer the avv and its derivative.
!
         CALL VEQU ( AVV,  RECORD(9)  )
         CALL VEQU ( DAVV, RECORD(12) )


      ELSE IF ( SUBTYP .EQ. C06TP3 ) THEN
!
!        No transformation required.
!
         CALL MOVED ( QUAT, 4, RECORD    )
         CALL VEQU  ( AVV,     RECORD(5) )

      ELSE

         CALL SETMSG ( 'SUBTYP was #.' )
         CALL ERRINT ( '#', SUBTYP     )
         CALL SIGERR ( 'SPICE(BUG)'    )
!         CALL CHKOUT ( 'T_XSUBTP' )
         RETURN

      END IF

!      CALL CHKOUT ( 'T_XSUBTP' )
      RETURN
      END
