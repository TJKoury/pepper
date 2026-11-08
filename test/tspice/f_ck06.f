C$Procedure F_CK06 ( CK data type 06 tests )
 
      SUBROUTINE F_CK06 ( OK )
 
C$ Abstract
C
C     Exercise routines associated with CK data type 06.
C
C$ Disclaimer
C
C     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
C     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
C     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
C     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
C     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
C     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
C     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
C     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
C     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
C     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
C
C     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
C     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
C     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
C     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
C     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
C     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
C
C     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
C     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
C     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
C     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
C
C$ Required_Reading
C
C     None.
C
C$ Keywords
C
C     TEST FAMILY
C
C$ Declarations
 
      IMPLICIT NONE

      INCLUDE 'ck06.inc'

      LOGICAL               OK
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     OK         O   logical indicating test status.
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C     OK         is a logical that indicates the test status to the
C                caller.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     This routine does not generate any errors. Routines in its
C     call tree may generate errors that are either intentional and
C     trapped or unintentional and need reporting.  The test family
C     utilities manage this.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine tests routines that write and read type 06 CK 
C     data.
C
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman     (JPL)
C
C$ Version
C
C-    TSPICE Version 2.0.0, 11-AUG-2015 (NJB)
C
C        Added test case for CKW06 invalid input SCLK rate.
C
C-    TSPICE Version 1.0.0, 24-MAR-2014 (NJB)
C
C
C-&
 
C
C     SPICELIB functions
C
      LOGICAL               EVEN
      LOGICAL               EXISTS

C
C     Local Parameters
C
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

C
C     Local Variables
C
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

C
C     Saved values
C
      SAVE

C
C     Initial values
C
      DATA                  PKTSZS / C06PS0, C06PS1, C06PS2, C06PS3 / 
C
C     Open the test family.
C
      CALL TOPEN ( 'F_CK06' )


C*****************************************************************
C*
C*    CKW06 error cases:
C*
C*****************************************************************


C
C     Test CKW06:  start out with error handling.
C     

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'CKW06 setup' )

C
C     Initialize the inputs to CKW06.
C
      XINST  = 3
      XAVFLG = .TRUE.
      XREF   = 'J2000'
C
C     Create a segment identifier.
C
      SEGID = 'CK type 06 test segment'

C
C     Open a new CK file.
C
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
C
C        Update total counts of records and packet elements.
C        The latter depends on the packet size of the current
C        mini-segment.
C
         IF ( SUBTPS(I) .EQ. C06TP0 ) THEN

            PKTSIZ = C06PS0

         ELSE IF ( SUBTPS(I) .EQ. C06TP1 ) THEN

            PKTSIZ = C06PS1

         ELSE
         
            CALL SETMSG ( 'Unexpected CK type 06 subtype # '
     .      //            'found in mini-segment #.'          )
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


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'CKW06 error case: descriptor times swapped.' )


      CALL CKW06 ( HANDLE,
     .             XINST,
     .             XREF,
     .             XAVFLG,
     .             LAST,
     .             FIRST,
     .             SEGID,
     .             NINTVL,
     .             NPKTS,
     .             SUBTPS,
     .             DEGRES,
     .             PACKTS,
     .             RATES,
     .             SCLKDP,
     .             IVLBDS,
     .             SELLST           )


      CALL CHCKXC ( .TRUE., 'SPICE(BADDESCRTIMES)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'CKW06 error case: bad frame name.'   )

      CALL CKW06 ( HANDLE,
     .             XINST,
     .             'SPUD',
     .             XAVFLG,
     .             FIRST,
     .             LAST,
     .             SEGID,
     .             NINTVL,
     .             NPKTS,
     .             SUBTPS,
     .             DEGRES,
     .             PACKTS,
     .             RATES,
     .             SCLKDP,
     .             IVLBDS,
     .             SELLST           )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDREFFRAME)', OK )
 


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'CKW06 error case: SEGID too long.'   )

      CALL CKW06 ( HANDLE,
     .             XINST,
     .             XREF,
     .             XAVFLG,
     .             FIRST,
     .             LAST,
     .             'X                                               X',
     .             NINTVL,
     .             NPKTS,
     .             SUBTPS,
     .             DEGRES,
     .             PACKTS,
     .             RATES,
     .             SCLKDP,
     .             IVLBDS,
     .             SELLST           )

      CALL CHCKXC ( .TRUE., 'SPICE(SEGIDTOOLONG)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'CKW06 error case: unprintable SEGID characters.' )

      CALL CKW06 ( HANDLE,
     .             XINST,
     .             XREF,
     .             XAVFLG,
     .             FIRST,
     .             LAST,
     .             CHAR(7),
     .             NINTVL,
     .             NPKTS,
     .             SUBTPS,
     .             DEGRES,
     .             PACKTS,
     .             RATES,
     .             SCLKDP,
     .             IVLBDS,
     .             SELLST           )

      CALL CHCKXC ( .TRUE., 'SPICE(NONPRINTABLECHARS)', OK )



C
C --- Case: ------------------------------------------------------
C


      CALL TCASE ( 'CKW06 error case: non-positive mini-segment count' )
 
      CALL CKW06 ( HANDLE,
     .             XINST,
     .             XREF,
     .             XAVFLG,
     .             FIRST,
     .             LAST,
     .             SEGID,
     .             0,
     .             NPKTS,
     .             SUBTPS,
     .             DEGRES,
     .             PACKTS,
     .             RATES,
     .             SCLKDP,
     .             IVLBDS,
     .             SELLST           )
 
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDCOUNT)', OK )
 
 
      NINTVL = 1
 


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'CKW06 error case: non-increasing '
     .//           'mini-segment bounds' )

      
      IVLBDS(2) = IVLBDS(1)
      FIRST     = 1.5D0
      LAST      = 1.6D0

      CALL CKW06 ( HANDLE,
     .             XINST,
     .             XREF,
     .             XAVFLG,
     .             FIRST,
     .             LAST,
     .             SEGID,
     .             NINTVL,
     .             NPKTS,
     .             SUBTPS,
     .             DEGRES,
     .             PACKTS,
     .             RATES,
     .             SCLKDP,
     .             IVLBDS,
     .             SELLST           )

      CALL CHCKXC ( .TRUE., 'SPICE(BOUNDSOUTOFORDER)', OK )

      IVLBDS(1) = 1.D0
      IVLBDS(2) = 2.D0

      FIRST     = IVLBDS(1)
      LAST      = IVLBDS(2)


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'CKW06 error case: descriptor  ' //
     .             'start time is too early.'       )


      CALL CKW06 ( HANDLE,
     .             XINST,
     .             XREF,
     .             XAVFLG,
     .             FIRST - 1.D0,
     .             LAST,
     .             SEGID,
     .             NINTVL,
     .             NPKTS,
     .             SUBTPS,
     .             DEGRES,
     .             PACKTS,
     .             RATES,
     .             SCLKDP,
     .             IVLBDS,
     .             SELLST           )

      CALL CHCKXC ( .TRUE., 'SPICE(COVERAGEGAP)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'CKW06 error case: descriptor end time is ' //
     .            'too late.'                                  )

      CALL CKW06 ( HANDLE,
     .             XINST,
     .             XREF,
     .             XAVFLG,
     .             FIRST,
     .             LAST + 1.D0,
     .             SEGID,
     .             NINTVL,
     .             NPKTS,
     .             SUBTPS,
     .             DEGRES,
     .             PACKTS,
     .             RATES,
     .             SCLKDP,
     .             IVLBDS,
     .             SELLST           )

      CALL CHCKXC ( .TRUE., 'SPICE(COVERAGEGAP)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'CKW06 error case: too few packets' )

      NPKTS(1) = 1

      CALL CKW06 ( HANDLE,
     .             XINST,
     .             XREF,
     .             XAVFLG,
     .             FIRST,
     .             LAST,
     .             SEGID,
     .             NINTVL,
     .             NPKTS,
     .             SUBTPS,
     .             DEGRES,
     .             PACKTS,
     .             RATES,
     .             SCLKDP,
     .             IVLBDS,
     .             SELLST           )


      CALL CHCKXC ( .TRUE., 'SPICE(TOOFEWPACKETS)', OK )

      NPKTS(1) = 2



C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'CKW06 error case: invalid SCLK rate.' )

      RATES(1) = 0.D0

      CALL CKW06 ( HANDLE,
     .             XINST,
     .             XREF,
     .             XAVFLG,
     .             FIRST,
     .             LAST,
     .             SEGID,
     .             NINTVL,
     .             NPKTS,
     .             SUBTPS,
     .             DEGRES,
     .             PACKTS,
     .             RATES,
     .             SCLKDP,
     .             IVLBDS,
     .             SELLST           )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDSCLKRATE)', OK )

      RATES(1) = -1.D0

      CALL CKW06 ( HANDLE,
     .             XINST,
     .             XREF,
     .             XAVFLG,
     .             FIRST,
     .             LAST,
     .             SEGID,
     .             NINTVL,
     .             NPKTS,
     .             SUBTPS,
     .             DEGRES,
     .             PACKTS,
     .             RATES,
     .             SCLKDP,
     .             IVLBDS,
     .             SELLST           )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDSCLKRATE)', OK )

      RATES(1) = 1.D0


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'CKW06 error case: invalid subtype.' )

      SUBTPS(1) = -1

      CALL CKW06 ( HANDLE,
     .             XINST,
     .             XREF,
     .             XAVFLG,
     .             FIRST,
     .             LAST,
     .             SEGID,
     .             NINTVL,
     .             NPKTS,
     .             SUBTPS,
     .             DEGRES,
     .             PACKTS,
     .             RATES,
     .             SCLKDP,
     .             IVLBDS,
     .             SELLST           )


      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDSUBTYPE)', OK )

      SUBTPS(1) = C06TP1



C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'CKW06 error case: polynomial degree too high.' )

      DEGRES(1) = 40

      CALL CKW06 ( HANDLE,
     .             XINST,
     .             XREF,
     .             XAVFLG,
     .             FIRST,
     .             LAST,
     .             SEGID,
     .             NINTVL,
     .             NPKTS,
     .             SUBTPS,
     .             DEGRES,
     .             PACKTS,
     .             RATES,
     .             SCLKDP,
     .             IVLBDS,
     .             SELLST           )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDDEGREE)', OK )

      DEGRES(1) = 1


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'CKW06 error case: polynomial degree too low.' )

      DEGRES(1) = 0

      CALL CKW06 ( HANDLE,
     .             XINST,
     .             XREF,
     .             XAVFLG,
     .             FIRST,
     .             LAST,
     .             SEGID,
     .             NINTVL,
     .             NPKTS,
     .             SUBTPS,
     .             DEGRES,
     .             PACKTS,
     .             RATES,
     .             SCLKDP,
     .             IVLBDS,
     .             SELLST           )


      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDDEGREE)', OK )

      DEGRES(1) = 1


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'CKW06 error case:  odd window size.' )

      DEGRES(1) = 2

      CALL CKW06 ( HANDLE,
     .             XINST,
     .             XREF,
     .             XAVFLG,
     .             FIRST,
     .             LAST,
     .             SEGID,
     .             NINTVL,
     .             NPKTS,
     .             SUBTPS,
     .             DEGRES,
     .             PACKTS,
     .             RATES,
     .             SCLKDP,
     .             IVLBDS,
     .             SELLST           )


      CALL CHCKXC ( .TRUE., 'SPICE(BADWINDOWSIZE)', OK )

      DEGRES(1) = 1



C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'CKW06 error case: epochs for first ' //
     .             'mini-segment are out of order.'      )

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

      CALL CKW06 ( HANDLE,
     .             XINST,
     .             XREF,
     .             XAVFLG,
     .             FIRST,
     .             LAST,
     .             SEGID,
     .             NINTVL,
     .             NPKTS,
     .             SUBTPS,
     .             DEGRES,
     .             PACKTS,
     .             RATES,
     .             SCLKDP,
     .             IVLBDS,
     .             SELLST           )

      CALL CHCKXC ( .TRUE., 'SPICE(TIMESOUTOFORDER)', OK )

      CALL SWAPD ( SCLKDP(2), SCLKDP(3) )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'CKW06 error case: first mini-segment ' 
     .//           'epoch follows interval start.'        )


      SCLKDP(1) = IVLBDS(1) + 0.1D0

      CALL CKW06 ( HANDLE,
     .             XINST,
     .             XREF,
     .             XAVFLG,
     .             FIRST,
     .             LAST,
     .             SEGID,
     .             NINTVL,
     .             NPKTS,
     .             SUBTPS,
     .             DEGRES,
     .             PACKTS,
     .             RATES,
     .             SCLKDP,
     .             IVLBDS,
     .             SELLST           )

      CALL CHCKXC ( .TRUE., 'SPICE(BOUNDSDISAGREE)', OK )


      SCLKDP(1) = 1.D0

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'CKW06 error case: last mini-segment ' 
     .//           'epoch precedes interval start.'        )


      DO I = 1, NPKTS(1)
         SCLKDP(I) = DBLE(I) - NPKTS(1) - 1.D0
      END DO

      CALL CKW06 ( HANDLE,
     .             XINST,
     .             XREF,
     .             XAVFLG,
     .             FIRST,
     .             LAST,
     .             SEGID,
     .             NINTVL,
     .             NPKTS,
     .             SUBTPS,
     .             DEGRES,
     .             PACKTS,
     .             RATES,
     .             SCLKDP,
     .             IVLBDS,
     .             SELLST           )

      CALL CHCKXC ( .TRUE., 'SPICE(BOUNDSDISAGREE)', OK )

      DO I = 1, NPKTS(1)
         SCLKDP(I) = DBLE(I)
      END DO

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'CKW06 *non* error case: epochs for first ' 
     .//           'mini-segment don''t cover first mini-segment '
     .//           'interval. A gap exists at the end of the '
     .//           'first mini-segment.'                           )

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

      CALL CKW06 ( HANDLE,
     .             XINST,
     .             XREF,
     .             XAVFLG,
     .             FIRST,
     .             LAST,
     .             SEGID,
     .             NINTVL,
     .             NPKTS,
     .             SUBTPS,
     .             DEGRES,
     .             PACKTS,
     .             RATES,
     .             SCLKDP,
     .             IVLBDS,
     .             SELLST           )

      CALL CHCKXC ( .FALSE., ' ', OK )

      DO I = 1, NPKTS(1)
         SCLKDP(I) = DBLE(I)
      END DO



C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'CKW06 error case: zero quaternion.'  )

      CALL FILLD ( 0.D0, 4, PACKTS )

      CALL CKW06 ( HANDLE,
     .             XINST,
     .             XREF,
     .             XAVFLG,
     .             FIRST,
     .             LAST,
     .             SEGID,
     .             NINTVL,
     .             NPKTS,
     .             SUBTPS,
     .             DEGRES,
     .             PACKTS,
     .             RATES,
     .             SCLKDP,
     .             IVLBDS,
     .             SELLST           )

      CALL CHCKXC ( .TRUE., 'SPICE(ZEROQUATERNION)', OK )

      DO I = 1, NPKTS(1)
         SCLKDP(I) = DBLE(I)
      END DO


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'CKW06 error case: quaternion sign error for ' 
     .//           'subtype 0.'                                  )


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

 
      CALL T_GENCSM ( SEGNO, MNSGNO, BEGREC, NPKTS(MNSGNO), XREF,
     .                RATE,  SUBTYP, SCLKDP, QUATS, AVBUFF, OK   ) 
      CALL CHCKXC ( .FALSE., ' ', OK )


      XIVBDS(1) = SCLKDP(1)
      XIVBDS(2) = SCLKDP(N)

      FIRST     = XIVBDS(1)
      LAST      = XIVBDS(2)

      PKTPTR = 1

      CALL CLEARD( 3, DAVV )

      DO I = 1, N

         PKTPTR = 1 + (I-1)*PKTSIZ

         CALL T_XSUBTP ( QUATS(0,I), AVBUFF(1,I), DAVV, 
     .                   SUBTYP,     PKTSIZ,      PACKTS(PKTPTR) )

C
C        Create error in quaternion sequence at packet index 5:
C        
         IF ( I .EQ. 5 ) THEN
            CALL VMINUG ( PACKTS(PKTPTR), 4, QNEG           )
            CALL MOVED  ( QNEG,           4, PACKTS(PKTPTR) )
         END IF
         
        
      END DO


      SEGID     = 'Subtype 0 test segment'
      SELLST    = .TRUE.

      CALL CKW06 ( HANDLE,
     .             XINST,
     .             XREF,
     .             XAVFLG,
     .             FIRST,
     .             LAST,
     .             SEGID,
     .             NINTVL,
     .             NPKTS,
     .             SUBTPS,
     .             DEGRES,
     .             PACKTS,
     .             RATES,
     .             SCLKDP,
     .             XIVBDS,
     .             SELLST           )
      CALL CHCKXC ( .TRUE., 'SPICE(BADQUATSIGN)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'CKW06 error case: quaternion sign error for ' 
     .//           'subtype 2.'                                  )

C
C     Use the inputs from the previous case, where possible.
C
      SUBTPS(1) = C06TP2
      PKTSIZ    = C06PS2

      PKTPTR = 1

      DO I = 1, N

         PKTPTR = 1 + (I-1)*PKTSIZ

         CALL T_XSUBTP ( QUATS(0,I), AVBUFF(1,I), DAVV, 
     .                   SUBTYP,     PKTSIZ,      PACKTS(PKTPTR) )

C
C        Create error in quaternion sequence at packet index 6:
C        
         IF ( I .EQ. 6 ) THEN
            CALL VMINUG ( PACKTS(PKTPTR), 4, QNEG           )
            CALL MOVED  ( QNEG,           4, PACKTS(PKTPTR) )
         END IF
                 
      END DO

      CALL CKW06 ( HANDLE,
     .             XINST,
     .             XREF,
     .             XAVFLG,
     .             FIRST,
     .             LAST,
     .             SEGID,
     .             NINTVL,
     .             NPKTS,
     .             SUBTPS,
     .             DEGRES,
     .             PACKTS,
     .             RATES,
     .             SCLKDP,
     .             XIVBDS,
     .             SELLST           )
      CALL CHCKXC ( .TRUE., 'SPICE(BADQUATSIGN)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'CKW06 non-error case: quaternion negation for ' 
     .//           'subtype 1. Make sure no error is signaled.'   )

C
C     Use the inputs from the previous case, where possible.
C
      SUBTPS(1) = C06TP1
      SUBTYP    = SUBTPS(1)
      PKTSIZ    = C06PS1

      PKTPTR = 1

      DO I = 1, N

         PKTPTR = 1 + (I-1)*PKTSIZ

         CALL T_XSUBTP ( QUATS(0,I), AVBUFF(1,I), DAVV, 
     .                   SUBTYP,     PKTSIZ,      PACKTS(PKTPTR) )

C
C        Create error in quaternion sequence at packet index 7:
C        
         IF ( I .EQ. 7 ) THEN
            CALL VMINUG ( PACKTS(PKTPTR), 4, QNEG           )
            CALL MOVED  ( QNEG,           4, PACKTS(PKTPTR) )
         END IF
                 
      END DO

      CALL CKW06 ( HANDLE,
     .             XINST,
     .             XREF,
     .             XAVFLG,
     .             FIRST,
     .             LAST,
     .             SEGID,
     .             NINTVL,
     .             NPKTS,
     .             SUBTPS,
     .             DEGRES,
     .             PACKTS,
     .             RATES,
     .             SCLKDP,
     .             XIVBDS,
     .             SELLST           )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'CKW06 non-error case: quaternion negation for ' 
     .//           'subtype 3. Make sure no error is signaled.'   )

C
C     Use the inputs from the previous case, where possible.
C
      SUBTPS(1) = C06TP3
      SUBTYP    = SUBTPS(1)
      PKTSIZ    = C06PS3

      PKTPTR = 1

      DO I = 1, N

         PKTPTR = 1 + (I-1)*PKTSIZ

         CALL T_XSUBTP ( QUATS(0,I), AVBUFF(1,I), DAVV, 
     .                   SUBTYP,     PKTSIZ,      PACKTS(PKTPTR) )

C
C        Create error in quaternion sequence at packet index 8:
C        
         IF ( I .EQ. 8 ) THEN
            CALL VMINUG ( PACKTS(PKTPTR), 4, QNEG           )
            CALL MOVED  ( QNEG,           4, PACKTS(PKTPTR) )
         END IF
                 
      END DO

      CALL CKW06 ( HANDLE,
     .             XINST,
     .             XREF,
     .             XAVFLG,
     .             FIRST,
     .             LAST,
     .             SEGID,
     .             NINTVL,
     .             NPKTS,
     .             SUBTPS,
     .             DEGRES,
     .             PACKTS,
     .             RATES,
     .             SCLKDP,
     .             XIVBDS,
     .             SELLST           )
      CALL CHCKXC ( .FALSE., ' ', OK )

       
      
C*****************************************************************
C*
C*    End of CKW06 error cases:
C*
C*****************************************************************
 
C
C     Close the CK file at the DAF level; CKCLS won't close
C     a file without segments.
C
      CALL DAFCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      

     
C*****************************************************************
C*
C*     CKW06 Non-error cases:
C*
C*****************************************************************


C*****************************************************************
C*
C*    Trivial case: write an CK containing a small subtype 1 
C*    segment.
C*
C*****************************************************************
 
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Test CKW06: create trivial subtype 1 segment.' )

      SEGNO  = 1
      MNSGNO = 1
      BEGREC = 1
      N      = 201
      XREF   = 'MARSIAU'

      CALL T_GENC06 ( SEGNO, MNSGNO, BEGREC, N,    XREF,
     .                1.D0,  SCLKDP, PACKTS, AVVS, OK )
 
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


C
C     Open new C-kernel.
C
      IF ( EXISTS(CK06T1) ) THEN
         CALL DELFIL( CK06T1 )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF

      CALL CKOPN ( CK06T1, CK06T1, 0, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Write segment.
C
      CALL CKW06 ( HANDLE,
     .             XINST,
     .             XREF,
     .             XAVFLG,
     .             FIRST,
     .             LAST,
     .             SEGID,
     .             NINTVL,
     .             NPKTS,
     .             SUBTPS,
     .             DEGRES,
     .             PACKTS,
     .             RATES,
     .             SCLKDP,
     .             XIVBDS,
     .             SELLST           )


C
C     Close kernel.
C
      CALL CKCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

       
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE  ( 'Read data records from trivial subtype 1 segment' )

      CALL DAFOPR ( CK06T1, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Get mini-segment count.
C
      CALL DAFBFS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFFNA ( FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFGS  ( DESCR )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CKNM06 ( HANDLE, DESCR, NMINI )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CKMP06 ( HANDLE, DESCR, 1,      RATE, SUBTYP,  
     .              WINSIZ, NREC,  IVLBDS, LSTEPC )
      CALL CHCKXC ( .FALSE., ' ', OK )


      PKTSIZ = C06PS1

      DO I = 1, NREC

         CALL CKGR06 ( HANDLE, DESCR, 1, I, RECORD )
         CALL CHCKXC ( .FALSE., ' ', OK )
C
C        Check the record. The time tag is the first element.
C        We expect exact matches for all elements.
C
         NAME = 'Time tag *'
         CALL REPMI ( NAME, '*', I, NAME )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSD ( NAME, RECORD(1), '=', SCLKDP(I), 0.D0, OK )

         NAME = 'Subtype code *'
         CALL REPMI ( NAME, '*', I, NAME )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSD ( NAME, RECORD(2),         '=', 
     .                       DBLE( SUBTPS(1) ), 0.D0, OK )

         NAME = 'Clock rate *'
         CALL REPMI ( NAME, '*', I, NAME )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSD ( NAME, RECORD(3), '=', RATES(1),  0.D0, OK )

C
C        Check the record data.
C
         NAME = 'Quaternion *'
         CALL REPMI ( NAME, '*', I, NAME )
         CALL CHCKXC ( .FALSE., ' ', OK )

         K = PKTSIZ*(I-1) + 1

         CALL CHCKAD ( NAME, RECORD(4), '=', 
     .                       PACKTS(K), PKTSIZ, 0.D0, OK )

      END DO

C
C     Close kernel.
C
      CALL CKCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )



C
C --- Case: ------------------------------------------------------
C
      CALL TCASE  ( 'Read data records from trivial subtype 1 segment. '
     .//            'Open CK for write access but don''t modify the '
     .//            'file.'                                            )

      CALL DAFOPW ( CK06T1, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Get mini-segment count.
C
      CALL DAFBFS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFFNA ( FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFGS  ( DESCR )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CKNM06 ( HANDLE, DESCR, NMINI )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CKMP06 ( HANDLE, DESCR, 1,      RATE, SUBTYP,  
     .              WINSIZ, NREC,  IVLBDS, LSTEPC )
      CALL CHCKXC ( .FALSE., ' ', OK )


      PKTSIZ = C06PS1

      DO I = 1, NREC

         CALL CKGR06 ( HANDLE, DESCR, 1, I, RECORD )
         CALL CHCKXC ( .FALSE., ' ', OK )
C
C        Check the record. The time tag is the first element.
C        We expect exact matches for all elements.
C
         NAME = 'Time tag *'
         CALL REPMI ( NAME, '*', I, NAME )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSD ( NAME, RECORD(1), '=', SCLKDP(I), 0.D0, OK )

         NAME = 'Subtype code *'
         CALL REPMI ( NAME, '*', I, NAME )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSD ( NAME, RECORD(2),         '=', 
     .                       DBLE( SUBTPS(1) ), 0.D0, OK )

         NAME = 'Clock rate *'
         CALL REPMI ( NAME, '*', I, NAME )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSD ( NAME, RECORD(3), '=', RATES(1),  0.D0, OK )

C
C        Check the record data.
C
         NAME = 'Quaternion *'
         CALL REPMI ( NAME, '*', I, NAME )
         CALL CHCKXC ( .FALSE., ' ', OK )

         K = PKTSIZ*(I-1) + 1

         CALL CHCKAD ( NAME, RECORD(4), '=', 
     .                       PACKTS(K), PKTSIZ, 0.D0, OK )

      END DO

C
C     Close kernel.
C
      CALL CKCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )





C
C --- Case: ------------------------------------------------------
C
      CALL TCASE  ( 'Read from trivial subtype 1 segment using CKGP' )

      CALL FURNSH ( CK06T1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CKTOL = 0.D0
      TOL   = VTIGHT

      DO I = 1, NREC

         CALL CKGP ( XINST, SCLKDP(I), CKTOL, 
     .               XREF,  CMAT,      CLKOUT, FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )
C
C        Make sure data were found.
C
         NAME = 'FOUND *'
         CALL REPMI ( NAME, '*', I, NAME )
         CALL CHCKXC ( .FALSE., ' ', OK )
         
         CALL CHCKSL ( NAME, FOUND, .TRUE., OK )

C
C        Check the output time.
C
         NAME = 'CLKOUT *'
         CALL REPMI ( NAME, '*', I, NAME )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSD ( NAME, CLKOUT, '=', SCLKDP(I), 0.D0, OK )

C
C        Check the C-matrix. Some round-off will occur here.
C
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



C*****************************************************************
C*
C*     Tests involving segment containing mini-segments having
C*     varied attributes
C*
C*****************************************************************

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Test CKW06: create segment containing ' 
     .//           'mini-segments of all subtypes.'        )

C
C     Each mini-segment will have a gap at the end.
C
      XINST     = -1000
      XAVFLG    = .TRUE.
      XREF      = 'GALACTIC'
      CKFRAM    = 'IAU_EARTH'
      NINTVL    = 4
      SEGID     = 'Subtype 1 test segment'
      SELLST    = .TRUE.
      SEGNO     = 1
      N         = 0
 
C
C     Below, we'll use different clock rates for each mini-segment. But
C     where we can,we'll adjust the angular velocity and quaternion
C     derivatives for the Hermite types so that the angular velocities
C     match when converted to radians/s.
C     
C     Where applicable, angular velocity will be scaled by the
C     corresponding rate. This will allow recovery of the original
C     angular velocity after conversion to radians/s.
C    
    
C
C     Pointer into packet array.
C
      PKTPTR    = 1
C
C     Mini-segment 1:
C      
      MNSGNO    = 1
      BEGREC    = 1
      NPKTS(1)  = 11
      SUBTPS(1) = C06TP0
      PKTSIZ    = C06PS0
      PSIZES(1) = PKTSIZ
      DEGRES(1) = 3
      RATES(1)  = 1.D-3

      CALL T_GENC06 ( SEGNO,    MNSGNO, BEGREC, NPKTS(1), XREF,
     .                RATES(1), SCLKDP, QUATS,  AVVS,     OK   ) 
      CALL CHCKXC ( .FALSE., ' ', OK )


      XIVBDS(1) = SCLKDP(1)

      FIRST     = XIVBDS(1)

C
C     Create quaternion derivatives from quaternions and avvs.
C     Pack quaternions and their derivatives into the PACKTS array.
C
      CALL CLEARD ( 3, DAVV )

      DO I = 1, NPKTS(1)         
C
C        We're not using the derivative of angular velocity for
C        this subtype.
C
         CALL T_XSUBTP ( QUATS(0,I), AVVS(1,I),  DAVV,  
     .                   SUBTPS(1),  PKTSIZ,     PACKTS(PKTPTR) )
         
         PKTPTR = PKTPTR + PKTSIZ
 
      END DO
      
C
C     Mini-segment 2:
C      
      MNSGNO    = 2
      BEGREC    = BEGREC + NPKTS(1)
      NPKTS(2)  = 12
      SUBTPS(2) = C06TP1
      PKTSIZ    = C06PS1
      PSIZES(2) = PKTSIZ
      DEGRES(2) = 7
      RATES(2)  = 2.D-3

      CALL T_GENC06 ( SEGNO,    MNSGNO, BEGREC, NPKTS(2), XREF,
     .                RATES(2), EPOCHS, QUATS,  AVVS,     OK ) 
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     No AVV scaling is needed because the vector is
C     already considered to have units of radians/s.
C
      CALL MOVED ( EPOCHS, NPKTS(2), SCLKDP(PKTPTR) )

      XIVBDS(2) = SCLKDP( BEGREC )

C
C     This mini-segment has subtype 1; we can just 
C     transfer the quaternions into the packet array.
C
      CALL MOVED ( QUATS, 4*NPKTS(2), PACKTS(PKTPTR) )

      PKTPTR = PKTPTR + NPKTS(2)*PKTSIZ


C
C     Mini-segment 3:
C      
      MNSGNO    = 3
      BEGREC    = BEGREC + NPKTS(2)
      NPKTS(3)  = 40
      SUBTPS(3) = C06TP2
      PKTSIZ    = C06PS2
      PSIZES(3) = PKTSIZ
      DEGRES(3) = 23
      RATES(3)  = 3.D-3

      CALL T_GENC06 ( SEGNO,    MNSGNO, BEGREC, NPKTS(3), XREF,
     .                RATES(3), EPOCHS, QUATS,  AVVS,     OK ) 
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL MOVED ( EPOCHS, NPKTS(3), SCLKDP(BEGREC) )

      XIVBDS(3) = SCLKDP( BEGREC )

C
C     Create quaternion derivatives from quaternions and avvs.
C     Pack quaternions and their derivatives, as well as the
C     avvs and their derivatives, into the PACKTS array.
C
      DO I = 1, NPKTS(3)         
C
C        We need to generate a derivative of the AVV. We'll
C        compute a 2-sided derivative if one's available;
C        otherwise we'll use a 1-sided derivative.
C
         IF ( I .EQ. 1 ) THEN
            
            DELTA = ( SCLKDP(2) - SCLKDP(1) ) * RATES(3)

            CALL VLCOM (  1.D0/DELTA, AVVS(1,I+1),
     .                   -1.D0/DELTA, AVVS(1,I), DAVV )
            

         ELSE IF ( I .LT. NPKTS(3) ) THEN

            DELTA = ( SCLKDP(I+1) - SCLKDP(I) ) * RATES(3) / 2
           
            CALL VLCOM (  1.D0/DELTA, AVVS(1,I+1),
     .                   -1.D0/DELTA, AVVS(1,I-1), DAVV )
            
         ELSE

            DELTA = ( SCLKDP(I) - SCLKDP(I-1) ) * RATES(3)

            CALL VLCOM (  1.D0/DELTA, AVVS(1,I  ),
     .                   -1.D0/DELTA, AVVS(1,I-1), DAVV )

         END IF
 
         CALL T_XSUBTP ( QUATS(0,I),  AVVS(1,I), DAVV,  
     .                   SUBTPS(3),   PKTSIZ,    PACKTS(PKTPTR) )

         PKTPTR = PKTPTR + PKTSIZ

      END DO


C
C     Mini-segment 4:
C      
      MNSGNO    = 4
      BEGREC    = BEGREC + NPKTS(3)
      NPKTS(4)  = 50
      SUBTPS(4) = C06TP3
      PKTSIZ    = C06PS3
      PSIZES(4) = PKTSIZ
      DEGRES(4) = 23
      RATES(4)  = 4.D-3

      CALL T_GENC06 ( SEGNO,    MNSGNO, BEGREC, NPKTS(4), XREF,
     .                RATES(4), EPOCHS, QUATS,  AVVS,     OK ) 
      CALL CHCKXC ( .FALSE., ' ', OK )


      XIVBDS(4) = EPOCHS( 1        )
      XIVBDS(5) = EPOCHS( NPKTS(4) ) + 1.D0
      LAST      = XIVBDS(5) 

      CALL MOVED ( EPOCHS, NPKTS(4), SCLKDP(BEGREC) )     

C
C     Copy both quaternions and avvs into the packet array.
C
      DO I = 1, NPKTS(4)

         CALL MOVED( QUATS(0,I), 4, PACKTS(PKTPTR) )
         PKTPTR = PKTPTR + 4

         CALL MOVED( AVVS(1,I),  3, PACKTS(PKTPTR) )
         PKTPTR = PKTPTR + 3

      END DO
             
C
C     Open new C-kernel.
C
      IF ( EXISTS(CK06MU) ) THEN
         CALL DELFIL( CK06MU )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF

      CALL CKOPN ( CK06MU, CK06MU, 0, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Write segment.
C
      CALL CKW06 ( HANDLE,
     .             XINST,
     .             XREF,
     .             XAVFLG,
     .             FIRST,
     .             LAST,
     .             SEGID,
     .             NINTVL,
     .             NPKTS,
     .             SUBTPS,
     .             DEGRES,
     .             PACKTS,
     .             RATES,
     .             SCLKDP,
     .             XIVBDS,
     .             SELLST           )


C
C     Close kernel.
C
      CALL CKCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

       

C*****************************************************************
C*
C*     CKNM06, CKMP06, CKGR06 Non-error cases:
C*
C*****************************************************************

C
C --- Case: ------------------------------------------------------
C
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

C
C     Check the mini-segment count:
C
      CALL CKNM06 ( HANDLE, DESCR, NMINI )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NMINI', NMINI, '=', 4, 0, OK )
      
C
C     Check the mini-segment parameters:
C
      BEGREC = 1
      PKTPTR = 1

      DO I = 1, NMINI

         CALL CKMP06 ( HANDLE, DESCR,  I,    RATE,
     .                 SUBTYP, WINSIZ, NREC, IVLBDS, LSTEPC ) 

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

C
C        Check the packets.
C
         PKTSIZ = PSIZES(I)

         DO J = 1, NPKTS(I)

            CALL CKGR06 ( HANDLE, DESCR, I, J, RECORD )
            CALL CHCKXC ( .FALSE., ' ', OK )

            K = PKTPTR + (J-1)*PKTSIZ


C
C           Check the record's time tag.
C
            NAME = 'Mini-segment *; record time tag *'
            CALL REPMI ( NAME, '*', I, NAME )
            CALL CHCKXC ( .FALSE., ' ', OK )
            CALL REPMI ( NAME, '*', J, NAME )
            CALL CHCKXC ( .FALSE., ' ', OK )

            XT   = SCLKDP(BEGREC + J - 1)

            CALL CHCKSD ( NAME, RECORD(1), '=', XT, 0.D0, OK )
C
C           Check the subtype and rate.
C
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


C
C           Check the data portion of the record.
C
            NAME = 'Mini-segment *; packet *'
            CALL REPMI ( NAME, '*', I, NAME )
            CALL CHCKXC ( .FALSE., ' ', OK )
            CALL REPMI ( NAME, '*', J, NAME )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKAD ( NAME, RECORD(4), '=', 
     .                          PACKTS(K), PKTSIZ, 0.D0, OK )    
            
         END DO

C
C        Update pointers.
C
         BEGREC = BEGREC + NPKTS(I)
         PKTPTR = PKTPTR + NPKTS(I)*PKTSIZ

      END DO


      CALL DAFCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


       



C*****************************************************************
C*
C*     CKE06 Non-error cases:
C*
C*****************************************************************



C
C --- Case: ------------------------------------------------------
C
      CALL TCASE  ( 'Look up and check data from CK06MU. Look ups ' 
     .//            'occur at time tags. Check first mini-segment.' )

      CALL FURNSH ( CK06MU )
      CALL CHCKXC ( .FALSE., ' ', OK )

      SEGNO = 1

      MNSGNO    = 1
      BEGREC    = 1
      PKTSIZ    = C06PS0

C
C     Generate test data again.
C
      CALL T_GENC06 ( SEGNO,    MNSGNO, BEGREC, NPKTS(1), XREF,
     .                RATES(1), SCLKDP, QUATS,  AVVS,     OK ) 
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Check data from the first mini-segment.
C
      PKTPTR = 1

      DO I = 1, NPKTS(1)

         J = BEGREC - 1 + I

         CALL CKGPAV ( XINST, SCLKDP(J), CKTOL, 
     .                 XREF,  CMAT,      AV,    CLKOUT, FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

C
C        Generate expected values.
C
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
C
C        For this case, we cannot expect high accuracy.
C        We're comparing angular velocity computed from
C        interpolated quaternion derivatives to the original
C        value.
C
        CALL CHCKAD ( NAME, AV, '~~/', XAV, 3, MED, OK )

      END DO      

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE  ( 'Look up and check data from CK06MU. Look ups ' 
     .//            'occur at time tags. Check second mini-segment.' )

 

C
C     Check data from the second mini-segment.
C
      MNSGNO    = 2
      BEGREC    = BEGREC + NPKTS(1)
      PKTSIZ    = C06PS1

C
C     Generate test data again.
C
      CALL T_GENC06 ( SEGNO,    MNSGNO, BEGREC, NPKTS(2), XREF,
     .                RATES(2), EPOCHS, QUATS,  AVVS,     OK ) 
      CALL CHCKXC ( .FALSE., ' ', OK )

      PKTPTR = NPKTS(1)*PSIZES(1) + 1

      DO I = 1, NPKTS(2)

         CALL CKGPAV ( XINST, EPOCHS(I), CKTOL, 
     .                 XREF,  CMAT,      AV,    CLKOUT, FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

C
C        Generate expected values.
C
         K = PKTPTR + (I-1)*PKTSIZ 

         CALL MOVED ( PACKTS(K), 4, XQ )

         CALL Q2M ( XQ, XCMAT )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL VEQU ( AVVS(1,I), XAV )
C
C        Scale the expected AV to be compatible with
C        AV derived from the quaternions.
C
C         CALL VSCLIP ( 1/RATES(2), XAV )


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
C
C        For this case, we cannot expect high accuracy.
C        We're comparing angular velocity computed from
C        interpolated quaternion derivatives to the original
C        value.
C
         CALL CHCKAD ( NAME, AV, '~~/', XAV, 3, MED, OK )
 
      END DO
      

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE  ( 'Look up and check data from CK06MU. Look ups ' 
     .//            'occur at time tags. Check third mini-segment.' )
 
C
C     Check data from the third mini-segment.
C

      MNSGNO    = 3
      BEGREC    = BEGREC + NPKTS(2)
      PKTSIZ    = C06PS2

C
C     Generate test data again.
C
      CALL T_GENC06 ( SEGNO,    MNSGNO, BEGREC, NPKTS(3), XREF,
     .                RATES(3), EPOCHS, QUATS,  AVVS,     OK ) 
      CALL CHCKXC ( .FALSE., ' ', OK )

 
      PKTPTR = PKTPTR + NPKTS(2)*PSIZES(2)

      DO I = 1, NPKTS(3)

         CALL CKGPAV ( XINST, EPOCHS(I), CKTOL, 
     .                 XREF,  CMAT,      AV,    CLKOUT, FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

C
C        Generate expected values.
C
         K = PKTPTR + (I-1)*PKTSIZ 

         CALL MOVED ( PACKTS(K), 4, XQ )

         CALL Q2M ( XQ, XCMAT )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL VEQU ( AVVS(1,I), XAV )
C
C        Scale the expected AV to be compatible with
C        AV derived from the quaternions.
C
C         CALL VSCLIP ( 1/RATES(3), XAV )


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
 

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE  ( 'Look up and check data from CK06MU. Look ups ' 
     .//            'occur at time tags. Check fourth mini-segment.' )

C
C     Check data from the fourth mini-segment.
C

      MNSGNO    = 4
      BEGREC    = BEGREC + NPKTS(3)
      PKTSIZ    = C06PS3

      CALL T_GENC06 ( SEGNO,    MNSGNO, BEGREC, NPKTS(4), XREF,
     .                RATES(4), EPOCHS, QUATS,  AVVS,     OK ) 
      CALL CHCKXC ( .FALSE., ' ', OK )

      PKTPTR = 1

      DO I = 1, 3
         PKTPTR = PKTPTR + NPKTS(I)*PSIZES(I)
      END DO


      DO I = 1, NPKTS(4)

         CALL CKGPAV ( XINST, EPOCHS(I), CKTOL,  XREF,
     .                 CMAT,  AV,        CLKOUT, FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

C
C        Generate expected values.
C
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

C
C        Check C-matrix.
C
         CALL CHCKAD ( NAME, CMAT, '~~/', XCMAT, 9, VTIGHT, OK )

         NAME = 'Mini-segment *;  * AVV'
         CALL REPMI ( NAME, '*', 4, NAME )
         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL REPMI ( NAME, '*', I, NAME )
         CALL CHCKXC ( .FALSE., ' ', OK )
C
C        Check AV.
C
         CALL CHCKAD ( NAME, AV, '~~/', XAV, 3, VTIGHT, OK )
        
      END DO


      CALL UNLOAD ( CK06MU )
      CALL CHCKXC ( .FALSE., ' ', OK )

       
C*****************************************************************
C*
C*     CKR06 Error cases:
C*
C*****************************************************************

C
C     Bad mini-segment subtype test:
C
C
C --- Case: ------------------------------------------------------
C
      TITLE = 'Create a trivial segment with an invalid subtype. Try '
     .//      'to read from this segment.'

      CALL TCASE ( TITLE )

      SEGNO  = 1
      MNSGNO = 1
      BEGREC = 1
      N      = 201
      XREF   = 'MARSIAU'

      CALL T_GENC06 ( SEGNO, MNSGNO, BEGREC, N,    XREF,
     .                1.D0,  SCLKDP, PACKTS, AVVS, OK )
 
      CALL CHCKXC ( .FALSE., ' ', OK )

      XINST     = -1000

C
C     Set the AV flag to FALSE to set up next test.
C
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


C
C     Open new C-kernel.
C
      IF ( EXISTS(CK06E) ) THEN
         CALL DELFIL(CK06E)
         CALL CHCKXC( .FALSE., ' ', OK )
      END IF

      CALL CKOPN( CK06E, 'Type 06 CK error file', 4, HANDLE )
      CALL CHCKXC( .FALSE., ' ', OK )

C
C     Write segment.
C
      CALL T_CKW06 ( HANDLE,
     .               XINST,
     .               XREF,
     .               XAVFLG,
     .               FIRST,
     .               LAST,
     .               SEGID,
     .               NINTVL,
     .               NPKTS,
     .               SUBTPS,
     .               DEGRES,
     .               PACKTS,
     .               RATES,
     .               SCLKDP,
     .               XIVBDS,
     .               SELLST )

C
C     Close kernel.
C
      CALL CKCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Load kernel for read access.
C
      CALL FURNSH ( CK06E )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Try a lookup.
C
      CKTOL = 0.D0
      CALL CKGP ( XINST, SCLKDP(1), CKTOL, 
     .            XREF,  CMAT,      CLKOUT, FOUND )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDSUBTYPE)', OK )

      
      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )

C
C     Unload kernel.
C
      CALL UNLOAD ( CK06E )
      CALL CHCKXC ( .FALSE., ' ', OK )



C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Try to read record with AV.' )

C
C     Use the CK from the previous test case.
C
      CALL DAFOPR ( CK06E, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFBFS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFFNA ( FOUND  )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFGS  ( DESCR  )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CKR06 ( HANDLE, DESCR, XIVBDS(1), 0.D0, .TRUE.,
     .             RECORD, FOUND                           )

      CALL CHCKXC ( .TRUE., 'SPICE(NOAVDATA)', OK )



C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Bad segment data type' )
                   

      CALL DAFUS ( DESCR, ND, NI, DC, IC )
      IC(3) = -3
      
      CALL DAFPS ( ND, NI, DC, IC, DESCR )


      CALL CKR06 ( HANDLE, DESCR, XIVBDS(1), 0.D0, .FALSE.,
     .             RECORD, FOUND                           )

      CALL CHCKXC ( .TRUE., 'SPICE(WRONGCKTYPE)', OK )


C
C     Close kernel.
C
      CALL DAFCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )
      

C
C --- Case: ------------------------------------------------------
C
      TITLE = 'Create a trivial segment with subtype 0 and a '
     .//      'quaternion sign error. Try to read from this segment.'

      CALL TCASE ( TITLE )

      SEGNO  = 1
      MNSGNO = 1
      BEGREC = 1
      N      = 201
      XREF   = 'MARSIAU'
      SUBTYP = C06TP0
      PKTSIZ = C06PS0

      CALL T_GENC06 ( SEGNO, MNSGNO, BEGREC, N,    XREF,
     .                1.D0,  SCLKDP, QUATS,  AVVS, OK )
 
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO I = 1, N

         PKTPTR = 1 + (I-1)*PKTSIZ

         CALL T_XSUBTP ( QUATS(0,I), AVVS(1,I), DAVV,  
     .                   SUBTYP,     PKTSIZ,    PACKTS(PKTPTR) )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        Create error in quaternion sequence at packet index 6:
C        
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


C
C     Open new C-kernel.
C
      IF ( EXISTS(CK06E) ) THEN
         CALL DELFIL(CK06E)
         CALL CHCKXC( .FALSE., ' ', OK )
      END IF

      CALL CKOPN( CK06E, 'Type 06 CK error file', 4, HANDLE )
      CALL CHCKXC( .FALSE., ' ', OK )

C
C     Write segment.
C
      CALL T_CKW06 ( HANDLE,
     .               XINST,
     .               XREF,
     .               XAVFLG,
     .               FIRST,
     .               LAST,
     .               SEGID,
     .               NINTVL,
     .               NPKTS,
     .               SUBTPS,
     .               DEGRES,
     .               PACKTS,
     .               RATES,
     .               SCLKDP,
     .               XIVBDS,
     .               SELLST )

C
C     Close kernel.
C
      CALL CKCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Load kernel for read access.
C
      CALL FURNSH ( CK06E )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Try a lookup.
C
      CKTOL = 0.D0
      CALL CKGP ( XINST, SCLKDP(6), CKTOL, 
     .            XREF,  CMAT,      CLKOUT, FOUND )
      CALL CHCKXC ( .TRUE., 'SPICE(BADQUATSIGN)', OK )

      
      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )

C
C     Unload kernel.
C
      CALL UNLOAD ( CK06E )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C --- Case: ------------------------------------------------------
C
      TITLE = 'Create a trivial segment with subtype 2 and a '
     .//      'quaternion sign error. Try to read from this segment.'

      CALL TCASE ( TITLE )

      SEGNO  = 1
      MNSGNO = 1
      BEGREC = 1
      N      = 201
      XREF   = 'MARSIAU'
      SUBTYP = C06TP2
      PKTSIZ = C06PS2

      CALL T_GENC06 ( SEGNO, MNSGNO, BEGREC, N,    XREF,
     .                1.D0,  SCLKDP, QUATS,  AVVS, OK )
 
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO I = 1, N

         PKTPTR = 1 + (I-1)*PKTSIZ

         CALL T_XSUBTP ( QUATS(0,I), AVVS(1,I), DAVV,  
     .                   SUBTYP,     PKTSIZ,    PACKTS(PKTPTR) )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        Create error in quaternion sequence at packet index 7:
C        
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


C
C     Open new C-kernel.
C
      IF ( EXISTS(CK06E) ) THEN
         CALL DELFIL(CK06E)
         CALL CHCKXC( .FALSE., ' ', OK )
      END IF

      CALL CKOPN( CK06E, 'Type 06 CK error file', 4, HANDLE )
      CALL CHCKXC( .FALSE., ' ', OK )

C
C     Write segment.
C
      CALL T_CKW06 ( HANDLE,
     .               XINST,
     .               XREF,
     .               XAVFLG,
     .               FIRST,
     .               LAST,
     .               SEGID,
     .               NINTVL,
     .               NPKTS,
     .               SUBTPS,
     .               DEGRES,
     .               PACKTS,
     .               RATES,
     .               SCLKDP,
     .               XIVBDS,
     .               SELLST )

C
C     Close kernel.
C
      CALL CKCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Load kernel for read access.
C
      CALL FURNSH ( CK06E )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Try a lookup.
C
      CKTOL = 0.D0
      CALL CKGP ( XINST, SCLKDP(6), CKTOL, 
     .            XREF,  CMAT,      CLKOUT, FOUND )
      CALL CHCKXC ( .TRUE., 'SPICE(BADQUATSIGN)', OK )

      
      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )

C
C     Unload kernel.
C
      CALL UNLOAD ( CK06E )
      CALL CHCKXC ( .FALSE., ' ', OK )



C
C --- Case: ------------------------------------------------------
C
      TITLE = 'Create a trivial segment with subtype 2 and a '
     .//      'window size equivalent to 2 mod 4. Try to read '
     .//      'from this segment.'

      CALL TCASE ( TITLE )

      SEGNO  = 1
      MNSGNO = 1
      BEGREC = 1
      N      = 201
      XREF   = 'MARSIAU'
      SUBTYP = C06TP2
      PKTSIZ = C06PS2

      CALL T_GENC06 ( SEGNO, MNSGNO, BEGREC, N,    XREF,
     .                1.D0,  SCLKDP, QUATS,  AVVS, OK )
 
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO I = 1, N

         PKTPTR = 1 + (I-1)*PKTSIZ

         CALL T_XSUBTP ( QUATS(0,I), AVVS(1,I), DAVV,  
     .                   SUBTYP,     PKTSIZ,    PACKTS(PKTPTR) )
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


C
C     Open new C-kernel.
C
      IF ( EXISTS(CK06E) ) THEN
         CALL DELFIL(CK06E)
         CALL CHCKXC( .FALSE., ' ', OK )
      END IF

      CALL CKOPN( CK06E, 'Type 06 CK error file', 4, HANDLE )
      CALL CHCKXC( .FALSE., ' ', OK )

C
C     Write segment.
C
      CALL T_CKW06 ( HANDLE,
     .               XINST,
     .               XREF,
     .               XAVFLG,
     .               FIRST,
     .               LAST,
     .               SEGID,
     .               NINTVL,
     .               NPKTS,
     .               SUBTPS,
     .               DEGRES,
     .               PACKTS,
     .               RATES,
     .               SCLKDP,
     .               XIVBDS,
     .               SELLST )

C
C     Close kernel.
C
      CALL CKCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Load kernel for read access.
C
      CALL FURNSH ( CK06E )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Try a lookup.
C
      CKTOL = 0.D0
      CALL CKGP ( XINST, SCLKDP(6), CKTOL, 
     .            XREF,  CMAT,      CLKOUT, FOUND )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDVALUE)', OK )

      
      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )

C
C     Unload kernel.
C
      CALL UNLOAD ( CK06E )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C --- Case: ------------------------------------------------------
C
      TITLE = 'Create a trivial segment with subtype 1 and an '
     .//      'even window size. Try to read from this segment.'

      CALL TCASE ( TITLE )

      CALL DAFOPW ( CK06E, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      SUBTYP = C06TP1
      PKTSIZ = C06PS1

      CALL T_GENC06 ( SEGNO, MNSGNO, BEGREC, N,    XREF,
     .                1.D0,  SCLKDP, QUATS,  AVVS, OK )
 
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO I = 1, N

         PKTPTR = 1 + (I-1)*PKTSIZ

         CALL T_XSUBTP ( QUATS(0,I), AVVS(1,I), DAVV,  
     .                   SUBTYP,     PKTSIZ,    PACKTS(PKTPTR) )
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

C
C     Write segment.
C
      CALL T_CKW06 ( HANDLE,
     .               XINST,
     .               XREF,
     .               XAVFLG,
     .               FIRST,
     .               LAST,
     .               SEGID,
     .               NINTVL,
     .               NPKTS,
     .               SUBTPS,
     .               DEGRES,
     .               PACKTS,
     .               RATES,
     .               SCLKDP,
     .               XIVBDS,
     .               SELLST )

C
C     Close kernel.
C
      CALL CKCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Load kernel for read access.
C
      CALL FURNSH ( CK06E )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Try a lookup.
C
      CKTOL = 0.D0
      CALL CKGP ( -2000, SCLKDP(6), CKTOL, 
     .            XREF,  CMAT,      CLKOUT, FOUND )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDVALUE)', OK )

      
      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )

C
C     Unload kernel.
C
      CALL UNLOAD ( CK06E )
      CALL CHCKXC ( .FALSE., ' ', OK )

      
C
C --- Case: ------------------------------------------------------
C
      TITLE = 'Create a trivial segment with subtype 1 and '
     .//      'window size too small. Try to read from this segment.'

      CALL TCASE ( TITLE )

      CALL DAFOPW ( CK06E, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      SUBTYP = C06TP1
      PKTSIZ = C06PS1

      CALL T_GENC06 ( SEGNO, MNSGNO, BEGREC, N,    XREF,
     .                1.D0,  SCLKDP, QUATS,  AVVS, OK )
 
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO I = 1, N

         PKTPTR = 1 + (I-1)*PKTSIZ

         CALL T_XSUBTP ( QUATS(0,I), AVVS(1,I), DAVV,  
     .                   SUBTYP,     PKTSIZ,    PACKTS(PKTPTR) )
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

C
C     Write segment.
C
      CALL T_CKW06 ( HANDLE,
     .               XINST,
     .               XREF,
     .               XAVFLG,
     .               FIRST,
     .               LAST,
     .               SEGID,
     .               NINTVL,
     .               NPKTS,
     .               SUBTPS,
     .               DEGRES,
     .               PACKTS,
     .               RATES,
     .               SCLKDP,
     .               XIVBDS,
     .               SELLST )

C
C     Close kernel.
C
      CALL CKCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Load kernel for read access.
C
      CALL FURNSH ( CK06E )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Try a lookup.
C
      CKTOL = 0.D0
      CALL CKGP ( -2000, SCLKDP(6), CKTOL, 
     .            XREF,  CMAT,      CLKOUT, FOUND )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDVALUE)', OK )

      
      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )

C
C     Unload kernel.
C
      CALL UNLOAD ( CK06E )
      CALL CHCKXC ( .FALSE., ' ', OK )

      
      
C
C --- Case: ------------------------------------------------------
C
      TITLE = 'Create a trivial segment with subtype 1 and '
     .//      'window size too large. Try to read from this segment.'

      CALL TCASE ( TITLE )

      CALL DAFOPW ( CK06E, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      SUBTYP = C06TP1
      PKTSIZ = C06PS1

      CALL T_GENC06 ( SEGNO, MNSGNO, BEGREC, N,    XREF,
     .                1.D0,  SCLKDP, QUATS,  AVVS, OK )
 
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO I = 1, N

         PKTPTR = 1 + (I-1)*PKTSIZ

         CALL T_XSUBTP ( QUATS(0,I), AVVS(1,I), DAVV,  
     .                   SUBTYP,     PKTSIZ,    PACKTS(PKTPTR) )
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

C
C     Write segment.
C
      CALL T_CKW06 ( HANDLE,
     .               XINST,
     .               XREF,
     .               XAVFLG,
     .               FIRST,
     .               LAST,
     .               SEGID,
     .               NINTVL,
     .               NPKTS,
     .               SUBTPS,
     .               DEGRES,
     .               PACKTS,
     .               RATES,
     .               SCLKDP,
     .               XIVBDS,
     .               SELLST )

C
C     Close kernel.
C
      CALL CKCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Load kernel for read access.
C
      CALL FURNSH ( CK06E )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Try a lookup.
C
      CKTOL = 0.D0
      CALL CKGP ( -2000, SCLKDP(6), CKTOL, 
     .            XREF,  CMAT,      CLKOUT, FOUND )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDVALUE)', OK )

      
      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )

C
C     Unload kernel.
C
      CALL UNLOAD ( CK06E )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C --- Case: ------------------------------------------------------
C

C
C     This is a "non-error" case: we want to make sure that
C     quaternion inversion is handled for subtype 1. The 
C     result of interpolation should be the same as if all
C     the quaternions had compatible signs.
C

      TITLE = 'Create a trivial segment with subtype 1 and a '
     .//      'quaternion sign error. Try to read from this segment.'

      CALL TCASE ( TITLE )

      SEGNO  = 1
      MNSGNO = 1
      BEGREC = 1
      N      = 201
      XREF   = 'MARSIAU'
      SUBTYP = C06TP1
      PKTSIZ = C06PS1

      CALL T_GENC06 ( SEGNO, MNSGNO, BEGREC, N,    XREF,
     .                1.D0,  SCLKDP, QUATS,  AVVS, OK )
 
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO I = 1, N

         PKTPTR = 1 + (I-1)*PKTSIZ

         CALL T_XSUBTP ( QUATS(0,I), AVVS(1,I), DAVV,  
     .                   SUBTYP,     PKTSIZ,    PACKTS(PKTPTR) )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        Create error in quaternion sequence at every even-
C        indexed quaternion.
C        
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


C
C     Open new C-kernel.
C
      IF ( EXISTS(CK06E) ) THEN
         CALL DELFIL(CK06E)
         CALL CHCKXC( .FALSE., ' ', OK )
      END IF

      CALL CKOPN( CK06E, 'Type 06 CK error file', 4, HANDLE )
      CALL CHCKXC( .FALSE., ' ', OK )

C
C     Write segment. We can use the SPICELIB type 6 writer 
C     since it's supposed to allow the quaternion mess we've
C     created when the subtype is either 1 or 3.
C
      CALL CKW06 ( HANDLE,
     .             XINST,
     .             XREF,
     .             XAVFLG,
     .             FIRST,
     .             LAST,
     .             SEGID,
     .             NINTVL,
     .             NPKTS,
     .             SUBTPS,
     .             DEGRES,
     .             PACKTS,
     .             RATES,
     .             SCLKDP,
     .             XIVBDS,
     .             SELLST )
      CALL CHCKXC( .FALSE., ' ', OK )

C
C     Create a second segment for instrument -2000. This one
C     has a normal quaternion sequence.
C
      CALL T_GENC06 ( SEGNO, MNSGNO, BEGREC, N,    XREF,
     .                1.D0,  SCLKDP, QUATS,  AVVS, OK )
 
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO I = 1, N

         PKTPTR = 1 + (I-1)*PKTSIZ

         CALL T_XSUBTP ( QUATS(0,I), AVVS(1,I), DAVV,  
     .                   SUBTYP,     PKTSIZ,    PACKTS(PKTPTR) )
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

      CALL CKW06 ( HANDLE,
     .             XINST,
     .             XREF,
     .             XAVFLG,
     .             FIRST,
     .             LAST,
     .             SEGID,
     .             NINTVL,
     .             NPKTS,
     .             SUBTPS,
     .             DEGRES,
     .             PACKTS,
     .             RATES,
     .             SCLKDP,
     .             XIVBDS,
     .             SELLST )
      CALL CHCKXC( .FALSE., ' ', OK )

C
C     Close kernel.
C
      CALL CKCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Load kernel for read access.
C
      CALL FURNSH ( CK06E )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Try lookups for each instrument.
C
      DO I = 1, N-1

         T = ( SCLKDP(I) + SCLKDP(I+1) ) / 2

         CKTOL = 0.D0
         CALL CKGPAV ( -2000, T,     CKTOL, 
     .                 XREF,  XCMAT, XAV,   CLKOUT, FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL CHCKSL ( 'FOUND 1', FOUND, .TRUE., OK )


         CALL CKGPAV ( -1000, T,    CKTOL, 
     .                 XREF,  CMAT, AV,   CLKOUT, FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL CHCKSL ( 'FOUND 2', FOUND, .TRUE., OK )


         CALL CHCKAD ( 'CMAT', CMAT, '~~/', XCMAT, 9, VTIGHT, OK )
         CALL CHCKAD ( 'CMAT', AV,   '~~/', XAV,   3, VTIGHT, OK )

      END DO
C
C     Unload kernel.
C
      CALL UNLOAD ( CK06E )
      CALL CHCKXC ( .FALSE., ' ', OK )



C
C --- Case: ------------------------------------------------------
C

C
C     This is a "non-error" case: same deal as before, this time
C     for subtype 3.
C

      TITLE = 'Create a trivial segment with subtype 3 and a '
     .//      'quaternion sign error. Try to read from this segment.'

      CALL TCASE ( TITLE )

      SEGNO  = 1
      MNSGNO = 1
      BEGREC = 1
      N      = 201
      XREF   = 'MARSIAU'
      SUBTYP = C06TP3
      PKTSIZ = C06PS3

      CALL T_GENC06 ( SEGNO, MNSGNO, BEGREC, N,    XREF,
     .                1.D0,  SCLKDP, QUATS,  AVVS, OK )
 
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO I = 1, N

         PKTPTR = 1 + (I-1)*PKTSIZ

         CALL T_XSUBTP ( QUATS(0,I), AVVS(1,I), DAVV,  
     .                   SUBTYP,     PKTSIZ,    PACKTS(PKTPTR) )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        Create error in quaternion sequence at every even-
C        indexed quaternion.
C        
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


C
C     Open new C-kernel.
C
      IF ( EXISTS(CK06E) ) THEN
         CALL DELFIL(CK06E)
         CALL CHCKXC( .FALSE., ' ', OK )
      END IF

      CALL CKOPN( CK06E, 'Type 06 CK error file', 4, HANDLE )
      CALL CHCKXC( .FALSE., ' ', OK )

C
C     Write segment. We can use the SPICELIB type 6 writer 
C     since it's supposed to allow the quaternion mess we've
C     created when the subtype is either 1 or 3.
C
      CALL CKW06 ( HANDLE,
     .             XINST,
     .             XREF,
     .             XAVFLG,
     .             FIRST,
     .             LAST,
     .             SEGID,
     .             NINTVL,
     .             NPKTS,
     .             SUBTPS,
     .             DEGRES,
     .             PACKTS,
     .             RATES,
     .             SCLKDP,
     .             XIVBDS,
     .             SELLST )
      CALL CHCKXC( .FALSE., ' ', OK )

C
C     Create a second segment for instrument -2000. This one
C     has a normal quaternion sequence.
C
      CALL T_GENC06 ( SEGNO, MNSGNO, BEGREC, N,    XREF,
     .                1.D0,  SCLKDP, QUATS,  AVVS, OK )
 
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO I = 1, N

         PKTPTR = 1 + (I-1)*PKTSIZ

         CALL T_XSUBTP ( QUATS(0,I), AVVS(1,I), DAVV,  
     .                   SUBTYP,     PKTSIZ,    PACKTS(PKTPTR) )
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

      CALL CKW06 ( HANDLE,
     .             XINST,
     .             XREF,
     .             XAVFLG,
     .             FIRST,
     .             LAST,
     .             SEGID,
     .             NINTVL,
     .             NPKTS,
     .             SUBTPS,
     .             DEGRES,
     .             PACKTS,
     .             RATES,
     .             SCLKDP,
     .             XIVBDS,
     .             SELLST )
      CALL CHCKXC( .FALSE., ' ', OK )

C
C     Close kernel.
C
      CALL CKCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Load kernel for read access.
C
      CALL FURNSH ( CK06E )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Try lookups for each instrument.
C
      DO I = 1, N-1

         T = ( SCLKDP(I) + SCLKDP(I+1) ) / 2

         CKTOL = 0.D0
         CALL CKGPAV ( -2000, T,     CKTOL, 
     .                 XREF,  XCMAT, XAV,   CLKOUT, FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL CHCKSL ( 'FOUND 1', FOUND, .TRUE., OK )


         CALL CKGPAV ( -1000, T,    CKTOL, 
     .                 XREF,  CMAT, AV,   CLKOUT, FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL CHCKSL ( 'FOUND 2', FOUND, .TRUE., OK )


         CALL CHCKAD ( 'CMAT', CMAT, '~~/', XCMAT, 9, VTIGHT, OK )
         CALL CHCKAD ( 'CMAT', AV,   '~~/', XAV,   3, VTIGHT, OK )

      END DO


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'CKR06 error case: negative tolerance.' )

C
C     We can use the CK from the previous test case.
C

C
C     Unload kernel; open for DAF read access.
C
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


C
C     Unload kernel.
C
      CALL DAFCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      

C*****************************************************************
C*
C*     CKR06 Non-error cases:
C*
C*****************************************************************

C
C     Use CK from previous tests to examine gap and non-zero
C     tolerance logic in CKR06.
C     
C     These tests use CK06MU. To start out, restore the arrays
C     of parameters used to create this file.
C

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE  ( 'Tolerance is positive; lookup time precedes '
     .//            'segment start by less than the tolerance. '
     .//            'Populate data arrays for further tests.'    )

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

            CALL CKMP06 ( HANDLE,    DESCR,  I,        RATES(I),
     .                    SUBTPS(I), WINSIZ, NPKTS(I), IVLBDS,
     .                    LSTEPC                                )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END DO

      END IF

      CALL DAFCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL FURNSH ( CK06MU )
      CALL CHCKXC ( .FALSE., ' ', OK )

      


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE  ( 'Tolerance is positive; lookup time precedes '
     .//            'segment start by less than the tolerance.'   )


      XINST     = -1000
      XREF      = 'GALACTIC'
      SEGNO     = 1

C
C     Re-generate data for the first mini-segment.
C
      MNSGNO    = 1
      BEGREC    = 1
      PKTSIZ    = NPKTS(1)

      N         = NPKTS(1)

      CALL T_GENC06 ( SEGNO,    MNSGNO, BEGREC, N,    XREF,
     .                RATES(1), EPOCHS, QUATS,  AVVS, OK ) 
      CALL CHCKXC ( .FALSE., ' ', OK )
 

      CKTOL = 0.5D0
      T     = EPOCHS( 1 ) - CKTOL + 1.D-6

      T = EPOCHS(1)

      FIRST = EPOCHS(1)
      LAST  = EPOCHS(N) + 1.D0


      CALL CKGPAV ( XINST, T,    CKTOL, 
     .              XREF,  CMAT, AV,   CLKOUT, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

C
C     The data should be those for the first epoch.
C
      CALL CHCKSD ( 'CLKOUT', CLKOUT, '=', EPOCHS(1), 0.D0, OK )


      CALL VEQU ( AVVS(1,1),  XAV )

      CALL CHCKAD ( 'AV', AV, '~~/', XAV, 3, MTIGHT, OK )

      CALL MOVED ( QUATS(0,1), 4, XQ )

      CALL Q2M  ( XQ, XCMAT )

      CALL CHCKAD ( 'CMAT', CMAT, '~~/', XCMAT, 9, VTIGHT, OK )


C
C     Convert expected AV to quaternion derivative and back.
C     The purpose of this test is to quantify the error made
C     by these conversions; this error should dominate the
C     error in the AV test above.
C
      CALL CLEARD ( 4,           AVQ    )
      CALL VSCL   ( -0.5D0, AV,  AVQ(1) )
      CALL QXQ    ( XQ,     AVQ, XDQ    )
      CALL QDQ2AV ( XQ,     XDQ, AV     )

      CALL CHCKAD ( 'AV 2', AV, '~~/', XAV, 3, MTIGHT, OK )

C
C     Make sure a second look-up produces the same results.
C 

      CALL CKGPAV ( XINST, T,    CKTOL, 
     .              XREF,  CMAT, AV,   CLKOUT, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

C
C     The data should be those for the first epoch.
C
      CALL CHCKSD ( 'CLKOUT', CLKOUT, '=', EPOCHS(1), 0.D0, OK )


      CALL VEQU ( AVVS(1,1),  XAV )

      CALL CHCKAD ( 'AV', AV, '~~/', XAV, 3, MTIGHT, OK )

      CALL MOVED ( QUATS(0,1), 4, XQ )

      CALL Q2M  ( XQ, XCMAT )

      CALL CHCKAD ( 'CMAT', CMAT, '~~/', XCMAT, 9, VTIGHT, OK )





C
C --- Case: ------------------------------------------------------
C
      CALL TCASE  ( 'Tolerance is positive; lookup time precedes '
     .//            'segment start by more than the tolerance.'   )

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


      CALL CKR06 ( HANDLE, DESCR, T, CKTOL, .FALSE.,
     .             RECORD, FOUND                           )      
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE  ( 'Tolerance is positive; lookup time exceeds '
     .//            'segment stop by more than the tolerance.'   )


      CKTOL = 0.5D0

      CALL DAFUS ( DESCR, ND, NI, DC, IC )

      T     = DC(2) + CKTOL + 1.D-6



      CALL CKR06 ( HANDLE, DESCR, T, CKTOL, .FALSE.,
     .             RECORD, FOUND                      )      
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )


      CALL DAFCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE  ( 'Tolerance is positive; lookup time exceeds '
     .//            'segment stop by less than the tolerance, but '
     .//            'is too far from the last epoch of the last '
     .//            'mini-segment for data to be found.'          )

      CALL FURNSH ( CK06MU )
      CALL CHCKXC ( .FALSE., ' ', OK )
 

C
C     Re-generate data for the last mini-segment.
C
      MNSGNO    = 4
      BEGREC    = NPKTS(1) + NPKTS(2) + NPKTS(3) + 1
      PKTSIZ    = C06PS3

      N         = NPKTS(4)

      CALL T_GENC06 ( SEGNO,    MNSGNO, BEGREC, N,    XREF,
     .                RATES(4), EPOCHS, QUATS,  AVVS, OK ) 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      FIRST = EPOCHS(1)
      LAST  = EPOCHS(N) + 1.D0

      CKTOL = 0.5D0
      T     = LAST  + CKTOL/2

      CALL CKGPAV ( XINST, T,    CKTOL, 
     .              XREF,  CMAT, AV,   CLKOUT, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE  ( 'Tolerance is positive; lookup time exceeds '
     .//            'segment stop by less than the tolerance, and '
     .//            'is close enough to the last epoch of the last '
     .//            'mini-segment for data to be found.'          )


C
C     Re-generate data for the last mini-segment.
C
      MNSGNO    = 4
      BEGREC    = NPKTS(1) + NPKTS(2) + NPKTS(3) + 1
      PKTSIZ    = C06PS3

      N         = NPKTS(4)


      CALL T_GENC06 ( SEGNO,    MNSGNO, BEGREC, N,    XREF,
     .                RATES(4), EPOCHS, QUATS,  AVVS, OK ) 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      FIRST = EPOCHS(1)
      LAST  = EPOCHS(N) + 1.D0

      CKTOL = 2.0D0
      T     = LAST  + CKTOL/2 - 1.D-6


      CALL CKGPAV ( XINST, T,    CKTOL, 
     .              XREF,  CMAT, AV,   CLKOUT, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
    

C
C     The data should be those for the last epoch.
C
      CALL CHCKSD ( 'CLKOUT', CLKOUT, '=', EPOCHS(N), 0.D0, OK )


      CALL VEQU ( AVVS(1,N),  XAV )

      CALL CHCKAD ( 'AV', AV, '~~/', XAV, 3, TIGHT, OK )

      CALL MOVED ( QUATS(0,N), 4, XQ )

      CALL Q2M  ( XQ, XCMAT )

      CALL CHCKAD ( 'CMAT', CMAT, '~~/', XCMAT, 9, TIGHT, OK )

C
C     Make sure a second look-up produces the same results.
C
      CALL CKGPAV ( XINST, T,    CKTOL, 
     .              XREF,  CMAT, AV,   CLKOUT, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
    

C
C     The data should be those for the last epoch.
C
      CALL CHCKSD ( 'CLKOUT', CLKOUT, '=', EPOCHS(N), 0.D0, OK )


      CALL VEQU ( AVVS(1,N),  XAV )

      CALL CHCKAD ( 'AV', AV, '~~/', XAV, 3, TIGHT, OK )

      CALL MOVED ( QUATS(0,N), 4, XQ )

      CALL Q2M  ( XQ, XCMAT )

      CALL CHCKAD ( 'CMAT', CMAT, '~~/', XCMAT, 9, TIGHT, OK )

       

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE  ( 'Lookup time is gap midpoint; tolerance is zero' )



      XINST     = -1000
      XAVFLG    = .TRUE.
      XREF      = 'GALACTIC'
      NINTVL    = 4
      SELLST    = .TRUE.
      SEGNO     = 1

C
C     Re-generate data for the third mini-segment.
C
      MNSGNO    = 3
      BEGREC    = NPKTS(1) + NPKTS(2) + 1
      PKTSIZ    = C06PS2

      N         = NPKTS(3)

      CALL T_GENC06 ( SEGNO,    MNSGNO, BEGREC, N,    XREF,
     .                RATES(3), EPOCHS, QUATS,  AVVS, OK ) 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Let T be the midpoint of the gap at the end of the
C     third mini-segment.
C     
      T     = EPOCHS( N ) + 0.5D0
      CKTOL = 0.D0


      CALL CKGPAV ( XINST, T,    CKTOL, 
     .              XREF,  CMAT, AV,   CLKOUT, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE  ( 'Lookup time is gap midpoint; tolerance is ' 
     .//            'positive but too small to find pointing.'  )

      T     = EPOCHS( N ) + 0.5D0
      CKTOL = 1.D-3


      CALL CKGPAV ( XINST, T,    CKTOL, 
     .              XREF,  CMAT, AV,   CLKOUT, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE  ( 'Lookup time is in gap and is before gap '
     .//            'midpoint; tolerance is ' 
     .//            '1/2 gap length. Mini-segment is not the last.' )

      CKTOL = 0.5D0

      T     = EPOCHS( N ) + 0.5D0 - 1.D-6

      CALL CKGPAV ( XINST, T,    CKTOL, 
     .              XREF,  CMAT, AV,   CLKOUT, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

C
C     The data should be those for the final epoch.
C
      CALL CHCKSD ( 'CLKOUT', CLKOUT, '=', EPOCHS(N), 0.D0, OK )


      CALL VEQU ( AVVS(1,N),  XAV )

      CALL CHCKAD ( 'AV', AV, '~~/', XAV, 3, TIGHT, OK )

      CALL MOVED ( QUATS(0,N), 4, XQ )

      CALL Q2M  ( XQ, XCMAT )

      CALL CHCKAD ( 'CMAT', CMAT, '~~/', XCMAT, 9, TIGHT, OK )

C
C     Make sure a second look-up produces the same results.
C
      CALL CKGPAV ( XINST, T,    CKTOL, 
     .              XREF,  CMAT, AV,   CLKOUT, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

C
C     The data should be those for the final epoch.
C
      CALL CHCKSD ( 'CLKOUT', CLKOUT, '=', EPOCHS(N), 0.D0, OK )


      CALL VEQU ( AVVS(1,N),  XAV )

      CALL CHCKAD ( 'AV', AV, '~~/', XAV, 3, TIGHT, OK )

      CALL MOVED ( QUATS(0,N), 4, XQ )

      CALL Q2M  ( XQ, XCMAT )

      CALL CHCKAD ( 'CMAT', CMAT, '~~/', XCMAT, 9, TIGHT, OK )


 

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE  ( 'Lookup time is in gap and is after gap '
     .//            'midpoint; tolerance is ' 
     .//            '1/2 gap length.'                          )

      CKTOL = 0.5D0

      T     = EPOCHS( N ) + 0.5D0 + 1.D-6

      CALL CKGPAV ( XINST, T,    CKTOL, 
     .              XREF,  CMAT, AV,   CLKOUT, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

C
C     The data should be those for the first epoch of the
C     fourth mini-segment. Re-generate data for that mini-segment.
C
      MNSGNO    = 4
      BEGREC    = NPKTS(1) + NPKTS(2) + NPKTS(3) + 1
      PKTSIZ    = C06PS3

      N         = NPKTS(4)

      CALL T_GENC06 ( SEGNO,    MNSGNO, BEGREC, N,    XREF,
     .                RATES(4), EPOCHS, QUATS,  AVVS, OK ) 
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSD ( 'CLKOUT', CLKOUT, '=', EPOCHS(1), 0.D0, OK )


      CALL VEQU ( AVVS(1,1),  XAV )

      CALL CHCKAD ( 'AV', AV, '~~/', XAV, 3, TIGHT, OK )

      CALL MOVED ( QUATS(0,1), 4, XQ )

      CALL Q2M  ( XQ, XCMAT )

      CALL CHCKAD ( 'CMAT', CMAT, '~~/', XCMAT, 9, TIGHT, OK )


C
C     Make sure a second look-up produces the same results.
C
      CALL CKGPAV ( XINST, T,    CKTOL, 
     .              XREF,  CMAT, AV,   CLKOUT, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

C
C     The data should be those for the first epoch of the
C     fourth mini-segment. Re-generate data for that mini-segment.
C
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

 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE  ( 'Lookup time is in gap and is before gap '
     .//            'midpoint in last mini-segment; tolerance is ' 
     .//            '1/2 gap length.'                             )

C
C     The data should be those for the last epoch of the
C     fourth mini-segment. Re-generate data for that mini-segment.
C
      MNSGNO    = 4
      BEGREC    = NPKTS(1) + NPKTS(2) + NPKTS(3) + 1
      PKTSIZ    = C06PS3

      N         = NPKTS(4)

      CALL T_GENC06 ( SEGNO,    MNSGNO, BEGREC, N,    XREF,
     .                RATES(4), EPOCHS, QUATS,  AVVS, OK ) 
      CALL CHCKXC ( .FALSE., ' ', OK )


      T = EPOCHS(N) + 0.5D0 - 1.D-6

      CALL CKGPAV ( XINST, T,    CKTOL, 
     .              XREF,  CMAT, AV,   CLKOUT, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )


      CALL CHCKSD ( 'CLKOUT', CLKOUT, '=', EPOCHS(N), 0.D0, OK )


      CALL VEQU ( AVVS(1,N),  XAV )

      CALL CHCKAD ( 'AV', AV, '~~/', XAV, 3, TIGHT, OK )

      CALL MOVED ( QUATS(0,N), 4, XQ )

      CALL Q2M  ( XQ, XCMAT )

      CALL CHCKAD ( 'CMAT', CMAT, '~~/', XCMAT, 9, TIGHT, OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE  ( 'Lookup time is in gap and is after gap '
     .//            'midpoint in last mini-segment; tolerance is ' 
     .//            '1/2 gap length.'                             )

C
C     The data should be those for the last epoch of the
C     fourth mini-segment. Re-generate data for that mini-segment.
C
      MNSGNO    = 4
      BEGREC    = NPKTS(1) + NPKTS(2) + NPKTS(3) + 1
      PKTSIZ    = C06PS3

      N         = NPKTS(4)

      CALL T_GENC06 ( SEGNO,    MNSGNO, BEGREC, N,    XREF,
     .                RATES(4), EPOCHS, QUATS,  AVVS, OK ) 
      CALL CHCKXC ( .FALSE., ' ', OK )


      T = EPOCHS(N) + 0.5D0 + 1.D-6

      CALL CKGPAV ( XINST, T,    CKTOL, 
     .              XREF,  CMAT, AV,   CLKOUT, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )
      

      CALL UNLOAD ( CK06MU )
      CALL CHCKXC ( .FALSE., ' ', OK )
      


C*****************************************************************
C*****************************************************************
C*
C*     CKR06: create CKs for search tests 
C*
C*****************************************************************
C*****************************************************************
  
C$$$

C     
C --- Case: ------------------------------------------------------
C
      CALL TCASE  ( 'Create CK for packet selection logic tests.' )

C
C     Create CKs for testing.
C
C     The first kernel contains three mini-segments, all for the
C     same instrument. The packet counts in these mini-segments
C     are such that the first one contains no packet directories,
C     the second contains fewer than one directory buffer full,
C     and the third contains too many to be buffered.
C
C     The selection flag indicates "select last."
C     
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

C
C     Create time tags and packets.
C      
      BEGREC = 1
      N      = 1
      PKTPTR = 1

      DO MNSGNO = 1, NINTVL
C
C        Record the index of the first non-pad time tag of
C        the current mini-segment.
C
         DATIDX(MNSGNO) = N + PAD

C
C        Record the corresponding start index in the packet array.
C        
         PKTBEG(MNSGNO) = PKTPTR

C
C        Generate time tags and packet data for the current
C        mini-segment.
C        
         RATE   = RATES (MNSGNO)         
         SUBTYP = SUBTPS(MNSGNO)

         CALL T_GENCSM ( SEGNO, MNSGNO, BEGREC, NPKTS(MNSGNO), XREF,
     .                   RATE,  SUBTYP, EPOCHS, QUATS,  AVVS,  OK   ) 
         CALL CHCKXC ( .FALSE., ' ', OK )
C
C        The start time of the current interval is the epoch at 
C        PAD positions later than the start epoch.
C
         XIVBDS( MNSGNO ) = EPOCHS( 1 + PAD )

C
C        Setting up the data arrays for CKW06 is simple: we simply
C        append the data for each mini-segment.
C
         CALL MOVED ( EPOCHS, NPKTS(MNSGNO),   SCLKDP(N) )

         CALL MOVED ( AVVS,   3*NPKTS(MNSGNO), XAVVS (1,N) )
         CALL MOVED ( QUATS,  4*NPKTS(MNSGNO), XQUATS(0,N) )

         DO J = 1, NPKTS(MNSGNO)
            CALL Q2M ( QUATS(0,J), XCMATS(1,N-1+J) )
         END DO

C
C        We're not using angular acceleration.
C         
         CALL CLEARD ( 3, DAVV )

         DO J = 1, NPKTS(MNSGNO)

            CALL T_XSUBTP ( QUATS(0,J),     AVVS(1,J),      DAVV, 
     .                      SUBTPS(MNSGNO), PSIZES(MNSGNO), RECORD )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL MOVED ( RECORD, PSIZES(MNSGNO), PACKTS(PKTPTR) )

            PKTPTR = PKTPTR + PSIZES(MNSGNO)

         END DO
 
C
C        Adjust the starting record number of the next mini-segment
C        to account for padding.
C        
         BEGREC = (BEGREC-1) + NPKTS(MNSGNO) - 2*PAD

C
C        Update the count of the records in the segment.
C
         N      = N + NPKTS(MNSGNO)

      END DO

C
C     The stop time of the last interval is the epoch at 
C     PAD positions before the final epoch.
C
      XIVBDS( NINTVL+1 ) = SCLKDP( N - PAD )

C
C     Let the segment bounds coincide with the start and stop
C     times of the first and last mini-segments.
C
      FIRST = XIVBDS(1)
      LAST  = XIVBDS(NINTVL+1)

C
C     Create the CK.
C
      IF ( EXISTS(CK) ) THEN
         CALL DELFIL(CK)
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF

      CALL CKOPN ( CK, CK, 0, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CKW06 ( HANDLE,
     .             XINST,
     .             XREF,
     .             XAVFLG,
     .             FIRST,
     .             LAST,
     .             SEGID,
     .             NINTVL,
     .             NPKTS,
     .             SUBTPS,
     .             DEGRES,
     .             PACKTS,
     .             RATES,
     .             SCLKDP,
     .             XIVBDS,
     .             SELLST           )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CKCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

       
C*****************************************************************
C*
C*     CKR06 reduction of order tests
C*
C*****************************************************************

C
C     These tests examine the behavior of the CKR06 interpolation
C     window construction algorithm for request times near mini-segment
C     interpolation interval boundaries.
C
C     We'll work with the third mini-segment of CK06B0. We start by
C     looking up the relevant mini-segment attributes:
C
C        - Interpolation interval bounds
C        - Window size
C        - Packet count
C

C
C --- Case: ------------------------------------------------------
C
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
      CALL CKMP06 ( HANDLE, DESCR, MNSGNO, RATE,  SUBTYP, 
     .              WINSIZ, NREC,  IVLBDS, LSTEPC        )
      CALL CHCKXC ( .FALSE., ' ', OK )

   
      PKTSIZ = PKTSZS ( SUBTYP )

C
C     Determine the left hand pad size.
C
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

C
C     See whether CKR06 returns the expected window for 
C     request times near the left boundary of the interval.
C
      DO I = ( 1 + WINSIZ/2 ), PAD+2, -1
C
C --- Case: ------------------------------------------------------
C
         TITLE = 'CK06B0: Reduction of order on left side. '
     .   //      'Request time is between tags at indices # '
     .   //      'and #.'

         CALL REPMI  ( TITLE, '#', I-1, TITLE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL REPMI  ( TITLE, '#', I,   TITLE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL TCASE  ( TITLE )

C
C        Construct the expected record for a request time
C        between time tags at indices I-1 and I.
C
C
C        N is the expected packet count.
C
         N = I + (WINSIZ/2) - 1

         DO J = 1, N

            CALL CKGR06 ( HANDLE, DESCR, MNSGNO, J, RECORD )
            CALL CHCKXC ( .FALSE., ' ', OK )
         
            K = 4 + (J-1)*PKTSIZ + 1       
C
C           Insert packet data into the expected record.
C
            CALL MOVED ( RECORD(4), PKTSIZ, XREC(K) )
            CALL CHCKXC ( .FALSE., ' ', OK )
C
C           Insert the time tag into the expected record.
C
            K = 4 + N*PKTSIZ + J

            XREC(K) = RECORD(1)
   
         END DO

C
C        Get the time tags that bracket the request time we
C        want to use.
C
         K  = 4 + N*PKTSIZ + (I-1)
         
         T0 = XREC(K  )
         T1 = XREC(K+1)

C
C        Generate the request time.
C
         T  = ( T0 + T1 ) / 2

C
C        Insert the initial parameters into the expected record.
C
         XREC(1) = T
         XREC(2) = SUBTYP
         XREC(3) = N
         XREC(4) = RATE

C
C        Look up the record for request time SCLKDP.
C
         CALL CKR06 ( HANDLE, DESCR, T, 
     .                0.D0,  .TRUE., RECORD, FOUND )
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




C
C@@@
C

C
C
C --- Case: ------------------------------------------------------
C
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

C
C     See whether CKR06 returns the expected window for 
C     request times near the right boundary of the interval.
C
      DO I = ( NREC - WINSIZ/2 + 1 ),   NREC-PAD
C
C --- Case: ------------------------------------------------------
C
C
C        Let M represent the loop iteration count. This is the
C        offset of I from the value preceding the loop start
C        value.
C         
         M = I - ( NREC - WINSIZ/2 ) 


         TITLE = 'CK06B0: Reduction of order on left side. '
     .   //      'Request time is between tags at indices # '
     .   //      'and #. Iteration = #.'

         CALL REPMI  ( TITLE, '#', I-1, TITLE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL REPMI  ( TITLE, '#', I,   TITLE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL REPMI  ( TITLE, '#', M,   TITLE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL TCASE  ( TITLE )


C
C        Construct the expected record for a request time
C        between time tags at indices I-1 and I.
C
C
C        N is the expected packet count.
C
         N = WINSIZ + 1 - M


         DO J = 1, N

            K = NREC - N + J

            CALL CKGR06 ( HANDLE, DESCR, MNSGNO, K, RECORD )
            CALL CHCKXC ( .FALSE., ' ', OK )
         
            K = 4 + (J-1)*PKTSIZ + 1       
C
C           Insert packet data into the expected record.
C
            CALL MOVED ( RECORD(4), PKTSIZ, XREC(K) )
            CALL CHCKXC ( .FALSE., ' ', OK )
C
C           Insert the time tag into the expected record.
C
            K = 4 + N*PKTSIZ + J

            XREC(K) = RECORD(1)
   
         END DO

C
C        Get the time tags that bracket the request time we
C        want to use. Note that for this case, the offset
C        is independent of J.
C
         K  = 4  + N*PKTSIZ  +  WINSIZ/2 
         
         T0 = XREC(K  )
         T1 = XREC(K+1)

C
C        Generate the request time.
C
         T  = ( T0 + T1 ) / 2

C
C        Insert the initial parameters into the expected record.
C
         XREC(1) = T
         XREC(2) = SUBTYP
         XREC(3) = N
         XREC(4) = RATE

C
C        Look up the record for request time SCLKDP.
C
         CALL CKR06 ( HANDLE, DESCR, T, 
     .                0.D0,  .TRUE., RECORD, FOUND )
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


 
C*****************************************************************
C*
C*     CKR06 packet selection logic tests
C*
C*****************************************************************

       
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE  ( 'Packet selection logic test: look up data at ' 
     .//            'time tags.'                                  )

      
      CK = CK06B0
      CALL FURNSH ( CK )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CKTOL = 0.D0

      BEGREC = 1
      N      = 1
      PKTPTR = 1

      DO MNSGNO = 1, NINTVL
C
C        Create the expected CMAT and AV values.
C
         RATE   = RATES (MNSGNO)         
         SUBTYP = SUBTPS(MNSGNO)

c         CALL MOVED ( XAVVS (1,N), 3*NPKTS(MNSGNO), AVVS )

         CALL CHCKXC ( .FALSE., ' ', OK )

         J      = DATIDX(MNSGNO)

C         N      = NPKTS (MNSGNO) - 2*PAD

         T      = SCLKDP(J)

         DO WHILE ( T .LT. XIVBDS(MNSGNO+1) )
 

            CALL VEQU  ( XAVVS  (1,J),    XAV   )
            CALL MOVED ( XCMATS(1,J), 9, XCMAT )

C
C           Look up pointing data.
C
            CALL CKGPAV ( XINST, T,    CKTOL, 
     .                    XREF,  CMAT, AV,    CLKOUT, FOUND )
            CALL CHCKXC ( .FALSE., ' ', OK )           

            IF ( .NOT. OK ) THEN
               CALL SIGERR( 'NO POINTING' )
               CALL CHCKXC ( .FALSE., ' ', OK )       
               STOP
            END IF

            CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

C
C           Check output time.
C
            NAME = 'Mini-segment *; CLKOUT no. *'
            CALL REPMI ( NAME, '*', MNSGNO, NAME )
            CALL CHCKXC ( .FALSE., ' ', OK )
            CALL REPMI ( NAME, '*', J, NAME )
            CALL CHCKXC ( .FALSE., ' ', OK )
            
            CALL CHCKSD ( NAME, CLKOUT, '~', T, 0.D0, OK )
            
C
C           Check C-matrix.
C

            NAME = 'Mini-segment *; CMAT no. *'
            CALL REPMI ( NAME, '*', MNSGNO, NAME )
            CALL CHCKXC ( .FALSE., ' ', OK )
            CALL REPMI ( NAME, '*', J, NAME )
            CALL CHCKXC ( .FALSE., ' ', OK )
            
            CALL CHCKAD ( NAME, CMAT, '~~/', XCMAT, 9, VTIGHT, OK )

C
C           Check AV.
C
            NAME = 'Mini-segment *; AV no. *'
            CALL REPMI ( NAME, '*', MNSGNO, NAME )
            CALL CHCKXC ( .FALSE., ' ', OK )
            CALL REPMI ( NAME, '*', J, NAME )
            CALL CHCKXC ( .FALSE., ' ', OK )
            
 
C
C           Check angular velocity.
C
C           For derived AV, use a looser tolerance.
C
            IF ( SUBTPS(MNSGNO) .EQ. C06TP1 ) THEN

               TOL = MTIGHT

C               TOL = MED

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
C
C        Adjust the starting record number of the next mini-segment
C        to account for padding.
C        
         BEGREC = (BEGREC-1) + NPKTS(MNSGNO) - 2*PAD

         N      = N + NPKTS (MNSGNO) 

      END DO


       

 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE  ( 'Packet selection logic test: look up data at ' 
     .//            'midpoints between time tags.'               )


      DO MNSGNO = 1, NINTVL
C
C        Create the expected CMAT and AV values.
C
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
C
C           Create expected CMAT and AV. This will only be 
C           an approximation.
C
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
 
C
C           Look up pointing data.
C
            CALL CKGPAV ( XINST, T, CKTOL, 
     .                    XREF,  CMAT,      AV, CLKOUT, FOUND )
            CALL CHCKXC ( .FALSE., ' ', OK )           

            IF ( .NOT. OK ) THEN
               CALL SIGERR( 'NO POINTING' )
               CALL CHCKXC ( .FALSE., ' ', OK )       
               STOP
            END IF

            CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

C
C           Check output time.
C
            NAME = 'Mini-segment *; CLKOUT no. *'
            CALL REPMI ( NAME, '*', MNSGNO, NAME )
            CALL CHCKXC ( .FALSE., ' ', OK )
            CALL REPMI ( NAME, '*', J, NAME )
            CALL CHCKXC ( .FALSE., ' ', OK )
            
            CALL CHCKSD ( NAME, CLKOUT, '~', T, 0.D0, OK )
            
C
C           Check C-matrix.
C
            CALL Q2M ( XQ, XCMAT )
            CALL CHCKXC ( .FALSE., ' ', OK )           

            NAME = 'Mini-segment *; CMAT no. *'
            CALL REPMI ( NAME, '*', MNSGNO, NAME )
            CALL CHCKXC ( .FALSE., ' ', OK )
            CALL REPMI ( NAME, '*', J, NAME )
            CALL CHCKXC ( .FALSE., ' ', OK )
            
C
C           Use a tight tolerance for CMAT midpoint comparisons.
C
            CALL CHCKAD ( NAME, CMAT, '~~/', XCMAT, 9, VTIGHT, OK ) 
C
C           Check angular velocity.
C
            NAME = 'Mini-segment *; AV no. *'
            CALL REPMI ( NAME, '*', MNSGNO, NAME )
            CALL CHCKXC ( .FALSE., ' ', OK )
            CALL REPMI ( NAME, '*', J, NAME )
            CALL CHCKXC ( .FALSE., ' ', OK )

C
C           Set the tolerance for the AV check.
C           
            IF ( SUBTPS(MNSGNO) .EQ. C06TP1 ) THEN
C
C              For derived AV, use a looser tolerance.
C
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

       
       
       


C*****************************************************************
C*
C*     CKR06 interval selection logic tests
C*
C*****************************************************************

C     
C     $$$
C

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE  ( 'Create CKs for interval selection logic tests.' )

C
C     Create CKs for testing.
C
C     The first kernel contains three segments for the three different
C     instruments. The interval counts in these segments
C     are such that the first one contains no interval directories,
C     the second contains fewer than one directory buffer full,
C     and the third contains too many to be buffered.
C
C     The selection flag for the first CK is set to indicate 
C     "select first."
C     
      CK = CK06B1

C
C     Create the CK.
C
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

C
C     Set the interval counts for the three segments.
C
      NIVALS(1)  = 98
      NIVALS(2)  = 398
      NIVALS(3)  = 10381

C
C     Create data for the segments and write them to the file.
C
      N      = 0
      SEGPTR = 0

      DO SEGNO = 1, NSEG

         SEGPTR = SEGPTR + N
C
C        Set the mini-segment attributes for the current segment.
C
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

C
C        Create time tags and packets for the current segment.
C      
         BEGREC = 1
         N      = 1
         PKTPTR = 1

         DO MNSGNO = 1, NINTVL
C
C           Record the index of the first non-pad time tag of
C           the current mini-segment.
C
            DATIDX(MNSGNO) = N + PAD

C
C           Record the corresponding start index in the packet array.
C        
            PKTBEG(MNSGNO) = PKTPTR

C
C           Generate time tags and packet data for the current
C           mini-segment.
C        
            RATE   = RATES (MNSGNO)         
            SUBTYP = SUBTPS(MNSGNO)

            CALL T_GENCSM ( SEGNO, MNSGNO, BEGREC, NPKTS(MNSGNO), XREF,
     .                      RATE,  SUBTYP, EPOCHS, QUATS,  AVVS,  OK   )
            CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C           The start time of the current interval is the epoch at 
C           PAD positions later than the start epoch.
C
            XIVBDS( MNSGNO ) = EPOCHS( 1 + PAD )

C
C           Setting up the data arrays for CKW06 is simple: we simply
C           append the data for each mini-segment.
C
            CALL MOVED ( EPOCHS,   NPKTS(MNSGNO), SCLKDP(N) )

            CALL MOVED ( EPOCHS,   NPKTS(MNSGNO), XEPOCH(SEGPTR+N)   )
            CALL MOVED ( AVVS,   3*NPKTS(MNSGNO), XAVVS (1,SEGPTR+N) )
            CALL MOVED ( QUATS,  4*NPKTS(MNSGNO), XQUATS(0,SEGPTR+N) )

            DO J = 1, NPKTS(MNSGNO)
               CALL Q2M ( QUATS(0,J), XCMATS(1,SEGPTR+N+J-1) )
            END DO

 
C
C           We're not using angular acceleration.
C         
            CALL CLEARD ( 3, DAVV )

            DO J = 1, NPKTS(MNSGNO)

               CALL T_XSUBTP ( QUATS(0,J),     AVVS(1,J),      DAVV, 
     .                         SUBTPS(MNSGNO), PSIZES(MNSGNO), RECORD )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL MOVED ( RECORD, PSIZES(MNSGNO), PACKTS(PKTPTR) )

               PKTPTR = PKTPTR + PSIZES(MNSGNO)

            END DO
 
C
C           Adjust the starting record number of the next mini-segment
C           to account for padding.
C        
            BEGREC = (BEGREC-1) + NPKTS(MNSGNO) - 2*PAD

C
C           Update the count of the records in the segment.
C
            N      = N + NPKTS(MNSGNO)

         END DO

C
C        The stop time of the last interval is the epoch at 
C        PAD positions before the final epoch.
C
         XIVBDS( NINTVL+1 ) = SCLKDP( N - PAD )

C
C        Let the segment bounds coincide with the start and stop
C        times of the first and last mini-segments.
C
         FIRST = XIVBDS(1)
         LAST  = XIVBDS(NINTVL+1)

         CALL CKW06 ( HANDLE,
     .                XINST,
     .                XREF,
     .                XAVFLG,
     .                FIRST,
     .                LAST,
     .                SEGID,
     .                NINTVL,
     .                NPKTS,
     .                SUBTPS,
     .                DEGRES,
     .                PACKTS,
     .                RATES,
     .                SCLKDP,
     .                XIVBDS,
     .                SELLST           )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

C
C     Close the kernel.
C
      CALL CKCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )
       
       

C     The selection flag for the second CK is set to indicate 
C     "select last." Other than that, the CK is identical to
C     the first.
C     
      CK = CK06B2

C
C     Create the CK.
C
      IF ( EXISTS(CK) ) THEN
         CALL DELFIL(CK)
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF

      CALL CKOPN ( CK, CK, 0, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

C      CALL TOSTDO ( 'Opened CK '//CK )

      NSEG      = 3

      DO I = 1, NSEG 
         XINSTS(I) = -1000 * I
      END DO
      
      XREFS(1)  = 'GALACTIC'
      XREFS(2)  = 'MARSIAU'
      XREFS(3)  = 'GALACTIC'
      CKFRAM    = 'IAU_EARTH'

      SELLST    = .TRUE.

C
C     Set the interval counts for the three segments.
C
      NIVALS(1)  = 98
      NIVALS(2)  = 398
      NIVALS(3)  = 10381

C
C     Create data for the segments and write them to the file.
C
      N      = 0
      SEGPTR = 0


      DO SEGNO = 1, NSEG

         SEGPTR = SEGPTR + N

C
C        Set the mini-segment attributes for the current segment.
C
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

C
C        Create time tags and packets for the current segment.
C      
         BEGREC = 1
         N      = 1
         PKTPTR = 1

         DO MNSGNO = 1, NINTVL
C
C           Record the index of the first non-pad time tag of
C           the current mini-segment.
C
            DATIDX(MNSGNO) = N + PAD

C
C           Record the corresponding start index in the packet array.
C        
            PKTBEG(MNSGNO) = PKTPTR

C
C           Generate time tags and packet data for the current
C           mini-segment.
C        
            RATE   = RATES (MNSGNO)         
            SUBTYP = SUBTPS(MNSGNO)

            CALL MOVED( XQUATS(0,SEGPTR+N), 4*NPKTS(MNSGNO), QUATS  )
            CALL MOVED( XAVVS (1,SEGPTR+N), 3*NPKTS(MNSGNO), AVVS   )
            CALL MOVED( XEPOCH(  SEGPTR+N),   NPKTS(MNSGNO), EPOCHS )

C
C           The start time of the current interval is the epoch at 
C           PAD positions later than the start epoch.
C
            XIVBDS( MNSGNO ) = EPOCHS( 1 + PAD )

C
C           Setting up the data arrays for CKW06 is simple: we simply
C           append the data for each mini-segment.
C
            CALL MOVED ( EPOCHS, NPKTS(MNSGNO), SCLKDP(N) )

C
C           We're not using angular acceleration.
C         
            CALL CLEARD ( 3, DAVV )

            DO J = 1, NPKTS(MNSGNO)

               CALL T_XSUBTP ( QUATS(0,J),     AVVS(1,J),      DAVV, 
     .                         SUBTPS(MNSGNO), PSIZES(MNSGNO), RECORD )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL MOVED ( RECORD, PSIZES(MNSGNO), PACKTS(PKTPTR) )

               PKTPTR = PKTPTR + PSIZES(MNSGNO)

            END DO
 
C
C           Adjust the starting record number of the next mini-segment
C           to account for padding.
C        
            BEGREC = (BEGREC-1) + NPKTS(MNSGNO) - 2*PAD

C
C           Update the count of the records in the segment.
C
            N      = N + NPKTS(MNSGNO)

         END DO

C
C        The stop time of the last interval is the epoch at 
C        PAD positions before the final epoch.
C
         XIVBDS( NINTVL+1 ) = SCLKDP( N - PAD )

C
C        Let the segment bounds coincide with the start and stop
C        times of the first and last mini-segments.
C
         FIRST = XIVBDS(1)
         LAST  = XIVBDS(NINTVL+1)

         CALL CKW06 ( HANDLE,
     .                XINST,
     .                XREF,
     .                XAVFLG,
     .                FIRST,
     .                LAST,
     .                SEGID,
     .                NINTVL,
     .                NPKTS,
     .                SUBTPS,
     .                DEGRES,
     .                PACKTS,
     .                RATES,
     .                SCLKDP,
     .                XIVBDS,
     .                SELLST           )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

C
C     Close the kernel.
C
      CALL CKCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


       
C
C --- Case: ------------------------------------------------------
C
C  ###
      
C
C     Perform tests on CK06B1.
C
      CK     = CK06B1
      SELLST = .FALSE.

      TITLE = 'Interval/Packet selection logic test:  ' 
     .//      'look up data at time tags within all '
     .//      'three segments of CK #.'

      CALL REPMC  ( TITLE, '#', CK, TITLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL TCASE  ( TITLE )

C
C     Load the test CK.
C
      CALL FURNSH ( CK )
      CALL CHCKXC ( .FALSE., ' ', OK )

      N      = 0
      SEGPTR = 0

      DO SEGNO = 1, NSEG

         SEGPTR = SEGPTR + N

C
C --- Case: ------------------------------------------------------
C

         TITLE = 'Interval/Packet selection logic test:  ' 
     .//         'look up data at time tags within '
     .//         'segment # of CK #.'

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


C
C        Create time tags and packets for the current segment.
C      
         BEGREC = 1
         N      = 1
         PKTPTR = 1

         DO MNSGNO = 1, NINTVL
C
C           Record the index of the first non-pad time tag of
C           the current mini-segment.
C
            DATIDX(MNSGNO) = N + PAD
C
C           Generate time tags and packet data for the current
C           mini-segment.
C        
            CALL MOVED ( XEPOCH(SEGPTR+N), NPKTS(MNSGNO), SCLKDP(N) )
 
C           Update the count of the records in the segment.
C
            N = N + NPKTS(MNSGNO)

         END DO

C
C        Check data for each interval.
C
         IF ( SEGNO .EQ. 3 ) THEN
C
C           SKIP can be reset to 40 to increase execution speed.
C
            SKIP = 1
         ELSE
            SKIP = 1
         END IF

         DO MNSGNO = 1, NINTVL, SKIP

            J = DATIDX(MNSGNO)

            IF ( ( .NOT. SELLST ) .AND. ( MNSGNO .GT. 1 ) ) THEN 
C
C              Skip the first time tag of each mini-segment but the
C              first, since the correct data will come from the
C              previous interval.
C         
               J = J + 1

            END IF

            JMAX = DATIDX(MNSGNO) - 1 + NPKTS(MNSGNO) - 2*PAD


            DO WHILE ( J .LE. JMAX )
            
               T = SCLKDP(J)

               CALL MOVED ( XAVVS (1,SEGPTR+J), 3, XAV )

               CALL MOVED ( XCMATS(1,SEGPTR+J), 9, XCMAT )
 
C
C              Look up pointing data.
C
               CKTOL = 0.D0

               CALL CKGPAV ( XINST, T,    CKTOL, 
     .                       XREF,  CMAT, AV, CLKOUT, FOUND )
               CALL CHCKXC ( .FALSE., ' ', OK )           

               IF ( .NOT. OK ) THEN
                  CALL SIGERR( 'NO POINTING' )
                  CALL CHCKXC ( .FALSE., ' ', OK )       
                  STOP
               END IF

               CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

C
C              Check output time.
C
               CALL CHCKSD ( 'CLKOUT', CLKOUT, '~', T, 0.D0, OK )

               IF ( .NOT. OK ) THEN
C
C                 Re-check with a more specific item name.
C
                  NAME = 'Mini-segment *; CLKOUT no. *'
                  CALL REPMI ( NAME, '*', MNSGNO, NAME )
                  CALL CHCKXC ( .FALSE., ' ', OK )
                  CALL REPMI ( NAME, '*', J, NAME )
                  CALL CHCKXC ( .FALSE., ' ', OK )
            
                  CALL CHCKSD ( NAME, CLKOUT, '~', T, 0.D0, OK )

               END IF
            
C
C              Check C-matrix.
C            
               CALL CHCKAD ( 'CMAT', CMAT, '~~/', XCMAT, 9, VTIGHT, OK )

               IF ( .NOT. OK ) THEN

                  NAME = 'Mini-segment *; CMAT no. *'
                  CALL REPMI ( NAME, '*', MNSGNO, NAME )
                  CALL CHCKXC ( .FALSE., ' ', OK )
                  CALL REPMI ( NAME, '*', J, NAME )
                  CALL CHCKXC ( .FALSE., ' ', OK )

                  CALL CHCKAD ( NAME, CMAT,  '~~/', 
     .                                XCMAT, 9,     VTIGHT, OK )
               END IF 

C
C              Check angular velocity.
C
C              For derived AV, use a looser tolerance.
C
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
C
C           End of tests for current interval.
C
         END DO
C
C        End of tests for current segment.
C
      END DO


      


C
C --- Case: ------------------------------------------------------
C
C
C     Perform tests using data lookups at midpoints
C     between consecutive time tags. This set of tests
C     is for segments using the "select first" option.
C
      DO SEGNO = 1, NSEG
C
C --- Case: ------------------------------------------------------
C

         TITLE = 'Interval/Packet selection logic test:  ' 
     .//         'look up data at midpoints between time tags within '
     .//         'segment # of CK #.'

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


C
C        Create time tags for the current segment.
C      
         BEGREC = 1
         N      = 1
         PKTPTR = 1

         DO MNSGNO = 1, NINTVL
C
C           Record the index of the first non-pad time tag of
C           the current mini-segment.
C
            DATIDX(MNSGNO) = N + PAD

            RATE   = RATES (MNSGNO)         
            SUBTYP = SUBTPS(MNSGNO)

            CALL T_GENTAG ( BEGREC, NPKTS(MNSGNO), EPOCHS )
            CALL CHCKXC ( .FALSE., ' ', OK )

C
C           The start time of the current interval is the epoch at 
C           PAD positions later than the start epoch.
C
            XIVBDS( MNSGNO ) = EPOCHS( 1 + PAD )
 
            CALL MOVED ( EPOCHS, NPKTS(MNSGNO), SCLKDP(N) )
 
C
C           Adjust the starting record number of the next mini-segment
C           to account for padding.
C        
            BEGREC = (BEGREC-1) + NPKTS(MNSGNO) - 2*PAD

C
C           Update the count of the records in the segment.
C
            N      = N + NPKTS(MNSGNO)

         END DO

C
C        Check data for each interval.
C
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

C              The following formula for ET looks strange, but it's
C              correct.
C
               ET = T * RATE
C
C              Generate expected results for ET.
C
               CALL SXFORM ( XREF, CKFRAM, ET, XFORM )
               CALL CHCKXC ( .FALSE., ' ', OK )       

               CALL XF2RAV ( XFORM, XCMAT, XAV )
C 
C              Look up pointing data.
C
               CKTOL = 0.D0
               CALL CKGPAV ( XINST, T,    CKTOL, 
     .                       XREF,  CMAT, AV, CLKOUT, FOUND )
               CALL CHCKXC ( .FALSE., ' ', OK )           

               IF ( .NOT. OK ) THEN
                  CALL SIGERR( 'NO POINTING' )
                  CALL CHCKXC ( .FALSE., ' ', OK )       
                  STOP
               END IF

               CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

C
C              Check output time.
C
               CALL CHCKSD ( 'CLKOUT', CLKOUT, '~', T, 0.D0, OK )

               IF ( .NOT. OK ) THEN
C
C                 Re-check with a more specific item name.
C
                  NAME = 'Mini-segment *; CLKOUT no. *'
                  CALL REPMI ( NAME, '*', MNSGNO, NAME )
                  CALL CHCKXC ( .FALSE., ' ', OK )
                  CALL REPMI ( NAME, '*', J, NAME )
                  CALL CHCKXC ( .FALSE., ' ', OK )
            
                  CALL CHCKSD ( NAME, CLKOUT, '~', T, 0.D0, OK )

               END IF
            
C
C              Check C-matrix.
C            
               CALL CHCKAD ( 'CMAT', CMAT, '~~/', XCMAT, 9, VTIGHT, OK )

               IF ( .NOT. OK ) THEN

                  NAME = 'Mini-segment *; CMAT no. *'
                  CALL REPMI ( NAME, '*', MNSGNO, NAME )
                  CALL CHCKXC ( .FALSE., ' ', OK )
                  CALL REPMI ( NAME, '*', J, NAME )
                  CALL CHCKXC ( .FALSE., ' ', OK )

                  CALL CHCKAD ( NAME, CMAT,  '~~/', 
     .                                XCMAT, 9,     VTIGHT, OK )

               END IF

 
C
C              Check angular velocity.
C
C              For derived AV, use a looser tolerance.
C
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
C
C           End of tests for current interval.
C
         END DO
C
C        End of tests for current segment.
C
      END DO

      CALL UNLOAD ( CK )
      CALL CHCKXC ( .FALSE., ' ', OK )
      
C
C --- Case: ------------------------------------------------------
C

C
C     Perform tests on CK06B2.
C
      CK     = CK06B2
      SELLST = .TRUE.

      TITLE = 'Interval/Packet selection logic test:  ' 
     .//      'look up data at time tags within all '
     .//      'three segments of CK #.'

      CALL REPMC  ( TITLE, '#', CK, TITLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL TCASE  ( TITLE )

C
C     Load the test CK.
C
      CALL FURNSH ( CK )
      CALL CHCKXC ( .FALSE., ' ', OK )

      N      = 0
      SEGPTR = 0

      DO SEGNO = 1, NSEG

         SEGPTR = SEGPTR + N
C
C --- Case: ------------------------------------------------------
C

         TITLE = 'Interval/Packet selection logic test:  ' 
     .//         'look up data at time tags within '
     .//         'segment # of CK #.'

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

C
C        Create time tags and packets for the current segment.
C      
         BEGREC = 1
         N      = 1
         PKTPTR = 1

         DO MNSGNO = 1, NINTVL
C
C           Record the index of the first non-pad time tag of
C           the current mini-segment.
C
            DATIDX(MNSGNO) = N + PAD

            CALL MOVED ( XEPOCH(SEGPTR+N), NPKTS(MNSGNO), SCLKDP(N) )
C
C           Update the count of the records in the segment.
C
            N = N + NPKTS(MNSGNO)

         END DO

C
C        The stop time of the last interval is the epoch at 
C        PAD positions before the final epoch.
C
         XIVBDS( NINTVL+1 ) = SCLKDP( N - PAD )

C
C        Let the segment bounds coincide with the start and stop
C        times of the first and last mini-segments.
C
         FIRST = XIVBDS(1)
         LAST  = XIVBDS(NINTVL+1)

C
C        Check data for each interval.
C
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
C
C              Skip the last time tag of each mini-segment but the
C              last, since the correct data will come from the
C              next interval.
C         
               JMAX = JMAX - 1

            END IF

C
C           In the test below, ".LT." is used for the "select last"
C           option.
C
            DO WHILE ( J .LT. JMAX )

               T = SCLKDP(J)
               
               CALL MOVED ( XQUATS(0,SEGPTR+J), 4, XQ  )
               CALL MOVED ( XAVVS (1,SEGPTR+J), 3, XAV )

               CALL Q2M ( XQ, XCMAT ) 
               CALL CHCKXC ( .FALSE., ' ', OK )           

C
C              Look up pointing data.
C
               CALL CKGPAV ( XINST, T, CKTOL, 
     .                       XREF,  CMAT,      AV, CLKOUT, FOUND )
               CALL CHCKXC ( .FALSE., ' ', OK )           

               IF ( .NOT. OK ) THEN
                  CALL SIGERR( 'NO POINTING' )
                  CALL CHCKXC ( .FALSE., ' ', OK )       
                  STOP
               END IF

               CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )


C
C              Check output time.
C
               CALL CHCKSD ( 'CLKOUT', CLKOUT, '~', T, 0.D0, OK )

               IF ( .NOT. OK ) THEN
C
C                 Re-check with a more specific item name.
C
                  NAME = 'Mini-segment *; CLKOUT no. *'
                  CALL REPMI ( NAME, '*', MNSGNO, NAME )
                  CALL CHCKXC ( .FALSE., ' ', OK )
                  CALL REPMI ( NAME, '*', J, NAME )
                  CALL CHCKXC ( .FALSE., ' ', OK )
            
                  CALL CHCKSD ( NAME, CLKOUT, '~', T, 0.D0, OK )

               END IF
            
C
C              Check C-matrix.
C            
               CALL CHCKAD ( 'CMAT', CMAT, '~~/', XCMAT, 9, VTIGHT, OK )

               IF ( .NOT. OK ) THEN

                  NAME = 'Mini-segment *; CMAT no. *'
                  CALL REPMI ( NAME, '*', MNSGNO, NAME )
                  CALL CHCKXC ( .FALSE., ' ', OK )
                  CALL REPMI ( NAME, '*', J, NAME )
                  CALL CHCKXC ( .FALSE., ' ', OK )

                  CALL CHCKAD ( NAME, CMAT,  '~~/', 
     .                                XCMAT, 9,     VTIGHT, OK )

               END IF

 
C
C              Check angular velocity.
C
C              For derived AV, use a looser tolerance.
C
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
C
C           End of tests for current interval.
C
         END DO
C
C        End of tests for current segment.
C
      END DO




C
C --- Case: ------------------------------------------------------
C
C
C     Perform tests using data lookups at midpoints
C     between consecutive time tags. This set of tests
C     is for segments using the "select last" option.
C
C     CK is CK06B2.
C

      DO SEGNO = 1, NSEG
C
C --- Case: ------------------------------------------------------
C

         TITLE = 'Interval/Packet selection logic test:  ' 
     .//         'look up data at midpoints between time tags within '
     .//         'segment # of CK #.'

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


C
C        Create time tags for the current segment.
C      
         BEGREC = 1
         N      = 1
         PKTPTR = 1

         DO MNSGNO = 1, NINTVL
C
C           Record the index of the first non-pad time tag of
C           the current mini-segment.
C
            DATIDX(MNSGNO) = N + PAD

            RATE   = RATES (MNSGNO)         
            SUBTYP = SUBTPS(MNSGNO)

            CALL T_GENTAG ( BEGREC, NPKTS(MNSGNO), EPOCHS )
            CALL CHCKXC ( .FALSE., ' ', OK )

C
C           The start time of the current interval is the epoch at 
C           PAD positions later than the start epoch.
C
            XIVBDS( MNSGNO ) = EPOCHS( 1 + PAD )
 
            CALL MOVED ( EPOCHS, NPKTS(MNSGNO), SCLKDP(N) )
 
C
C           Adjust the starting record number of the next mini-segment
C           to account for padding.
C        
            BEGREC = (BEGREC-1) + NPKTS(MNSGNO) - 2*PAD

C
C           Update the count of the records in the segment.
C
            N      = N + NPKTS(MNSGNO)

         END DO

C
C        The stop time of the last interval is the epoch at 
C        PAD positions before the final epoch.
C
         XIVBDS( NINTVL+1 ) = SCLKDP( N - PAD )

C
C        Let the segment bounds coincide with the start and stop
C        times of the first and last mini-segments.
C
         FIRST = XIVBDS(1)
         LAST  = XIVBDS(NINTVL+1)


C
C        Check data for each interval.
C
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

C              The following formula for ET looks strange, but it's
C              correct.
C
               ET = T * RATE
C
C              Generate expected results for ET.
C
               CALL SXFORM ( XREF, CKFRAM, ET, XFORM )
               CALL CHCKXC ( .FALSE., ' ', OK )       

               CALL XF2RAV ( XFORM, XCMAT, XAV )
C 
C              Look up pointing data.
C
               CALL CKGPAV ( XINST, T, CKTOL, 
     .                       XREF,  CMAT,      AV, CLKOUT, FOUND )
               CALL CHCKXC ( .FALSE., ' ', OK )           

               IF ( .NOT. OK ) THEN
                  CALL SIGERR( 'NO POINTING' )
                  CALL CHCKXC ( .FALSE., ' ', OK )       
                  STOP
               END IF

               CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

C
C              Check output time.
C
               CALL CHCKSD ( 'CLKOUT', CLKOUT, '~', T, 0.D0, OK )

               IF ( .NOT. OK ) THEN
C
C                 Re-check with a more specific item name.
C
                  NAME = 'Mini-segment *; CLKOUT no. *'
                  CALL REPMI ( NAME, '*', MNSGNO, NAME )
                  CALL CHCKXC ( .FALSE., ' ', OK )
                  CALL REPMI ( NAME, '*', J, NAME )
                  CALL CHCKXC ( .FALSE., ' ', OK )
            
                  CALL CHCKSD ( NAME, CLKOUT, '~', T, 0.D0, OK )

               END IF
            
C
C              Check C-matrix.
C            
               CALL CHCKAD ( 'CMAT', CMAT, '~~/', XCMAT, 9, VTIGHT, OK )

               IF ( .NOT. OK ) THEN

                  NAME = 'Mini-segment *; CMAT no. *'
                  CALL REPMI ( NAME, '*', MNSGNO, NAME )
                  CALL CHCKXC ( .FALSE., ' ', OK )
                  CALL REPMI ( NAME, '*', J, NAME )
                  CALL CHCKXC ( .FALSE., ' ', OK )

                  CALL CHCKAD ( NAME, CMAT,  '~~/', 
     .                                XCMAT, 9,     VTIGHT, OK )

               END IF

 
C
C              Check angular velocity.
C
C              For derived AV, use a looser tolerance.
C
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
C
C           End of tests for current interval.
C
         END DO
C
C        End of tests for current segment.
C
      END DO



      CALL UNLOAD ( CK )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C --- Case: ------------------------------------------------------
C
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
 
C
C     Close out the test family.
C
      CALL T_SUCCESS ( OK )
      RETURN 
      END







C*****************************************************************
C*****************************************************************
C*
C*
C*
C*
C*    T E S T   U T I L I T I E S
C*
C*
C*
C*
C*****************************************************************
C*****************************************************************



C*****************************************************************
C
C     T_GENC06
C
C*****************************************************************

C
C     Generate C-matrices for CK type 06 software testing.
C
C        Version 1.0.0 03-FEB-2014 (NJB)
C
C
C     Output AVV has units of radians/s.
C
      SUBROUTINE T_GENC06 ( SEGNO, MNSGNO, BEGREC, N,    FRAME,
     .                      RATE,  EPOCHS, QUATS,  AVVS, OK    )
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

C
C     SPICELIB functions
C
      DOUBLE PRECISION      DPR

      LOGICAL               EQSTR
      LOGICAL               RETURN

C
C     Local parameters
C      
      CHARACTER*(*)         IREF
      PARAMETER           ( IREF    = 'J2000' )

C
C     Local variables
C
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

C
C     We'll construct a sequence of C-matrices and angular velocities
C     representing rotations about a constant axis in the frame
C     designated by IREF. Those rotations and corresponding angular
C     velocities will be transformed to the requested output frame.
C
      SAMFRM = EQSTR( IREF, FRAME )

C
C     Below we define some parameters used for rotation angle
C     construction.
C
C     S is a scale factor for the segment number.
C      
      S = 2.5D9
C
C     M is a scale factor for the mini-segment number.
C
      M = 5.D4

C
C     AXIS is the rotation axis in the J2000 frame.
C
      CALL VPACK ( 0.D0, 0.D0, 1.D0, AXIS )

C
C     ANGLE1 is the base angle used to generate all of the rotation
C     angles. These angles are computed using the formula
C
C                                          S*SEGNO + M*MNSGNO + I 
C        ANGLE( SEGNO, MNSGNO, I ) = ANGLE1
C                                     


      ANGLE1 = DPR() ** ( 1.D0 / (S*SEGNO + M*MNSGNO + 100000) )

      DO I = 1, N

         J         = BEGREC - 1 + I

         EPOCHS(I) = DBLE(J) 

         ANGLE     = ANGLE1 ** (S*SEGNO + M*MNSGNO + J)

         CALL AXISAR ( AXIS, -ANGLE, CMAT )
C
C        Create an angular velocity vector that is roughly consistent
C        with the C-matrix sequence. We'll treat the angle as a
C        function of I and differentiate the angle formula with respect
C        to I. Note that
C
C           ANGLE = EXP( LOG(ANGLE1) * (S*SEGNO + M*MNSGNO + I) )
C
C        
         DANGL1 = ANGLE * LOG(ANGLE1) 
C
C        Scale the angular rate from radians/tick to radians/s.
C          
C        RATE has units of seconds/tick.
C
         DANGL1 = DANGL1 / RATE

         CALL VSCL ( DANGL1, AXIS, AVVS(1,I) )
C
C        Transform the C-matrix and AVV to the output frame
C        if necessary. The output frame is the new base frame.
C
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



C*****************************************************************
C
C     T_GENCSM ( Generate C-matrices from smooth data )
C
C*****************************************************************

C
C     Generate C-matrices for CK type 06 software testing.
C     Use smooth data: earth rotation from a test utility PCK.
C
C        Version 1.0.0 03-FEB-2014 (NJB)
C
      SUBROUTINE T_GENCSM ( SEGNO, MNSGNO, BEGREC, N,     FRAME,
     .                      RATE,  SUBTYP, EPOCHS, QUATS, AVVS, OK )
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
C
C     SPICELIB functions
C
      DOUBLE PRECISION      VDISTG
      INTEGER               TOUCHI
      LOGICAL               RETURN

C
C     Local parameters
C      
      CHARACTER*(*)         CKFRAM
      PARAMETER           ( CKFRAM = 'IAU_EARTH' )

      CHARACTER*(*)         PCK0
      PARAMETER           ( PCK0   = 'test.tpc' )

C
C     Local variables
C
      DOUBLE PRECISION      CMAT   ( 3, 3 )
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      QN     ( 0:3 )
      DOUBLE PRECISION      XFORM  ( 6, 6 )

      INTEGER               I
      INTEGER               J

      LOGICAL               PASS1

C
C     Saved variables
C
      SAVE                  PASS1

C
C     Initial values        
C
      DATA                  PASS1 / .TRUE. /


      IF ( RETURN() ) THEN
         RETURN
      END IF

C      CALL CHKIN ( 'T_GENCSM' )

      I = TOUCHI( MNSGNO )
      I = TOUCHI( SEGNO  )

C
C     Load a test PCK on the first pass.
C
      IF ( PASS1 ) THEN

         CALL TSTPCK ( PCK0, .TRUE., .FALSE. )
         CALL CHCKXC ( .FALSE., ' ', OK )

         PASS1 = .FALSE.

      END IF

C
C     We'll construct a sequence of C-matrices and angular velocities
C     for the orientation of the earth relative to the IREF frame.
C     Those rotations and corresponding angular velocities will be
C     transformed to the requested output frame.
C
      DO I = 1, N

         J         = BEGREC - 1 + I

         EPOCHS(I) = DBLE(J)

C
C        Convert SCLK to ET for SXFORM lookup. This is necessary
C        in order to derived angular velocity consistent with 
C        orientation. RATE has units of seconds/tick.
C
         ET        = EPOCHS(I) * RATE

C
C         Initial orientation is that of the earth relative 
C         to J2000.
C          
         CALL SXFORM ( FRAME, CKFRAM, ET, XFORM )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL XF2RAV ( XFORM, CMAT, AVVS(1,I) )
         CALL CHCKXC ( .FALSE., ' ', OK )


         CALL M2Q ( CMAT, QUATS(0,I) )
         CALL CHCKXC ( .FALSE., ' ', OK )


C
C        Adjust data to achieve better consistency, if necessary.
C
         IF ( I .GT. 1 ) THEN
            
            IF (      ( SUBTYP .EQ. C06TP0 ) 
     .           .OR. ( SUBTYP .EQ. C06TP2 ) ) THEN


               CALL VMINUG ( QUATS(0,I), 4, QN )

               IF (      VDISTG( QUATS(0,I), QUATS(0,I-1), 4 ) 
     .              .GT. VDISTG( QN,         QUATS(0,I-1), 4 )  ) THEN
C
C                 Replace the original quaternion with its negative.
C
                  CALL MOVED  ( QN, 4, QUATS(0,I) )

               END IF

            END IF

         END IF
 
      END DO

C      CALL CHKOUT ( 'T_GENCSM' )
      RETURN
      END 


 
C*****************************************************************
C
C     T_GENTAG (Generate time tags for T_GENCSM)
C
C*****************************************************************

C
C     Generate just the time tag sequence generated by T_GENCSM
C     for a given set of inputs.
C
C        Version 1.0.0 07-FEB-2014 (NJB)
C
      SUBROUTINE T_GENTAG ( BEGREC, N, EPOCHS )
      IMPLICIT NONE
      INCLUDE 'ck06.inc'

      INTEGER               BEGREC
      INTEGER               N
      DOUBLE PRECISION      EPOCHS ( * )
C
C     SPICELIB functions
C
      LOGICAL               RETURN

C
C     Local parameters
C      

C
C     Local variables
C
      INTEGER               I
      INTEGER               J


      IF ( RETURN() ) THEN
         RETURN
      END IF
 
C
C     We'll construct a sequence of C-matrices and angular velocities
C     for the orientation of the earth relative to the IREF frame.
C     Those rotations and corresponding angular velocities will be
C     transformed to the requested output frame.
C
      DO I = 1, N

         J         = BEGREC - 1 + I

         EPOCHS(I) = DBLE(J)
 
      END DO

      RETURN
      END 


 




C*****************************************************************
C
C     T_XSUBTP
C
C*****************************************************************

C
C     Transform record subtype. Input is always a quaternion
C     and avv; output is any other type.
C
C     For subtype 2, the caller must supply the derivative
C     with respect to ticks of the angular velocity.
C
      SUBROUTINE T_XSUBTP ( QUAT, AVV, DAVV, SUBTYP, SIZE, RECORD )
      IMPLICIT NONE
      
      INCLUDE 'ck06.inc'

      DOUBLE PRECISION      QUAT   ( 0:3 )
      DOUBLE PRECISION      AVV    ( 3 )
      DOUBLE PRECISION      DAVV   ( 3 )
      INTEGER               SUBTYP
      INTEGER               SIZE
      DOUBLE PRECISION      RECORD ( * )

C
C     SPICELIB functions
C
C      DOUBLE PRECISION      VDOTG
      LOGICAL               RETURN

C
C     Local variables
C
      DOUBLE PRECISION      AVQ ( 0 : 3 )
      DOUBLE PRECISION      DQ  ( 0 : 3 )


      IF ( RETURN() ) THEN
         RETURN
      END IF

C      CALL CHKIN ( 'T_XSUBTP' )

      CALL CLEARD ( SIZE, RECORD )
     
      IF ( SUBTYP .EQ. C06TP0 ) THEN
C
C        Create a quaternion derivative from a quaternion and avv. Pack
C        the quaternion and its derivative into the RECORD array.
C         
         CALL MOVED( QUAT, 4, RECORD )
C
C        From the header of QDQ2AV we have the equation:
C
C                             *
C           AV  =  Im ( -2 * Q  * DQ )                        (1)
C
C        Here AV can be viewed as a quaternion with real part
C        equal to 0. Then
C
C           DQ = -0.5 * Q * AV
C

         AVQ(0) = 0.D0

         CALL VSCL  ( -0.5D0, AVV, AVQ(1)    )         
         CALL QXQ   ( QUAT,   AVQ, DQ        )
         CALL MOVED ( DQ,     4,   RECORD(5) )
               

      ELSE IF ( SUBTYP .EQ. C06TP1 ) THEN
C
C        No transformation required.
C
         CALL MOVED ( QUAT, 4, RECORD )

      ELSE IF ( SUBTYP .EQ. C06TP2 ) THEN
C
C        Create a quaternion derivative from a quaternion and avv. Pack
C        the quaternion and its derivative into the RECORD array.
C         
         CALL MOVED( QUAT, 4, RECORD )
C
C        From the header of QDQ2AV we have the equation:
C
C                             *
C           AV  =  Im ( -2 * Q  * DQ )                        (1)
C
C        Here AV can be viewed as a quaternion with real part
C        equal to 0. Then
C
C           DQ = -0.5 * Q * AV
C
C
         AVQ(0) = 0.D0

         CALL VSCL  ( -0.5D0, AVV, AVQ(1)    )         
         CALL QXQ   ( QUAT,   AVQ, DQ        )
         CALL MOVED ( DQ,     4,   RECORD(5) )
C
C        Transfer the avv and its derivative.
C
         CALL VEQU ( AVV,  RECORD(9)  )
         CALL VEQU ( DAVV, RECORD(12) )


      ELSE IF ( SUBTYP .EQ. C06TP3 ) THEN
C
C        No transformation required.
C
         CALL MOVED ( QUAT, 4, RECORD    )
         CALL VEQU  ( AVV,     RECORD(5) )

      ELSE

         CALL SETMSG ( 'SUBTYP was #.' )
         CALL ERRINT ( '#', SUBTYP     )
         CALL SIGERR ( 'SPICE(BUG)'    )
C         CALL CHKOUT ( 'T_XSUBTP' )
         RETURN

      END IF

C      CALL CHKOUT ( 'T_XSUBTP' )
      RETURN
      END
