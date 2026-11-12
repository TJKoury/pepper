!$Procedure F_SPK19 ( SPK data type 19 tests )

      SUBROUTINE F_SPK19 ( OK )
      use pepper_legacy
      use pepper_inc_spk19

! $ Abstract
!
!     Exercise routines associated with SPK data type 19.
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
!     This routine tests the following routines that write and read
!     type 19 SPK data:
!
!        SPKE19
!        SPKR19
!        SPKW19
!
!     The SPK type 19 subsetter has its own test family.
!
!     The higher level SPK routine
!
!        SPKPVN
!
!     is also exercised by this test family.
!
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
!-    TSPICE Version 2.0.0, 21-DEC-2015 (NJB)
!
!        Updated to support subtype 2.
!
!-    TSPICE Version 1.0.0, 24-DEC-2013 (NJB)
!
!
!-&

!
!     SPICELIB functions
!


!
!     Local Parameters
!
      CHARACTER*(*)         SPK19E
      PARAMETER           ( SPK19E = 'test19err.bsp' )

      CHARACTER*(*)         SP19T0
      PARAMETER           ( SP19T0  = 'sp19t0.bsp' )

      CHARACTER*(*)         SP19T1
      PARAMETER           ( SP19T1  = 'sp19t1.bsp' )

      CHARACTER*(*)         SP19T2
      PARAMETER           ( SP19T2  = 'sp19t2.bsp' )


      DOUBLE PRECISION      TIGHT
      PARAMETER           ( TIGHT  = 1.D-14 )



      INTEGER               MAXIVL
      PARAMETER           ( MAXIVL = 21000 )

      INTEGER               BIGN
      PARAMETER           ( BIGN   = MAXIVL * 100 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

      INTEGER               MSGLEN
      PARAMETER           ( MSGLEN = 800 )

      INTEGER               NDSCRT
      PARAMETER           ( NDSCRT = 9 )

      INTEGER               SIDLEN
      PARAMETER           ( SIDLEN = 60 )

      INTEGER               FRNMLN
      PARAMETER           ( FRNMLN = 32 )

!
!     Local Variables
!
      CHARACTER*(LNSIZE)    NAME
      CHARACTER*(SIDLEN)    SEGID
      CHARACTER*(MSGLEN)    TITLE
      CHARACTER*(FRNMLN)    XREF

      DOUBLE PRECISION      DSCEPC (     NDSCRT )
      DOUBLE PRECISION      DSCSTS ( 6,  NDSCRT )
      DOUBLE PRECISION      EPOCHS ( BIGN )
      DOUBLE PRECISION      ENDEPC
      DOUBLE PRECISION      ESCALE
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      FIRST
      DOUBLE PRECISION      IVLBDS ( MAXIVL + 1 )
      DOUBLE PRECISION      LAST
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      MIDET
      DOUBLE PRECISION      MIDSTA ( 6 )
      DOUBLE PRECISION      PACKTS ( BIGN * S19PS0 )
      DOUBLE PRECISION      STATE  ( 6 )
      DOUBLE PRECISION      XSTATE ( 6 )
      DOUBLE PRECISION      XVALS  ( MAXDEG + 1 )
      DOUBLE PRECISION      YVALS  ( MAXDEG + 1 )
      DOUBLE PRECISION      WORK   ( MAXDEG + 1 )

      INTEGER               B
      INTEGER               DEGRES ( MAXIVL )
      INTEGER               E
      INTEGER               EPCTO
      INTEGER               HANDLE
      INTEGER               I
      INTEGER               J
      INTEGER               K
      INTEGER               LB
      INTEGER               NINTVL
      INTEGER               NPKTS  ( MAXIVL )
      INTEGER               PKTSIZ
      INTEGER               PKTTO
      INTEGER               PKTTOT
      INTEGER               RECTOT
      INTEGER               SUBTPS ( MAXIVL )
      INTEGER               UB
      INTEGER               XBODY
      INTEGER               XCENTR
      INTEGER               WINSIZ

      LOGICAL               SELLST

!
!     Saved values
!
      SAVE

!
!     Initial values
!


!
!     Epochs and states:
!
      DATA  DSCEPC / 100.D0, 200.D0, 300.D0, 400.D0, 500.D0, &
                     600.D0, 700.D0, 800.D0, 900.D0         /

      DATA  DSCSTS / &
                    101.D0, 201.D0, 301.D0, 401.D0, 501.D0, 601.D0, &
                    102.D0, 202.D0, 302.D0, 402.D0, 502.D0, 602.D0, &
                    103.D0, 203.D0, 303.D0, 403.D0, 503.D0, 603.D0, &
                    104.D0, 204.D0, 304.D0, 404.D0, 504.D0, 604.D0, &
                    105.D0, 205.D0, 305.D0, 405.D0, 505.D0, 605.D0, &
                    106.D0, 206.D0, 306.D0, 406.D0, 506.D0, 606.D0, &
                    107.D0, 207.D0, 307.D0, 407.D0, 507.D0, 607.D0, &
                    108.D0, 208.D0, 308.D0, 408.D0, 508.D0, 608.D0, &
                    109.D0, 209.D0, 309.D0, 409.D0, 509.D0, 609.D0  /



!
!     Open the test family.
!
      CALL TOPEN ( 'F_SPK19' )


!*****************************************************************
!*
!*    SPKW19 error cases:
!*
!*****************************************************************


!
!     Test SPKW19:  start out with error handling.
!
!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'SPKW19 error case: bad frame name.'   )

!
!     Initialize the inputs to SPKW19.
!
      XBODY  = 3
      XCENTR = 10
      XREF   = 'J2000'
!
!     Create a segment identifier.
!
      SEGID = 'SPK type 19 test segment'

!
!     Open a new SPK file.
!
      IF ( EXISTS(SPK19E) ) THEN
         CALL DELFIL(SPK19E)
      END IF

      CALL SPKOPN( SPK19E, 'Type 19 SPK internal file name', 4, HANDLE )
      CALL CHCKXC( .FALSE., ' ', OK )


      NINTVL    = 1
      NPKTS(1)  = 2
      SUBTPS(1) = S19TP1
      DEGRES(1) = 1


      RECTOT = 0
      PKTTOT = 0

      DO I = 1, NINTVL
!
!        Update total counts of records and packet elements.
!        The latter depends on the packet size of the current
!        mini-segment.
!
         IF ( SUBTPS(I) .EQ. S19TP0 ) THEN

            PKTSIZ = S19PS0

         ELSE IF ( SUBTPS(I) .EQ. S19TP1 ) THEN

            PKTSIZ = S19PS1

         ELSE

            CALL SETMSG ( 'Unexpected SPK type 19 subtype # ' &
            //            'found in mini-segment #.'          )
            CALL ERRINT ( '#',  SUBTPS(I)                     )
            CALL ERRINT ( '#',  I                             )
            CALL SIGERR ( 'SPICE(INVALIDSUBTYPE)'             )


            CALL CHCKXC( .FALSE., ' ', OK )

         END IF

         PKTTOT = PKTTOT + ( NPKTS(I) * PKTSIZ )

         RECTOT = RECTOT + NPKTS(I)

      END DO

      CALL CLEARD ( PKTTOT, PACKTS )


      DO I = 1, RECTOT
         EPOCHS(I) = DBLE(I)
      END DO

      FIRST     = EPOCHS(1)
      LAST      = EPOCHS(2)

      IVLBDS(1) = FIRST
      IVLBDS(2) = LAST

      SELLST    = .TRUE.

      CALL SPKW19 ( HANDLE, &
                    XBODY, &
                    XCENTR, &
                    'SPUD', &
                    FIRST, &
                    LAST, &
                    SEGID, &
                    NINTVL, &
                    NPKTS, &
                    SUBTPS, &
                    DEGRES, &
                    PACKTS, &
                    EPOCHS, &
                    IVLBDS, &
                    SELLST           )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDREFFRAME)', OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'SPKW19 error case: SEGID too long.'   )

      CALL SPKW19 ( HANDLE, &
                    XBODY, &
                    XCENTR, &
                    XREF, &
                    FIRST, &
                    LAST, &
                    'X                                               X', &
                    NINTVL, &
                    NPKTS, &
                    SUBTPS, &
                    DEGRES, &
                    PACKTS, &
                    EPOCHS, &
                    IVLBDS, &
                    SELLST           )

      CALL CHCKXC ( .TRUE., 'SPICE(SEGIDTOOLONG)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'SPKW19 error case: unprintable SEGID characters.' )

      CALL SPKW19 ( HANDLE, &
                    XBODY, &
                    XCENTR, &
                    XREF, &
                    FIRST, &
                    LAST, &
                    CHAR(7), &
                    NINTVL, &
                    NPKTS, &
                    SUBTPS, &
                    DEGRES, &
                    PACKTS, &
                    EPOCHS, &
                    IVLBDS, &
                    SELLST           )

      CALL CHCKXC ( .TRUE., 'SPICE(NONPRINTABLECHARS)', OK )


!
! --- Case: ------------------------------------------------------
!


      CALL TCASE ( 'SPKW19 error case: non-positive interval count' )

      CALL SPKW19 ( HANDLE, &
                    XBODY, &
                    XCENTR, &
                    XREF, &
                    FIRST, &
                    LAST, &
                    SEGID, &
                    0, &
                    NPKTS, &
                    SUBTPS, &
                    DEGRES, &
                    PACKTS, &
                    EPOCHS, &
                    IVLBDS, &
                    SELLST           )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDCOUNT)', OK )


      NINTVL = 1



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'SPKW19 error case: non-increasing interval bounds' )


      IVLBDS(2) = IVLBDS(1)
      FIRST     = 1.5D0
      LAST      = 1.6D0

      CALL SPKW19 ( HANDLE, &
                    XBODY, &
                    XCENTR, &
                    XREF, &
                    FIRST, &
                    LAST, &
                    SEGID, &
                    NINTVL, &
                    NPKTS, &
                    SUBTPS, &
                    DEGRES, &
                    PACKTS, &
                    EPOCHS, &
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
      CALL TCASE ( 'SPKW19 error case: polynomial degree too high.' )

      DEGRES(1) = 40

      CALL SPKW19 ( HANDLE, &
                    XBODY, &
                    XCENTR, &
                    XREF, &
                    FIRST, &
                    LAST, &
                    SEGID, &
                    NINTVL, &
                    NPKTS, &
                    SUBTPS, &
                    DEGRES, &
                    PACKTS, &
                    EPOCHS, &
                    IVLBDS, &
                    SELLST           )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDDEGREE)', OK )

      DEGRES(1) = 1


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'SPKW19 error case: polynomial degree too low.' )

      DEGRES(1) = 0

      CALL SPKW19 ( HANDLE, &
                    XBODY, &
                    XCENTR, &
                    XREF, &
                    FIRST, &
                    LAST, &
                    SEGID, &
                    NINTVL, &
                    NPKTS, &
                    SUBTPS, &
                    DEGRES, &
                    PACKTS, &
                    EPOCHS, &
                    IVLBDS, &
                    SELLST           )


      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDDEGREE)', OK )

      DEGRES(1) = 1


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'SPKW19 error case:  odd window size.' )

      DEGRES(1) = 2

      CALL SPKW19 ( HANDLE, &
                    XBODY, &
                    XCENTR, &
                    XREF, &
                    FIRST, &
                    LAST, &
                    SEGID, &
                    NINTVL, &
                    NPKTS, &
                    SUBTPS, &
                    DEGRES, &
                    PACKTS, &
                    EPOCHS, &
                    IVLBDS, &
                    SELLST           )


      CALL CHCKXC ( .TRUE., 'SPICE(BADWINDOWSIZE)', OK )

      SUBTPS(1) = S19TP0

      CALL SPKW19 ( HANDLE, &
                    XBODY, &
                    XCENTR, &
                    XREF, &
                    FIRST, &
                    LAST, &
                    SEGID, &
                    NINTVL, &
                    NPKTS, &
                    SUBTPS, &
                    DEGRES, &
                    PACKTS, &
                    EPOCHS, &
                    IVLBDS, &
                    SELLST           )


      CALL CHCKXC ( .TRUE., 'SPICE(BADWINDOWSIZE)', OK )

      SUBTPS(1) = S19TP1
      DEGRES(1) = 1


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'SPKW19 error case: too few states' )

      NPKTS(1) = 1

      CALL SPKW19 ( HANDLE, &
                    XBODY, &
                    XCENTR, &
                    XREF, &
                    FIRST, &
                    LAST, &
                    SEGID, &
                    NINTVL, &
                    NPKTS, &
                    SUBTPS, &
                    DEGRES, &
                    PACKTS, &
                    EPOCHS, &
                    IVLBDS, &
                    SELLST           )


      CALL CHCKXC ( .TRUE., 'SPICE(TOOFEWPACKETS)', OK )

      NPKTS(1) = 2

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'SPKW19 error case: descriptor times swapped.' )


      CALL SPKW19 ( HANDLE, &
                    XBODY, &
                    XCENTR, &
                    XREF, &
                    LAST, &
                    FIRST, &
                    SEGID, &
                    NINTVL, &
                    NPKTS, &
                    SUBTPS, &
                    DEGRES, &
                    PACKTS, &
                    EPOCHS, &
                    IVLBDS, &
                    SELLST           )


      CALL CHCKXC ( .TRUE., 'SPICE(BADDESCRTIMES)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'SPKW19 error case: descriptor start time is ' // &
                   'too early.'                                   )


      CALL SPKW19 ( HANDLE, &
                    XBODY, &
                    XCENTR, &
                    XREF, &
                    FIRST - 1.D0, &
                    LAST, &
                    SEGID, &
                    NINTVL, &
                    NPKTS, &
                    SUBTPS, &
                    DEGRES, &
                    PACKTS, &
                    EPOCHS, &
                    IVLBDS, &
                    SELLST           )

      CALL CHCKXC ( .TRUE., 'SPICE(COVERAGEGAP)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'SPKW19 error case: descriptor end time is ' // &
                   'too late.'                                  )

      CALL SPKW19 ( HANDLE, &
                    XBODY, &
                    XCENTR, &
                    XREF, &
                    FIRST, &
                    LAST + 1.D0, &
                    SEGID, &
                    NINTVL, &
                    NPKTS, &
                    SUBTPS, &
                    DEGRES, &
                    PACKTS, &
                    EPOCHS, &
                    IVLBDS, &
                    SELLST           )

      CALL CHCKXC ( .TRUE., 'SPICE(COVERAGEGAP)', OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'SPKW19 error case: epochs for first ' // &
                   'mini-segment are out of order.'      )

      NINTVL    = 1
      NPKTS(1)  = 4

      DO I = 1, NPKTS(1)
         EPOCHS(I) = DBLE(I)
      END DO

      FIRST     = EPOCHS( 1 )
      LAST      = EPOCHS( NPKTS(1) )

      IVLBDS(1) = FIRST
      IVLBDS(2) = LAST

      CALL SWAPD ( EPOCHS(2), EPOCHS(3) )

      CALL SPKW19 ( HANDLE, &
                    XBODY, &
                    XCENTR, &
                    XREF, &
                    FIRST, &
                    LAST, &
                    SEGID, &
                    NINTVL, &
                    NPKTS, &
                    SUBTPS, &
                    DEGRES, &
                    PACKTS, &
                    EPOCHS, &
                    IVLBDS, &
                    SELLST           )

      CALL CHCKXC ( .TRUE., 'SPICE(TIMESOUTOFORDER)', OK )

      CALL SWAPD ( EPOCHS(2), EPOCHS(3) )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'SPKW19 error case: epochs for first ' &
      //           'mini-segment don''t cover first interpolation ' &
      //           'interval.'                                     )

      NINTVL    = 1
      NPKTS(1)  = 4

      DO I = 1, NPKTS(1)
         EPOCHS(I) = DBLE(I)
      END DO

      FIRST     = EPOCHS( 1 )
      LAST      = EPOCHS( NPKTS(1) )

      IVLBDS(1) = FIRST
      IVLBDS(2) = LAST

      EPOCHS(1) = EPOCHS(1) + 0.5D0

      CALL SPKW19 ( HANDLE, &
                    XBODY, &
                    XCENTR, &
                    XREF, &
                    FIRST, &
                    LAST, &
                    SEGID, &
                    NINTVL, &
                    NPKTS, &
                    SUBTPS, &
                    DEGRES, &
                    PACKTS, &
                    EPOCHS, &
                    IVLBDS, &
                    SELLST           )

      CALL CHCKXC ( .TRUE., 'SPICE(BOUNDSDISAGREE)', OK )

      EPOCHS(1) = 1.D0
      EPOCHS(4) = EPOCHS(4) - 0.5D0

      CALL SPKW19 ( HANDLE, &
                    XBODY, &
                    XCENTR, &
                    XREF, &
                    FIRST, &
                    LAST, &
                    SEGID, &
                    NINTVL, &
                    NPKTS, &
                    SUBTPS, &
                    DEGRES, &
                    PACKTS, &
                    EPOCHS, &
                    IVLBDS, &
                    SELLST           )

      CALL CHCKXC ( .TRUE., 'SPICE(BOUNDSDISAGREE)', OK )


      DO I = 1, NPKTS(1)
         EPOCHS(I) = DBLE(I)
      END DO


!
!     End of SPKW19 error cases.
!


!
!     Close the SPK file at the DAF level; SPKCLS won't close
!     a file without segments.
!
      CALL DAFCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )




!
!     SPKW19 Non-error cases:
!

!*****************************************************************
!*
!*    Trivial case: write an SPK containing a small subtype 1
!*    segment.
!*
!*****************************************************************

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'Write a 1-interval subtype 1 segment.' )

!
!     This file lacks both interval directories and mini-segment
!     epoch directories.
!
!
!     Initialize the inputs to SPKW19.
!
      XBODY  = 3
      XCENTR = 10
      XREF   = 'J2000'
!
!     Create a segment identifier.
!
      SEGID = 'SPK type 19 test segment'


      CALL SPKOPN ( SP19T1, SP19T1, 0, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


      NINTVL    = 1
      NPKTS(1)  = NDSCRT
      SUBTPS(1) = S19TP1
      DEGRES(1) = 3

      FIRST     = DSCEPC( 3 )
      LAST      = DSCEPC( NPKTS(1)-2 )

      IVLBDS(1) = DSCEPC(2)
      IVLBDS(2) = DSCEPC( NPKTS(1)-1 )

      SELLST    = .TRUE.

      CALL SPKW19 ( HANDLE, &
                    XBODY, &
                    XCENTR, &
                    XREF, &
                    FIRST, &
                    LAST, &
                    SEGID, &
                    NINTVL, &
                    NPKTS, &
                    SUBTPS, &
                    DEGRES, &
                    DSCSTS, &
                    DSCEPC, &
                    IVLBDS, &
                    SELLST           )

      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL SPKCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'Read states from the file just created. ' &
      //            'Epochs match input epochs.'               )


      CALL FURNSH ( SP19T1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XBODY  = 3
      XCENTR = 10
      XREF   = 'J2000'

      DO I = 3, NPKTS(1)-2

         ET = DSCEPC(I)

         CALL SPKEZ  ( XBODY, ET, XREF, 'NONE', XCENTR, STATE, LT )
         CALL CHCKXC ( .FALSE., ' ', OK )

         IF ( OK ) THEN

            NAME = 'State #.'
            CALL REPMI ( NAME, '#', I, NAME )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKAD ( NAME, STATE, '~~', DSCSTS(1,I), 6, TIGHT, OK )

         END IF

      END DO



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'Read states from the file just created. ' &
      //            'Epochs are at midpoints between adjacent ' &
      //            'input epochs.'                             )



      XBODY  = 3
      XCENTR = 10
      XREF   = 'J2000'

      DO I = 3, NPKTS(1)-3

         ET = (  DSCEPC(I) + DSCEPC(I+1)  ) / 2

         CALL SPKEZ  ( XBODY, ET, XREF, 'NONE', XCENTR, STATE, LT )
         CALL CHCKXC ( .FALSE., ' ', OK )

         IF ( OK ) THEN

            NAME = 'State #.'
            CALL REPMI ( NAME, '#', I, NAME )
            CALL CHCKXC ( .FALSE., ' ', OK )

            DO J = 1, 6
               XSTATE(J) = ( DSCSTS(J,I) + DSCSTS(J,I+1) ) / 2
            END DO

            CALL CHCKAD ( NAME, STATE, '~~', XSTATE, 6, TIGHT, OK )

         END IF

      END DO

      CALL UNLOAD ( SP19T1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Remove the trivial SPK we created; we'll create a new
!     one with more complexity.
!
      CALL DELFIL ( SP19T1 )
      CALL CHCKXC ( .FALSE., ' ', OK )



!*****************************************************************
!*
!*    Test subtype 2: write an SPK containing a small subtype 2
!*    mini-segment.
!*
!*****************************************************************

!
!     Below, it will be convenient to use cubic interpolation and a
!     window size of 2.

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'Create an SPK with subtype 2 mini-segments.' )


      CALL SPKOPN ( SP19T2, SP19T2, 0, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Set the interval count.
!
      NINTVL = 10

!
!     Set the packet counts, subtypes, and degrees.
!
!     Since we're concentrating on interval selection
!     logic, we'll use small packet counts.
!
      DO I = 1, NINTVL

         SUBTPS(I) = S19TP2
         DEGRES(I) = 3

         IF ( ODD(I) ) THEN

            NPKTS(I)  = 3
         ELSE
            NPKTS(I)  = 4
         END IF

      END DO

!
!     Generate epochs and packets.
!
      EPCTO = 0
!
!     The end epoch of each interval must be the start epoch of
!     the next.
!
      ENDEPC = 0.D0

      DO I = 1, NINTVL
!
!        Set the interval start time.
!
         IVLBDS(I) = ENDEPC

!
!        Set the separation of epochs.
!
         IF ( ODD(I) ) THEN

            ESCALE = 10.D0
         ELSE
            ESCALE = 20.D0
         END IF


         DO J = 1, NPKTS(I)

            EPCTO = EPCTO + 1

            EPOCHS( EPCTO ) = ENDEPC  +  ( ESCALE * (J-1) )

         END DO

!
!        Save the last epoch of this interval.
!
         ENDEPC = EPOCHS( EPCTO )

      END DO

!
!     Save the stop time of the last interval.
!
      IVLBDS( NINTVL+1 ) = ENDEPC

!
!     Set the descriptor bounds. We don't need to do anything
!     fancy here.
!
      FIRST = IVLBDS( 1          )
      LAST  = IVLBDS( NINTVL + 1 )

!
!     Now assign the packets. Since we're using two-point
!     interpolation, selecting the wrong packets to interpolate
!     should yield an obviously wrong answer.
!
!     We also want to have large packet discontinuities at
!     interval boundaries, so the consequence of selecting
!     the wrong interval is obvious.
!

      PKTTO = 0

      DO I = 1, NINTVL
!
!        Assign the packets for the Ith interval.
!
         DO J = 1, NPKTS(I)
!
!           Position components:
!
            DO K = 1, 3

               PKTTO = PKTTO + 1

               PACKTS( PKTTO ) =   (  1.D9  * I ) &
                                 + (  1.D2  * J ) &
                                 +            K
            END DO

!
!           Velocities associated with positions:
!
            DO K = 1, 3

               PKTTO = PKTTO + 1

               PACKTS ( PKTTO ) = 0.D0

            END DO

         END DO

      END DO


!
!     For this segment, we'll select the last applicable interval
!     when the request time lies on an interval boundary.
!
      SELLST  = .TRUE.

      CALL SPKW19 ( HANDLE, &
                    XBODY, &
                    XCENTR, &
                    XREF, &
                    FIRST, &
                    LAST, &
                    SEGID, &
                    NINTVL, &
                    NPKTS, &
                    SUBTPS, &
                    DEGRES, &
                    PACKTS, &
                    EPOCHS, &
                    IVLBDS, &
                    SELLST           )

      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL SPKCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'Read states from type 2 file. ' &
      //            'SELLST = .TRUE.'                )


      CALL FURNSH ( SP19T2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      IF ( OK ) THEN
!
!        Read states corresponding to epochs.
!
         DO I = 1, NINTVL

            IF ( ODD(I) ) THEN

               ESCALE = 10.D0
            ELSE
               ESCALE = 20.D0
            END IF

!
!           Note that the last epoch of each interval,
!           except the last, is a special case. The
!           state returned for that epoch is computed
!           from data in the following interval, if any.
!
            IF ( I .LT. NINTVL ) THEN

               UB = NPKTS(I) - 1
            ELSE
               UB = NPKTS(I)
            END IF

            DO J = 1, UB

!
! --- Case: ------------------------------------------------------
!
               ET    =  IVLBDS(I)  +  ( ESCALE * (J-1) )

               TITLE = 'State at interval #; packet index #'

               CALL REPMI ( TITLE, '#', I, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMI ( TITLE, '#', J, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )
               CALL TCASE  ( TITLE )



               CALL SPKEZ ( XBODY,  ET,     XREF, &
                            'NONE', XCENTR, STATE, LT )
               CALL CHCKXC ( .FALSE., ' ', OK )


               DO K = 1, 3

                  XSTATE(K) =      (  1.D9  * I ) &
                                 + (  1.D2  * J ) &
                                 +            K

                  XSTATE(K+3) = 0.D0
!
!                 Also compute the state at the midpoint
!                 between the Jth epoch and the next. Because
!                 the cubics are fit to endpoints with
!                 zero derivatives, the function value
!                 at the midpoint is just the average of
!                 the values at the endpoints.
!
!                 The midpoint velocity should be 3/2 * the difference
!                 between the Jth and (J+1)st positions, divided by
!                 the epoch difference. This can be derived from the
!                 simple case
!
!                              3       2
!                    p(x) = a x  +  a x  +  a x  +  a
!                            3       2       1       0
!
!
!                    p(0)      = alpha
!                    p(1)      = beta
!                    dp/dx (0) = 0
!                    dp/dx (1) = 0
!
!
!
                  MIDSTA(K)   = XSTATE(K) + ( 1.D2 * 0.5D0 )
                  MIDSTA(K+3) = ( 1.5D0 / ESCALE ) * 1.D2

               END DO

!
!              Check position at the Jth epoch.
!
               CALL CHCKAD ( 'Position', STATE,     '~~/', &
                                         XSTATE,    3, TIGHT, OK )
!
!              Check velocity at the Jth epoch.
!
               CALL CHCKAD ( 'Velocity', STATE(4),  '~~/', &
                                         XSTATE(4), 3, TIGHT, OK )


!
! --- Case: ------------------------------------------------------
!

!
!              Perform this test only if the packet index
!              is not the last of the interval.
!
               IF ( J .LT. NPKTS(I) ) THEN

                  TITLE = 'State in interval #; epoch midway ' &
                  //      'between that of packet index # ' &
                  //      'and packet index #.'

                  CALL REPMI ( TITLE, '#', I, TITLE )
                  CALL CHCKXC ( .FALSE., ' ', OK )

                  CALL REPMI ( TITLE, '#', J, TITLE )
                  CALL CHCKXC ( .FALSE., ' ', OK )

                  CALL REPMI ( TITLE, '#', J+1, TITLE )
                  CALL CHCKXC ( .FALSE., ' ', OK )

                  CALL TCASE  ( TITLE )

                  MIDET =  ET +  ( ESCALE * 0.5D0 )


                  CALL SPKEZ ( XBODY,  MIDET, XREF, &
                               'NONE', XCENTR, STATE, LT )
                  CALL CHCKXC ( .FALSE., ' ', OK )

!
!                 Check position midway between the Jth and J+1st
!                 epochs.
!
                  CALL CHCKAD ( 'Mid pos',  STATE,     '~~/', &
                                            MIDSTA,    3, TIGHT, OK )
!
!                 Check velocity midway between the Jth and J+1st
!                 epochs.
!
                  CALL CHCKAD ( 'Mid vel',  STATE(4),  '~~/', &
                                            MIDSTA(4), 3, TIGHT, OK )

               END IF


            END DO

         END DO



      END IF

      CALL UNLOAD ( SP19T2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Delete the file we created.
!
      CALL DELFIL ( SP19T2 )
      CALL CHCKXC ( .FALSE., ' ', OK )







!*****************************************************************
!*
!*    Test subtype 2: write an SPK containing a small subtype 2
!*    mini-segment. This time use different velocities for each
!*    component.
!*
!*****************************************************************

!
!     Below, we'll use linear functions for each position component.
!     The rates for each component will be distinct.
!
!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'Create an SPK with subtype 2 mini-segments. ' &
      //            'Use linear position functions with different ' &
      //            'rates for each component.'                    )


      CALL SPKOPN ( SP19T2, SP19T2, 0, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Set the interval count.
!
      NINTVL = 10

!
!     Set the packet counts, subtypes, and degrees.
!
!     Since we're concentrating on interval selection
!     logic, we'll use small packet counts.
!
      DO I = 1, NINTVL

         SUBTPS(I) = S19TP2
         DEGRES(I) = 3

         IF ( ODD(I) ) THEN

            NPKTS(I)  = 3
         ELSE
            NPKTS(I)  = 4
         END IF

      END DO

!
!     Generate epochs and packets.
!
      EPCTO = 0
!
!     The end epoch of each interval must be the start epoch of
!     the next.
!
      ENDEPC = 0.D0

      DO I = 1, NINTVL
!
!        Set the interval start time.
!
         IVLBDS(I) = ENDEPC

!
!        Set the separation of epochs.
!
         IF ( ODD(I) ) THEN

            ESCALE = 10.D0
         ELSE
            ESCALE = 20.D0
         END IF


         DO J = 1, NPKTS(I)

            EPCTO = EPCTO + 1

            EPOCHS( EPCTO ) = ENDEPC  +  ( ESCALE * (J-1) )

         END DO

!
!        Save the last epoch of this interval.
!
         ENDEPC = EPOCHS( EPCTO )

      END DO

!
!     Save the stop time of the last interval.
!
      IVLBDS( NINTVL+1 ) = ENDEPC

!
!     Set the descriptor bounds. We don't need to do anything
!     fancy here.
!
      FIRST = IVLBDS( 1          )
      LAST  = IVLBDS( NINTVL + 1 )

!
!     Now assign the packets. Since we're using two-point
!     interpolation, selecting the wrong packets to interpolate
!     should yield an obviously wrong answer.
!
!     We also want to have large packet discontinuities at
!     interval boundaries, so the consequence of selecting
!     the wrong interval is obvious.
!

      PKTTO = 0

      DO I = 1, NINTVL

         IF ( ODD(I) ) THEN

            ESCALE = 10.D0
         ELSE
            ESCALE = 20.D0
         END IF

!
!        Assign the packets for the Ith interval.
!
         DO J = 1, NPKTS(I)
!
!           Position components:
!
            DO K = 1, 3

               PKTTO = PKTTO + 1

               PACKTS( PKTTO ) =   (  1.D9  * I     ) &
                                 + (  1.D2  * J * K ) &
                                 +    K
            END DO

!
!           Velocities associated with positions:
!
            DO K = 1, 3

               PKTTO = PKTTO + 1

               PACKTS ( PKTTO ) = ( 1.D2 * K ) / ESCALE

            END DO

         END DO

      END DO


!
!     For this segment, we'll select the last applicable interval
!     when the request time lies on an interval boundary.
!
      SELLST  = .TRUE.

      CALL SPKW19 ( HANDLE, &
                    XBODY, &
                    XCENTR, &
                    XREF, &
                    FIRST, &
                    LAST, &
                    SEGID, &
                    NINTVL, &
                    NPKTS, &
                    SUBTPS, &
                    DEGRES, &
                    PACKTS, &
                    EPOCHS, &
                    IVLBDS, &
                    SELLST           )

      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL SPKCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'Read states from constant rate type 2 file. ' &
      //            'SELLST = .TRUE.'                             )


      CALL FURNSH ( SP19T2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      IF ( OK ) THEN
!
!        Read states corresponding to epochs.
!
         DO I = 1, NINTVL

            IF ( ODD(I) ) THEN

               ESCALE = 10.D0
            ELSE
               ESCALE = 20.D0
            END IF

!
!           Note that the last epoch of each interval,
!           except the last, is a special case. The
!           state returned for that epoch is computed
!           from data in the following interval, if any.
!
            IF ( I .LT. NINTVL ) THEN

               UB = NPKTS(I) - 1
            ELSE
               UB = NPKTS(I)
            END IF

            DO J = 1, UB

!
! --- Case: ------------------------------------------------------
!
               ET    =  IVLBDS(I)  +  ( ESCALE * (J-1) )

               TITLE = 'State at interval #; packet index #; ' &
               //      'subtype 2; linear case'

               CALL REPMI ( TITLE, '#', I, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMI ( TITLE, '#', J, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )
               CALL TCASE  ( TITLE )



               CALL SPKEZ ( XBODY,  ET,     XREF, &
                            'NONE', XCENTR, STATE, LT )
               CALL CHCKXC ( .FALSE., ' ', OK )


               DO K = 1, 3

                  XSTATE(K) =      (  1.D9  * I ) &
                                 + (  1.D2  * J  * K ) &
                                 +    K

                  XSTATE(K+3) =  ( 1.D2 * K ) / ESCALE

!
!                 Also compute the state at the midpoint
!                 between the Jth epoch and the next.
!
                  MIDSTA(K)   = XSTATE(K) + ( 1.D2 * K * 0.5D0 )
                  MIDSTA(K+3) = 1.D2 * K / ESCALE

               END DO

!
!              Check position at the Jth epoch.
!
               CALL CHCKAD ( 'Position', STATE,     '~~/', &
                                         XSTATE,    3, TIGHT, OK )
!
!              Check velocity at the Jth epoch.
!
               CALL CHCKAD ( 'Velocity', STATE(4),  '~~/', &
                                         XSTATE(4), 3, TIGHT, OK )


!
! --- Case: ------------------------------------------------------
!

!
!              Perform this test only if the packet index
!              is not the last of the interval.
!
               IF ( J .LT. NPKTS(I) ) THEN

                  TITLE = 'State in interval #; epoch midway ' &
                  //      'between that of packet index # ' &
                  //      'and packet index #. Subtype 2; ' &
                  //      'linear case.'

                  CALL REPMI ( TITLE, '#', I, TITLE )
                  CALL CHCKXC ( .FALSE., ' ', OK )

                  CALL REPMI ( TITLE, '#', J, TITLE )
                  CALL CHCKXC ( .FALSE., ' ', OK )

                  CALL REPMI ( TITLE, '#', J+1, TITLE )
                  CALL CHCKXC ( .FALSE., ' ', OK )

                  CALL TCASE  ( TITLE )

                  MIDET =  ET +  ( ESCALE * 0.5D0 )


                  CALL SPKEZ ( XBODY,  MIDET, XREF, &
                               'NONE', XCENTR, STATE, LT )
                  CALL CHCKXC ( .FALSE., ' ', OK )

!
!                 Check position midway between the Jth and J+1st
!                 epochs.
!
                  CALL CHCKAD ( 'Mid pos',  STATE,     '~~/', &
                                            MIDSTA,    3, TIGHT, OK )
!
!                 Check velocity midway between the Jth and J+1st
!                 epochs.
!
                  CALL CHCKAD ( 'Mid vel',  STATE(4),  '~~/', &
                                            MIDSTA(4), 3, TIGHT, OK )

               END IF

            END DO

         END DO

      END IF

      CALL UNLOAD ( SP19T2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Delete the file we created.
!
      CALL DELFIL ( SP19T2 )
      CALL CHCKXC ( .FALSE., ' ', OK )




!*****************************************************************
!*
!*    SPKR19 window selection logic cases:
!*
!*****************************************************************


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'Create a subtype 1 segment suitable for testing ' &
      //            'interpolation window packet selection.'          )
!
!     The position and velocity data in this segment are defined by
!     exponential functions of time.
!
      CALL SPKOPN ( SP19T1, SP19T1, 0, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Initialize the inputs to SPKW19.
!
      XBODY  = 3
      XCENTR = 10
      XREF   = 'J2000'
!
!     Create a segment identifier.
!
      SEGID = 'SPK type 19 test segment'


      NINTVL    = 1
      NPKTS(1)  = 2 * ( MAXDEG + 1 )
      SUBTPS(1) = S19TP1
      DEGRES(1) = MAXDEG

!
!     Assign the packet epochs.
!
      DO I = 1, NPKTS(1)

         EPOCHS(I) = DBLE( I )

      END DO

      FIRST     = EPOCHS ( 1        )
      LAST      = EPOCHS ( NPKTS(1) )

      IVLBDS(1) = FIRST
      IVLBDS(2) = LAST

!
!     Assign the packet values.
!
      PKTTO = 0

      DO I = 1, NPKTS(1)

         DO J = 1, 6

            PKTTO = PKTTO + 1

            PACKTS(PKTTO) = ( 10.D0**J )  * EXP ( I / 100.D0 )

         END DO

      END DO


      SELLST    = .TRUE.

      CALL SPKW19 ( HANDLE, &
                    XBODY, &
                    XCENTR, &
                    XREF, &
                    FIRST, &
                    LAST, &
                    SEGID, &
                    NINTVL, &
                    NPKTS, &
                    SUBTPS, &
                    DEGRES, &
                    PACKTS, &
                    EPOCHS, &
                    IVLBDS, &
                    SELLST           )

      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL SPKCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'Look up states at midpoints between packet ' &
      //            'epochs of segment we just created.'         )


!
!     Touch array WORK to make f2c happy. WORK is simply a workspace
!     array.
!
      CALL CLEARD ( MAXDEG + 1, WORK )

      CALL FURNSH ( SP19T1 )
      CALL CHCKXC ( .FALSE., ' ', OK )


      WINSIZ = MAXDEG + 1

      DO I = 1, NPKTS(1) - 1

         ET = ( EPOCHS(I) + EPOCHS(I+1) ) / 2

         CALL SPKEZ ( XBODY, ET, XREF, 'NONE', XCENTR, STATE, LT )

         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Generate expected state components at ET.
!
!
!        Let B and E be the begin and end indices of the
!        packets comprising the interpolation window.
!
         B = MAX ( 1,         I - ( (WINSIZ/2) - 1 )     )
         E = MIN ( NPKTS(1),  I + ( (WINSIZ/2) - 1 ) + 1 )

         DO J = 1, 6

            DO K = B, E

               XVALS( K-B+1 ) =  EPOCHS( K )

               YVALS( K-B+1 ) =  ( 10.D0**J ) * EXP ( K / 100.D0 )

            END DO

            XSTATE(J) = LGRINT ( E-B+1, XVALS, YVALS, WORK, ET )

            CALL CHCKXC ( .FALSE., ' ', OK )

         END DO

!
!        Test the state at ET.
!
         DO J = 1, 6
!
! --- Case: ------------------------------------------------------
!
            TITLE = 'State # component #'
            CALL REPMI ( TITLE, '#', I, TITLE )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL REPMI ( TITLE, '#', J, TITLE )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL TCASE ( TITLE )

            CALL CHCKSD ( 'State', STATE(J), '~/', &
                                   XSTATE(J), TIGHT, OK )


          END DO


      END DO


!
!     Delete the file we created so we can start over.
!
      CALL UNLOAD ( SP19T1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( SP19T1 )
      CALL CHCKXC ( .FALSE., ' ', OK )



!*****************************************************************
!*
!*    SPKR19 interval re-use logic cases:
!*
!*****************************************************************





!*****************************************************************
!*
!*    SPKR19 subtype logic cases:
!*
!*****************************************************************





!*****************************************************************
!*
!*    SPKR19 interval selection logic cases:
!*
!*****************************************************************


!
!     Below, we'll use type 0 segments because it will
!     be convenient to use cubic interpolation and
!     a window size of 2.
!
!     The velocity/acceleration data will not be compatible with the
!     position/velocity data.
!

!
! --- Case: ------------------------------------------------------
!

!
!     We need a segment containing enough interval directories
!     so that multiple buffers full of directory entries must
!     be read. This implies we need more than DIRSIZ**2 intervals
!     in the segment.
!


      CALL TCASE  ( 'Create an SPK with interval ' &
      //            'directories.'                )


      CALL SPKOPN ( SP19T0, SP19T0, 0, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Set the interval count.
!
      NINTVL = MAXIVL

!
!     Set the packet counts, subtypes, and degrees.
!
!     Since we're concentrating on interval selection
!     logic, we'll use small packet counts.
!
      DO I = 1, NINTVL

         SUBTPS(I) = S19TP0
         DEGRES(I) = 3

         IF ( ODD(I) ) THEN

            NPKTS(I)  = 3
         ELSE
            NPKTS(I)  = 4
         END IF

      END DO

!
!     Generate epochs and packets.
!
      EPCTO = 0
!
!     The end epoch of each interval must be the start epoch of
!     the next.
!
      ENDEPC = 0.D0

      DO I = 1, NINTVL
!
!        Set the interval start time.
!
         IVLBDS(I) = ENDEPC

!
!        Set the separation of epochs.
!
         IF ( ODD(I) ) THEN

            ESCALE = 10.D0
         ELSE
            ESCALE = 20.D0
         END IF


         DO J = 1, NPKTS(I)

            EPCTO = EPCTO + 1

            EPOCHS( EPCTO ) = ENDEPC  +  ( ESCALE * (J-1) )

         END DO

!
!        Save the last epoch of this interval.
!
         ENDEPC = EPOCHS( EPCTO )

      END DO

!
!     Save the stop time of the last interval.
!
      IVLBDS( NINTVL+1 ) = ENDEPC

!
!     Set the descriptor bounds. We don't need to do anything
!     fancy here.
!
      FIRST = IVLBDS( 1          )
      LAST  = IVLBDS( NINTVL + 1 )

!
!     Now assign the packets. Since we're using two-point
!     interpolation, selecting the wrong packets to interpolate
!     should yield an obviously wrong answer.
!
!     We also want to have large packet discontinuities at
!     interval boundaries, so the consequence of selecting
!     the wrong interval is obvious.
!

      PKTTO = 0

      DO I = 1, NINTVL
!
!        Assign the packets for the Ith interval.
!
         DO J = 1, NPKTS(I)

!
!           Position components:
!
            DO K = 1, 3

               PKTTO = PKTTO + 1

               PACKTS( PKTTO ) =   (  1.D9  * I ) &
                                 + (  1.D2  * J ) &
                                 +            K
            END DO

!
!           Velocities associated with positions:
!
            DO K = 1, 3

               PKTTO = PKTTO + 1

               PACKTS ( PKTTO ) = 0.D0

            END DO

!
!           Velocity/acceleration packets:
!
            DO K = 1, 6

               PKTTO = PKTTO + 1

               PACKTS ( PKTTO ) = PACKTS ( PKTTO - 6 ) * 10.D0

            END DO


         END DO

      END DO


!
!     For this segment, we'll select the last applicable interval
!     when the request time lies on an interval boundary.
!
      SELLST  = .TRUE.

      CALL SPKW19 ( HANDLE, &
                    XBODY, &
                    XCENTR, &
                    XREF, &
                    FIRST, &
                    LAST, &
                    SEGID, &
                    NINTVL, &
                    NPKTS, &
                    SUBTPS, &
                    DEGRES, &
                    PACKTS, &
                    EPOCHS, &
                    IVLBDS, &
                    SELLST           )

      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL SPKCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'Read states from large type 0 file. ' &
      //            'SELLST = .TRUE.'                     )


      CALL FURNSH ( SP19T0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      IF ( OK ) THEN
!
!        Read states corresponding to epochs.
!
         DO I = 1, NINTVL

            IF ( ODD(I) ) THEN

               ESCALE = 10.D0
            ELSE
               ESCALE = 20.D0
            END IF

!
!           Note that the last epoch of each interval,
!           except the last, is a special case. The
!           state returned for that epoch is computed
!           from data in the following interval, if any.
!
            IF ( I .LT. NINTVL ) THEN

               UB = NPKTS(I) - 1
            ELSE
               UB = NPKTS(I)
            END IF

            DO J = 1, UB

!
! --- Case: ------------------------------------------------------
!
               ET    =  IVLBDS(I)  +  ( ESCALE * (J-1) )

               TITLE = 'State at interval #; packet index #'

               CALL REPMI ( TITLE, '#', I, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMI ( TITLE, '#', J, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )
               CALL TCASE  ( TITLE )



               CALL SPKEZ ( XBODY,  ET,     XREF, &
                            'NONE', XCENTR, STATE, LT )
               CALL CHCKXC ( .FALSE., ' ', OK )


               DO K = 1, 3

                  XSTATE(K) =      (  1.D9  * I ) &
                                 + (  1.D2  * J ) &
                                 +            K

                  XSTATE(K+3) = 10.D0 * XSTATE(K)

!
!                 Also compute the state at the midpoint
!                 between the Jth epoch and the next. Because
!                 the cubics are fit to endpoints with
!                 zero derivatives, the function value
!                 at the midpoint is just the average of
!                 the values at the endpoints.
!
                  MIDSTA(K)   = XSTATE(K) + ( 1.D2 * 0.5D0 )
                  MIDSTA(K+3) = 10.D0 * MIDSTA(K)

               END DO

!
!              Check position at the Jth epoch.
!
               CALL CHCKAD ( 'Position', STATE,     '~~/', &
                                         XSTATE,    3, TIGHT, OK )
!
!              Check velocity at the Jth epoch.
!
               CALL CHCKAD ( 'Velocity', STATE(4),  '~~/', &
                                         XSTATE(4), 3, TIGHT, OK )



!
! --- Case: ------------------------------------------------------
!

!
!              Perform this test only if the packet index
!              is not the last of the interval.
!
               IF ( J .LT. NPKTS(I) ) THEN

                  TITLE = 'State in interval #; epoch midway ' &
                  //      'between that of packet index # ' &
                  //      'and packet index #.'

                  CALL REPMI ( TITLE, '#', I, TITLE )
                  CALL CHCKXC ( .FALSE., ' ', OK )

                  CALL REPMI ( TITLE, '#', J, TITLE )
                  CALL CHCKXC ( .FALSE., ' ', OK )

                  CALL REPMI ( TITLE, '#', J+1, TITLE )
                  CALL CHCKXC ( .FALSE., ' ', OK )

                  CALL TCASE  ( TITLE )

                  MIDET =  ET +  ( ESCALE * 0.5D0 )


                  CALL SPKEZ ( XBODY,  MIDET, XREF, &
                               'NONE', XCENTR, STATE, LT )
                  CALL CHCKXC ( .FALSE., ' ', OK )

!
!                 Check position midway between the Jth and J+1st
!                 epochs.
!
                  CALL CHCKAD ( 'Mid pos',  STATE,     '~~/', &
                                            MIDSTA,    3, TIGHT, OK )
!
!                 Check velocity midway between the Jth and J+1st
!                 epochs.
!
                  CALL CHCKAD ( 'Mid vel',  STATE(4),  '~~/', &
                                            MIDSTA(4), 3, TIGHT, OK )

               END IF

            END DO

         END DO



      END IF

      CALL UNLOAD ( SP19T0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Delete the file we created so we can start over.
!
      CALL DELFIL ( SP19T0 )
      CALL CHCKXC ( .FALSE., ' ', OK )




!
! --- Case: ------------------------------------------------------
!

!
!
!     Repeat the previous cases using a segment for which the
!     first applicable interval is picked.
!
      CALL SPKOPN ( SP19T0, SP19T0, 0, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      SELLST = .FALSE.

      CALL SPKW19 ( HANDLE, &
                    XBODY, &
                    XCENTR, &
                    XREF, &
                    FIRST, &
                    LAST, &
                    SEGID, &
                    NINTVL, &
                    NPKTS, &
                    SUBTPS, &
                    DEGRES, &
                    PACKTS, &
                    EPOCHS, &
                    IVLBDS, &
                    SELLST           )

      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL SPKCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'Read states from large type 0 file. ' &
      //            'SELLST = .FALSE.'                     )


      CALL FURNSH ( SP19T0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      IF ( OK ) THEN
!
!        Read states corresponding to epochs.
!
         DO I = 1, NINTVL

            IF ( ODD(I) ) THEN

               ESCALE = 10.D0
            ELSE
               ESCALE = 20.D0
            END IF

!
!           Note that the first epoch of each interval,
!           except the first, is a special case. The
!           state returned for that epoch is computed
!           from data in the preceding interval, if any.
!
            IF ( I .GT. 1 ) THEN
               LB = 2
            ELSE
               LB = 1
            END IF

            DO J = LB, NPKTS(I)
!
! --- Case: ------------------------------------------------------
!
               ET    =  IVLBDS(I)  +  ( ESCALE * (J-1) )

               TITLE = 'State at interval #; packet index #'

               CALL REPMI ( TITLE, '#', I, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMI ( TITLE, '#', J, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )
               CALL TCASE  ( TITLE )



               CALL SPKEZ ( XBODY,  ET,     XREF, &
                            'NONE', XCENTR, STATE, LT )
               CALL CHCKXC ( .FALSE., ' ', OK )


               DO K = 1, 3

                  XSTATE(K) =      (  1.D9  * I ) &
                                 + (  1.D2  * J ) &
                                 +            K

                  XSTATE(K+3) = 10.D0 * XSTATE(K)

!
!                 Also compute the state at the midpoint
!                 between the Jth epoch and the J-1st. Because
!                 the cubics are fit to endpoints with
!                 zero derivatives, the function value
!                 at the midpoint is just the average of
!                 the values at the endpoints.
!
                  MIDSTA(K)   = XSTATE(K) - ( 1.D2 * 0.5D0 )
                  MIDSTA(K+3) = 10.D0 * MIDSTA(K)

               END DO

!
!              Check position at the Jth epoch.
!
               CALL CHCKAD ( 'Position', STATE,     '~~/', &
                                         XSTATE,    3, TIGHT, OK )
!
!              Check velocity at the Jth epoch.
!
               CALL CHCKAD ( 'Velocity', STATE(4),  '~~/', &
                                         XSTATE(4), 3, TIGHT, OK )



!
! --- Case: ------------------------------------------------------
!

!
!              Perform the following test only if a preceding
!              index exists.
!
               IF ( J .GT. 1 ) THEN

                  TITLE = 'State in interval #; epoch midway ' &
                  //      'between that of packet index # ' &
                  //      'and packet index #.'

                  CALL REPMI ( TITLE, '#', I, TITLE )
                  CALL CHCKXC ( .FALSE., ' ', OK )

                  CALL REPMI ( TITLE, '#', J, TITLE )
                  CALL CHCKXC ( .FALSE., ' ', OK )

                  CALL REPMI ( TITLE, '#', J-1, TITLE )
                  CALL CHCKXC ( .FALSE., ' ', OK )

                  CALL TCASE  ( TITLE )

                  MIDET =  ET -  ( ESCALE * 0.5D0 )

                  CALL SPKEZ ( XBODY,  MIDET, XREF, &
                            'NONE', XCENTR, STATE, LT )
                  CALL CHCKXC ( .FALSE., ' ', OK )

!
!                 Check position midway between the Jth and J-1st
!                 epochs.
!
                  CALL CHCKAD ( 'Mid pos',  STATE,     '~~/', &
                                            MIDSTA,    3, TIGHT, OK )
!
!                 Check velocity midway between the Jth and J-1st
!                 epochs.
!
                  CALL CHCKAD ( 'Mid vel',  STATE(4),  '~~/', &
                                            MIDSTA(4), 3, TIGHT, OK )

               END IF

            END DO

         END DO



      END IF

      CALL UNLOAD ( SP19T0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Delete the SPK file.
!
      CALL DELFIL ( SP19T0 )
      CALL CHCKXC ( .FALSE., ' ', OK )




!*****************************************************************
!*
!*    SPKR19 packet selection logic cases:
!*
!*****************************************************************


!
!     Below, we'll use type 0 segments because it will
!     be convenient to use cubic interpolation and
!     a window size of 2.
!
!     The velocity/acceleration data will not be compatible with the
!     position/velocity data.
!

!
! --- Case: ------------------------------------------------------
!

!
!     We must now test the packet selection logic. The first
!     aspect to test is use of packet directories. We must have
!     at least one interval that contains enough directories so
!     that they can't all be buffered. It's all right to have
!     a small number of intervals for this test.
!

      CALL TCASE  ( 'Create an SPK with interval ' &
      //            'directories.'                )


      CALL SPKOPN ( SP19T0, SP19T0, 0, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Set the interval count.
!
      NINTVL = 2

!
!     Set the packet counts, subtypes, and degrees.
!
      DO I = 1, NINTVL

         SUBTPS(I) = S19TP0
         DEGRES(I) = 3

         IF ( ODD(I) ) THEN

            NPKTS(I)  = MAXIVL - 100
         ELSE
            NPKTS(I)  = MAXIVL
         END IF

      END DO

!
!     Generate epochs and packets.
!
      EPCTO = 0
!
!     The end epoch of each interval must be the start epoch of
!     the next.
!
      ENDEPC = 0.D0

      DO I = 1, NINTVL
!
!        Set the interval start time.
!
         IVLBDS(I) = ENDEPC

!
!        Set the separation of epochs.
!
         IF ( ODD(I) ) THEN

            ESCALE = 1.D0
         ELSE
            ESCALE = 2.D0
         END IF


         DO J = 1, NPKTS(I)

            EPCTO = EPCTO + 1

            EPOCHS( EPCTO ) = ENDEPC  +  ( ESCALE * (J-1) )

         END DO

!
!        Save the last epoch of this interval.
!
         ENDEPC = EPOCHS( EPCTO )

      END DO

!
!     Save the stop time of the last interval.
!
      IVLBDS( NINTVL+1 ) = ENDEPC

!
!     Set the descriptor bounds. We don't need to do anything
!     fancy here.
!
      FIRST = IVLBDS( 1          )
      LAST  = IVLBDS( NINTVL + 1 )

!
!     Now assign the packets. Since we're using two-point
!     interpolation, selecting the wrong packets to interpolate
!     should yield an obviously wrong answer.
!
!     We also want to have large packet discontinuities at
!     interval boundaries, so the consequence of selecting
!     the wrong interval is obvious.
!

      PKTTO = 0

      DO I = 1, NINTVL
!
!        Assign the packets for the Ith interval.
!
         DO J = 1, NPKTS(I)

!
!           Position components:
!
            DO K = 1, 3

               PKTTO = PKTTO + 1

               PACKTS( PKTTO ) =   (  1.D9  * I ) &
                                 + (  1.D2  * J ) &
                                 +            K
            END DO

!
!           Velocities associated with positions:
!
            DO K = 1, 3

               PKTTO = PKTTO + 1

               PACKTS ( PKTTO ) = 0.D0

            END DO

!
!           Velocity/acceleration packets:
!
            DO K = 1, 6

               PKTTO = PKTTO + 1

               PACKTS ( PKTTO ) = PACKTS ( PKTTO - 6 ) * 10.D0

            END DO


         END DO

      END DO


!
!     For this segment, we'll select the last applicable interval
!     when the request time lies on an interval boundary.
!
      SELLST  = .TRUE.

      CALL SPKW19 ( HANDLE, &
                    XBODY, &
                    XCENTR, &
                    XREF, &
                    FIRST, &
                    LAST, &
                    SEGID, &
                    NINTVL, &
                    NPKTS, &
                    SUBTPS, &
                    DEGRES, &
                    PACKTS, &
                    EPOCHS, &
                    IVLBDS, &
                    SELLST           )

      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL SPKCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'Read states from large type 0 file. ' &
      //            'Packet selection test. SELLST = .TRUE.' )


      CALL FURNSH ( SP19T0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      IF ( OK ) THEN
!
!        Read states corresponding to epochs.
!
         DO I = 1, NINTVL

            IF ( ODD(I) ) THEN

               ESCALE = 1.D0
            ELSE
               ESCALE = 2.D0
            END IF

!
!           Note that the last epoch of each interval,
!           except the last, is a special case. The
!           state returned for that epoch is computed
!           from data in the following interval, if any.
!
            IF ( I .LT. NINTVL ) THEN

               UB = NPKTS(I) - 1
            ELSE
               UB = NPKTS(I)
            END IF

            DO J = 1, UB

!
! --- Case: ------------------------------------------------------
!
               ET    =  IVLBDS(I)  +  ( ESCALE * (J-1) )

               TITLE = 'State at interval #; packet index #'

               CALL REPMI ( TITLE, '#', I, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMI ( TITLE, '#', J, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )
               CALL TCASE  ( TITLE )



               CALL SPKEZ ( XBODY,  ET,     XREF, &
                            'NONE', XCENTR, STATE, LT )
               CALL CHCKXC ( .FALSE., ' ', OK )


               DO K = 1, 3

                  XSTATE(K) =      (  1.D9  * I ) &
                                 + (  1.D2  * J ) &
                                 +            K

                  XSTATE(K+3) = 10.D0 * XSTATE(K)

!
!                 Also compute the state at the midpoint
!                 between the Jth epoch and the next. Because
!                 the cubics are fit to endpoints with
!                 zero derivatives, the function value
!                 at the midpoint is just the average of
!                 the values at the endpoints.
!
                  MIDSTA(K)   = XSTATE(K) + ( 1.D2 * 0.5D0 )
                  MIDSTA(K+3) = 10.D0 * MIDSTA(K)

               END DO

!
!              Check position at the Jth epoch.
!
               CALL CHCKAD ( 'Position', STATE,     '~~/', &
                                         XSTATE,    3, TIGHT, OK )
!
!              Check velocity at the Jth epoch.
!
               CALL CHCKAD ( 'Velocity', STATE(4),  '~~/', &
                                         XSTATE(4), 3, TIGHT, OK )



!
! --- Case: ------------------------------------------------------
!

!
!              Perform this test only if the packet index
!              is not the last of the interval.
!
               IF ( J .LT. NPKTS(I) ) THEN

                  TITLE = 'State in interval #; epoch midway ' &
                  //      'between that of packet index # ' &
                  //      'and packet index #.'

                  CALL REPMI ( TITLE, '#', I, TITLE )
                  CALL CHCKXC ( .FALSE., ' ', OK )

                  CALL REPMI ( TITLE, '#', J, TITLE )
                  CALL CHCKXC ( .FALSE., ' ', OK )

                  CALL REPMI ( TITLE, '#', J+1, TITLE )
                  CALL CHCKXC ( .FALSE., ' ', OK )

                  CALL TCASE  ( TITLE )

                  MIDET =  ET +  ( ESCALE * 0.5D0 )


                  CALL SPKEZ ( XBODY,  MIDET, XREF, &
                               'NONE', XCENTR, STATE, LT )
                  CALL CHCKXC ( .FALSE., ' ', OK )

!
!                 Check position midway between the Jth and J+1st
!                 epochs.
!
                  CALL CHCKAD ( 'Mid pos',  STATE,     '~~/', &
                                            MIDSTA,    3, TIGHT, OK )
!
!                 Check velocity midway between the Jth and J+1st
!                 epochs.
!
                  CALL CHCKAD ( 'Mid vel',  STATE(4),  '~~/', &
                                            MIDSTA(4), 3, TIGHT, OK )

               END IF

            END DO

         END DO



      END IF

      CALL UNLOAD ( SP19T0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Delete the file we created so we can start over.
!
      CALL DELFIL ( SP19T0 )
      CALL CHCKXC ( .FALSE., ' ', OK )








!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'Clean up:  delete SPK files.' )

      IF (  EXISTS( SPK19E )  ) THEN
         CALL DELFIL ( SPK19E )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF

      IF (  EXISTS( SP19T0 )  ) THEN
         CALL DELFIL ( SP19T0 )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF

      IF (  EXISTS( SP19T1 )  ) THEN
         CALL DELFIL ( SP19T1 )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF

!
!     Close out the test family.
!
      CALL T_SUCCESS ( OK )
      RETURN
      END


