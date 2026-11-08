!$Procedure F_SPK03 ( SPK type 3 tests )

      SUBROUTINE F_SPK03 ( OK )

! $ Abstract
!
!     Exercise routines associated with SPK data type 3.
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

      INCLUDE 'spkrec.inc'
      INCLUDE 'spk03.inc'

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
!     This routine tests routines specific to SPK data type 3.
!
!     In addition, the type 3 reader and evaluator routines
!     are indirectly exercised by this test family.
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
!-    TSPICE Version 2.0.0, 18-JAN-2014 (NJB)
!
!        Updated to use new short error messages for coverage gaps.
!        Added test cases for new SPKE03 and SPKW03 error checks.
!        Deleted declarations of unused variables. Re-ordered header
!        sections.
!
!-    TSPICE Version 1.0.0, 20-OCT-2005 (NJB)
!
!
!-&

!
!     SPICELIB functions
!
      LOGICAL               EXISTS

!
!     Local Parameters
!
      CHARACTER*(*)         SPK3
      PARAMETER           ( SPK3   = 'test3.bsp' )

      CHARACTER*(*)         SPK3E
      PARAMETER           ( SPK3E  = 'test3err.bsp' )

      CHARACTER*(*)         SPK3S
      PARAMETER           ( SPK3S  = 'test3sub.bsp' )

      CHARACTER*(*)         SPK3B
      PARAMETER           ( SPK3B  = 'test3big.bsp' )


      DOUBLE PRECISION      TIGHT
      PARAMETER           ( TIGHT  = 1.D-11 )


      INTEGER               BIGN
      PARAMETER           ( BIGN   = 10101 )

      INTEGER               CHBDEG
      PARAMETER           ( CHBDEG = 2 )

      INTEGER               DSCSIZ
      PARAMETER           ( DSCSIZ = 5 )

      INTEGER               NCHBRC
      PARAMETER           ( NCHBRC = 4 )

      INTEGER               ND
      PARAMETER           ( ND     = 2 )

      INTEGER               NEPOCH
      PARAMETER           ( NEPOCH = NCHBRC + 1 )

      INTEGER               NI
      PARAMETER           ( NI     = 6 )

      INTEGER               SIDLEN
      PARAMETER           ( SIDLEN = 60 )

      INTEGER               FRNMLN
      PARAMETER           ( FRNMLN = 32 )

!
!     Local Variables
!
      CHARACTER*(SIDLEN)    SEGID
      CHARACTER*(SIDLEN)    SEGID2
      CHARACTER*(FRNMLN)    XREF

      DOUBLE PRECISION      BEPLST (    BIGN+1 )
      DOUBLE PRECISION      CHBCF3 ( 0:CHBDEG, 6, NCHBRC )
      DOUBLE PRECISION      CHBCFB ( 0:CHBDEG, 6, BIGN   )
      DOUBLE PRECISION      DC     (    ND     )
      DOUBLE PRECISION      DESCR  (    DSCSIZ )
      DOUBLE PRECISION      DSCEPC (    NEPOCH )
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      INTLEN
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      MIDPT
      DOUBLE PRECISION      RADIUS
      DOUBLE PRECISION      RECORD ( MAXREC )
      DOUBLE PRECISION      STATE  ( 6 )
      DOUBLE PRECISION      T
      DOUBLE PRECISION      THETA
      DOUBLE PRECISION      XSTATE ( 6 )

      INTEGER               HANDLE
      INTEGER               I
      INTEGER               IC     ( NI )
      INTEGER               J
      INTEGER               K
      INTEGER               N
      INTEGER               NEWH
      INTEGER               XBODY
      INTEGER               XCENTR

      LOGICAL               FOUND


!
!     Saved values
!
      SAVE                  DSCEPC
      SAVE                  CHBCF3

!
!     Initial values
!

!
!     Epochs associated with coefficient sets.
!
      DATA  DSCEPC / 100.D0, 200.D0, 300.D0, 400.D0, 500.D0 /


!
!     Chebyshev coefficients for testing SPKW03.
!
      DATA  (   (  ( CHBCF3(K,J,I), K=0,2 ),  J=1,3  ), I=1,4  ) &
       / &
         1.0101D0,  1.0102D0,  1.0103D0, &
         1.0201D0,  1.0202D0,  1.0203D0, &
         1.0301D0,  1.0302D0,  1.0303D0, &
         2.0101D0,  2.0102D0,  2.0103D0, &
         2.0201D0,  2.0202D0,  2.0203D0, &
         2.0301D0,  2.0302D0,  2.0303D0, &
         3.0101D0,  3.0102D0,  3.0103D0, &
         3.0201D0,  3.0202D0,  3.0203D0, &
         3.0301D0,  3.0302D0,  3.0303D0, &
         4.0101D0,  4.0102D0,  4.0103D0, &
         4.0201D0,  4.0202D0,  4.0203D0, &
         4.0301D0,  4.0302D0,  4.0303D0 &
       /

      DATA  (  ( ( CHBCF3(K,J,I), K=0,2 ), J=4,6 ), I=1,4 ) &
       / &
        -1.0101D0, -1.0102D0, -1.0103D0, &
        -1.0201D0, -1.0202D0, -1.0203D0, &
        -1.0301D0, -1.0302D0, -1.0303D0, &
        -2.0101D0, -2.0102D0, -2.0103D0, &
        -2.0201D0, -2.0202D0, -2.0203D0, &
        -2.0301D0, -2.0302D0, -2.0303D0, &
        -3.0101D0, -3.0102D0, -3.0103D0, &
        -3.0201D0, -3.0202D0, -3.0203D0, &
        -3.0301D0, -3.0302D0, -3.0303D0, &
        -4.0101D0, -4.0102D0, -4.0103D0, &
        -4.0201D0, -4.0202D0, -4.0203D0, &
        -4.0301D0, -4.0302D0, -4.0303D0 &
       /

!
!     Statement functions
!
!
!     T(n,theta) represents the Chebyshev polynomial
!
!       T ( theta )
!        n
!
      T ( N, THETA ) = COS(  N*ACOS( MIN( 1.D0, MAX(-1.D0, THETA) ) )  )




!
!     Open the test family.
!
      CALL TOPEN ( 'F_SPK03' )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Setup' )

!
!     Create and load a leapseconds kernel.
!
      CALL TSTLSK
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Test SPKW03:  start out with error handling.
!

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Bad coefficient set count.' )

      IF ( EXISTS(SPK3E) ) THEN

         CALL DELFIL ( SPK3E )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END IF

      CALL SPKOPN ( SPK3E, 'Type 3 SPK internal file name', 4, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XBODY  = 301
      XCENTR = 3
      XREF   = 'J2000'
      SEGID  = 'SPK Type 3 test segment'

      INTLEN = DSCEPC(2) - DSCEPC(1)

      CALL SPKW03 ( HANDLE, &
                    XBODY, &
                    XCENTR, &
                    XREF, &
                    DSCEPC(1), &
                    DSCEPC(NCHBRC+1), &
                    SEGID, &
                    INTLEN, &
                    0, &
                    CHBDEG, &
                    CHBCF3, &
                    DSCEPC(1) )

      CALL CHCKXC ( .TRUE., 'SPICE(NUMCOEFFSNOTPOS)', OK )




!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Polynomial degree too high.' )

!
!     POLYDG = MAXDEG + 1
!
      CALL SPKW03 ( HANDLE, &
                    XBODY, &
                    XCENTR, &
                    XREF, &
                    DSCEPC(1), &
                    DSCEPC(NCHBRC+1), &
                    SEGID, &
                    INTLEN, &
                    NCHBRC, &
                    28, &
                    CHBCF3, &
                    DSCEPC(1) )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDDEGREE)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Polynomial degree too low.' )

      CALL SPKW03 ( HANDLE, &
                    XBODY, &
                    XCENTR, &
                    XREF, &
                    DSCEPC(1), &
                    DSCEPC(NCHBRC+1), &
                    SEGID, &
                    INTLEN, &
                    NCHBRC, &
                    -1, &
                    CHBCF3, &
                    DSCEPC(1) )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDDEGREE)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Invalid interval length.' )

      INTLEN = DSCEPC(1) - DSCEPC(2)

      CALL SPKW03 ( HANDLE, &
                    XBODY, &
                    XCENTR, &
                    XREF, &
                    DSCEPC(1), &
                    DSCEPC(NCHBRC+1), &
                    SEGID, &
                    INTLEN, &
                    NCHBRC, &
                    CHBDEG, &
                    CHBCF3, &
                    DSCEPC(1) )

      CALL CHCKXC ( .TRUE., 'SPICE(INTLENNOTPOS)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Invalid reference frame.' )

      INTLEN = DSCEPC(2) - DSCEPC(1)

      CALL SPKW03 ( HANDLE, &
                    XBODY, &
                    XCENTR, &
                    'SPUD', &
                    DSCEPC(1), &
                    DSCEPC(NCHBRC+1), &
                    SEGID, &
                    INTLEN, &
                    NCHBRC, &
                    CHBDEG, &
                    CHBCF3, &
                    DSCEPC(1) )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDREFFRAME)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Descriptor times out of order.' )

      INTLEN = DSCEPC(2) - DSCEPC(1)

      CALL SPKW03 ( HANDLE, &
                    XBODY, &
                    XCENTR, &
                    XREF, &
                    DSCEPC(NCHBRC+1), &
                    DSCEPC(1), &
                    SEGID, &
                    INTLEN, &
                    NCHBRC, &
                    CHBDEG, &
                    CHBCF3, &
                    DSCEPC(1) )

      CALL CHCKXC ( .TRUE., 'SPICE(BADDESCRTIMES)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Descriptor start time is too early.' )

      INTLEN = DSCEPC(2) - DSCEPC(1)

      CALL SPKW03 ( HANDLE, &
                    XBODY, &
                    XCENTR, &
                    XREF, &
                    DSCEPC(1) - 1.D-3, &
                    DSCEPC(NCHBRC+1), &
                    SEGID, &
                    INTLEN, &
                    NCHBRC, &
                    CHBDEG, &
                    CHBCF3, &
                    DSCEPC(1) )

      CALL CHCKXC ( .TRUE., 'SPICE(COVERAGEGAP)', OK )


      CALL SPKW03 ( HANDLE, &
                    XBODY, &
                    XCENTR, &
                    XREF, &
                    DSCEPC(1) - 1.D-13, &
                    DSCEPC(NCHBRC+1), &
                    SEGID, &
                    INTLEN, &
                    NCHBRC, &
                    CHBDEG, &
                    CHBCF3, &
                    DSCEPC(1) )
      CALL CHCKXC ( .FALSE., ' ', OK )



! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Descriptor stop time is too late.' )

      INTLEN = DSCEPC(2) - DSCEPC(1)

      CALL SPKW03 ( HANDLE, &
                    XBODY, &
                    XCENTR, &
                    XREF, &
                    DSCEPC(1), &
                    DSCEPC(NCHBRC+1) + 1.D-3, &
                    SEGID, &
                    INTLEN, &
                    NCHBRC, &
                    CHBDEG, &
                    CHBCF3, &
                    DSCEPC(1) )

      CALL CHCKXC ( .TRUE., 'SPICE(COVERAGEGAP)', OK )


      CALL SPKW03 ( HANDLE, &
                    XBODY, &
                    XCENTR, &
                    XREF, &
                    DSCEPC(1), &
                    DSCEPC(NCHBRC+1) + 1.D-13, &
                    SEGID, &
                    INTLEN, &
                    NCHBRC, &
                    CHBDEG, &
                    CHBCF3, &
                    DSCEPC(1) )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     Close this file.  Note that the file contains no segments,
!     so SPKCLS won't close it.
!
      CALL DAFCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Test SPKE03: bad coefficient count.' )

      CALL CLEARD ( MAXREC, RECORD )
      CALL CHCKXC ( .FALSE., ' ', OK )

      RECORD(1) = 0

      CALL SPKE03 ( 0.D0, RECORD, STATE )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDCOUNT)', OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Test SPKE03: bad interval radius.' )

      CALL CLEARD ( MAXREC, RECORD )
      CALL CHCKXC ( .FALSE., ' ', OK )

      RECORD(1) = 9
      RECORD(3) = 0.D0

      CALL SPKE03 ( 0.D0, RECORD, STATE )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDRADIUS)', OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Test SPKW03: write small segment.' )


      XBODY  = 3
      XCENTR = 10
      XREF   = 'J2000'

!
!     Create a segment identifier.
!
      SEGID = 'SPK type 3 test segment'

!
!     Open a new SPK file.
!
      IF ( EXISTS(SPK3) ) THEN
         CALL DELFIL(SPK3)
      END IF

      CALL SPKOPN ( SPK3, 'Type 3 SPK internal file name', 4, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


      INTLEN = DSCEPC(2) - DSCEPC(1)

!
!     Create a type 3 segment.
!
      CALL SPKW03 ( HANDLE, &
                    XBODY, &
                    XCENTR, &
                    XREF, &
                    DSCEPC(1), &
                    DSCEPC(NCHBRC+1), &
                    SEGID, &
                    INTLEN, &
                    NCHBRC, &
                    CHBDEG, &
                    CHBCF3, &
                    DSCEPC(1) )

      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Close the SPK file.
!
      CALL SPKCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Test SPKR03, SPKE03: read small segment.' )

!
!     Load the SPK file.
!
      CALL SPKLEF ( SPK3, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Look up states for each epoch in our list.  Compare.
!
      DO I = 1, NCHBRC
!
!        Generate look-up epoch ET.
!
         RADIUS = 0.5D0 * INTLEN
         MIDPT  = DSCEPC(I) + RADIUS

         ET     = MIDPT + 0.5D0 * RADIUS

         CALL SPKGEO ( XBODY, ET, XREF, XCENTR, STATE, LT )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Evaluate the position "manually."
!
         THETA = ( ET - MIDPT ) / RADIUS

         DO J = 1, 3

            XSTATE(J) = 0.D0

            DO K = 0, CHBDEG

               XSTATE(J  ) =  XSTATE(J) + CHBCF3(K,J,I) * T( K, THETA )
               XSTATE(J+3) = -XSTATE(J)

            END DO

         END DO

         CALL CHCKAD ( 'type 3 position', &
                       STATE, &
                       '~', &
                       XSTATE, &
                       3, &
                       TIGHT, &
                       OK             )

         CALL CHCKAD ( 'type 3 velocity', &
                       STATE(4), &
                       '~', &
                       XSTATE(4), &
                       3, &
                       TIGHT, &
                       OK             )

      END DO

!
!     Unload the SPK file.
!
      CALL SPKUEF ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Test SPKS03: write new file having a segment '  // &
                   'created by subsetting small segment from SPK3.' )


      XBODY  = 3
      XCENTR = 10
      XREF   = 'J2000'

!
!     Create a segment identifier.
!
      SEGID = 'SPK type 3 test subset segment'

!
!     Open a new SPK file.
!
      IF ( EXISTS(SPK3S) ) THEN
         CALL DELFIL(SPK3S)
      END IF

      CALL SPKOPN ( SPK3S, 'Type 3 SPK internal file name', 0, NEWH )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Open SPK3 and extract segment descriptor and ID of first segment.
!
      CALL DAFOPR ( SPK3, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFBFS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFFNA ( FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFGS  ( DESCR )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFGN  ( SEGID2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Create a type 3 segment in new file.  Shorten the time
!     coverage by knocking off the coverage contributed by
!     the first and last packets of the source segment.
!
      CALL SPKSUB ( HANDLE, &
                    DESCR, &
                    SEGID, &
                    DSCEPC(2), &
                    DSCEPC(NEPOCH-1), &
                    NEWH               )

      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Close the new SPK file.
!
      CALL SPKCLS ( NEWH )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Close the old SPK file.
!
      CALL SPKCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Test SPKS03: check descriptor bounds on ' // &
                   'subsetted file.'                          )

!
!     Open SPK3S and extract segment descriptor and ID of first
!     segment.
!
      CALL DAFOPR ( SPK3S, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFBFS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFFNA ( FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFGS  ( DESCR )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFUS  ( DESCR, ND, NI, DC, IC )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check the time bounds.
!
      I = NEPOCH-1

      CALL CHCKSD ( 'Segment start', DC(1), '=', DSCEPC(2), 0.D0, OK )
      CALL CHCKSD ( 'Segment end',   DC(2), '=', DSCEPC(I), 0.D0, OK )

      CALL SPKCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Test SPKS03: read states from subsetted file.' )

!
!     Load the SPK file.
!
      CALL SPKLEF ( SPK3S, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XBODY  = 3
      XCENTR = 10
      XREF   = 'J2000'
      INTLEN = DSCEPC(2) - DSCEPC(1)

!
!     Look up states for each epoch in our list.  Compare.
!
      DO I = 2, NCHBRC-1
!
!        Generate look-up epoch ET.
!
         RADIUS = 0.5D0 * INTLEN
         MIDPT  = DSCEPC(I) + RADIUS

         ET     = MIDPT + 0.5D0 * RADIUS

         CALL SPKGEO ( XBODY, ET, XREF, XCENTR, STATE, LT )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Evaluate the position "manually."
!
         THETA = ( ET - MIDPT ) / RADIUS

         DO J = 1, 3

            XSTATE(J) = 0.D0

            DO K = 0, CHBDEG

               XSTATE(J  ) =  XSTATE(J) + CHBCF3(K,J,I) * T( K, THETA )
               XSTATE(J+3) = -XSTATE(J)

            END DO

         END DO

         CALL CHCKAD ( 'type 3 position', &
                       STATE, &
                       '~', &
                       XSTATE, &
                       3, &
                       TIGHT, &
                       OK             )

         CALL CHCKAD ( 'type 3 velocity', &
                       STATE(4), &
                       '~', &
                       XSTATE(4), &
                       3, &
                       TIGHT, &
                       OK             )

      END DO

!
!     Unload the SPK file.
!
      CALL SPKUEF ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'SPKR03/SPKE03 test:  create a large segment with '// &
                   'multiple directories.'                            )

!
!     Create the coefficient and epoch values we'll use. We're going to
!     follow a pattern similar to that used for the smaller segments
!     created so far: each coefficient will have the value
!
!        I + J*10**-4 + K*10**-8
!
!     where I is the coefficient set index, J is the component (X,Y, or
!     Z) index, and K-1 is the associated degree.
!
      DO I = 1, BIGN

         DO J = 1, 3

            DO K = 0, CHBDEG

               CHBCFB(K, J,   I ) = I   +   J * 1.D-4   +   K * 1.D-8

               CHBCFB(K, J+3, I ) = - CHBCFB(K,J,I)

            END DO

         END DO

      END DO

      DO I = 1, BIGN+1
!
!        Initialize the Ith epoch.
!
         BEPLST(I) = 100 * I

      END DO


      XBODY  = 3
      XCENTR = 10
      XREF   = 'J2000'

!
!     Create a segment identifier.
!
      SEGID = 'SPK type 3 big test segment'

!
!     Open a new SPK file.
!
      IF ( EXISTS(SPK3B) ) THEN
         CALL DELFIL(SPK3B)
      END IF

      CALL SPKOPN ( SPK3B, 'Type 3 SPK internal file name', 0, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


      INTLEN = DSCEPC(2) - DSCEPC(1)

!
!     Create a type 3 segment.
!
      CALL SPKW03 ( HANDLE, &
                    XBODY, &
                    XCENTR, &
                    XREF, &
                    BEPLST(1), &
                    BEPLST(BIGN+1), &
                    SEGID, &
                    INTLEN, &
                    BIGN, &
                    CHBDEG, &
                    CHBCFB, &
                    BEPLST(1) )

      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Close the SPK file.
!
      CALL SPKCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Test SPKS03: read states from large file.' )

!
!     Load the SPK file.
!
      CALL SPKLEF ( SPK3B, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XBODY  = 3
      XCENTR = 10
      XREF   = 'J2000'
      INTLEN = DSCEPC(2) - DSCEPC(1)

!
!     Look up states for each epoch in our list.  Compare.
!
      DO I = 1, BIGN
!
!        Generate look-up epoch ET.
!
         RADIUS = 0.5D0 * INTLEN
         MIDPT  = BEPLST(I) + RADIUS

         ET     = MIDPT + 0.5D0 * RADIUS

         CALL SPKGEO ( XBODY, ET, XREF, XCENTR, STATE, LT )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Evaluate the position "manually."
!
         THETA = ( ET - MIDPT ) / RADIUS

         DO J = 1, 3

            XSTATE(J) = 0.D0

            DO K = 0, CHBDEG

               XSTATE(J)   =   XSTATE(J) + CHBCFB(K,J,I) * T( K, THETA )
               XSTATE(J+3) = - XSTATE(J)

            END DO

         END DO

         CALL CHCKAD ( 'type 3 position', &
                       STATE, &
                       '~', &
                       XSTATE, &
                       3, &
                       TIGHT, &
                       OK             )

         CALL CHCKAD ( 'type 3 velocity', &
                       STATE(4), &
                       '~', &
                       XSTATE(4), &
                       3, &
                       TIGHT, &
                       OK             )
      END DO
!
!     Unload the SPK file.
!
      CALL SPKUEF ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Deleting SPK files at clean-up time.' )

!
!     Clean up the SPK files.
!
      CALL DELFIL ( SPK3  )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( SPK3E )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( SPK3S )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( SPK3B )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Close out the test family.
!
      CALL T_SUCCESS ( OK )
      RETURN
      END


